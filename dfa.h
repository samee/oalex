/*  Copyright 2019 Google LLC

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

/* Future plans:
     * Add zero-width anchors: \b ^ $.
     * Add CopyEdge, CustomParserEdge.
       - CopyEdge must be the only non-LabelEdge going out of a node. This can
         even be an null-string transition.
     * Augment Dfa::labelsMap to support AND and NOT.
     * Add explicit support for returning errors and warnings from GssHooks.
     * Better error reporting features.

  Either:
     * Forbid PushEdge on entrypoints (stState or PushEdge::dest).
     * Allow full recursion.
  The former is closer to what we already have. The latter is cleaner. Requires:
     * charCanStart is recursive.
     * outsThruPushes is recursive.
     * No loops of PushEdge, checked with a DFS.
     * Limit maximum PushEdge depth to 100 to avoid stack overflow.
     * Prefix-free checks transitively expand PusheEdge closures.
     * GlrCtx::shift can now push multiple edges.
     * GlrCtx::enqueueAllLabelsInHead can now push multiple edges.
     * GssReduceLater can call use_value instead of extend if going from
       zero lengths.
     * GssEdge can now have zero lengths (check implicit assumptions).
 */
#pragma once

#include<functional>
#include<list>
#include<memory>
#include<queue>
#include<string_view>
#include<variant>
#include<vector>

#include"input_view.h"

namespace oalex {

struct DfaState { using int_type=int32_t; int_type to_int; };
struct DfaLabel { using int_type=int32_t; int_type to_int; };

inline bool operator!=(DfaState a,DfaState b) { return a.to_int!=b.to_int; }
inline bool operator==(DfaState a,DfaState b) { return a.to_int==b.to_int; }

inline bool operator!=(DfaLabel a,DfaLabel b) { return a.to_int!=b.to_int; }
inline bool operator==(DfaLabel a,DfaLabel b) { return a.to_int==b.to_int; }

struct CharRangeEdge { uint8_t st,en; DfaState dest; };  // inclusive range.
struct LabelEdge { DfaLabel lbl; DfaState dest; };
struct PushEdge { DfaState dest; };
struct StringEdge { std::string s; DfaState dest; };

using DfaEdge=std::variant<CharRangeEdge,LabelEdge,PushEdge,StringEdge>;

bool operator!=(const DfaEdge& e1,const DfaEdge& e2);
bool operator==(const DfaEdge& e1,const DfaEdge& e2);

std::string edgeDebug(const DfaEdge& e) noexcept;

// We may want to have priority over states rather than labels, to make merges
// easier in the main loop (see pseudocode in dfa.cpp).
struct Dfa {
  std::vector<std::vector<DfaEdge>> adjList;
  std::vector<std::vector<DfaLabel>> labelsMap;
  std::vector<int32_t> statePrioMap;
  DfaState stState;
  DfaLabel enLabel;

  const std::vector<DfaLabel>& labels(DfaState s) const
    { return labelsMap.at(s.to_int); }
  int32_t statePrio(DfaState s) const
    { return statePrioMap.at(s.to_int); }
  const std::vector<DfaEdge>& outOf(DfaState s) const
    { return adjList.at(s.to_int); }

  bool isEnState(DfaState s) const {
    for(DfaLabel l:labels(s)) if(l==enLabel) return true;
    return false;
  }

  // Empty string return means no error. Otherwise it's an error message.
  std::string checkError() const;
};

struct SemVal {
  size_t stPos,enPos;
  SemVal(size_t st,size_t en) : stPos(st),enPos(en) {}
  virtual ~SemVal() = default;
};

// Used by the default implementation of GssHooks::make_string.
struct StringVal : SemVal {
  std::string s;
  StringVal(size_t st,size_t en,std::string ss)
    : SemVal(st,en), s(std::move(ss)) {}
};
struct EmptyVal : SemVal { EmptyVal(size_t st,size_t en):SemVal(st,en){} };

// Gss manages data structure and ownership. Has nothing to do with string
// parsing, other than the fact that we store SemVals on each edge, and
// endpoints point to DfaStates. The "heads" actually return a vector of edges,
// not nodes. When at the head, the edges are "open ended" like this:
//
//      --*--- ...        |
//     /   \              |
//    /     \             |
//   * ------*-- ...      |
//    \                   |
//     --- ...            |
//
// Gss: Graph Structured Stack. It's a directed acyclic graph, but this term is
// common in GLR lingo.
//
// Each GssEdge represents a path of LabelEdges, where at most the first
// LabelEdge has push set to true. It does not store any information about the
// path other than the end state, though, so any information about the input
// substring represented by this edge should be included in the associated
// SemVal. The "size" of a GssEdge is the number of bytes it represents. Any
// two GssEdges with the same end state, size, and starting point get merged
// into one.
//
// Clarify: Note on July 27th: When did a LabelEdge have a 'push' attribute?
//
// The heads of the Gss are represented by GssHead, in that they don't have
// an end-point yet; they represent the "top" of the parsing stack. In contrast
// to GssEdges, GssHead represent a path of terminal edges. When they
// undergo a reduction, they turn into GssEdge.

using shared_value=std::shared_ptr<const SemVal>;

// GssHooks do not contain any mutable state by default. But implementations
// are free to have callback methods modify hook state if they so choose. This
// could easily be problematic since the same inputs can be repeatedly processed
// by hooks in undetermined order. Generally, keeping state here is discouraged.
// If necessary, it should only keep state describing the input string directly,
// not a particular SemVal or parsed AST, since those can get invalidated later.
class GssHooks {
 public:
  virtual shared_value extend(DfaState fromState,
      const DfaEdge& withEdge,
      const shared_value& fromVal,
      const shared_value& withVal) = 0;

  // nullptr return means parsing invalid.
  virtual shared_value
    make_string(size_t st,size_t en,std::string s) {
    return std::make_shared<StringVal>(st,en,std::move(s));
  }
  // Move or copy a value from val for lbl.
  // nullptr return means parsing invalid.
  virtual shared_value use_value(DfaLabel lbl,shared_value val) = 0;
  // Returning nullptr indicates parsing is invalid. We can discard both.
  virtual shared_value merge(DfaState en,
      shared_value v1,shared_value v2) = 0;
  virtual ~GssHooks() = default;
};

namespace internal {

// These objects are only created in closeHead, by converting from
// GssHead. They are usually merged as GssHead, before turning into GssEdges.
// This means we will never have two GssEdge objects with the same stPos(),
// enPos, and v.
struct GssEdge {
  shared_value v;
  DfaState enState;
  // Invariant: for all p,q in prev: p->enPos == q->enPos;
  size_t enPos;
  std::vector<std::shared_ptr<const GssEdge>> prev;
  size_t stPos() const { return prev.empty()?0:prev[0]->enPos; }
  size_t size() const { return enPos-stPos(); }
};

struct MidString { const StringEdge* se; size_t edgeStart; };

struct GssHead {
  shared_value v;
  std::variant<DfaState,MidString> enState;
  std::vector<std::shared_ptr<const GssEdge>> prev;
  size_t stPos() const { return prev.empty()?0:prev[0]->enPos; }
};

/* Fields:
     * legnth and statePrio are used for reduction priority. They determine
       the order of reductions.
     * lbl not sure if anybody will need this. We may remove this later.
     * The rest are arguments for either reduceValue or changeValue, depending
       on pushAgain.
   */
struct GssPendingReduce {
  size_t length;
  int32_t statePrio;  // dfa.statePrio(destinationState)
  GssHead h;  // A little bit heavyweight.
  std::shared_ptr<const GssEdge> oldPrev;  // Same as newPrev for use_value.
  const LabelEdge *labeledEdge;
  bool pushAgain;
};

using GssPendingQueue=std::priority_queue<GssPendingReduce,
      std::vector<GssPendingReduce>,
      bool(*)(const GssPendingReduce&,const GssPendingReduce&)>;

class GlrCtx {
  input_buffer buf_;
  std::list<internal::GssHead> heads_;
  const Dfa* dfa_;
  GssHooks* hooks_;
  friend class GlrCtxTest;

  size_t pos() const { return buf_.end_offset(); }
  void enqueueLabeledHeads(GssPendingQueue& q) const;
  void enqueueAllLabelsInHead(const internal::GssHead& h,
                              internal::GssPendingQueue& q,
      const internal::GssPendingReduce& curReduce) const;
  shared_value valFromString(const SemVal* sv) const;
  std::optional<internal::GssHead>
    reduceValue(const internal::GssEdge& prev,shared_value v,
                const DfaEdge& edge);
  std::optional<internal::GssHead> changeValue(
      std::shared_ptr<const internal::GssEdge> prev,
      shared_value v,const LabelEdge& edge);
  std::optional<internal::GssHead> mergeHeads(internal::GssHead h1,
                                              internal::GssHead h2);
  std::optional<internal::GssHead> mergeHeads(
      std::optional<internal::GssHead> h1,
      std::optional<internal::GssHead> h2);
  static GssHead startingHeadAt(DfaState s);
  struct SegfaultOnHooks {};
  void shiftAllPushEdges(DfaState s,char ch,std::list<GssHead>::iterator it);
  bool shiftTerminalEdge(
      char ch,std::variant<DfaState,MidString>& enState) const;
 public:
  GlrCtx(const Dfa& dfa,GssHooks& hk) : dfa_(&dfa), hooks_(&hk) {}
  // Used only in a unit test.
  GlrCtx(const Dfa& dfa,SegfaultOnHooks) : dfa_(&dfa), hooks_(nullptr) {}
  void shift(char ch);
  std::vector<shared_value> parse(std::function<int16_t()> getch);
};

}  // namespace internal

/*  glrParse(). Parse an input using GLR algorithm.
    Returns a vector of possible parse trees, as built up by the hooks provided.

    Args:
      dfa   - The DFA that defines the grammar. It is assumed Dfa::checkError
              passes. Otherwise, behavior is undefined.
      hk    - Provides recipes for building a shared_value in case of a
              successful parse.
      getch - The actual input source. It should return -1 on EOF or error.
              Right now we don't distinguish between these two cases.

    The parse trees are typically represented by some user-defined subclass of
    SemVal, as returned by the various components of hk. In case of parse
    failure, just returns an empty vector. Error reporting may be done by either
    recording errors in hk or by returning a "valid" shared_value that actually
    represents an error and not a parse tree.

    It never throws an exception, but it can BugDie(), either for internal bugs
    or for malformed dfa. Exceptions from hk and getch are all propagated
    unhindered, though.
*/
std::vector<shared_value> glrParse(
    const Dfa& dfa,GssHooks& hk,std::function<int16_t()> getch);

}  // namespace oalex
