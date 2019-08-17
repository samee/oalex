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
     * Add explicit support for returning errors and warnings from
       GssHooks.
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
     * GssReduceLater can call useVal instead of extend if going from
       zero lengths.
     * GssEdge can now have zero lengths (check implicit assumptions).
 */
#pragma once

#include<functional>
#include<iostream>
#include<list>
#include<memory>
#include<queue>
#include<string_view>
#include<variant>
#include<vector>

#include"input_view.h"

namespace oalex {

struct DfaState { using int_type=int32_t; int_type toInt; };
struct DfaLabel { using int_type=int32_t; int_type toInt; };

inline bool operator!=(DfaState a,DfaState b) { return a.toInt!=b.toInt; }
inline bool operator==(DfaState a,DfaState b) { return a.toInt==b.toInt; }

inline bool operator!=(DfaLabel a,DfaLabel b) { return a.toInt!=b.toInt; }
inline bool operator==(DfaLabel a,DfaLabel b) { return a.toInt==b.toInt; }

struct CharRangeEdge { uint8_t st,en; DfaState dest; };  // inclusive range.
struct LabelEdge { DfaLabel lbl; DfaState dest; };
struct PushEdge { DfaState dest; };
struct StringEdge { std::string s; DfaState dest; };

using DfaEdge=std::variant<CharRangeEdge,LabelEdge,PushEdge,StringEdge>;

bool operator!=(const DfaEdge& e1,const DfaEdge& e2);
bool operator==(const DfaEdge& e1,const DfaEdge& e2);

// Debugging aids.
inline std::ostream& operator<<(std::ostream& os,DfaState s) {
  return os<<"DfaState{"<<s.toInt<<'}';
}
inline std::ostream& operator<<(std::ostream& os,DfaLabel s) {
  return os<<"DfaLabel{"<<s.toInt<<'}';
}

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
    { return labelsMap.at(s.toInt); }
  int32_t statePrio(DfaState s) const
    { return statePrioMap.at(s.toInt); }
  const std::vector<DfaEdge>& outOf(DfaState s) const
    { return adjList.at(s.toInt); }

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

// Used by GlrCtx::valFromString.
struct StringVal : SemVal {
  std::string s;
  StringVal(size_t st,size_t en,std::string ss)
    : SemVal(st,en), s(std::move(ss)) {}
};
using SharedStringVal=std::shared_ptr<const StringVal>;

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

using SharedVal=std::shared_ptr<const SemVal>;

using SharedListVal=std::shared_ptr<const struct ListVal>;

struct ListVal : public SemVal {
  SharedListVal prev;
  SharedVal last;
  size_t size;
  SharedVal at(size_t i) const { return i+1==size?last:prev->at(i); }
  friend SharedListVal Append(SharedListVal prev,SharedVal last);  // factory
 private:
  ListVal(size_t st,size_t en) : SemVal(st,en) {}
};

// prev can be null, last must not be null. Corollary: size can't be 0.
inline SharedListVal Append(SharedListVal prev,SharedVal last) {
  ListVal lv(prev?prev->stPos:last->stPos,last->enPos);
  lv.size=(prev?prev->size+1:1);
  lv.prev=std::move(prev);
  lv.last=std::move(last);
  return std::make_shared<const ListVal>(std::move(lv));
}

// GssHooks do not contain any mutable state by default. But
// implementations are free to have callback methods modify hook state if they
// so choose. This could easily be problematic since the same inputs can be
// repeatedly processed by hooks in undetermined order. Generally, keeping
// state here is discouraged.  If necessary, it should only keep state
// describing the input string directly, not a particular SemVal or parsed AST,
// since those can get invalidated later.
//
// All of these may return nullptr to indicate invalid parsing.

class GssHooks {
 public:
  virtual SharedListVal merge(DfaState en,
                              SharedListVal lv1,SharedListVal lv2);
  virtual SharedVal reduceString(DfaLabel lbl,SharedStringVal sv);
  virtual SharedVal reduceList(DfaLabel lbl,SharedListVal lv) = 0;
};


namespace internal {

// These objects are only created in closeHead, by converting from
// GssHead. They are usually merged as GssHead, before turning into GssEdges.
// This means we will never have two GssEdge objects with the same stPos(),
// enPos, and v.
struct GssEdge {
  SharedVal v;
  DfaState enState;
  // Invariant: for all p,q in prev: p->enPos == q->enPos;
  size_t enPos;
  std::vector<std::shared_ptr<const GssEdge>> prev;
  size_t stPos() const { return prev.empty()?0:prev[0]->enPos; }
  size_t size() const { return enPos-stPos(); }
};

struct MidString { const StringEdge* se; size_t edgeStart; };

// v can only be one of:
//  - nullptr
//  - InputViewVal - internal type, used inside a string component.
//  - ListVal - used in label components.
// It is an error for GssHead::v to point to any other type of SharedVal.
//
// enState:
//  - MidString means we are in the middle of a StringEdge. It only exists
//    because we don't want to represent every character in a string as a
//    separate state. So for a StringEdge matching "foo", we'll use MidString
//    if we've seen "f" or "fo". If we haven't seen anything at all, we'll use
//    DfaState{start}, and if we've seen "foo", we'll use DfaState{end}.
//    This can only appear inside a string component.
//  - DfaState: used both for string component or label component.
struct GssHead {
  SharedVal v;
  std::variant<DfaState,MidString> enState;
  std::vector<std::shared_ptr<const GssEdge>> prev;
  size_t stPos() const { return prev.empty()?0:prev[0]->enPos; }
};

/* Fields:
     * legnth and statePrio are used for reduction priority. They determine
       the order of reductions.
     * lbl not sure if anybody will need this. We may remove this later.
     * The rest are arguments for either extendValue or changeValue, depending
       on pushAgain.
   */
struct GssPendingReduce {
  size_t length;
  int32_t statePrio;  // dfa.statePrio(destinationState)
  GssHead h;  // A little bit heavyweight.
  std::shared_ptr<const GssEdge> oldPrev;  // Same as newPrev for useVal.
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
  SharedVal valFromString(const SemVal* sv) const;
  std::optional<internal::GssHead>
    extendValue(const internal::GssEdge& prev,SharedVal v,
                const LabelEdge& edge);
  std::optional<internal::GssHead> changeValue(
      std::shared_ptr<const internal::GssEdge> prev,
      SharedVal v,const LabelEdge& edge);
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
  std::vector<SharedVal> parse(std::function<int16_t()> getch);
};

}  // namespace internal

/*  glrParse(). Parse an input using GLR algorithm.
    Returns a vector of possible parse trees, as built up by the hooks provided.

    Args:
      dfa   - The DFA that defines the grammar. It is assumed Dfa::checkError
              passes. Otherwise, behavior is undefined.
      hk    - Provides recipes for building a SharedVal in case of a
              successful parse.
      getch - The actual input source. It should return -1 on EOF or error.
              Right now we don't distinguish between these two cases.

    The parse trees are typically represented by some user-defined subclass of
    SemVal, as returned by the various components of hk. In case of parse
    failure, just returns an empty vector. Error reporting may be done by either
    recording errors in hk or by returning a "valid" SharedVal that actually
    represents an error and not a parse tree.

    It never throws an exception, but it can BugDie(), either for internal bugs
    or for malformed dfa. Exceptions from hk and getch are all propagated
    unhindered, though.
*/
std::vector<SharedVal> glrParse(
    const Dfa& dfa,GssHooks& hk,std::function<int16_t()> getch);

}  // namespace oalex
