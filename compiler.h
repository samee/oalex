/*  Copyright 2020-2021 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

// This file was refactored out of frontend.h. As a result, even today all of
// these functions are directly called by frontend.h.
//
// TODO write unit tests. Previously all changes
// were tested by integration tests.
#pragma once
#include <memory>
#include <string_view>
#include <utility>
#include <vector>

#include "runtime/diags.h"
#include "codegen.h"
#include "pattern.h"

namespace oalex {
using LocPair = std::pair<ssize_t,ssize_t>;
constexpr LocPair nrange{-1,-1};

// This class keeps location information around while we are still building
// up the vector<Rule>. This allows us to provide error messages such as
// "variable used here but never defined". Once we are sure there is no error,
// we can release rules() into the world for codegen, and forget location info.
class RulesWithLocs {
 public:
  ssize_t ssize() const { return rules_.size(); }
  Rule& operator[](ssize_t i);

  /* Searches for ident in rules[].nameOrNull().
     If found, returns the index.
     If not found, appends a new UnassignedRule with the ident, and returns the
       index of the new element. In this case, it also records thisPos in
       firstUseLocs_.
     Assumes ident.empty() == false
  */
  ssize_t findOrAppendIdent(const Ident& id);

  /* Returns the index of UnassignedRule named ident.
       If one already exists, its index is returned with no change.
       If one doesn't already exist,
         one is appended and the new index is returned.
     If an assigned rule named ident already exists, it produces a
     "multiple definition" error and returns -1.

     In case we are actually appending a new entry, the firstUseLocs_ remains
     nrange() so that it is later filled in by findOrAppendIdent.
  */
  ssize_t defineIdent(DiagsDest ctx, const Ident& ident);

  /* Tries to reserve a local name just so we can later detect a conflict
     with a global name later. If a global name is already defined, this
     function will immediately raise an error, but otherwise not do much else.
  */
  void reserveLocalName(DiagsDest ctx, const Ident& ident);

  /* Utility for anon rules that also appends a dummy first-use location entry.
     Anonymous rules don't need usage location so far, since we never refer to
     them in error messages. They are implicitly generated, so the user won't
     know what to make of errors about rules they didn't write.

     Returns the index of the new element: newsize - 1

     Named rules should use defineIdent followed by direct assignment.
  */
  template <class X> ssize_t appendAnonRule(X x);

  /* For assigning to a rule after they have already been named */
  template <class X> void deferred_assign(ssize_t idx, X x);

  /* This is checked just before producing rules as output */
  bool hasUndefinedRules(DiagsDest ctx) const;

  /* Reduces sizes of rules_ and firstUseLocs_ to n, if it's larger */
  void resize_down(ssize_t n) noexcept;

  std::vector<std::unique_ptr<Rule>> releaseRules();

 private:
  // Invariant: these two must have equal sizes at all times.
  std::vector<std::unique_ptr<Rule>> rules_;
  std::vector<LocPair> firstUseLocs_;
};

// These are the usual entries in a `where:` stanza of a rule. An entry:
//
//   "patt" as var ~ lhs
//
// is represented as { .pp = "patt", .outTmplKey = var, .ruleName = lhs }
struct PatternToRuleBinding {
  PartPattern pp;
  Ident outTmplKey;
  Ident ruleName;
};

void
assignLiteralOrError(RulesWithLocs& rl, size_t ruleIndex,
		     std::string_view literal);
ssize_t
appendLiteralOrError(RulesWithLocs& rl, std::string_view literal);
ssize_t
appendWordOrError(RulesWithLocs& rl, std::string_view word);
void
assignRegexOrError(RulesWithLocs& rl, size_t ruleIndex,
                   std::string errmsg, std::unique_ptr<const Regex> regex);
ssize_t
appendRegexOrError(RulesWithLocs& rl, std::unique_ptr<const Regex> regex);

void
appendPatternRules(DiagsDest ctx, const Ident& ident,
                   lex::GluedString patt_string, const LexDirective& lexOpts,
                   std::vector<PatternToRuleBinding> pattToRule,
                   JsonTmpl jstmpl, RulesWithLocs& rl);

/*
Pattern to Rule compilation
---------------------------

Objects of class PatternToRulesCompiler are not usually long-lived. They are
used only for the compilation of a single input rule. Conventions followed by
methods of this class:

  * They append newly compiled rules into RulesWithLocs rl_.
  * Each function may append one or more Rules.
  * They return the location of the entry point index for the newly created
    block of Rules.

The result of pattern compilation is some rule that produces one of two things:

  * The entire Pattern is just one Ident and nothing else.
  * All the identifiers in the pattern is to be gathered up into a single
    flat JsonLoc::Map object (e.g. by using ConcatFlatRule).

These identifiers are then assembled by an OutputTmpl object specified outside
the Pattern object. The syntax for that varies. The impliciation is that if
we want a particular rule output to be available to the outermost OutputTmpl,
we need to keep propagating it up.

Every pattern produces a map, except for the literal string ones (e.g.
appendWordOrError, appendLiteralOrError, appendRegexOrError). The Word and
Literal variants produce a string but that result is ignored when part of a
Pattern object. The Regex variant also returns a string, but the result is
generally propagated up to the user. They could produce maps too, or nothing at
all, but right now they produce strings just so the functions can be used
elsewhere in the frontend.

How to follow this convention for new rules types with subcomponents:
If you want something to be preserved until an OutputTmpl catches it, make it a
named field in a map. You can safely use passthroughTmpl: map fields will be
propagated, while strings will be dropped either by OutputTmpl or
ConcatFlatRule.
*/

class PatternToRulesCompiler {
  DiagsDest ctx_;      // These is where all errors and warnings get logged.
  RulesWithLocs* rl_;  // This is where new rules get appended.
  // Rule index for placeholders
  const std::vector<std::pair<Ident,ssize_t>>* p2rule_;
  ssize_t processConcat(const PatternConcat& concatPatt);
  ssize_t processOrList(const PatternOrList& orPatt);
  ssize_t processOptional(const PatternOptional& optPatt);
  ssize_t processIdent(const Ident& ident);
  ssize_t processRepeat(const PatternRepeat& repPatt);
  ssize_t processFold(const PatternFold& foldPatt);
 public:
  PatternToRulesCompiler(DiagsDest ctx, RulesWithLocs& rl,
                         const std::vector<std::pair<Ident,ssize_t>>& p2rule) :
    ctx_(ctx), rl_(&rl), p2rule_(&p2rule) {}
  // Just to prevent accidental copying.
  PatternToRulesCompiler(const PatternToRulesCompiler&) = delete;
  ssize_t process(const Pattern& patt);
};

}  // namespace oalex
