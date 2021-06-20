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

#pragma once

#include <functional>
#include <string>
#include <string_view>
#include <vector>
#include <utility>
#include "ident.h"
#include "runtime/jsonloc.h"
#include "runtime/regex.h"
#include "runtime/skipper.h"

namespace oalex {

// Some of this is not specific to codegen, and should move elsewhere.

// Dev-note: Keep this class abstract, just so we can easily switch out
// of RTTI and dynamic_cast if they become unbearably slow. We can use
// a separate RuleUnassigned to represent an empty rule.
class Rule {
 public:
  Rule() {}
  explicit Rule(Ident name) : name_(std::move(name)) {}
  virtual ~Rule() {}

  // Returns optional to make it harder to forget the empty case.
  std::optional<Ident> name() const {
    if(!name_) return std::nullopt; else return name_;
  }
  void deferred_name(Ident name);

  // Used for debugging/logging.
  virtual std::string specifics_typename() const = 0;

 private:
  Ident name_;
};

// isTentativeTarget should be true if this rule is a target of either some:
// * OrRule::comps[].lookidx in the containing RuleSet, or some
// * QuietMatch::compidx in the containing RuleSet
bool needsName(const Rule& rule, bool isTentativeTarget);

class ConcatFlatRule final : public Rule {
 public:
  // outputPlaceholder can be empty if you never need to refer to the result.
  // For ConcatFlatRule, but not ConcatRule, it is required to be empty for
  // rules returning a JsonLoc::Map.
  struct Component { ssize_t idx; std::string outputPlaceholder; };
  explicit ConcatFlatRule(std::vector<Component> c) : comps(std::move(c)) {}
  std::string specifics_typename() const override { return "ConcatFlatRule"; }
  std::vector<Component> comps;
};

// Mostly deprecated. It is still kept around in case I need concatenation
// without flattening. For the most part, use ConcatFlatRule with OutputTmpl.
// E.g. This doesn't tolerate missing optional fields.
struct ConcatRule {
  // empty outputPlaceholder means the component is discarded.
  struct Component { ssize_t idx; std::string outputPlaceholder; };
  std::vector<Component> comps;
  JsonLoc outputTmpl;
};

// This is typically used to organize the components of a ConcatFlatRule. The
// normal usage is that (possibly multiple layers of) ConcatFlatRule produces
// maps, gets passed down through optionals and OrRules, before being assembled
// by this OutputTmpl according to the outputs-stanza of a rule.
//
// If the rule at childidx returns JsonLoc::ErrorValue, it is passed through
// unchanged like all other rules.
//
// Assuming the child component was successful, i.e. it did not return any
// ErrorValue, there are some requirements on its type.
//
//   * If the child component returns something that's not a JsonLoc::Map,
//     it is given a name childName. The outputTmpl will be searched for this
//     placeholder, and be replaced with the child's return value unchanged.
//     The frontend must ensure that no other placeholder appears in
//     outputTmpl. It is, however, valid for outputTmpl to have no placeholders
//     at all, in which case we return it unchanged (in the absence of a child
//     error). Child's non-error output will be ignored in that case.
//
//   * If the child component produces a JsonLoc::Map, its keys will represent
//     placeholders. childName will be ignored in this case.
class OutputTmpl final : public Rule {
 public:
  OutputTmpl(ssize_t childidx, std::string_view childName, JsonLoc outputTmpl)
    : childidx(childidx), childName(childName),
      outputTmpl(std::move(outputTmpl)) {}
  std::string specifics_typename() const override { return "OutputTmpl"; }
  ssize_t childidx;
  std::string childName;  // used only if child is not producing JsonLoc::Map.
  JsonLoc outputTmpl;
};

// Produces a loop that keeps parsing children, optionally interspersed with
// some "glue" component (if glueidx != -1). It stops right after parsing
// children components. If lookidx == -1, it will just try to parse
// this child, and break out of the loop if it fails. If lookidx != -1, it
// will use that rule as the lookahead decider.
//
// The skipidx is available here separately instead of having it be folded into
// the glueidx or partidx rule. This is so that we can properly backtrack from
// it if the next component fails. We have no plans to allow such inline
// concatenation for anything other than a SkipRule target. Even this feature
// might be removed once we can share parsing results between lookaheads and
// parsers.
struct LoopRule{
  ssize_t partidx;  // mandatory, must not be -1
  std::string partname;
  ssize_t glueidx;  // -1 means no glue
  std::string gluename;
  ssize_t lookidx;  // -1 means no lookahead. Unimplemented.
  ssize_t skipidx;  // -1 means no skip.
};


inline const JsonLoc passthroughTmpl(JsonLoc::Placeholder{"child"}, 0, 0);
inline bool isPassthroughTmpl(const JsonLoc& jsloc) {
  return isPlaceholder(jsloc, "child");
}

struct OrRule {
  // The tmpl must have at most a single placeholder, called 'child'.
  // This is what takes the successful child's value. It is possible to
  // pass the child's result unchanged by using passthroughTmpl.
  //
  // If flattenOnDemand is true, and some ConcatFlatRule considers this OrRule
  // to be a child component, this needs to always return a JsonLoc::Map.
  // As a general rule, flattenOnDemand is false for named multi-match rules,
  // but true for anonymous subparts of a pattern.
  //
  // Setting lookidx to -1 makes this rule not do a separate lookahead.
  // parseidx must always be a valid index.
  //
  // Dev-note: When we need more complicated templates, the plan is to use
  // passthroughTmpl with an OutputTmpl target.
  struct Component { ssize_t lookidx, parseidx; JsonLoc tmpl; };
  std::vector<Component> comps;
  bool flattenOnDemand;
};

// This Rule is used as an OrRule target when something else fails, or in
// response to an obviously bad lookahead. Empty message indicates quiet
// failure that produces no diagnostics (as opposed to an empty error diag).
struct ErrorRule {
  std::string msg;
};

// Literally wraps a rule in quietMatch(). Currently not used in frontend.cpp,
// but it will be used as part of PatternOrList compilation.
struct QuietMatch { ssize_t compidx; };

struct SkipPoint {
  bool stayWithinLine = false;  // If true, skip comments should end in '\n'
  const Skipper* skip;  // usually &RuleSet::skip, but can be overridden.
};

// Just a strong typedef of std::string. It is used as a Rule variant, in case
// we want a word-preserving match. That is a match that doesn't break an input
// word into two.
// Someday, it might become a more complex struct with customizable
// RegexWordChar*. For now, it just uses the default one in RuleSet.
struct WordPreserving {
  std::string s;
  WordPreserving() {}
  explicit WordPreserving(std::string_view init) : s(init) {}
  explicit operator std::string& () { return s; }
  explicit operator const std::string& () const { return s; }
  explicit operator std::string_view () const { return s; }
  const std::string& operator*() const { return s; }
  const std::string* operator->() const { return &s; }
};

// This is used for initial testing only. Real-life grammars should instead
// compose tentative-mode parsing with unconditional errors, whenever all of
// that is implemented.
// Deprecated in favor of a composition of OrRule and ErrorRule.
// TODO remove completely when ErrorRule is functional.
struct MatchOrError {
  ssize_t compidx;
  std::string errmsg;
};

// Note: we currently don't support ExternParser in tentative contexts.
struct ExternParser { };

struct RuleVariant final : public Rule {
  // TODO other component types like RawString.
  template <class X> explicit RuleVariant(X x)
    : Rule{{}}, specifics_(std::move(x)) {}
  template <class X> RuleVariant(X x, Ident name)
    : Rule(std::move(name)), specifics_(std::move(x)) {}

  // This is just to discourage mutation in the frontend, which led to
  // suboptimal coding style (e.g. having to specify the name twice).
  // See deferred_name() below.
  RuleVariant(RuleVariant&&) = default;
  RuleVariant& operator=(const RuleVariant&) = delete;

  std::string specifics_typename() const override;

  template <class X> friend bool holds_alternative(const Rule& rule);
  template <class X> friend X* get_if(Rule* rule);
  template <class X> friend const X* get_if(const Rule* rule);
 private:
  /*
  Return types are either an ErrorValue or:

    * monostate: Doesn't return, not to be used in eval or codegen.
    * string, WordPreserving, Regex: Returns string.
    * SkipPoint: Something dummy (to be checked).
    * ConcatRule: Depends on outputtmpl, not used by pattern-compilation.
    * ConcatFlatRule: Returns Map, flattenable.
    * OutputTmpl: Returns Map, not flattenable.
    * LoopRule: Returns Map, flattenable.
    * OrRule: Depends on flattenOnDemand. If set, we require all children
      to be makesFlattenableMap() too.
    * ErrorRule: ErrorValue only. Satisfies makesFlattenableMap().
      Maybe rename to makesFlattenableMapOrError().
    * QuietMatch: Same as its child.
    * MatchOrError: Same as child.
  */
  std::variant<std::monostate, std::string, WordPreserving, ExternParser,
               std::unique_ptr<const Regex>, SkipPoint, ConcatRule,
               LoopRule, OrRule, ErrorRule,
               QuietMatch, MatchOrError> specifics_;
  using VariantType = decltype(std::declval<RuleVariant>().specifics_);

 public:
  template <class X> static constexpr bool validType =
    isMemberOfVariantV<X, VariantType>;
};

template <class X> bool holds_alternative(const Rule& rule) {
  if constexpr(RuleVariant::validType<X>) {
    if(auto* rvar = dynamic_cast<const RuleVariant*>(&rule))
      return std::holds_alternative<X>(rvar->specifics_);
    else return false;
  }else return dynamic_cast<const X*>(&rule) != nullptr;
}

template <class X> X* get_if(Rule* rule) {
  if constexpr(RuleVariant::validType<X>) {
    if(auto* rvar = dynamic_cast<RuleVariant*>(rule))
      return std::get_if<X>(&rvar->specifics_);
    else return nullptr;
  }else return dynamic_cast<X*>(rule);
}

template <class X> const X* get_if(const Rule* rule) {
  if constexpr(isMemberOfVariantV<X, RuleVariant::VariantType>) {
    if(auto* rvar = dynamic_cast<const RuleVariant*>(rule))
      return std::get_if<X>(&rvar->specifics_);
    else return nullptr;
  }else return dynamic_cast<const X*>(rule);
}

// TODO this needs a debug() printer.
struct RuleSet {
  std::vector<std::unique_ptr<Rule>> rules;
  RegexOptions regexOpts;
};

using OutputStream = std::function<void(std::string_view)>;
void codegen(const RuleSet& ruleset, ssize_t ruleIndex,
             const OutputStream& cppos, const OutputStream& hos);
void codegenDefaultRegexOptions(const RuleSet& ruleset,
                                const OutputStream& cppos);

JsonLoc eval(InputDiags& ctx, ssize_t& i,
             const RuleSet& ruleset, ssize_t ruleIndex);

}  // namespace oalex
