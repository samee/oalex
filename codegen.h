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

struct ConcatFlatRule {
  // outputPlaceholder can be empty if you never need to refer to the result.
  // For ConcatFlatRule, but not ConcatRule, it is required to be empty for
  // rules returning a JsonLoc::Map.
  struct Component { ssize_t idx; std::string outputPlaceholder; };
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
struct OutputTmpl {
  ssize_t childidx;
  std::string childName;  // used only if child is not producing JsonLoc::Map.
  JsonLoc outputTmpl;
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

struct Rule {
  // TODO other component types like RawString.
  template <class X> explicit Rule(X x) : specifics_(std::move(x)), name_() {}
  template <class X> Rule(X x, Ident name) :
    specifics_(std::move(x)), name_(std::move(name)) {}

  // This is just to discourage mutation in the frontend, which led to
  // suboptimal coding style (e.g. having to specify the name twice).
  // See deferred_name() and deferred_assign() below.
  Rule(Rule&&) = default;
  Rule& operator=(const Rule&) = delete;

  std::string specifics_typename() const;  // Used for debugging/logging.
  // Returns optional to make it harder to forget the empty case.
  std::optional<Ident> name() const {
    if(!name_) return std::nullopt; else return name_;
  }
  void deferred_name(Ident name);
  bool needsName(bool isLookaheadTarget) const;

  // Assign to specifics if it is in std::monostate.
  // We disallow later assignments to discourage mutation.
  template <class X> void deferred_assign(X x);

  template <class X> friend bool holds_alternative(const Rule& rule);
  template <class X> friend X* get_if(Rule* rule);
  template <class X> friend const X* get_if(const Rule* rule);
 private:
  std::variant<std::monostate, std::string, WordPreserving, ExternParser,
               std::unique_ptr<const Regex>, SkipPoint, ConcatRule,
               ConcatFlatRule, OutputTmpl, OrRule, ErrorRule, QuietMatch,
               MatchOrError> specifics_;
  Ident name_;
};

template <class X> inline void
Rule::deferred_assign(X x) {
  if(!std::holds_alternative<std::monostate>(specifics_))
    oalex::Bug("deferred_assign() cannot be used on non-monostate Rules");
  specifics_ = std::move(x);
}

template <class X> bool holds_alternative(const Rule& rule) {
  return std::holds_alternative<X>(rule.specifics_);
}

template <class X> X* get_if(Rule* rule) {
  return std::get_if<X>(&rule->specifics_);
}

template <class X> const X* get_if(const Rule* rule) {
  return std::get_if<X>(&rule->specifics_);
}

struct RuleSet {
  std::vector<Rule> rules;
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
