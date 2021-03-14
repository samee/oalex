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
// ErrorValue, there are some requirements on its type. If outputTmpl has any
// placeholders at all, the rule at childidx must produce a JsonLoc::Map with
// keys needed to fill it, and it is the frontend's job to ensure that. Keys in
// the child must match placeholder names in outputTmpl. Any key outputTmpl may
// or may not have is irrelevant. Indeed, outputTmpl is not required to be a
// JsonLoc::Map at all.
//
// If outputTmpl has no placeholders, any non-error output of childidx is
// ignored. In that case, we simply produce outputTmpl as is.
struct OutputTmpl {
  ssize_t childidx;
  JsonLoc outputTmpl;
};

struct OrRule {
  // The tmpl must have at most a single placeholder, called 'child'.
  //
  // While it *is* possible for an OrRule to not return a JsonLoc::Map,
  // it isn't always allowed. In particular, the frontend needs to ensure that
  // every OrRule that appears as a component of ConcatFlatRule is a
  // JsonLoc::Map. This assumption is important whenever canBeFlat() is
  // used in codegen. The only way to avoid returning a map is to set tmpl
  // to exactly JsonLoc::Placeholder{"child"}.
  //
  // TODO Stop supporting this non-map return value. We could instead
  // support auto-boxing like ConcatFlatRule and OutputTmpl, where the
  // component name depends on the chosen index.
  struct Component { ssize_t idx; JsonLoc tmpl; };
  std::vector<Component> comps;
};

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
struct MatchOrError {
  ssize_t compidx;
  std::string errmsg;
};

// Note: we currently don't support ExternParser in tentative contexts.
struct ExternParser { };

struct Rule {
  // TODO other component types like RawString.
  template <class X> explicit Rule(X x) : specifics_(std::move(x)), name_() {}
  template <class X> Rule(X x, std::string name) :
    specifics_(std::move(x)), name_(std::move(name)) {}
  std::string specifics_typename() const;  // Used for debugging/logging.
  std::optional<std::string> name() const {
    if(name_.empty()) return std::nullopt; else return name_;
  }
  void name(std::string_view name) { name_ = name; }
  bool needsName() const;

  template <class X> friend bool holds_alternative(const Rule& rule);
  template <class X> friend X* get_if(Rule* rule);
  template <class X> friend const X* get_if(const Rule* rule);
 private:
  std::variant<std::monostate, std::string, WordPreserving, ExternParser,
               std::unique_ptr<const Regex>, SkipPoint, ConcatRule,
               ConcatFlatRule, OutputTmpl, OrRule, MatchOrError> specifics_;
  std::string name_;
};

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
