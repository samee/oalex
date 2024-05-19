/*  Copyright 2019-2024 The oalex authors.

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

#include <string>
#include <string_view>
#include <vector>
#include <utility>
#include "ident.h"
#include "jsontmpl.h"
#include "runtime/regex.h"
#include "runtime/skipper.h"

namespace oalex {

// Some of this is not specific to codegen, and should move elsewhere.

struct RuleField {
  std::string field_name;  // Never empty. We might turn it into an Ident.

  // The schema_source must have a valid index of a non-flattenable rule that
  // defines the output type for this field. Output types:
  //
  //   * string rules: StringLoc
  //   * OutputTmpl: a generated struct
  //   * ExternParser: JsonLike
  //   * OrRule, non-flattenable: JsonLike
  ssize_t schema_source;

  // Says whether the output type derived from schema_source needs to be
  // wrapped in optional (sz::any_of, really) or std::vector.
  enum Container { single, optional, vector } container;
};

// Dev-note: Keep this class abstract, just so we can easily switch out
// of RTTI and dynamic_cast if they become unbearably slow. We can use
// a separate UnassignedRule to represent an empty rule.
class Rule {
 public:
  Rule() {}
  explicit Rule(Ident name) : name_(std::move(name)) {}
  virtual ~Rule() {}

  const Ident* nameOrNull() const {
    if(!name_) return nullptr; else return &name_;
  }
  void deferred_name(Ident name);
  void context_skipper(ssize_t skipper_index);
  ssize_t context_skipper() const { return contextSkipper_; }
  void flatFields(std::vector<RuleField> ff) { flatFields_ = std::move(ff); }
  const std::vector<RuleField>& flatFields() const { return flatFields_; }

  // Used for debugging/logging.
  virtual std::string specifics_typename() const = 0;

  enum ContextSkipper : ssize_t {
    helperRuleNoContext = -1,  // The vast majority of rules use this value.
    removedContext = -2,       // This is typically used by expression rules.
  };
 private:
  Ident name_;

  // This is index of the Skipper to use if this rule is used by itself,
  // not as part of some other rule.
  ssize_t contextSkipper_ = helperRuleNoContext;

  // Used iff resultFlattenableOrError() is true for this rule.
  // If we define a struct for this Rule's return value, flatFields_ gives us
  // the names and types.
  //
  // Note that OutputTmpl is not flattenable.
  // Therefore this is always empty for OutputTmpl rules.
  std::vector<RuleField> flatFields_;
};

// UnassignedRule really means used but not defined. It is used when we have
// partially processed a file, and have not yet seen a definition of a name that
// has already been used. If these names remain undefined at the end of the file
// we can report an "undefined rule" error.
class UnassignedRule final : public Rule {
 public:
  explicit UnassignedRule(Ident name) : Rule(std::move(name)) {}
  std::string specifics_typename() const override { return "(unassigned)"; }
};

class DefinitionInProgress final : public Rule {
 public:
  DefinitionInProgress() = default;
  // For parameter context_skipper, see comment in RulesWithLocs::defineIdent().
  explicit DefinitionInProgress(Ident name, ssize_t context_skipper)
    : Rule(std::move(name)) { this->context_skipper(context_skipper); }
  std::string specifics_typename() const override
    { return "DefinitionInProgress"; }
};

// isTentativeTarget should be true if this rule is a target of either some:
// * OrRule::comps[].lookidx in the containing RuleSet, or some
// * QuietMatch::compidx in the containing RuleSet
bool needsName(const Rule& rule, bool isTentativeTarget);

class ConcatFlatRule final : public Rule {
 public:
  // outputPlaceholder can be empty if you never need to refer to the result.
  // It is also required to be empty for components returning a flattenable map.
  // TODO: change outputPlaceholder type to ident,
  // since they go into struct fields.
  struct Component { ssize_t idx; std::string outputPlaceholder; };
  explicit ConcatFlatRule(std::vector<Component> c) : comps(std::move(c)) {}
  std::string specifics_typename() const override { return "ConcatFlatRule"; }
  std::vector<Component> comps;
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
  OutputTmpl(ssize_t childidx, std::string_view childName, JsonTmpl outputTmpl)
    : childidx(childidx), childName(childName),
      outputTmpl(std::move(outputTmpl)) {}
  std::string specifics_typename() const override { return "OutputTmpl"; }
  ssize_t childidx;
  std::string childName;  // used only if child is not producing JsonLoc::Map.
  JsonTmpl outputTmpl;
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
//
// The fields are kept in a separate struct to allow designated initializers.
// They are inherited into LoopRule, so you can still access them through the
// familiar dot or arrow notations.
struct LoopRuleFields {
  ssize_t partidx;  // mandatory, must not be -1
  ssize_t glueidx;  // -1 means no glue
  ssize_t lookidx;  // -1 means no lookahead. Unimplemented.
  ssize_t skipidx;  // -1 means no skip.
};

class LoopRule final : public LoopRuleFields, public Rule {
 public:
  explicit LoopRule(LoopRuleFields f) : LoopRuleFields{f} {}
  std::string specifics_typename() const override { return "LoopRule"; }
};

inline const JsonTmpl passthroughTmpl{JsonTmpl::Placeholder{"child"}};
inline bool isPassthroughTmpl(const JsonTmpl& jstmpl) {
  return isPlaceholder(jstmpl, "child");
}

class OrRule final : public Rule {
  // The tmpl must have at most a single placeholder, called 'child'.
  // This is what takes the successful child's value. It is possible to
  // pass the child's result unchanged by using passthroughTmpl. We support
  // only three different output templates: passthroughTmpl (i.e. `child`),
  // JsonTmpl::Map{} (empty map), and a single kv-pair (`{ key: child }` for
  // some valid placeholder `key`.
  //
  // If flattenOnDemand is true, and some ConcatFlatRule considers this OrRule
  // to be a child component, this needs to always return a JsonLoc::Map.
  // As a general rule, flattenOnDemand is false for named multi-match rules
  // without an explicit `outputs` stanza, but true for all other cases.
  // For example, anonymous subparts of a pattern.
  //
  // Setting lookidx to -1 makes this rule not do a separate lookahead.
  // parseidx must always be a valid index.
  //
  // In codegen, the return value will either be JsonLike or a custom generated
  // struct convertible to JsonLoc, based on the value of flattenOnDemand.
  //
  // For each component, the tmpl field is only allowed certain values.
  //
  //   An empty JsonTmpl::Map{} is always allowed.
  //   else if (flattenOnDemand == true) {
  //     // Return type is now a generated struct.
  //     if(branchFlattenable)
  //       either passthroughTmpl or kv-pair tmpl are allowed.
  //     else
  //       require tmpl kv-pair, not passthroughTmpl.
  //       // We can disallow this too in future and,
  //       // switch to using OutputTmpl instead.
  //   }else {
  //     // Return type is now JsonLike
  //     require branchFlattenable == false and passthroughTmpl.
  //   }
  //
  // Above, we take care to not allow any case that requires the user to cast
  // from JsonLike to some internal type that the user cannot name.
  //
  // Dev-note: When we need more complicated templates, the plan is to use
  // passthroughTmpl with an OutputTmpl target. The template constraints above
  // aren't yet uniformly enforced in code.
 public:
  struct Component { ssize_t lookidx, parseidx; JsonTmpl tmpl; };
  explicit OrRule(std::vector<Component> comps, bool fod)
    : comps{std::move(comps)}, flattenOnDemand{fod} {}
  std::string specifics_typename() const override { return "OrRule"; }
  std::vector<Component> comps;
  bool flattenOnDemand;
};

// This Rule is used as an OrRule target when something else fails, or in
// response to an obviously bad lookahead. Empty message indicates quiet
// failure that produces no diagnostics (as opposed to an empty error diag).
class ErrorRule final : public Rule {
 public:
  explicit ErrorRule(std::string msg) : msg(std::move(msg)) {}
  std::string specifics_typename() const override { return "ErrorRule"; }
  std::string msg;
};

// Literally wraps a rule in quietMatch(). Currently not used in frontend.cpp,
// but it will be used as part of PatternOrList compilation.
class QuietMatch final : public Rule {
 public:
  explicit QuietMatch(ssize_t idx) : compidx{idx} {}
  std::string specifics_typename() const override { return "QuietMatch"; }
  ssize_t compidx;
};

class SkipPoint final : public Rule {
 public:
  SkipPoint(ssize_t skipperIndex) : skipperIndex{skipperIndex} {}
  std::string specifics_typename() const override { return "SkipPoint"; }
  ssize_t skipperIndex; // Index into RuleSet::skips.
};

// Wraps an std::string, for when we want a word-preserving match. That is a
// match that doesn't break an input word into two.
// Someday, it might become a more complex struct with customizable
// RegexWordChar*. For now, it just uses the default one in RuleSet.
class WordPreserving final : public Rule {
 public:
  std::string s;
  ssize_t regexOptsIdx = -1;
  WordPreserving() {}
  explicit WordPreserving(std::string_view init, ssize_t roi)
    : s(init), regexOptsIdx(roi) {}
  explicit operator std::string& () { return s; }
  explicit operator const std::string& () const { return s; }
  explicit operator std::string_view () const { return s; }
  const std::string& operator*() const { return s; }
  const std::string* operator->() const { return &s; }
  std::string specifics_typename() const override { return "WordPreserving"; }
};

// This is used for initial testing only. Real-life grammars should instead
// compose tentative-mode parsing with unconditional errors, whenever all of
// that is implemented.
// Deprecated in favor of a composition of OrRule and ErrorRule.
// TODO remove completely when ErrorRule is functional.
class MatchOrError final : public Rule {
 public:
  MatchOrError(ssize_t compidx, std::string errmsg)
    : compidx{compidx}, errmsg{std::move(errmsg)} {}
  std::string specifics_typename() const override { return "MatchOrError"; }
  ssize_t compidx;
  std::string errmsg;
};

class AliasRule final : public Rule {
 public:
  AliasRule(ssize_t targetidx) : targetidx{targetidx} {}
  std::string specifics_typename() const override { return "AliasRule"; }
  ssize_t targetidx;
};

class StringLoc;  // forward declaration

// Note: we currently don't support ExternParser in tentative contexts.
class ExternParser final : public Rule {
  std::string externalName_;
  std::vector<ssize_t> params_;
 public:
  static bool requireValidNameAndParamCount(
      const oalex::StringLoc& extName, ssize_t providedParamCount,
      oalex::DiagsDest ctx);
  ExternParser(
      std::string_view extName,
      std::vector<ssize_t> params);
  std::string specifics_typename() const override { return "ExternParser"; }
  const std::string& externalName() const;
  const std::vector<ssize_t>& params() const { return params_; }
};

class RegexRule final : public Rule {
 public:
  explicit RegexRule(std::unique_ptr<const Regex> patt, ssize_t roi)
    : patt(std::move(patt)), regexOptsIdx(roi) {}
  std::string specifics_typename() const override { return "RegexRule"; }
  std::unique_ptr<const Regex> patt;
  ssize_t regexOptsIdx = -1;
};

class StringRule final : public Rule {
 public:
  explicit StringRule(std::string s) : val(std::move(s)) {}
  StringRule(std::string s, Ident name)
    : Rule(std::move(name)), val(std::move(s)) {}

  // This is just to discourage mutation in the frontend, which led to
  // suboptimal coding style (e.g. having to specify the name twice).
  // See deferred_name() below.
  StringRule(StringRule&&) = default;
  StringRule& operator=(const StringRule&) = delete;

  std::string specifics_typename() const override { return "StringRule"; }
  std::string val;
};

// TODO this needs a debug() printer.
struct RuleSet {
  std::vector<std::unique_ptr<Rule>> rules;
  std::vector<Skipper> skips;
  std::vector<RegexOptions> regexOpts;
};

// Populates ruleset.rules[].flatFields(). This is used after a RuleSet
// object is completed by the compiler, but before it is passed on to a backend.
// Currently it is only used by the codegen() backend, and not eval().
void populateFlatFields(RuleSet& ruleset);

class OutputStream {
 public:
  virtual void operator()(std::string_view) const = 0;
  virtual ~OutputStream() {}
};

/*
Return types are either an ErrorValue or:

  * UnassignedRule: Doesn't return, not to be used in eval or codegen.
  * string, WordPreserving, RegexRule: Returns string.
  * SkipPoint: Something dummy (to be checked).
  * ConcatFlatRule: Returns Map, flattenable.
  * OutputTmpl: Returns Map, not flattenable.
  * LoopRule: Returns Map, flattenable.
  * OrRule: Depends on flattenOnDemand. If set, we require all children
    to be resultFlattenableOrError() too.
  * ErrorRule: ErrorValue only. Satisfies resultFlattenableOrError().
    Maybe rename to resultFlattenableOrErrorOrError().
  * QuietMatch: Same as its child.
  * MatchOrError: Same as child.
  * AliasRule: Same as its target.
*/
void codegen(const RuleSet& ruleset, ssize_t ruleIndex,
             const OutputStream& cppos, const OutputStream& hos);
void codegenDefaultRegexOptions(const RuleSet& ruleset,
                                const OutputStream& cppos);

JsonLoc eval(InputDiags& ctx, ssize_t& i,
             const RuleSet& ruleset, ssize_t ruleIndex);

/* This is the same as eval, but is sandwiched between Skipper calls so as to
   skip over comments and spaces. This only works if:

     ruleset.rules[i].context_skipper() != Rule::helperRuleNoContext

  Dev-note: Right now, there is no analog for this in codegen(). The user can
  always manually call the appropriate skipper (if they know the name). Later,
  we can provide some syntax for explicitly naming these:

    derive rule bigfoo: trim(foo)
    derive rule skipfoo: skip(foo)
*/
JsonLoc trimAndEval(InputDiags& ctx, ssize_t& i,
                    const RuleSet& ruleset, ssize_t ruleIndex);

}  // namespace oalex
