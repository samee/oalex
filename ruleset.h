/*  Copyright 2019-2025 The oalex authors.

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

#include <memory>
#include <string>
#include <vector>
#include "runtime/regex.h"
#include "runtime/skipper.h"
#include "ident.h"
#include "jsontmpl.h"

namespace oalex {

// Forward decl.
struct RuleSet;
class Rule;

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

// Unfortunately C++ does not support sum types. If it did, the last
// Two enum values would have extra fields:
// * jsonTmpl: JsonTmpl.
// * flatStruct: vector<RuleField>.
// Plus, an index to the target rule for wrappers, so we can produce struct
// type names if they are pointing to types.
enum class OutputType {
  unassigned = 0,  // UnassignedRule, DefinitionInProgress.
  boolean,         // SkipPoint.
  nullopt,         // ErrorRule.
  string,          // StringRule, RegexRule, WordPreserving.
  jsonLike,        // ExternRule, maybe OrRule.
  jsonTmpl,        // OutputTmpl.
  flatStruct,      // ConcatFlatRule, LoopRule, maybe OrRule.
};

// This type is never stored explicitly in rules, but created on-demand
// by their outputTypeInfo() virtual method. The value gets invalidated if
// rs->rules is modified in any way: it has pointers to Rule objects.
//
// That method (and therefore the constructor here) take in a RuleSet to chase
// wrappers like AliasRule, MatchOrError, and QuietMatch.
//
// Dev-note: There are reasons for keeping the typeSource_ a Rule*, and not an
// index into a RuleSet:
//
//   * OutputTypeInfo objects are constructed from overridden outType()
//     methods, where we don't always know the index. One hacky option would be
//     to keep this index initialized to some special value like -1 to indicate
//     "this rule". And have it be protected, while some public interface (like
//     a friend outType(RuleSet, idx) function) looks up the rule, calls
//     outType() and substitutes out the special value. I tried it for a bit,
//     and didn't like it.
//   * Many of the callers are in codegen(SomeRule), where we don't have an
//     index. It is used, for instance, to figure out the name of a generated
//     struct type. But at this poiint, the idx has already been resolved to
//     a specific Rule.
//
// One concern was the pointer validity. However, this turned out not to be a
// problem for short-lived objects like OutputTypeInfo.
class OutputTypeInfo {
 public:
  OutputTypeInfo(const RuleSet* rs, const Rule& ts, OutputType t)
    : ruleset_{rs}, type_{t}, typeSource_{&ts} {}
  OutputType type() const { return type_; }
 private:
  const RuleSet* ruleset_;

  OutputType type_;
  const Rule* typeSource_;  // Never a WrapperRule.
};

// Encapsulates a single integer. This type is used to indicate whether a given
// rule produces a user-visible type, or not. And if it's visible, what makes it
// visible.
//
// These are used in computeUserExposureForTypes() for two things:
//
//   * Catch internal bugs, where we are leaving a user-facing return type
//     unnamed. This has the effect of them being assigned arbitrary unstable
//     numeric names like `ParsedRule42`, which can change unpredictably.
//   * Marking local field names as nested structs, like ParsedAddr::ZipCode.
//     A nested type is the cleanest way I could think of generating sensible
//     type names that do not conflict with other generated names.
class UserExposure final {
 public:
  enum State : ssize_t {
    // Positive values are used internally to indicate parent container index for
    // nested fields. The `nested` value here is never explicitly stored.
    // Precedence order: if builtin applies, as well as topLevel or nested,
    // we represent it here as builtin. Other combinations are semantically
    // impossible.

    // We have not yet figured out if the type is exposed in the public API.
    unknown = -1,

    // The type is exposed, since it represents the output of a top-level rule.
    topLevel = -2,

    // The built-in types StringLoc and JsonLike are always in the public API.
    builtin = -3,

    // We know for sure this type is only used internally in generated code.
    notExposed = -4,

    // We never generate any type for this rule. Right now, this is used
    // for WrapperRule which copies the target rule. Codegen always
    // resolves their target before generating a type.
    notGenerated = -5,

    // This type is exposed since they are defined nested in another type.
    // Nesting is the cleanest way we could avoid collisions among generated
    // names that need to be stable.
    nested = -6,
  };

  // Sets exposure state only if it has not been set yet.
  // Returns true on success. A "nested" state is set via the nestedIn() setter.
  bool state(State s) {
    if(state_ != unknown) return false;
    switch(s) {
      case topLevel:
      case builtin:
      case notExposed:
      case notGenerated:
        state_ = s;
        return true;
      default:
        return false;
    }
  }
  State state() const;

  // Returns a real value only if state() is nested.
  std::optional<ssize_t> nestedIn() const {
    if(state_ < 0) return std::nullopt;
    else return state_;
  }
  // Verifies that s is positive.
  bool nestedIn(ssize_t s) {
    if(s < 0) return false;
    if(state_ != unknown && state_ != s) return false;
    state_ = s;
    return true;
  }

  UserExposure() = default;
  UserExposure(const UserExposure&) = default;
 private:
  ssize_t state_ = unknown;
  State validOrBug() const;
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
  UserExposure exposure() const { return exposure_; }
  UserExposure& exposure() { return exposure_; }

  // Used for debugging/logging.
  virtual std::string specifics_typename() const = 0;

  enum ContextSkipper : ssize_t {
    helperRuleNoContext = -1,  // The vast majority of rules use this value.
    removedContext = -2,       // This is typically used by expression rules.
  };

  // Careful: this may not work on wrapper types
  // before resolveWrapperTypes() have been called.
  virtual OutputTypeInfo outType(const RuleSet& rs) const = 0;

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

  // This field always starts out as UserExposure::unknown.
  // Over time, a rule may acquire a reason for its type to be exposed to the
  // user, depending on what role it plays in the generated API. If it remains
  // unknown by the time of codegen, we set it to UserExposure::notExposed.
  UserExposure exposure_;
};

// UnassignedRule really means used but not defined. It is used when we have
// partially processed a file, and have not yet seen a definition of a name that
// has already been used. If these names remain undefined at the end of the file
// we can report an "undefined rule" error.
class UnassignedRule final : public Rule {
 public:
  explicit UnassignedRule(Ident name) : Rule(std::move(name)) {}
  std::string specifics_typename() const override { return "(unassigned)"; }
  OutputTypeInfo outType(const RuleSet& rs) const override {
    return {&rs, *this, OutputType::unassigned};
  }
};

class DefinitionInProgress final : public Rule {
 public:
  DefinitionInProgress() = default;
  // For parameter context_skipper, see comment in RulesWithLocs::defineIdent().
  explicit DefinitionInProgress(Ident name, ssize_t context_skipper)
    : Rule(std::move(name)) { this->context_skipper(context_skipper); }
  std::string specifics_typename() const override
    { return "DefinitionInProgress"; }
  OutputTypeInfo outType(const RuleSet& rs) const override {
    return {&rs, *this, OutputType::unassigned};
  }
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
  OutputTypeInfo outType(const RuleSet& rs) const override {
    return {&rs, *this, OutputType::flatStruct};
  }
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
  OutputTypeInfo outType(const RuleSet& rs) const override {
    return {&rs, *this, OutputType::jsonTmpl};
  }
};

// First parses rule initidx, then starts a loop that keeps parsing elements of
// loopbody. The first `looklen` elements of `loopbody` are "tenative", in that
// any error returned from those cause the loop to silently break.
//
// looklen needs to be >=1, and <=loopbody.size().
//
// The fields are kept in a separate struct to allow designated initializers.
// They are inherited into LoopRule, so you can still access them through the
// familiar dot or arrow notations.
struct LoopRuleFields {
  ssize_t initidx, looklen;
  std::vector<ssize_t> loopbody;
};

class LoopRule final : public LoopRuleFields, public Rule {
 public:
  explicit LoopRule(LoopRuleFields f) : LoopRuleFields{std::move(f)} {}
  std::string specifics_typename() const override { return "LoopRule"; }
  OutputTypeInfo outType(const RuleSet& rs) const override {
    return {&rs, *this, OutputType::flatStruct};
  }
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
  OutputTypeInfo outType(const RuleSet& rs) const override {
    auto ot = flattenOnDemand ? OutputType::flatStruct : OutputType::jsonLike;
    return {&rs, *this, ot};
  }
};

// This Rule is used as an OrRule target when something else fails, or in
// response to an obviously bad lookahead. Empty message indicates quiet
// failure that produces no diagnostics (as opposed to an empty error diag).
class ErrorRule final : public Rule {
 public:
  explicit ErrorRule(std::string msg) : msg(std::move(msg)) {}
  std::string specifics_typename() const override { return "ErrorRule"; }
  std::string msg;
  OutputTypeInfo outType(const RuleSet& rs) const override {
    return {&rs, *this, OutputType::nullopt};
  }
};

// These are "wrappers", whose output types depend on some other rule. The
// typeSource is set during resolveWrapperTargets, after checking that we do
// not have a cycle of wrappers. Before that, this function returns -1.
//
// Meant to be called during codegen, which comes after analysis anyway.
class WrapperRule : public Rule {
 public:
  explicit WrapperRule(ssize_t target) : tg_{target} {}
  ssize_t target() const { return tg_; }

  // This is the recursive version of wrapTarget.
  ssize_t typeSource() const { return ts_; }
  void typeSource(ssize_t idx) { ts_ = idx; }

  OutputTypeInfo outType(const RuleSet& rs) const override;

 private:
  // It'd be nice to keep RuleSet* or even Rule* instead of an index
  // to the rule. But the parent objects (esp. RuleSet) can be moved.
  ssize_t ts_ = -1;
  ssize_t tg_;
};

// Literally wraps a rule in quietMatch(). Used for
// [optional] rules and "patterns", "(or | patterns)",
// and customized error (which requires suppressing existing errors).
class QuietMatch final : public WrapperRule {
 public:
  explicit QuietMatch(ssize_t idx) : WrapperRule{idx} {}
  std::string specifics_typename() const override { return "QuietMatch"; }
};

class SkipPoint final : public Rule {
 public:
  SkipPoint(ssize_t skipperIndex) : skipperIndex{skipperIndex} {}
  std::string specifics_typename() const override { return "SkipPoint"; }
  ssize_t skipperIndex; // Index into RuleSet::skips.
  OutputTypeInfo outType(const RuleSet& rs) const override {
    return {&rs, *this, OutputType::boolean};
  }
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
  OutputTypeInfo outType(const RuleSet& rs) const override {
    return {&rs, *this, OutputType::string};
  }
};

// Right now MatchOrError is used only with producesString() rules.
//
// This was introduced for initial testing. Today, we should be able to replace
// this with a composition of OrRule and ErrorRule.
//
// TODO Delete this, but make sure WrapperRule handling special case this
// composition too.
class MatchOrError final : public WrapperRule {
 public:
  MatchOrError(ssize_t compidx, std::string errmsg)
    : WrapperRule{compidx}, errmsg{std::move(errmsg)} {}
  std::string specifics_typename() const override { return "MatchOrError"; }
  std::string errmsg;
};

class AliasRule final : public WrapperRule {
 public:
  AliasRule(ssize_t targetidx) : WrapperRule{targetidx} {}
  std::string specifics_typename() const override { return "AliasRule"; }
};

class StringLoc;  // forward declaration

ssize_t expectedParamCount(std::string_view builtinSuff);

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
  OutputTypeInfo outType(const RuleSet& rs) const override {
    return {&rs, *this, OutputType::jsonLike};
  }
};

class RegexRule final : public Rule {
 public:
  explicit RegexRule(std::unique_ptr<const Regex> patt, ssize_t roi)
    : patt(std::move(patt)), regexOptsIdx(roi) {}
  std::string specifics_typename() const override { return "RegexRule"; }
  OutputTypeInfo outType(const RuleSet& rs) const override {
    return {&rs, *this, OutputType::string};
  }
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
  OutputTypeInfo outType(const RuleSet& rs) const override {
    return {&rs, *this, OutputType::string};
  }

  std::string val;
};

// TODO: Explicit representation for generated types, so codegen doesn't have
// to make decisions about what is optional and what needs a JsonLike.
//
// It should be trivial to look at this structure (which is the output of the
// compiler) and figure out what code is being generated.
//
// TODO this needs a debug() printer.
struct RuleSet {
  std::vector<std::unique_ptr<Rule>> rules;
  std::vector<Skipper> skips;
  std::vector<RegexOptions> regexOpts;
};

}  // namespace oalex
