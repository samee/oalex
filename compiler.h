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

// This file was refactored out of frontend.h. As a result, even today all of
// these functions are directly called by frontend.h.
#pragma once
#include <memory>
#include <string_view>
#include <utility>
#include <vector>

#include "runtime/diags.h"
#include "runtime/jsonlike.h"
#include "codegen.h"
#include "frontend_pieces.h"
#include "pattern.h"

struct ParsedExternRule;  // bootstrapped

namespace oalex {
// This class keeps location information around while we are still building
// up the vector<Rule>. This allows us to provide error messages such as
// "variable used here but never defined". Once we are sure there is no error,
// we can release rules() into the world for codegen, and forget location info.
class RulesWithLocs {
 public:
  ssize_t ssize() const { return rules_.size(); }
  const Rule& operator[](ssize_t i) const;

  /* Searches for ident in rules[].nameOrNull().
     If found, returns the index.
     If not found, appends a new UnassignedRule with the ident, and returns the
       index of the new element. In this case, it also records thisPos in
       firstUseLocs_.
     Assumes ident.empty() == false
  */
  ssize_t findOrAppendIdent(DiagsDest ctx, const Ident& id);

  /* Returns the index of a new DefinitionInProgress rule named ident.
     This is usually called when we start processing a definition. When we
     have the complete rule definition, the DefinitionInProgress is replaced
     with a real rule using deferred_assign().

       If an UnassignedRule with the same name exists, that is returned
         after changing it to DefinitionInProgress.
       If that name doesn't already exist,
         one is appended and the new index is returned.

     If some other rule named ident already exists, it produces a
     "multiple definition" error and returns -1.

     In case this appends a new entry, the firstUseLocs_ remains
     nrange() so that it is later filled in by findOrAppendIdent.

     This just so happens to also be a convenient place for us to record
     context skippers, in case the new rule is ever used by itself. The
     possible values can be any of Rule::ContextSkipper, or anything in
     the range [ 0, skips_.size() ).
  */
  ssize_t defineIdent(DiagsDest ctx, const Ident& ident,
                      ssize_t context_skipper);
  ssize_t defineIdentForTest(DiagsDest ctx, const Ident& ident)
    { return defineIdent(ctx, ident, Rule::removedContext); }

  /* Tries to reserve a local name just so we can later detect a conflict
     with a global name later. If a global name is already defined, this
     function will immediately raise an error, but otherwise not do much else.
  */
  void reserveLocalName(DiagsDest ctx, const Ident& ident);
  Ident findReservedLocalIdent(const Ident& ident) const;

  /* Utility for anon rules that also appends a dummy first-use location entry.
     Anonymous rules don't need usage location so far, since we never refer to
     them in error messages. They are implicitly generated, so the user won't
     know what to make of errors about rules they didn't write.

     Returns the index of the new element: newsize - 1

     Named rules should use defineIdent followed by direct assignment.
  */
  ssize_t appendAnonRulePtr(std::unique_ptr<Rule> rule);
  template <class X> ssize_t appendAnonRule(X x);

  /* Returns -1 if not found */
  ssize_t findIdent(DiagsDest ctx, const Ident& ident) const;

  /* For assigning to a rule after they have already been named */
  void deferred_assign_ptr(ssize_t idx, std::unique_ptr<Rule> rule);
  template <class X> void deferred_assign(ssize_t idx, X x);

  /* These Skipper objects are indexed to by SkipPoint{} */
  ssize_t addSkipper(Skipper skip);

  /* These RegexOptions objects are indexed
     to by regexOptsIdx of various Rules. */
  ssize_t addRegexOpts(RegexOptions regexOpts);

  /* The setter can only be called before any rule or skipper have been added.
     If called any later, it returns false without any effect. Otherwise it
     returns true, indicating success. */
  /* Dev-note: This is not part of the constructor, since we want to allow the
     user to set it somewhat late */
  bool defaultLexopts(LexDirective lexopts);
  LexDirective defaultLexopts() const;
  ssize_t defaultSkipper() const;  // Added by defaultLexOpts(...).

  /* This is checked just before producing rules as output */
  bool hasUndefinedRules(DiagsDest ctx) const;

  RuleSet releaseRules();

 private:
  // Invariant: these two must have equal sizes at all times.
  std::vector<std::unique_ptr<Rule>> rules_;
  std::vector<LocPair> firstUseLocs_;

  std::vector<Ident> reservedLocalNames_;
  std::vector<Skipper> skips_;
  std::vector<RegexOptions> regexOpts_;
};

// We generalize the ruleName in LocalBinding to the more general
// concept of "rule expressions".
class RuleExpr {
  protected: RuleExpr() {}
  public: virtual ~RuleExpr() {}
};
class RuleExprIdent final : public RuleExpr {
 public:
  explicit RuleExprIdent(Ident id) : ident{std::move(id)} {}
  Ident ident;
};
class RuleExprSquoted final : public RuleExpr {
 public:
  explicit RuleExprSquoted(std::string s) : s{std::move(s)} {}
  std::string s;
};
class RuleExprDquoted final : public RuleExpr {
 public:
  explicit RuleExprDquoted(lex::GluedString gs) : gs{std::move(gs)} {}
  lex::GluedString gs;
};
class RuleExprRegex final : public RuleExpr {
 public:
  explicit RuleExprRegex(std::unique_ptr<const Regex> r)
    : regex{std::move(r)} {}
  std::unique_ptr<const Regex> regex;
};
class RuleExprMappedIdent final : public RuleExpr {
 public:
  RuleExprMappedIdent(Ident l, std::unique_ptr<const RuleExpr> r)
    : lhs{std::move(l)}, rhs{std::move(r)} {}
  Ident lhs;
  std::unique_ptr<const RuleExpr> rhs;  // Only allow Ident, Regex, and Squoted
};
class RuleExprConcat final : public RuleExpr {
 // TODO: have a way to discard a component. Perhaps, with
 // a blank mapped ident, like so:  X ( _ ~ partA ) Y
 // Can be represented with a blank Ident in RuleExprMappedIdent.
 public:
  explicit RuleExprConcat(std::vector<std::unique_ptr<const RuleExpr>> p)
    : parts{std::move(p)} {}
  // Used both for (...) and [...],
  // wrapped in RuleExprOptional in the second case.
  std::vector<std::unique_ptr<const RuleExpr>> parts;
};
class RuleExprRepeat final : public RuleExpr {
 public:
  /* Must be RuleExprIdent or RuleExprSquoted, or RuleExprConcat containing only
     those types. The frontend is expected to disallow any further nesting or
     other types. */
  RuleExprRepeat(std::unique_ptr<const RuleExpr> part,
                 std::unique_ptr<const RuleExpr> glue)
    : part{std::move(part)}, glue{std::move(glue)} {}
  std::unique_ptr<const RuleExpr> part, glue;
};
class RuleExprOptional final : public RuleExpr {
 public:
  explicit RuleExprOptional(std::unique_ptr<const RuleExpr> p)
    : part{std::move(p)} {}
  std::unique_ptr<const RuleExpr> part;
};

// These are the usual entries in a `where:` stanza of a rule. An entry:
//
//   "patt" as var ~ lhs
//
// is represented as { .pp = "patt", .localName = var, .ruleExpr = lhs }
struct LocalBinding {
  PartPattern pp;
  Ident localName;
  std::unique_ptr<const RuleExpr> ruleExpr;
};

// This struct stores the result of parsing `where`, `outputs`, `errors`,
// and everything else that can accompany a rule. It's meant to be reused
// between pattern rules and expression rules.
//
// Here, many of the values here have default values, which we use if that
// stanza was missing or had some error. The various `saw*Kw` booleans are
// meant to help with diagnostics: they tell the caller if the corresponding
// keyword was completely missing, or if it was present but had other errors
// that resulted in us using the default value.
struct RuleStanzas {
  bool sawOutputsKw = false, sawWhereKw = false, sawLexicalKw = false;
  bool sawErrorsKw = false;
  JsonTmpl jstmpl = JsonTmpl::Ellipsis{};
  std::vector<LocalBinding> local_decls = {};
  LexDirective lexopts;
  ParsedIndentedList errors = {};
  explicit RuleStanzas(LexDirective lo) : lexopts{std::move(lo)} {}
};

void
appendExternRule(const ParsedExternRule& ext, DiagsDest ctx, RulesWithLocs& rl);

// Don't use this for local rules. This function takes in a global name for a
// new rule. Local rules are added implicitly using locals parameters.
// The name is intentionally backwards: ExprRule is meant to mirror ExternRule.
void
appendExprRule(DiagsDest ctx, const Ident& ruleName, const RuleExpr& rxpr,
               const RuleStanzas& stz, RulesWithLocs& rl);

// Dev-note: Right now, we only support a few combinations:
//
// * The action can be either:
//   - ident, represented with
//       { target=&RuleExprIdent{...}, diagMsg="", diagType=none }
//   - error "msg"
//       { target=nullptr, diagMsg="msg", diagType=error }
//   - quiet error
//       { target=nullptr, diagMsg="", diagType=error }
// * In all three actions, the lookahead is optional. But if present, it
//   must either be RuleExprIdent or RuleExprSquoted.
//
// Over time, we are likely to implement more of the valid combinations, as
// well as add warning diagType.
struct RuleBranch {
  std::unique_ptr<const RuleExpr> lookahead, target;
  std::string diagMsg;
  enum class DiagType { none, error } diagType;
};

void
appendMultiExprRule(DiagsDest ctx, const Ident& ruleName,
                    std::vector<RuleBranch> branches, const RuleStanzas& stz,
                    RulesWithLocs& rl);

// Internal functions, exposed for testing only
// Dev-note: consider a separate compiler_testables.h

class Ident;
class JsonLoc;
std::vector<std::pair<Ident, std::string>>
destructureErrors(DiagsDest ctx, const ParsedIndentedList& errors);

}  // namespace oalex
