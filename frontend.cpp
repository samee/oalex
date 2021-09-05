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

#include "frontend.h"

#include <algorithm>
#include <map>
#include <memory>
#include <optional>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>
#include "fmt/core.h"

#include "lexer.h"
#include "pattern.h"
#include "regex_io.h"
#include "jsontmpl_parsers.h"
#include "runtime/util.h"

using fmt::format;
using oalex::DiagsDest;
using oalex::LexDirective;
using oalex::Note;
using oalex::OutputTmpl;
using oalex::parseJsonTmplFromBracketGroup;
using oalex::parsePattern;
using oalex::parseRegexCharSet;
using oalex::passthroughTmpl;
using oalex::Pattern;
using oalex::tokenizePattern;
using oalex::WholeSegment;
using oalex::lex::enPos;
using oalex::lex::BracketGroup;
using oalex::lex::BracketType;
using oalex::lex::ExprToken;
using oalex::lex::GluedString;
using oalex::lex::IndentCmp;
using oalex::lex::indentCmp;
using oalex::lex::inputSegment;
using oalex::lex::isToken;
using oalex::lex::lexIndentedSource;
using oalex::lex::lexListEntries;
using oalex::lex::lexNextLine;
using oalex::lex::lookaheadParIndent;
using oalex::lex::NewlineChar;
using oalex::lex::oalexSkip;
using oalex::lex::oalexWSkip;
using oalex::lex::RegexPattern;
using oalex::lex::stPos;
using std::get_if;
using std::make_unique;
using std::map;
using std::nullopt;
using std::optional;
using std::pair;
using std::sort;
using std::string;
using std::string_view;
using std::tuple;
using std::unique;
using std::unique_ptr;
using std::vector;

namespace oalex {

MappedPos::operator string() const {
  return "line " + itos(this->line);
}

bool
Expectation::matches(const JsonLoc& jsloc,
                     const std::vector<Diag>& diags) const {
  if(Example::runSucceeded(jsloc, diags) != this->isForSuccess()) return false;
  switch(matchType_) {
    case Expectation::successMatchingOutput:
      return jsloc == jstmpl_.outputIfFilled();
    case Expectation::successAnyOutput: return true;
    case Expectation::failedWithErrorSubstr:
      for(const auto& d: diags) if(isSubstr(errorSubstr_, d.msg)) return true;
      return false;
    default: Bug("Unknown Expectation type {}", matchType_);
  }
  return false;
}

namespace {

string
debug(const ExprToken& x) {
  if(auto* tok = get_if<WholeSegment>(&x)) return **tok;
  else if(auto* s = get_if<GluedString>(&x)) return "\"" + string(*s) + "\"";
  else return "(bracket group)";
}

// TODO move to ident.cpp and provide validity guarnatees.
bool
resemblesIdent(const ExprToken& x) {
  auto* seg = get_if<WholeSegment>(&x);
  if(!seg) return false;
  const string& s = **seg;
  if(s.empty() || isdigit(s[0])) return false;
  for(char ch : s) if(!isalnum(ch) && ch != '_') return false;
  return true;
}

using LocPair = pair<ssize_t,ssize_t>;
constexpr LocPair nrange{-1,-1};

// TODO move this class to a different file so we can unit-test it.
// This class keeps location information around while we are still building
// up the vector<Rule>. This allows us to provide error messages such as
// "variable used here but never defined". Once we are sure there is no error,
// we can release rules() into the world for codegen, and forget location info.
class RulesWithLocs {
 public:
  ssize_t ssize() const { return rules_.size(); }
  Rule& operator[](ssize_t i) {
    if(rules_[i]) return *rules_[i];
    else Bug("Dereferencing null Rule at index {}", i);
  }

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
  template <class X> auto deferred_assign(ssize_t idx, X x);

  /* This is checked just before producing rules as output */
  bool hasUndefinedRules(DiagsDest ctx) const;

  /* Reduces sizes of rules_ and firstUseLocs_ to n, if it's larger */
  void resize_down(ssize_t n) noexcept;

  vector<unique_ptr<Rule>> releaseRules();

 private:
  // Invariant: these two must have equal sizes at all times.
  vector<unique_ptr<Rule>> rules_;
  vector<LocPair> firstUseLocs_;
};

ssize_t
RulesWithLocs::findOrAppendIdent(const Ident& id) {
  LocPair thisPos{id.stPos(), id.enPos()};
  for(ssize_t i=0; i<this->ssize(); ++i) {
    const Ident* name = rules_[i]->nameOrNull();
    if(name == nullptr || id != *name) continue;
    if(firstUseLocs_[i] == nrange) firstUseLocs_[i] = thisPos;
    return i;
  }
  rules_.push_back(make_unique<UnassignedRule>(id));
  firstUseLocs_.push_back(thisPos);
  return this->ssize()-1;
}

void
logLocalNamesakeError(DiagsDest ctx, const Ident& ident) {
  Error(ctx, ident.stPos(), ident.enPos(),
        format("Local variable name '{}' conflicts with a global name",
               ident.preserveCase()));
}

ssize_t
RulesWithLocs::defineIdent(DiagsDest ctx, const Ident& ident) {
  for(ssize_t i=0; i<this->ssize(); ++i) {
    const Ident* name = rules_[i]->nameOrNull();
    if(name == nullptr || ident != *name) continue;
    if(dynamic_cast<const ReservedRule*>(rules_[i].get())) {
      logLocalNamesakeError(ctx, *name);
      rules_[i] = make_unique<UnassignedRule>(ident);
      return i;
    }else if(!dynamic_cast<const UnassignedRule*>(rules_[i].get())) {
      Error(ctx, ident.stPos(), ident.enPos(),
            format("'{}' has multiple definitions", ident.preserveCase()));
      return -1;
    }else return i;
  }
  rules_.push_back(make_unique<UnassignedRule>(ident));
  firstUseLocs_.push_back(nrange);
  return this->ssize()-1;
}

void
RulesWithLocs::reserveLocalName(DiagsDest ctx, const Ident& ident) {
  for(ssize_t i=0; i<this->ssize(); ++i) {
    const Ident* name = rules_[i]->nameOrNull();
    if(name == nullptr || ident != *name) continue;
    if(dynamic_cast<const UnassignedRule*>(rules_[i].get())) {
      rules_[i] = make_unique<ReservedRule>(ident);
    }else if(!dynamic_cast<const ReservedRule*>(rules_[i].get())) {
      logLocalNamesakeError(ctx, ident);
      return;
    }
  }
  rules_.push_back(make_unique<ReservedRule>(ident));
  firstUseLocs_.push_back(nrange);
}

template <class X> ssize_t
RulesWithLocs::appendAnonRule(X x) {
  rules_.push_back(move_to_unique(x));
  firstUseLocs_.emplace_back(-1, -1);
  return rules_.size()-1;
}

bool
RulesWithLocs::hasUndefinedRules(DiagsDest ctx) const {
  for(ssize_t i=0; i<this->ssize(); ++i)
    if(dynamic_cast<const UnassignedRule*>(rules_[i].get())) {
      const Ident* name = rules_[i]->nameOrNull();
      if(!name) Bug("Anonymous rules should always be initialized");
      const auto [st, en] = firstUseLocs_[i];
      Error(ctx, st, en, format("Rule '{}' was used but never defined",
                                name->preserveCase()));
      return true;
    }
  return false;
}

void
RulesWithLocs::resize_down(ssize_t n) noexcept {
  if(n >= this->ssize()) return;
  // Don't use erase() because it needs an assignment operator.
  // Don't use resize() because it needs default constructor.
  // Repeated pop_back() it is.
  while(n < this->ssize()) rules_.pop_back();
  firstUseLocs_.resize(n);
}

vector<unique_ptr<Rule>>
RulesWithLocs::releaseRules() {
  firstUseLocs_.clear();
  return std::move(rules_);  // This is guaranteed to clear rules_.
}

template <class X>
auto RulesWithLocs::deferred_assign(ssize_t idx, X x) {
  if(rules_[idx] != nullptr &&
     !dynamic_cast<const UnassignedRule*>(rules_[idx].get()))
    oalex::Bug("deferred_assign() cannot be used a rule already assigned");
  Ident name;
  if(rules_[idx] != nullptr) {
    if(auto name2 = rules_[idx]->nameOrNull()) name = std::move(*name2);
  }
  x.deferred_name(std::move(name));
  rules_[idx] = move_to_unique(x);
}

bool
requireEol(const vector<ExprToken>& linetoks, size_t eolPos, DiagsDest ctx) {
  if(linetoks.size() < eolPos)
    Bug("requireEol({}) assumes earlier tokens are already processed", eolPos);
  if(linetoks.size() > eolPos) {
    Error(ctx, stPos(linetoks[eolPos]), "Expected end of line");
    return false;
  }
  return true;
}

char
bracketStart(BracketType bt) {
  switch(bt) {
    case BracketType::square: return '[';
    case BracketType::brace: return '{';
    case BracketType::paren: return '(';
    default: Bug("Unknown BracketType {}", int(bt));
  }
}

bool
requireBracketType(const BracketGroup& bg, BracketType bt, DiagsDest ctx) {
  if(bg.type != bt) {
    Error(ctx, bg.stPos, format("Was expecting '{}'", bracketStart(bt)));
    return false;
  }else return true;
}

template <class T> T*
get_if_in_bound(vector<ExprToken>& toks, size_t i, DiagsDest ctx) {
  if(toks.empty()) Bug("get_if_in_bound expects non-empty input");
  if(i >= toks.size()) {
    Error(ctx, enPos(toks.back()), "Unexpected end of expression");
    return nullptr;
  }
  return get_if<T>(&toks[i]);
}

template <class T> const T*
get_if_in_bound(const vector<ExprToken>& toks, size_t i, DiagsDest ctx) {
  if(toks.empty()) Bug("get_if_in_bound expects non-empty input");
  if(i >= toks.size()) {
    Error(ctx, enPos(toks.back()), "Unexpected end of expression");
    return nullptr;
  }
  return get_if<T>(&toks[i]);
}

/*
resemblesX() vs parseX().
  - resemblesX() is the lookahead. It does enough sanitization to commit to
    this parsing branch. It only returns a bool, never any diagnosis.
  - parseX() is called after resemblesX() passes. It can still fail, but
    is expected to produce actual diagnostics. Failure does not cause
    backtracking. Indeed, it can choose to consume additional characters in
    the face of an error just so subsequent parsing has a better chance
    of making forward progress.
*/
bool
resemblesBnfRule(const vector<ExprToken>& linetoks) {
  return linetoks.size() >= 2 && isToken(linetoks[1], ":=");
}

void
assignLiteralOrError(RulesWithLocs& rl, size_t ruleIndex, string_view literal) {
  rl.deferred_assign(ruleIndex, MatchOrError{
      rl.ssize(), format("Expected '{}'", literal)
  });
  rl.appendAnonRule(StringRule(string(literal)));
}
ssize_t
appendLiteralOrError(RulesWithLocs& rl, string_view literal) {
  ssize_t newIndex = rl.ssize();
  rl.appendAnonRule(UnassignedRule{});
  assignLiteralOrError(rl, newIndex, literal);
  return newIndex;
}

void
assignRegexOrError(RulesWithLocs& rl, size_t ruleIndex,
                   string errmsg, RegexPattern regex) {
  rl.deferred_assign(ruleIndex, MatchOrError{rl.ssize(), std::move(errmsg)});
  rl.appendAnonRule(RegexRule{std::move(regex.patt)});
}

ssize_t
appendWordOrError(RulesWithLocs& rl, string_view word) {
  ssize_t newIndex = rl.ssize();
  rl.appendAnonRule(WordPreserving{word});
  rl.appendAnonRule(MatchOrError{newIndex, format("Expected '{}'", word)});
  return newIndex + 1;
}
ssize_t
appendRegexOrError(RulesWithLocs& rl, unique_ptr<const Regex> regex) {
  ssize_t newIndex = rl.ssize();
  rl.appendAnonRule(RegexRule{std::move(regex)});
  rl.appendAnonRule(MatchOrError{newIndex, "Does not match expected pattern"});
  return newIndex + 1;
}

/* This function is called when linetoks is of the form
   {someVar, ":=", "Concat", ...}. It ignores these first 3 tokens, then
   parses the part after "Concat". On success, it returns a ConcatRule that
   should be inserted into findOrAppendIdent(someVar). On failure, it returns
   nullopt.
*/
optional<ConcatRule>
parseConcatRule(vector<ExprToken> linetoks, DiagsDest ctx, RulesWithLocs& rl) {
  auto* bg = get_if_in_bound<BracketGroup>(linetoks, 3, ctx);
  if(!bg) return nullopt;
  if(!requireBracketType(*bg, BracketType::square, ctx)) return nullopt;
  vector<vector<ExprToken>> comps =
    splitCommaNoEmpty(ctx, std::move(bg->children));
  if(comps.empty()) return Error(ctx, *bg, "Concat rule cannot be empty");

  BracketGroup* tmpl = nullptr;
  if(linetoks.size() > 4) {
    if(!isToken(linetoks[4], "->")) {
      return Error(ctx, linetoks[4], "Was expecting end of line or an '->'");
    }
    tmpl = get_if_in_bound<BracketGroup>(linetoks, 5, ctx);
    if(!tmpl)
      return Error(ctx, enPos(linetoks[4]), "Expected braces after this");
    if(!requireBracketType(*tmpl, BracketType::brace, ctx))
      return nullopt;
  }

  ConcatRule concat{ {}, JsonTmpl::Map() };
  size_t argc = 0;
  for(auto&& comp : comps) {
    string argname = "arg" + itos(++argc);
    if(comp.size() >= 2 && isToken(comp[0], "word")) {
      const auto* s = get_if<GluedString>(&comp[1]);
      if(s == nullptr || s->ctor() != GluedString::Ctor::squoted) {
        Error(ctx, comp[1], "Expected quoted string");
        continue;
      }
      ssize_t newIndex = appendWordOrError(rl, *s);
      concat.comps.push_back({newIndex, argname});
      if(comp.size() > 2) {
        Error(ctx, comp[2], "Was expecting a comma");
        continue;
      }
      continue;
    }
    if(comp.size() > 1) {
      Error(ctx, comp[1], "Expected ','");
      continue;
    }
    if(const auto* tok = get_if<WholeSegment>(&comp[0])) {
      if(Ident id = Ident::parse(ctx, *tok))
        concat.comps.push_back({rl.findOrAppendIdent(id), argname});
      else return nullopt;
    }else if(const auto* s = get_if<GluedString>(&comp[0])) {
      if(s->ctor() != GluedString::Ctor::squoted) {
        Error(ctx, *s, "Expected strings to be single-quoted");
        continue;
      }
      ssize_t newIndex = rl.ssize();
      appendLiteralOrError(rl, *s);
      concat.comps.push_back({newIndex, argname});
    }else if(auto* regex = get_if<RegexPattern>(&comp[0])) {
      ssize_t newIndex = appendRegexOrError(rl, std::move(regex->patt));
      concat.comps.push_back({newIndex, argname});
    }else {
      Error(ctx, comp[0], "Was expecting a string or an identifier");
      continue;
    }
  }
  if(tmpl != nullptr) {
    if(auto opt = parseJsonTmplFromBracketGroup(ctx, std::move(*tmpl)))
      concat.outputTmpl = std::move(*opt);
    if(!requireEol(linetoks, 6, ctx)) return nullopt;
  }
  return concat;
}

optional<SkipPoint>
parseSkipPoint(const vector<ExprToken>& linetoks, DiagsDest ctx) {
  auto* seg = get_if_in_bound<WholeSegment>(linetoks, 3, ctx);
  if(!seg) return nullopt;
  bool withinLine;
  if(**seg == "withinLine") withinLine = true;
  else if(**seg == "acrossLines") withinLine = false;
  else return Error(ctx, *seg, "Expected either 'withinLine' or 'acrossLines'");
  if(!requireEol(linetoks, 4, ctx)) return nullopt;
  return SkipPoint{ /* stayWithinLine */ withinLine, &oalexSkip};
}

// Resets RulesWithLoc back to initial size unless disarm() is called.
class RuleRewinder {
 public:
  explicit RuleRewinder(RulesWithLocs& rl)
    : rl{&rl}, orig_size{rl.ssize()} {}
  ~RuleRewinder() { if(rl) rl->resize_down(orig_size); }
  void disarm() { rl = nullptr; }
 private:
  RulesWithLocs* rl;
  ssize_t orig_size;
};

void
parseBnfRule(vector<ExprToken> linetoks, DiagsDest ctx, RulesWithLocs& rl) {
  const Ident ident = Ident::parse(ctx, std::get<WholeSegment>(linetoks[0]));
  if(!ident) {
    Error(ctx, linetoks[0], "Identifier expected");
    return;
  }
  RuleRewinder rewinder(rl);
  const ssize_t ruleIndex = rl.defineIdent(ctx, ident);
  if(ruleIndex == -1) return;
  if(linetoks.size() < 3) {
    Error(ctx, linetoks[1], "Rule's right-hand side missing");
  }else if(const auto* literal = get_if<GluedString>(&linetoks[2])) {
    if(!requireEol(linetoks, 3, ctx)) return;
    assignLiteralOrError(rl, ruleIndex, *literal);
    rewinder.disarm();
  }else if(auto* regex = get_if<RegexPattern>(&linetoks[2])) {
    if(!requireEol(linetoks, 3, ctx)) return;
    string errmsg = format("Expected {}", ident.preserveCase());
    assignRegexOrError(rl, ruleIndex, std::move(errmsg), std::move(*regex));
    rewinder.disarm();
  }else if(isToken(linetoks[2], "Concat")) {
    if(optional<ConcatRule> c = parseConcatRule(std::move(linetoks),ctx,rl)) {
      rl.deferred_assign(ruleIndex, std::move(*c));
      rewinder.disarm();
    }
  }else if(isToken(linetoks[2], "SkipPoint")) {
    if(optional<SkipPoint> sp = parseSkipPoint(linetoks, ctx)) {
      rl.deferred_assign(ruleIndex, std::move(*sp));
      rewinder.disarm();
    }
  }else {
    Error(ctx, linetoks[2], "Expected string literal");
  }
}

// Returns true iff tokens is a sequence of WholeSegments matching
// expectations. Empty elements in expectations are treated as wildcards,
// and they will match anything, even other ExprToken types.
// But thsoe wildcards do need *some* token to match against, so we return false
// if tokens.size() is too small.
bool
matchesTokens(const vector<ExprToken>& tokens, ssize_t start,
              const vector<string_view>& expectations) {
  if(start + tokens.size() < expectations.size()) return false;
  for(size_t i=0; i<expectations.size(); ++i)
    if(!expectations[i].empty() && !isToken(tokens[start+i], expectations[i]))
      return false;
  return true;
}

bool
matchesTokens(const vector<ExprToken>& tokens,
              const vector<string_view>& expectations) {
  return matchesTokens(tokens, 0, expectations);
}

WholeSegment
indent_of(const Input& input, const ExprToken& tok) {
  ssize_t bol = input.bol(stPos(tok));
  ssize_t indent_end = oalexWSkip.withinLine(input, bol);
  return inputSegment(bol, indent_end, input);
}

bool
requireCompatIndent(IndentCmp cmpres, DiagsDest ctx,
                    const WholeSegment& nextLineIndent) {
  if(cmpres == IndentCmp::bad) {
    Error(ctx, nextLineIndent,
          "Bad mix of spaces and tabs compared to the previous line");
    return false;
  }
  return true;
}

bool
requireGoodIndent(DiagsDest ctx, string_view desc, const WholeSegment& indent1,
                  const WholeSegment& indent2) {
  IndentCmp cmpres = indentCmp(*indent1, *indent2);
  if(!requireCompatIndent(cmpres, ctx, indent2)) return false;
  else if(cmpres != IndentCmp::lt) {
    Error(ctx, indent2, format("{} needs more indentation", desc));
    return false;
  }
  else return true;
}

// Dev-note: This comment block was associated with a now-removed function
// called trimNewlines(). The comment is left behind since it raises valid
// issues that will likely have to be figured out at some point.
//
// Decide how lexIndentedSource should treat leading and trailing newlines.
// As a general rule, if newlines matter, the user should be encouraged to use
// fenced inputs or quoted inputs.
//
// As a corollary, we shouldn't call trimNewlines() on fenced inputs.
//
// Option (a) Trim all surrounding newlines
//        (b) Keep one single trailing newline, but trim the rest
//        (c) Keep them all
//        (d) Trim all newlines, but replace them with regex anchors ^ and $
//
// Constraints:
//
//   * "Equivalent" inputs of fenced and indented
//      source blocks should behave identically
//   * Rules and Examples with identical string
//     literal inputs must always match.
//   * When one rule is composed inside another concatenation,
//     the result need to make sense. E.g. Never require two newlines
//     just because one of the internal components happened to have an
//     implicit trailing newline. We should also allow composition without
//     newlines.
//
// Preference: Maybe don't allow surprise matches that start or end mid-line.

const LexDirective&
defaultLexopts() {
  static const auto* var = new LexDirective{
    parseRegexCharSet("[_a-zA-Z]"),
    Skipper{ {{"/*","*/"},{"//","\n"}}, {} },
    false
  };
  return *var;
}

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
  const vector<pair<Ident,ssize_t>>* p2rule_;  // Rule index for placeholders
  ssize_t processConcat(const PatternConcat& concatPatt);
  ssize_t processOrList(const PatternOrList& orPatt);
  ssize_t processOptional(const PatternOptional& optPatt);
  ssize_t processIdent(const Ident& ident);
  ssize_t processRepeat(const PatternRepeat& repPatt);
  ssize_t processFold(const PatternFold& foldPatt);
 public:
  PatternToRulesCompiler(DiagsDest ctx, RulesWithLocs& rl,
                         const vector<pair<Ident,ssize_t>>& p2rule) :
    ctx_(ctx), rl_(&rl), p2rule_(&p2rule) {}
  // Just to prevent accidental copying.
  PatternToRulesCompiler(const PatternToRulesCompiler&) = delete;
  ssize_t process(const Pattern& patt);
};

ssize_t
PatternToRulesCompiler::processConcat(const PatternConcat& concatPatt) {
  ConcatFlatRule concatRule{ {} };
  for(ssize_t i = 0; i < (ssize_t)concatPatt.parts.size(); ++i) {
    if(i > 0) {
      // Intersperse concat components with SkipPoint components.
      concatRule.comps.push_back({rl_->ssize(), {}});
      rl_->appendAnonRule(SkipPoint{/* stayWithinLine */ false, &oalexSkip});
    }
    const Pattern& child = concatPatt.parts[i];
    ssize_t j = this->process(child);
    concatRule.comps.push_back({j, {}});
  }
  rl_->appendAnonRule(std::move(concatRule));
  return rl_->ssize()-1;
}

// TODO failed tests should output location. Or tests should have names.
ssize_t
PatternToRulesCompiler::processOrList(const PatternOrList& orPatt) {
  OrRule orRule{{}, /* flattenOnDemand */ true};
  for(ssize_t i = 0; i < ssize(orPatt.parts); ++i) {
    const Pattern& child = orPatt.parts[i];
    ssize_t j = this->process(child);
    if(i+1 != ssize(orPatt.parts)) j = rl_->appendAnonRule(QuietMatch{j});
    orRule.comps.push_back({-1, j, passthroughTmpl});
  }
  rl_->appendAnonRule(std::move(orRule));
  return rl_->ssize()-1;
}

ssize_t
PatternToRulesCompiler::processOptional(const PatternOptional& optPatt) {
  ssize_t i;
  OrRule orRule{{}, /* flattenOnDemand */ true};

  i = this->process(optPatt.part);
  i = rl_->appendAnonRule(QuietMatch{i});
  orRule.comps.push_back({-1, i, passthroughTmpl});

  // This branch always produces a Map on success.
  i = rl_->appendAnonRule(StringRule{{}});
  orRule.comps.push_back({-1, i, JsonTmpl::Map{}});

  return rl_->appendAnonRule(std::move(orRule));
}

ssize_t
PatternToRulesCompiler::processIdent(const Ident& ident) {
  size_t i;
  for(i=0; i<p2rule_->size(); ++i) if(ident == p2rule_->at(i).first) break;
  if(i == p2rule_->size())
    Bug("Ident map should already have an entry for '{}'",
        ident.preserveCase());
  return rl_->appendAnonRule(ConcatFlatRule{{
      {p2rule_->at(i).second, ident.preserveCase()},
  }});
}

ssize_t
PatternToRulesCompiler::processRepeat(const PatternRepeat& repPatt) {
  ssize_t i = this->process(repPatt.part);
  ssize_t ski = rl_->appendAnonRule(SkipPoint{/* stayWithinLine */ false,
                                              &oalexSkip});
  return rl_->appendAnonRule(LoopRule{{
      .partidx = i, .partname = "", .glueidx = -1, .gluename = "",
      .lookidx = -1, .skipidx = ski}});
}

ssize_t
PatternToRulesCompiler::processFold(const PatternFold& foldPatt) {
  ssize_t pi = this->process(foldPatt.part);
  ssize_t gi = this->process(foldPatt.glue);
  ssize_t ski = rl_->appendAnonRule(SkipPoint{/* stayWithinLine */ false,
                                              &oalexSkip});
  return rl_->appendAnonRule(LoopRule{{
      .partidx = pi, .partname = "", .glueidx = gi, .gluename = "",
      .lookidx = -1, .skipidx = ski}});
}

ssize_t
PatternToRulesCompiler::process(const Pattern& patt) {
  if(auto* word = get_if_unique<WordToken>(&patt)) {
    return appendWordOrError(*rl_, **word);
  }else if(auto* oper = get_if_unique<OperToken>(&patt)) {
    return appendLiteralOrError(*rl_, **oper);
  }else if(get_if_unique<NewlineChar>(&patt)) {
    return appendLiteralOrError(*rl_, "\n");
  }else if(auto* ident = get_if_unique<Ident>(&patt)) {
    return processIdent(*ident);
  }else if(auto* concatPatt = get_if_unique<PatternConcat>(&patt)) {
    return processConcat(*concatPatt);
  }else if(auto* orPatt = get_if_unique<PatternOrList>(&patt)) {
    return processOrList(*orPatt);
  }else if(auto* optPatt = get_if_unique<PatternOptional>(&patt)) {
    return processOptional(*optPatt);
  }else if(auto* repPatt = get_if_unique<PatternRepeat>(&patt)) {
    return processRepeat(*repPatt);
  }else if(auto* foldPatt = get_if_unique<PatternFold>(&patt)) {
    return processFold(*foldPatt);
  }else {
    Unimplemented("Pattern compilation of index {}", patt.index());
  }
}

// Looks for [p, ...] for some `JsonTmpl::Placeholder p`. If found, returns
// &p, which is guaranteed to point to a JsonTmpl::Placeholder. If not, returns
// nullptr. Produces error only for [p, ..., "extra items"]
const JsonTmpl*
simpleListPlaceholder(DiagsDest ctx, const JsonTmpl& jstmpl) {
  const JsonTmpl::Vector* vec = jstmpl.getIfVector();
  if(!vec || vec->size() < 2 || !vec->at(1).holdsEllipsis()) return nullptr;
  if(vec->at(0).holdsPlaceholder()) {
    if(vec->size() > 2)
      Error(ctx, vec->at(2).stPos, "This list should have ended here");
    // Return even if size() > 2, just ignore the other elements.
    return &(*vec)[0];
  }
  return nullptr;
}

// Return false means there were uncleaned ellipsis left in.
// TODO generalize this to accept interlaced outputs.
bool
desugarEllipsisPlaceholdersRecur(DiagsDest ctx, JsonTmpl& jstmpl,
                                 vector<WholeSegment>& listNames) {
  if(const JsonTmpl* p = simpleListPlaceholder(ctx, jstmpl)) {
    listNames.emplace_back(p->stPos, p->enPos, p->getIfPlaceholder()->key);
    jstmpl = *p;
    return true;
  }
  if(jstmpl.holdsEllipsis()) {
    Error(ctx, jstmpl.stPos, jstmpl.enPos, "Ellipsis out of place");
    return false;
  }
  if(jstmpl.holdsString() || jstmpl.holdsPlaceholder()) return true;
  bool rv = true;
  if(auto* v = jstmpl.getIfVector()) {
    for(auto& elt : *v)
      if(!desugarEllipsisPlaceholdersRecur(ctx, elt, listNames))
        rv = false;
  }else if(auto* m = jstmpl.getIfMap()) {
    for(auto& [k,v] : *m)
      if(!desugarEllipsisPlaceholdersRecur(ctx, v, listNames))
        rv = false;
  }else Bug("Unknown JsonTmpl variant in desugarEllipsis {}", jstmpl.tagName());
  return rv;
}

// If any ellipsis is found out of place, the whole tmpl is cleared empty
// and an error is logged. The idea is that we should still be able to
// continue compilation. Modifies jstmpl in-place to convert any
// `[p, ...]` to just `p`. Returns all the converted placeholders
// in an array, so caller can figure out which placeholders were converted.
vector<WholeSegment>
desugarEllipsisPlaceholders(DiagsDest ctx, JsonTmpl& jstmpl) {
  vector<WholeSegment> rv;
  if(!desugarEllipsisPlaceholdersRecur(ctx, jstmpl, rv)) {
    jstmpl = JsonTmpl::Map{};
    rv.clear();
  }else {
    sort(rv.begin(), rv.end(), [](auto& a, auto& b) { return *a < *b; });
    rv.erase(unique(rv.begin(), rv.end(),
                    [](auto& a, auto& b) { return *a == *b; }),
             rv.end());
  }
  return rv;
}

// Checks if all Ident Patterns nested in PatternRepeat or PatternFold
// are in present listNames and, conversely, if Ident Patterns not in those
// constructs are absent in listNames. `repeat` indicates if we are currently
// inside a fold or repeat pattern.
bool
checkPlaceholderTypes(DiagsDest ctx, const vector<WholeSegment>& listNames,
                      const Pattern& patt, bool repeat) {
  if(holds_one_of_unique<WordToken, OperToken, NewlineChar>(patt)) return true;
  else if(auto* id = get_if_unique<Ident>(&patt)) {
    bool rv = !repeat;
    for(auto& n : listNames) if(*n == id->preserveCase()) {
      rv = repeat;
      break;
    }
    if(!rv) {
      string msg = format(repeat ? "Should be list-expanded: [{}, ...]"
                                 : "`{}` is a single element",
                          id->preserveCase());
      Error(ctx, id->stPos(), id->enPos(), msg);
    }
    return rv;
  }else if(auto* seq = get_if_unique<PatternConcat>(&patt)) {
    for(auto& elt : seq->parts)
      if(!checkPlaceholderTypes(ctx, listNames, elt, repeat))
        return false;
    return true;
  }else if(auto* ors = get_if_unique<PatternOrList>(&patt)) {
    for(auto& elt : ors->parts)
      if(!checkPlaceholderTypes(ctx, listNames, elt, repeat))
        return false;
    return true;
  }else if(auto* opt = get_if_unique<PatternOptional>(&patt)) {
    return checkPlaceholderTypes(ctx, listNames, opt->part, repeat);
  }else if(auto* rep = get_if_unique<PatternRepeat>(&patt)) {
    return checkPlaceholderTypes(ctx, listNames, rep->part, true);
  }else if(auto* fold = get_if_unique<PatternFold>(&patt)) {
    return checkPlaceholderTypes(ctx, listNames, fold->part, true) &&
           checkPlaceholderTypes(ctx, listNames, fold->glue, true);
  }else
    Bug("Unknown pattern index in checkPlaceholderTypes() {}", patt.index());
}

bool
checkMultipleTmplPartsRecur(DiagsDest ctx,
                            vector<pair<string, int>>& counts,
                            const Pattern& patt) {
  if(holds_one_of_unique<WordToken, OperToken, NewlineChar>(patt)) return true;
  else if(auto* id = get_if_unique<Ident>(&patt)) {
    for(auto& [k, count] : counts) if(k == id->preserveCase() && ++count > 1) {
      Error(ctx, id->stPos(), id->enPos(),
        format("Output part '{}' appears multiple times in the pattern", k));
      return false;
    }
    return true;
  }else if(auto* seq = get_if_unique<PatternConcat>(&patt)) {
    for(auto& elt : seq->parts)
      if(!checkMultipleTmplPartsRecur(ctx, counts, elt))
        return false;
    return true;
  }else if(auto* ors = get_if_unique<PatternOrList>(&patt)) {
    for(auto& elt : ors->parts)
      if(!checkMultipleTmplPartsRecur(ctx, counts, elt))
        return false;
    return true;
  }else if(auto* opt = get_if_unique<PatternOptional>(&patt)) {
    return checkMultipleTmplPartsRecur(ctx, counts, opt->part);
  }else if(auto* rep = get_if_unique<PatternRepeat>(&patt)) {
    return checkMultipleTmplPartsRecur(ctx, counts, rep->part);
  }else if(auto* fold = get_if_unique<PatternFold>(&patt)) {
    return checkMultipleTmplPartsRecur(ctx, counts, fold->part) &&
           checkMultipleTmplPartsRecur(ctx, counts, fold->glue);
  }else
    Bug("Unknown pattern index in checkMultipleTmplParts() {}", patt.index());
}

// Dev-note: we do intend to allow Pattern Idents that do *not* appear in
// the output template. While such Idents cannot be represented in the input
// syntax today, it will become possible later.
bool
checkMultipleTmplParts(DiagsDest ctx, const JsonTmpl::ConstPlaceholderMap& m,
                       const Pattern& patt) {
  vector<pair<string, int>> counts;
  for(auto& [k,v] : m) counts.emplace_back(k, 0);
  return checkMultipleTmplPartsRecur(ctx, counts, patt);
}

// We can later add where-stanza arguments for extracting partPatterns
map<Ident,PartPattern>
makePartPatterns(DiagsDest ctx, const JsonTmpl& jstmpl) {
  if(jstmpl.holdsVector())
    Unimplemented("Directly outputting list not encased in a map");
  const JsonTmpl::Map* jstmplmap = jstmpl.getIfMap();
  if(jstmplmap == nullptr)
    Bug("parseJsonTmplFromBracketGroup() returned something strange");

  map<Ident, PartPattern> rv;
  for(const auto& [p, j] : jstmpl.allPlaceholders()) {
    WholeSegment seg(j->stPos, j->enPos, p);
    Ident id = Ident::parse(ctx, seg);
    if(id) rv.insert({id, GluedString(std::move(seg))});
  }
  return rv;
}

void
registerLocations(RulesWithLocs& rl, const Ident& id) {
  rl.findOrAppendIdent(id);
}

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

// Caller must already ensure no duplicate bindings for the same outTmplKey.
const PatternToRuleBinding*
findRuleLocalBinding(string_view outk,
                     const vector<PatternToRuleBinding>& pattToRule) {
  for(auto& binding : pattToRule) if(binding.outTmplKey.preserveCase() == outk)
    return &binding;
  return nullptr;
}

Ident
identOf(DiagsDest ctx, const JsonTmpl& jstmpl) {
  auto* p = jstmpl.getIfPlaceholder();
  if(!p) Bug("Expected a Placeholder, got {}", jstmpl.prettyPrint());
  return Ident::parse(ctx, WholeSegment{jstmpl.stPos, jstmpl.enPos, p->key});
}

vector<pair<Ident, ssize_t>>
mapToRule(DiagsDest ctx, RulesWithLocs& rl,
          const vector<PatternToRuleBinding>& pattToRule,
          const JsonTmpl::ConstPlaceholderMap& outputKeys) {
  vector<pair<Ident, ssize_t>> rv;
  for(auto& [k, kcontainer] : outputKeys) {
    Ident outputIdent = identOf(ctx, *kcontainer);
    Ident ruleIdent = outputIdent;

    const PatternToRuleBinding* local = findRuleLocalBinding(k, pattToRule);
    if(local != nullptr) {
      rl.reserveLocalName(ctx, local->outTmplKey);
      ruleIdent = local->ruleName;
    }
    ssize_t ruleIndex = rl.findOrAppendIdent(ruleIdent);
    rv.emplace_back(std::move(outputIdent), ruleIndex);
  }
  return rv;
}

// Once we have extracted everything we need from InputDiags,
// this is where we compile the extracted string fragments into a rule.
// InputDiags is still used as a destination for error messages.
// Dev-note: we assume no duplicate binding for same jstmpl Placeholder.
void
appendPatternRules(DiagsDest ctx, const Ident& ident,
                   GluedString patt_string,
                   vector<PatternToRuleBinding> pattToRule, JsonTmpl jstmpl,
                   RulesWithLocs& rl) {
  vector<WholeSegment> listNames = desugarEllipsisPlaceholders(ctx, jstmpl);
  map<Ident,PartPattern> partPatterns = makePartPatterns(ctx, jstmpl);

  auto toks = tokenizePattern(ctx, patt_string, partPatterns, defaultLexopts());
  if(!patt_string.empty() && toks.empty()) return;
  optional<Pattern> patt = parsePattern(ctx, std::move(toks));
  if(!patt.has_value()) return;
  if(!checkPlaceholderTypes(ctx, listNames, *patt, false)) return;
  if(!checkMultipleTmplParts(ctx, jstmpl.allPlaceholders(), *patt)) return;
  vector<pair<Ident, ssize_t>> pl2ruleMap
    = mapToRule(ctx, rl, pattToRule, jstmpl.allPlaceholders());

  // Register locations late to avoid spurious 'used but undefined' messages.
  for(auto& [id, pp] : partPatterns) registerLocations(rl, id);
  PatternToRulesCompiler comp{ctx, rl, pl2ruleMap};
  ssize_t newIndex = comp.process(*patt);
  ssize_t newIndex2 = rl.defineIdent(ctx, ident);
  if(newIndex2 == -1) return;
  rl.deferred_assign(newIndex2, OutputTmpl{
      /* childidx */ newIndex,
      /* childName */ "",
      /* outputTmpl */ std::move(jstmpl)
  });
}

// ---------- End of Pattern to Rule compilation ----------

// Assumes colonPos > 0, since the error message
// is attached to the previous token in linetok.
bool
requireColonEol(const vector<ExprToken>& linetoks, size_t colonPos,
                DiagsDest ctx) {
  if(linetoks.size() <= colonPos || !isToken(linetoks[colonPos], ":")) {
    Error(ctx, enPos(linetoks[colonPos-1]), "Was expecting a ':' after this");
    return false;
  }
  if(linetoks.size() > colonPos+1) {
    Error(ctx, stPos(linetoks[colonPos+1]),
          "Input block needs to be on the following line");
    return false;
  }
  return true;
}

// Requires block to be indented strictly more than the reference indent.
optional<GluedString>
parseIndentedBlock(InputDiags& ctx, size_t& i, const WholeSegment& refIndent,
                   string_view blockName) {
  optional<GluedString> rv;
  if(optional<WholeSegment> ind = lookaheadParIndent(ctx, i)) {
    // Consume input the next block even if it is not indented enough.
    rv = lexIndentedSource(ctx, i, **ind);
    if(!requireGoodIndent(ctx, "Code block", refIndent, *ind)) return nullopt;
  }
  if(!rv.has_value())
    Error(ctx, i, format("No indented {} follows", blockName));
  return rv;
}

// Dev-notes: This is being used both for examples and rules.
//   Make sure any new syntax makes sense for both cases. Later, we might have
//   to either (a) split this up into two different functions, or
//          or (b) return a more general type here that can be sanitized by
//                 the caller.
template <int pos>
optional<JsonTmpl>
parseOutputBraces(vector<ExprToken> linetoks, DiagsDest ctx) {
  static_assert(pos > 0, "pos must be positive for proper error-reporting");
  BracketGroup* bg;
  if(linetoks.size() <= pos ||
     (bg = get_if<BracketGroup>(&linetoks[pos])) == nullptr )
    return Error(ctx, enPos(linetoks[pos-1]), "Was expecting '{' on this line");
  optional<JsonTmpl> jstmpl
    = parseJsonTmplFromBracketGroup(ctx, std::move(*bg));
  // If there's an error, parseJsonTmplFromBracketGroup() should have already
  // produced diags.
  if(!jstmpl.has_value()) return nullopt;
  if(!requireEol(linetoks, pos+1, ctx)) return nullopt;
  return jstmpl;
}

bool
hasEllipsis(const JsonTmpl& jstmpl) {
  if(jstmpl.holdsEllipsis()) return true;
  else if(jstmpl.holdsString() || jstmpl.holdsPlaceholder()) return false;
  else if(auto* v = jstmpl.getIfVector()) {
    for(auto& elt : *v) if(hasEllipsis(elt)) return true;
    return false;
  }else if(auto* m = jstmpl.getIfMap()) {
    for(auto& [k,v] : *m) if(hasEllipsis(v)) return true;
    return false;
  }else Bug("Unknown JsonTmpl variant in hasEllipsis(): {}", jstmpl.tagName());
}

// Like all skippers, can return npos if comment is unfinished.
size_t
skipToIndentLe(InputDiags& ctx, size_t i, string_view refIndent) {
  const Input& input = ctx.input;
  while(true) {
    while(input.sizeGt(i) && input.bol(i) != i) ++i;
    i = oalexWSkip.acrossLines(input, i);
    if(!input.sizeGt(i)) return i;
    size_t bol = input.bol(i);
    IndentCmp cmpres = indentCmp(input.substr(bol, i-bol), refIndent);
    if(cmpres == IndentCmp::lt || cmpres == IndentCmp::eq) return bol;
  }
}

bool
skipStanzaIfSeen(bool& done_indicator, const WholeSegment& stanzaLeader,
                 InputDiags& ctx, size_t& i) {
  if(done_indicator) {
    WholeSegment leaderIndent = indent_of(ctx.input, stanzaLeader);
    i = skipToIndentLe(ctx, stanzaLeader.enPos, *leaderIndent);
    Error(ctx, stanzaLeader,
        format("Only one `{}` stanza allowed per rule", *stanzaLeader));
    return true;
  }else {
    done_indicator = true;
    return false;
  }
}

optional<JsonTmpl>
parseRuleOutput(vector<ExprToken> linetoks, InputDiags& ctx) {
  // TODO: See if this entire function can be absorbed into parseOutputBraces.
  if(linetoks.size() < 2 || !isToken(linetoks[1], ":")) {
    Error(ctx, enPos(linetoks[0]), "Expected ':' after 'outputs'");
    return nullopt;
  }

  return parseOutputBraces<2>(std::move(linetoks), ctx);
}

bool
requireUniqueBinding(const vector<PatternToRuleBinding>& collected,
                     const PatternToRuleBinding& found, DiagsDest ctx) {
  for(auto& c : collected) if(c.outTmplKey == found.outTmplKey) {
    Error(ctx, found.outTmplKey.stPos(), found.outTmplKey.enPos(),
          format("Duplicate definition for placeholder '{}'",
                 found.outTmplKey.preserveCase()));
    return false;
  }
  return true;
}

// On a bad error error, the caller should advance i to the next line that
// matches leaderIndent. Such errors are indicated by an empty return vector.
vector<PatternToRuleBinding>
parseRuleLocalDecls(InputDiags& ctx, size_t& i,
                    const WholeSegment& leaderIndent) {
  size_t j = i;
  vector<PatternToRuleBinding> rv;

  vector<ExprToken> line = lexNextLine(ctx, j);
  if(line.empty()) return rv;
  WholeSegment lineIndent = indent_of(ctx.input, line[0]);
  if(!requireGoodIndent(ctx, "Local declaration", leaderIndent, lineIndent))
    return rv;
  WholeSegment refIndent = lineIndent;
  IndentCmp cmpres;
  do {
    i = j;
    auto* lhs = get_if<WholeSegment>(&line[0]);
    auto* rhs = (line.size() >= 3 ? get_if<WholeSegment>(&line[2]) : nullptr);
    if(lhs == nullptr) Error(ctx, line[0], "Expected pattern name");
    else if(line.size() < 2 || !isToken(line[1], "~"))
      Error(ctx, enPos(line[0]), "Expected '~' after this");
    else if(rhs == nullptr) Error(ctx, enPos(line[2]), "Expected rule name");
    else {
      PatternToRuleBinding binding{
        .pp{GluedString{*lhs}},
        .outTmplKey{Ident::parse(ctx, *lhs)},
        .ruleName{Ident::parse(ctx, *rhs)},
      };
      if(requireUniqueBinding(rv, binding, ctx))
        rv.push_back(std::move(binding));
    }

    line = lexNextLine(ctx, j);
    if(line.empty()) break;
    lineIndent = indent_of(ctx.input, line[0]);
    cmpres = indentCmp(*refIndent, *lineIndent);
    if(cmpres == IndentCmp::lt)
      Error(ctx, stPos(line[0]), "This line is indented too deep");
  }while(cmpres == IndentCmp::eq || cmpres == IndentCmp::lt);
  if(!requireCompatIndent(cmpres, ctx, lineIndent)) return {};

  return rv;
}

// Checks second token just so it is not a BNF rule of the form
// `rule :=`. We want to avoid requiring too many reserved keywords
// if possible.
bool
resemblesRule(const vector<ExprToken>& linetoks) {
  return linetoks.size() >= 2 && isToken(linetoks[0], "rule")
         && resemblesIdent(linetoks[1]);
}

// Assumes i == ctx.input.bol(i), as we just finished lexNextLine().
void
parseRule(vector<ExprToken> linetoks, InputDiags& ctx, size_t& i,
          RulesWithLocs& rl) {
  if(!requireColonEol(linetoks, 2, ctx)) return;
  optional<GluedString> patt =
      parseIndentedBlock(ctx, i, indent_of(ctx.input, linetoks[0]),
                         "pattern");
  if(!patt.has_value()) return;
  // Guaranteed to succeed by resemblesRule().
  const auto ident = Ident::parse(ctx, std::get<WholeSegment>(linetoks[1]));
  bool sawOutputsKw = false, sawWhereKw = false;
  optional<JsonTmpl> jstmpl;
  vector<PatternToRuleBinding> local_decls;

  // Consume next line for the outputs stanza.
  while(true) {
    size_t oldi = i;
    auto toks = lexNextLine(ctx, i);
    if(toks.empty()) break;
    auto* leader = get_if<WholeSegment>(&toks[0]);
    if(!leader) { i = oldi; break; }
    else if(**leader == "outputs") {
      if(skipStanzaIfSeen(sawOutputsKw, *leader, ctx, i)) continue;
      jstmpl = parseRuleOutput(std::move(toks), ctx);
    }else if(**leader == "where") {
      if(skipStanzaIfSeen(sawWhereKw, *leader, ctx, i)) continue;
      WholeSegment leaderIndent = indent_of(ctx.input, toks[0]);
      auto new_local_decls = parseRuleLocalDecls(ctx, i, leaderIndent);
      if(!new_local_decls.empty()) local_decls = std::move(new_local_decls);
      else i = skipToIndentLe(ctx, i, *leaderIndent);
    }else { i = oldi; break; }
  }

  if(!sawOutputsKw) {
    Error(ctx, i, format("outputs stanza missing in rule {}",
                         ident.preserveCase()));
    return;
  }

  if(!jstmpl.has_value()) return;
  appendPatternRules(ctx, ident, std::move(*patt), std::move(local_decls),
                     std::move(*jstmpl), rl); }

// For error-locating, it assumes !v.empty().
Ident
parseIdentFromExprVec(DiagsDest ctx, const vector<ExprToken>& v, size_t idx) {
  if(v.size() <= idx) {
    Error(ctx, enPos(v.back()), "Expected identifier after this");
    return {};
  }
  Ident rv;
  auto* seg = get_if<WholeSegment>(&v[idx]);
  if(seg) rv = Ident::parse(ctx, *seg);
  if(!rv) Error(ctx, v[idx], "Expected identifier");
  return rv;
}

// Dev-note: maybe move to pattern.h
bool
isUserWord(string_view s) {
  for(char ch : s) if(!matchesRegexCharSet(ch, defaultLexopts().wordChars))
    return false;
  return true;
}

// Assumes linetoks.size() > idx
ssize_t
lookaheadRuleIndex(DiagsDest ctx, const vector<ExprToken>& linetoks, size_t idx,
                   RulesWithLocs& rl) {
  if(auto* s = get_if<GluedString>(&linetoks[idx])) {
    if(!isUserWord(*s)) {
      Error(ctx, *s, "Non-word inline lookahead");
      return -1;
    }else return rl.appendAnonRule(WordPreserving{*s});
  }
  const Ident lookId = parseIdentFromExprVec(ctx, linetoks, idx);
  if(!lookId) return -1;
  return rl.findOrAppendIdent(lookId);
}

Ident
parseSingleIdentBranch(DiagsDest ctx, const vector<ExprToken>& branch,
                       ssize_t rhsidx) {
  const Ident parseId = parseIdentFromExprVec(ctx, branch, rhsidx);
  if(!parseId || !requireEol(branch, rhsidx+1, ctx)) return Ident{};
  else return parseId;
}

bool
resemblesErrorBranch(const vector<ExprToken>& branch, ssize_t rhsidx) {
  return ssize(branch) > rhsidx+1 && isToken(branch[rhsidx], "error");
}

string_view
parseErrorBranch(
    DiagsDest ctx, const vector<ExprToken>& branch, ssize_t rhsidx) {
  if(!requireEol(branch, rhsidx+2, ctx)) return "";
  const auto* s = get_if<GluedString>(&branch[rhsidx+1]);
  if(s == nullptr || (s->ctor() != GluedString::Ctor::squoted &&
                      s->ctor() != GluedString::Ctor::dquoted)) {
    Error(ctx, branch[rhsidx+1], "Expected a quoted error message");
    return "";
  }
  string_view rv = *s;
  if(rv.empty()) Error(ctx, *s, "Error message cannot be empty");
  return rv;
}

void
orRuleAppendPassthrough(OrRule& orRule, ssize_t lookIdx, ssize_t parseIdx) {
  orRule.comps.push_back({
      .lookidx = lookIdx, .parseidx = parseIdx, .tmpl{passthroughTmpl}
      });
}

bool
resemblesLookaheadBranch(const vector<ExprToken>& branch) {
  return branch.size() >= 3 && isToken(branch[2], "->");
}

bool
parseLookaheadBranchAction(const vector<ExprToken>& branch,
                           ssize_t pos, ssize_t lookidx,
                           DiagsDest ctx, OrRule& orRule, RulesWithLocs& rl) {
  if(resemblesErrorBranch(branch, pos)) {
    string_view err = parseErrorBranch(ctx, branch, pos);
    // TODO deduplicate SkipRules in codegen
    if(!err.empty()) orRuleAppendPassthrough(orRule, lookidx,
                       rl.appendAnonRule(ErrorRule{string(err)}));
  }else if(matchesTokens(branch, pos, {"quiet"})) {
    if(ssize(branch) <= pos+1 || !isToken(branch[pos+1], "error")) {
      Error(ctx, enPos(branch[pos]), "Expected keyword 'error' after 'quiet'");
      return false;
    }
    orRuleAppendPassthrough(orRule, lookidx,
                            rl.appendAnonRule(ErrorRule{string{}}));
  }else if(const Ident parseId = parseSingleIdentBranch(ctx, branch, pos))
    orRule.comps.push_back({
        .lookidx = lookidx, .parseidx = rl.findOrAppendIdent(parseId),
        .tmpl{passthroughTmpl}
    });
  return true;
}

bool
resemblesLookaheadRule(const vector<ExprToken>& linetoks) {
  return linetoks.size() >= 3 && isToken(linetoks[0], "rule")
         && resemblesIdent(linetoks[1]) && isToken(linetoks[2], "lookaheads");
}

void
parseLookaheadRule(vector<ExprToken> linetoks,
                        InputDiags& ctx, size_t& i, RulesWithLocs& rl) {
  if(!requireColonEol(linetoks, 3, ctx)) return;
  vector<vector<ExprToken>> branches = lexListEntries(ctx, i, '|');
  if(branches.empty()) {
    Error(ctx, enPos(linetoks.back()),
          "Expected lookahead branches after this");
    return;
  }
  const Ident ruleName = Ident::parse(ctx, std::get<WholeSegment>(linetoks[1]));
  OrRule orRule{{}, /* flattenOnDemand */ false};
  for(const auto& branch : branches) {
    if(branch.empty() || !isToken(branch[0], "|"))
      Bug("lexListEntries() should return at least the bullet");
    ssize_t lookidx = -1;
    ssize_t actionPos = 1;
    if(resemblesLookaheadBranch(branch)) {
      lookidx = lookaheadRuleIndex(ctx, branch, 1, rl);
      if(lookidx == -1) continue;
      actionPos += 2;   // Skip over "->"
    }
    if(!parseLookaheadBranchAction(branch, actionPos, lookidx, ctx, orRule, rl))
      continue;
  }
  ssize_t orIndex = rl.defineIdent(ctx, ruleName);
  if(orIndex != -1) rl.deferred_assign(orIndex, std::move(orRule));
}

// Checks second token just so it is not a BNF rule of the form
// `example :=`. We want to avoid requiring too many reserved keywords
// if possible.
bool
resemblesExample(const vector<ExprToken>& linetoks) {
  return linetoks.size() >= 2 && isToken(linetoks[0], "example")
         && resemblesIdent(linetoks[1]);
}

// Assumes i == ctx.input.bol(i), as we just finished lexNextLine().
optional<Example>
parseExample(vector<ExprToken> linetoks, InputDiags& ctx, size_t& i) {
  if(!requireColonEol(linetoks, 2, ctx)) return nullopt;
  Example rv;
  // Guaranteed to succeed by resemblesExample().
  rv.mappedPos = {.line = ctx.input.rowCol(stPos(linetoks[0])).first};
  rv.ruleName = Ident::parse(ctx, *get_if<WholeSegment>(&linetoks[1]));
  if(!rv.ruleName) return nullopt;

  if(auto sampleInput =
      parseIndentedBlock(ctx, i, indent_of(ctx.input, linetoks[0]),
                         "example input")) {
    rv.sampleInput = std::move(*sampleInput);
  }else return nullopt;

  vector<ExprToken> linetoks2 = lexNextLine(ctx, i);
  if(linetoks2.empty()) return nullopt;

  if(matchesTokens(linetoks2, {"outputs", "success"})) {
    if(!requireEol(linetoks2, 2, ctx)) return nullopt;
    rv.expectation = Expectation::Success;
  }
  else if(matchesTokens(linetoks2, {"outputs", "error", "with"})) {
    if(linetoks2.size() < 4)
      return Error(ctx, enPos(linetoks2[2]),
                   "The expected error should be on this line");
    const auto* s = get_if<GluedString>(&linetoks2[3]);
    if(s == nullptr || s->ctor() != GluedString::Ctor::squoted)
      return Error(ctx, stPos(linetoks2[3]),
                   "The expected error should be 'single-quoted'");
    rv.expectation = Expectation::ErrorSubstr{string(*s)};
    if(!requireEol(linetoks2, 4, ctx)) return nullopt;
  }else if(matchesTokens(linetoks2, {"outputs", ":"})) {
    optional<JsonTmpl> jstmpl = parseOutputBraces<2>(std::move(linetoks2), ctx);
    if(!jstmpl.has_value()) return nullopt;
    if(hasEllipsis(*jstmpl)) Unimplemented("Ellipsis in output templates");
    if(jstmpl->substitutionsNeeded())
      return Error(ctx, linetoks[2], "Values need to be properly quoted");
    rv.expectation = Expectation::SuccessWithJson{std::move(*jstmpl)};
  }else if(matchesTokens(linetoks2, {"outputs"}))
    return Error(ctx, enPos(linetoks2[0]),
                 "Was expecting ': {', 'success', or 'error with' after this");
  else return Error(ctx, i, "Was expecting 'outputs'");

  // FIXME invalid escape code error appeared multiple times.
  return rv;
}

const JsonTmpl*
getTemplate(const Rule& rule) {
  if(auto* concat = dynamic_cast<const ConcatRule*>(&rule))
    return &concat->outputTmpl;
  if(auto* tmpl = dynamic_cast<const OutputTmpl*>(&rule))
    return &tmpl->outputTmpl;
  return nullptr;
}

optional<tuple<const JsonTmpl*, const JsonTmpl*, string>>
hasDuplicatePlaceholders(const JsonTmpl& tmpl) {
  auto pmap = tmpl.allPlaceholders();
  if(!pmap.empty()) for(auto it=++pmap.begin(); it!=pmap.end(); ++it)
    if(it->first == (it-1)->first)
      return tuple{(it-1)->second, it->second, it->first};
  return nullopt;
}

// We enforce this constraint because it allows us to std::move() components
// in codegen, instead of copying them. Doing it at the end allows us to not
// miss it in any of the places we create an outputTmpl.
bool
hasDuplicatePlaceholders(const vector<unique_ptr<Rule>>& rules, DiagsDest ctx) {
  for(const auto& rule : rules) if(const JsonTmpl* tmpl = getTemplate(*rule)) {
    if(auto opt = hasDuplicatePlaceholders(*tmpl)) {
      auto [p1, p2, key] = *opt;
      Error(ctx, p2->stPos, p2->enPos, format("Duplicate placeholder {}", key));
      Note(ctx, p1->stPos, p1->enPos, "Previously used here");
      return true;
    }
  }
  return false;
}

// TODO improve autogenerated names. E.g. 'Hello' --> Hello.
void
fillInNames(vector<unique_ptr<Rule>>& rules) {
  vector<bool> istentative(rules.size(), false);
  for(auto& rule : rules) {
    if(auto* orrule = dynamic_cast<const OrRule*>(rule.get())) {
      for(auto& comp : orrule->comps) if(comp.lookidx != -1)
        istentative[comp.lookidx] = true;
    }else if(auto* qmrule = dynamic_cast<const QuietMatch*>(rule.get()))
      istentative[qmrule->compidx] = true;
    else if(auto* loop = dynamic_cast<const LoopRule*>(rule.get())) {
      if(loop->lookidx != -1) istentative[loop->lookidx] = true;
      else if(loop->glueidx != -1) istentative[loop->glueidx] = true;
      // TODO put this next statement under an else clause after we have
      // resolved `first` todo in codegen.cpp:codegen(LoopRule).
      istentative[loop->partidx] = true;
      if(loop->skipidx != -1) istentative[loop->skipidx] = true;
    }
  }

  size_t nc = 1;
  for(size_t i=0; i<rules.size(); ++i)
    if(needsName(*rules[i], istentative[i]) && !rules[i]->nameOrNull())
      rules[i]->deferred_name(Ident::parseGenerated("rule" + itos(nc++)));
}

}  // namespace

optional<ParsedSource>
parseOalexSource(InputDiags& ctx) {
  static const auto* userRegexOpts = new RegexOptions{
    // Do not use user-supplied input. See regex_io.h for details.
    .word = parseRegexCharSet("[0-9A-Za-z_]")
  };

  size_t i = 0;
  vector<Example> examples;
  RulesWithLocs rl;
  while(ctx.input.sizeGt(i)) {
    if(i != ctx.input.bol(i))
      FatalBug(ctx, i, "Rules must start at bol()");
    vector<ExprToken> linetoks = lexNextLine(ctx, i);
    if(linetoks.empty()) {
      if(ctx.input.sizeGt(i)) return nullopt;  // Error case
      else break;
    }

    if(resemblesBnfRule(linetoks)) {
      parseBnfRule(std::move(linetoks), ctx, rl);
    }else if(resemblesExample(linetoks)) {
      if(auto ex = parseExample(std::move(linetoks), ctx, i))
        examples.push_back(std::move(*ex));
    }else if(resemblesLookaheadRule(linetoks)) {
      parseLookaheadRule(std::move(linetoks), ctx, i, rl);
    }else if(resemblesRule(linetoks)) {
      parseRule(std::move(linetoks), ctx, i, rl);
    }else
      return Error(ctx, linetoks[0],
                   format("Unexpected '{}', was expecting 'example' or 'rule'",
                          debug(linetoks[0])));
  }
  if(rl.ssize() == 0) return Error(ctx, 0, "File doesn't define any rule");
  if(rl.hasUndefinedRules(ctx)) return nullopt;
  RuleSet rs{rl.releaseRules(), *userRegexOpts};
  if(hasDuplicatePlaceholders(rs.rules, ctx) ||
     hasError(ctx.diags)) return nullopt;
  fillInNames(rs.rules);
  return ParsedSource{std::move(rs), std::move(examples)};
}

// TODO make this nicer. Escape with dquoted() on single-line outputs,
// indent on multi-line.
string
describeTestFailure(const Example& ex, bool succeeded) {
  string_view input = ex.sampleInput;
  if(!input.empty() && input.back() == '\n') input.remove_suffix(1);

  if(auto msg = ex.expectation.isForErrorSubstr()) {
    if(succeeded) {
      return format("Test failed at {}\n"
                    "Was expecting {} to fail on input '{}'. "
                    "Succeeded unexpectedly.", string(ex.mappedPos),
                    ex.ruleName.preserveCase(), input);
    }else {
      return format("Test failed at {}\n"
                    "Was expecting failure with substring '{}'.",
                    string(ex.mappedPos), *msg);
    }
  }else {
    if(auto jstmpl = ex.expectation.jstmpl()) {
      return format("Test failed\nWas expecting output {}.",
                    jstmpl->prettyPrint());
    }else {
      return format("Test failed at {}\n"
                    "Was expecting {} to succeed on input '{}'",
                    string(ex.mappedPos), ex.ruleName.preserveCase(), input);
    }
  }
}

}  // namespace oalex
