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

#include <iterator>
#include <map>
#include <memory>
#include <optional>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>
#include "fmt/core.h"

#include "lexer.h"
#include "pattern.h"
#include "regex_io.h"
#include "jsonloc_io.h"

using fmt::format;
using oalex::DiagsDest;
using oalex::LexDirective;
using oalex::Note;
using oalex::OutputTmpl;
using oalex::parseJsonLocFromBracketGroup;
using oalex::parsePattern;
using oalex::parseRegexCharSet;
using oalex::passthroughTmpl;
using oalex::Pattern;
using oalex::tokenizePattern;
using oalex::lex::enPos;
using oalex::lex::BracketGroup;
using oalex::lex::BracketType;
using oalex::lex::ExprToken;
using oalex::lex::GluedString;
using oalex::lex::IndentCmp;
using oalex::lex::indentCmp;
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
using oalex::lex::WholeSegment;
using std::make_unique;
using std::map;
using std::nullopt;
using std::optional;
using std::pair;
using std::string;
using std::string_view;
using std::tuple;
using std::unique_ptr;
using std::vector;

namespace oalex {

MappedPos::operator string() const {
  return "line " + itos(this->line);
}

bool
Expectation::matches(const JsonLoc& jsloc,
                     const std::vector<Diag>& diags) const {
  if(Example::runSucceeded(jsloc, diags) != success_) return false;
  if(success_)
    return jsloc_.supportsEquality() ? jsloc == jsloc_ : true;
  for(const auto& d: diags) if(isSubstr(errorSubstr_, d.msg)) return true;
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

  /* Searches for ident in rules[].name().
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
  for(ssize_t i=0; i<this->ssize(); ++i) if(id == rules_[i]->name()) {
    if(firstUseLocs_[i] == nrange) firstUseLocs_[i] = thisPos;
    return i;
  }
  rules_.push_back(make_unique<UnassignedRule>(id));
  firstUseLocs_.push_back(thisPos);
  return this->ssize()-1;
}

ssize_t
RulesWithLocs::defineIdent(DiagsDest ctx, const Ident& ident) {
  for(ssize_t i=0; i<this->ssize(); ++i) if(ident == rules_[i]->name()) {
    if(!dynamic_cast<const UnassignedRule*>(rules_[i].get())) {
      Error(ctx, ident.stPos(), ident.enPos(),
            format("'{}' has multiple definitions", ident.preserveCase()));
      return -1;
    }else return i;
  }
  rules_.push_back(make_unique<UnassignedRule>(ident));
  firstUseLocs_.push_back(nrange);
  return this->ssize()-1;
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
      optional<Ident> name = *rules_[i]->name();
      if(!name.has_value()) Bug("Anonymous rules should always be initialized");
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
    if(auto name2 = rules_[idx]->name()) name = std::move(*name2);
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

  ConcatRule concat{ {}, JsonLoc::Map() };
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
    if(auto opt = parseJsonLocFromBracketGroup(ctx, std::move(*tmpl)))
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
  return WholeSegment(bol, indent_end, input);
}

bool
goodIndent(DiagsDest ctx, const WholeSegment& indent1,
           const WholeSegment& indent2) {
  IndentCmp cmpres = indentCmp(*indent1, *indent2);
  if(cmpres == IndentCmp::bad) {
    Error(ctx, indent2,
          "Bad mix of spaces and tabs compared to the previous line");
    return false;
  }
  else if(cmpres != IndentCmp::lt) {
    Error(ctx, indent2, "Code block needs more indentation");
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

This block of code is almost all helpers of appendPatternRules (plural).
Most functions here follow this convention:

  * They append newly compiled rules into RulesWithLocs.
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
appendWordOrError, appendLiteralOrError). They produce a string but that result
is ignored when part of a Pattern object. They could produce maps too, or
nothing at all, but right now they produce strings just so the functions can
be used elsewhere in the frontend.

How to follow this convention for new rules types with subcomponents:
If you want something to be preserved until an OutputTmpl catches it, make it a
named field in a map. You can safely use passthroughTmpl: map fields will be
propagated, while strings will be dropped either by OutputTmpl or
ConcatFlatRule.
*/

// Forward decl.
ssize_t
appendPatternRule(DiagsDest ctx, const Pattern& patt, RulesWithLocs& rl);

ssize_t
appendPatternConcat(DiagsDest ctx, const PatternConcat& concatPatt,
                    RulesWithLocs& rl) {
  ConcatFlatRule concatRule{ {} };
  for(ssize_t i = 0; i < (ssize_t)concatPatt.parts.size(); ++i) {
    if(i > 0) {
      // Intersperse concat components with SkipPoint components.
      concatRule.comps.push_back({rl.ssize(), {}});
      rl.appendAnonRule(SkipPoint{/* stayWithinLine */ false, &oalexSkip});
    }
    const Pattern& child = concatPatt.parts[i];
    ssize_t j = appendPatternRule(ctx, child, rl);
    concatRule.comps.push_back({j, {}});
  }
  rl.appendAnonRule(std::move(concatRule));
  return rl.ssize()-1;
}

// TODO failed tests should output location. Or tests should have names.
ssize_t
appendPatternOrList(DiagsDest ctx, const PatternOrList& orPatt,
                    RulesWithLocs& rl) {
  OrRule orRule{{}, /* flattenOnDemand */ true};
  for(ssize_t i = 0; i < ssize(orPatt.parts); ++i) {
    const Pattern& child = orPatt.parts[i];
    ssize_t j = appendPatternRule(ctx, child, rl);
    if(i+1 != ssize(orPatt.parts)) j = rl.appendAnonRule(QuietMatch{j});
    orRule.comps.push_back({-1, j, passthroughTmpl});
  }
  rl.appendAnonRule(std::move(orRule));
  return rl.ssize()-1;
}

ssize_t
appendPatternOptional(DiagsDest ctx, const PatternOptional& optPatt,
                      RulesWithLocs& rl) {
  ssize_t i;
  OrRule orRule{{}, /* flattenOnDemand */ true};

  i = appendPatternRule(ctx, optPatt.part, rl);
  i = rl.appendAnonRule(QuietMatch{i});
  orRule.comps.push_back({-1, i, passthroughTmpl});

  // This branch always produces a Map on success.
  i = rl.appendAnonRule(StringRule{{}});
  orRule.comps.push_back({-1, i, JsonLoc::Map{}});

  return rl.appendAnonRule(std::move(orRule));
}

ssize_t
appendPatternIdent(const Ident& ident, RulesWithLocs& rl) {
  ssize_t i = rl.findOrAppendIdent(ident);
  return rl.appendAnonRule(ConcatFlatRule{{
      {i, ident.preserveCase()},
  }});
}

ssize_t
appendPatternRepeat(DiagsDest ctx, const PatternRepeat& repPatt,
                    RulesWithLocs& rl) {
  ssize_t i = appendPatternRule(ctx, repPatt.part, rl);
  ssize_t ski = rl.appendAnonRule(SkipPoint{/* stayWithinLine */ false,
                                            &oalexSkip});
  return rl.appendAnonRule(LoopRule{{
      .partidx = i, .partname = "", .glueidx = -1, .gluename = "",
      .lookidx = -1, .skipidx = ski}});
}

ssize_t
appendPatternFold(DiagsDest ctx, const PatternFold& foldPatt,
                  RulesWithLocs& rl) {
  ssize_t pi = appendPatternRule(ctx, foldPatt.part, rl);
  ssize_t gi = appendPatternRule(ctx, foldPatt.glue, rl);
  ssize_t ski = rl.appendAnonRule(SkipPoint{/* stayWithinLine */ false,
                                            &oalexSkip});
  return rl.appendAnonRule(LoopRule{{
      .partidx = pi, .partname = "", .glueidx = gi, .gluename = "",
      .lookidx = -1, .skipidx = ski}});
}

ssize_t
appendPatternRule(DiagsDest ctx, const Pattern& patt, RulesWithLocs& rl) {
  if(auto* word = get_if_unique<WordToken>(&patt)) {
    return appendWordOrError(rl, **word);
  }else if(auto* oper = get_if_unique<OperToken>(&patt)) {
    return appendLiteralOrError(rl, **oper);
  }else if(get_if_unique<NewlineChar>(&patt)) {
    return appendLiteralOrError(rl, "\n");
  }else if(auto* ident = get_if_unique<Ident>(&patt)) {
    return appendPatternIdent(*ident, rl);
  }else if(auto* concatPatt = get_if_unique<PatternConcat>(&patt)) {
    return appendPatternConcat(ctx, *concatPatt, rl);
  }else if(auto* orPatt = get_if_unique<PatternOrList>(&patt)) {
    return appendPatternOrList(ctx, *orPatt, rl);
  }else if(auto* optPatt = get_if_unique<PatternOptional>(&patt)) {
    return appendPatternOptional(ctx, *optPatt, rl);
  }else if(auto* repPatt = get_if_unique<PatternRepeat>(&patt)) {
    return appendPatternRepeat(ctx, *repPatt, rl);
  }else if(auto* foldPatt = get_if_unique<PatternFold>(&patt)) {
    return appendPatternFold(ctx, *foldPatt, rl);
  }else {
    Unimplemented("Pattern compilation of index {}", patt.index());
  }
}

// We can later add where-stanza arguments for extracting partPatterns
map<Ident,PartPattern>
makePartPatterns(DiagsDest ctx, const JsonLoc& jsloc) {
  if(holds_alternative<JsonLoc::Vector>(jsloc))
    Unimplemented("Directly outputting list not encased in a map");
  const JsonLoc::Map* jslocmap = get_if<JsonLoc::Map>(&jsloc);
  if(jslocmap == nullptr)
    Bug("parseJsonLocFromBracketGroup() returned something strange");

  map<Ident, PartPattern> rv;
  for(const auto& [p, j] : jsloc.allPlaceholders()) {
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

// Once we have extracted everything we need from InputDiags,
// this is where we compile the extracted string fragments into a rule.
// InputDiags is still used as a destination for error messages.
void
appendPatternRules(DiagsDest ctx, const Ident& ident,
                   GluedString patt_string, JsonLoc jsloc, RulesWithLocs& rl) {
  map<Ident,PartPattern> partPatterns = makePartPatterns(ctx, jsloc);
  for(auto& [id, pp] : partPatterns) registerLocations(rl, id);

  optional<Pattern> patt =
    parsePattern(ctx, tokenizePattern(ctx, patt_string, partPatterns,
                                      defaultLexopts()));
  if(!patt.has_value()) return;

  ssize_t newIndex = appendPatternRule(ctx, *patt, rl);
  ssize_t newIndex2 = rl.defineIdent(ctx, ident);
  if(newIndex2 == -1) return;
  rl.deferred_assign(newIndex2, OutputTmpl{
      /* childidx */ newIndex,
      /* childName */ "",
      /* outputTmpl */ std::move(jsloc)
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
    // TODO rename goodIndent -> requireGoodIndent to indicate error diags.
    if(!goodIndent(ctx, refIndent, *ind)) return nullopt;
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
optional<JsonLoc>
parseOutputBraces(vector<ExprToken> linetoks, DiagsDest ctx) {
  static_assert(pos > 0, "pos must be positive for proper error-reporting");
  BracketGroup* bg;
  if(linetoks.size() <= pos ||
     (bg = get_if<BracketGroup>(&linetoks[pos])) == nullptr )
    return Error(ctx, enPos(linetoks[pos-1]), "Was expecting '{' on this line");
  optional<JsonLoc> jsloc = parseJsonLocFromBracketGroup(ctx, std::move(*bg));
  // If there's an error, parseJsonLocFromBracketGroup() should have already
  // produced diags.
  if(!jsloc.has_value()) return nullopt;
  if(!requireEol(linetoks, pos+1, ctx)) return nullopt;
  return jsloc;
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

  // Consume next line for the outputs stanza.
  if(auto toks = lexNextLine(ctx, i);
     !toks.empty() && matchesTokens(toks, {"outputs"}))
    linetoks = std::move(toks);
  else {
    Error(ctx, i, format("outputs stanza missing in rule {}",
                         ident.preserveCase()));
    return;
  }

  if(linetoks.size() < 2 || !isToken(linetoks[1], ":")) {
    Error(ctx, enPos(linetoks[0]), "Expected ':' after 'outputs'");
    return;
  }

  optional<JsonLoc> jsloc = parseOutputBraces<2>(std::move(linetoks), ctx);
  if(!jsloc.has_value()) return;
  appendPatternRules(ctx, ident, std::move(*patt), std::move(*jsloc), rl);
}

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
  rl.deferred_assign(orIndex, std::move(orRule));
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
    optional<JsonLoc> jsloc = parseOutputBraces<2>(std::move(linetoks2), ctx);
    if(!jsloc.has_value()) return nullopt;
    if(!jsloc->supportsEquality())
      return Error(ctx, linetoks[2], "Values need to be properly quoted");
    rv.expectation = Expectation::SuccessWithJson{std::move(*jsloc)};
  }else if(matchesTokens(linetoks2, {"outputs"}))
    return Error(ctx, enPos(linetoks2[0]),
                 "Was expecting ': {', 'success', or 'error with' after this");
  else return Error(ctx, i, "Was expecting 'outputs'");

  // FIXME invalid escape code error appeared multiple times.
  return rv;
}

const JsonLoc*
getTemplate(const Rule& rule) {
  if(auto* concat = dynamic_cast<const ConcatRule*>(&rule))
    return &concat->outputTmpl;
  if(auto* tmpl = dynamic_cast<const OutputTmpl*>(&rule))
    return &tmpl->outputTmpl;
  return nullptr;
}

optional<tuple<const JsonLoc*, const JsonLoc*, string>>
hasDuplicatePlaceholders(const JsonLoc& tmpl) {
  auto pmap = tmpl.allPlaceholders();
  for(auto& [k, v] : pmap) if(pmap.count(k) > 1) {
    auto it = pmap.lower_bound(k);
    auto jt = ++it;  // guaranteed to be valid, because duplicate.
    return tuple{it->second, jt->second, k};
  }
  return nullopt;
}

// We enforce this constraint because it allows us to std::move() components
// in codegen, instead of copying them. Doing it at the end allows us to not
// miss it in any of the places we create an outputTmpl.
bool
hasDuplicatePlaceholders(const vector<unique_ptr<Rule>>& rules, DiagsDest ctx) {
  for(const auto& rule : rules) if(const JsonLoc* tmpl = getTemplate(*rule)) {
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
    if(needsName(*rules[i], istentative[i]) && !rules[i]->name().has_value())
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
    if(auto jsloc = ex.expectation.jsloc()) {
      return format("Test failed\nWas expecting output {}.", *jsloc);
    }else {
      return format("Test failed at {}\n"
                    "Was expecting {} to succeed on input '{}'",
                    string(ex.mappedPos), ex.ruleName.preserveCase(), input);
    }
  }
}

}  // namespace oalex
