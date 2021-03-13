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
#include <utility>
#include <vector>
#include "fmt/core.h"

#include "lexer.h"
#include "pattern.h"
#include "regex_io.h"
#include "jsonloc_io.h"

using fmt::format;
using oalex::InputDiagsRef;
using oalex::LexDirective;
using oalex::Note;
using oalex::OutputTmpl;
using oalex::parseJsonLocFromBracketGroup;
using oalex::parsePattern;
using oalex::parseRegexCharSet;
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
using oalex::lex::lexNextLine;
using oalex::lex::lookaheadParIndent;
using oalex::lex::NewlineChar;
using oalex::lex::oalexSkip;
using oalex::lex::oalexWSkip;
using oalex::lex::RegexPattern;
using oalex::lex::stPos;
using oalex::lex::WholeSegment;
using std::map;
using std::monostate;
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

bool Expectation::matches(const JsonLoc& jsloc,
                          const std::vector<Diag>& diags) const {
  if(jsloc.holdsError()==success_) return false;
  if(success_)
    return jsloc_.supportsEquality() ? jsloc == jsloc_ : true;
  for(const auto& d: diags) if(isSubstr(errorSubstr_, d.msg)) return true;
  return false;
}

static string debug(const ExprToken& x) {
  if(auto* tok = get_if<WholeSegment>(&x)) return **tok;
  else if(auto* s = get_if<GluedString>(&x)) return "\"" + string(*s) + "\"";
  else return "(bracket group)";
}

static auto getIfIdent(const ExprToken& x) -> optional<string> {
  auto* seg = get_if<WholeSegment>(&x);
  if(!seg) return nullopt;
  const string& s = **seg;
  if(s.empty() || isdigit(s[0])) return nullopt;
  for(char ch : s) if(!isalnum(ch) && ch != '_') return nullopt;
  return s;
}

static auto posPair(const ExprToken& x) -> pair<ssize_t,ssize_t> {
  return {stPos(x), enPos(x)};
}

constexpr size_t npos = -1;
constexpr pair<ssize_t,ssize_t> nrange{-1,-1};

/*
  Searches for ident in rules[].name().
  If found, returns the index.
  If not found, appends a new monostate rule with the ident, and returns the
    index of the new element. In this case, it also records thisPos in
    firstUseLocs.
  Assumes ident.empty() == false
*/
static size_t findOrAppendIdent(
    vector<Rule>& rules, vector<pair<ssize_t,ssize_t>>& firstUseLocs,
    string_view ident, pair<ssize_t, ssize_t> thisPos) {
  for(size_t i=0; i<rules.size(); ++i) if(ident == rules[i].name()) {
    if(firstUseLocs.size() != rules.size())
      Bug("firstUseLocs size mismatch: {} != {}",
          firstUseLocs.size(), rules.size());
    if(firstUseLocs[i] == nrange) firstUseLocs[i] = thisPos;
    return i;
  }
  rules.emplace_back(monostate{}, string(ident));
  firstUseLocs.push_back(thisPos);
  return rules.size()-1;
}
/*
  Returns the index of a monostate rule named ident.
    If one already exists, its index is returned with no change.
    If one doesn't already exist, one is appended and the new index is returned.
  If a non-monostate rule named ident already exists, it produces a
    "multiple definition" error and returns npos.

  In case we are actually appending a new entry, the firstUseLocs() remains
  nrange() so that it is later filled in by findOrAppendIdent.
*/
static size_t defineIdent(InputDiagsRef ctx, vector<Rule>& rules,
                          vector<pair<ssize_t,ssize_t>>& firstUseLocs,
                          string_view ident, pair<ssize_t, ssize_t> thisPos) {
  for(size_t i=0; i<rules.size(); ++i) if(ident == rules[i].name()) {
    if(!holds_alternative<monostate>(rules[i])) {
      Error(ctx, thisPos.first, thisPos.second,
            format("'{}' has multiple definitions", ident));
      return npos;
    }else return i;
  }
  rules.emplace_back(monostate{}, string(ident));
  firstUseLocs.push_back(nrange);
  return rules.size()-1;
}

// Utility for anon rules that also appends a dummy first-use location entry.
// Anonymous rules don't need usage location so far, since we never refer to
// them in error messages. They are implicitly generated, so the user won't
// know what to make of errors about rules they didn't write.
//
// Named rules should use findOrAppendIdent followed by direct assignment.
template <class...Args> static void
emplaceBackAnonRule(vector<Rule>& rules,
                    vector<pair<ssize_t,ssize_t>>& firstUseLocs,
                    Args&&...args) {
  rules.emplace_back(std::forward<Args>(args)...);
  firstUseLocs.emplace_back(-1, -1);
}

static bool requireEol(const vector<ExprToken>& linetoks, size_t eolPos,
                       InputDiagsRef ctx) {
  if(linetoks.size() < eolPos)
    Bug("requireEol({}) assumes earlier tokens are already processed", eolPos);
  if(linetoks.size() > eolPos) {
    Error(ctx, stPos(linetoks[eolPos]), "Expected end of line");
    return false;
  }
  return true;
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
static bool resemblesPolitenessDirective(const vector<ExprToken>& linetoks) {
  return !linetoks.empty() && isToken(linetoks[0], "require_politeness");
}
static void parsePolitenessDirective(
    const vector<ExprToken>& linetoks, InputDiagsRef ctx,
    vector<Rule>& rules, vector<pair<ssize_t,ssize_t>>& firstUseLocs,
    vector<Example>& examples) {
  const char hello_ident[] = "required_hello";
  ssize_t hello_index =
    defineIdent(ctx, rules, firstUseLocs, hello_ident, posPair(linetoks[0]));
  if(size_t(hello_index) == npos) return;
  if(linetoks.size() == 1) {
    rules[hello_index] = Rule{
        MatchOrError{ssize_t(rules.size()), "Failed at politeness test"},
        hello_ident};
    emplaceBackAnonRule(rules, firstUseLocs, "Hello!");
    size_t testLine = ctx.input->rowCol(stPos(linetoks[0])).first;
    examples.push_back(
        Example{testLine, "required_hello", "Hello!", Expectation::Success});
    examples.push_back(
        Example{testLine, "required_hello", "Goodbye!",
                Expectation::ErrorSubstr{"Failed at politeness test"}});
  }else if(linetoks.size() >= 2 && isToken(linetoks[1], "jsonized")) {
    if(!requireEol(linetoks, 2, ctx)) return;
    const ssize_t nextRuleIndex = rules.size();
    rules[hello_index] = Rule{
        MatchOrError{nextRuleIndex, "Failed at politeness test"}, hello_ident
    };
    emplaceBackAnonRule(rules, firstUseLocs, "Hello!");
    InputDiags tmplinput{Input{"{msg: msg}"}};
    size_t tmplpos = 0;
    ConcatRule single{
      {{hello_index, "msg"}},
      *parseJsonLoc(tmplinput, tmplpos)
    };
    emplaceBackAnonRule(rules, firstUseLocs,
                        std::move(single), "required_hello_in_json");
  }else Error(ctx, linetoks[1], "Was expecting end of line or 'jsonized'");
}

static char bracketStart(BracketType bt) {
  switch(bt) {
    case BracketType::square: return '[';
    case BracketType::brace: return '{';
    case BracketType::paren: return '(';
    default: Bug("Unknown BracketType {}", int(bt));
  }
}

static bool requireBracketType(const BracketGroup& bg, BracketType bt,
                               InputDiagsRef ctx) {
  if(bg.type != bt) {
    Error(ctx, bg.stPos, format("Was expecting '{}'", bracketStart(bt)));
    return false;
  }else return true;
}

template <class T> T*
get_if_in_bound(vector<ExprToken>& toks, size_t i,
                InputDiagsRef ctx) {
  if(toks.empty()) Bug("get_if_in_bound expects non-empty input");
  if(i >= toks.size()) {
    Error(ctx, enPos(toks.back()), "Unexpected end of expression");
    return nullptr;
  }
  return get_if<T>(&toks[i]);
}

template <class T> const T*
get_if_in_bound(const vector<ExprToken>& toks, size_t i,
                InputDiagsRef ctx) {
  if(toks.empty()) Bug("get_if_in_bound expects non-empty input");
  if(i >= toks.size()) {
    Error(ctx, enPos(toks.back()), "Unexpected end of expression");
    return nullptr;
  }
  return get_if<T>(&toks[i]);
}

static bool resemblesBnfRule(const vector<ExprToken>& linetoks) {
  return linetoks.size() >= 2 && isToken(linetoks[1], ":=");
}
static void assignLiteralOrError(vector<Rule>& rules,
                                 vector<pair<ssize_t,ssize_t>>& firstUseLocs,
                                 size_t ruleIndex,
                                 string_view ruleName, string_view literal) {
  rules[ruleIndex] = Rule{
    MatchOrError{ssize_t(rules.size()), format("Expected '{}'", literal)},
    string(ruleName)
  };
  emplaceBackAnonRule(rules, firstUseLocs, string(literal));
}
static void assignRegexOrError(vector<Rule>& rules,
                                 vector<pair<ssize_t,ssize_t>>& firstUseLocs,
                                 size_t ruleIndex,
                                 string_view ruleName,
                                 RegexPattern regex) {
  rules[ruleIndex] = Rule{
    MatchOrError{ssize_t(rules.size()), format("Expected {}", ruleName)},
    string(ruleName)
  };
  emplaceBackAnonRule(rules, firstUseLocs, std::move(regex.patt));
}

ssize_t emplaceBackWordOrError(vector<Rule>& rules,
                               vector<pair<ssize_t,ssize_t>>& firstUseLocs,
                               string_view word) {
  ssize_t newIndex = rules.size();
  emplaceBackAnonRule(rules, firstUseLocs, WordPreserving{word});
  emplaceBackAnonRule(rules, firstUseLocs,
      MatchOrError{newIndex, format("Expected '{}'", word)});
  return newIndex + 1;
}
ssize_t emplaceBackRegexOrError(vector<Rule>& rules,
                                vector<pair<ssize_t,ssize_t>>& firstUseLocs,
                                unique_ptr<const Regex> regex) {
  ssize_t newIndex = rules.size();
  emplaceBackAnonRule(rules, firstUseLocs, std::move(regex));
  emplaceBackAnonRule(rules, firstUseLocs,
      MatchOrError{newIndex, format("Does not match expected pattern")});
  return newIndex + 1;
}

/* This function is called when linetoks is of the form
   {someVar, ":=", "Concat", ...}. It ignores these first 3 tokens, then
   parses the part after "Concat". On success, it returns a ConcatRule that
   should be inserted into findOrAppendIdent(someVar). On failure, it returns
   nullopt.
*/
static auto parseConcatRule(vector<ExprToken> linetoks,
                            InputDiagsRef ctx,
                            vector<Rule>& rules,
                            vector<pair<ssize_t, ssize_t>>& firstUseLocs)
  -> optional<ConcatRule> {

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
      ssize_t newIndex = emplaceBackWordOrError(rules, firstUseLocs, *s);
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
      concat.comps.push_back({
          ssize_t(findOrAppendIdent(rules, firstUseLocs, **tok, posPair(*tok))),
          argname});
    }else if(const auto* s = get_if<GluedString>(&comp[0])) {
      if(s->ctor() != GluedString::Ctor::squoted) {
        Error(ctx, *s, "Expected strings to be single-quoted");
        continue;
      }
      ssize_t newIndex = rules.size();
      emplaceBackAnonRule(rules, firstUseLocs, monostate{});
      assignLiteralOrError(rules, firstUseLocs, newIndex, {}, *s);
      concat.comps.push_back({newIndex, argname});
    }else if(auto* regex = get_if<RegexPattern>(&comp[0])) {
      ssize_t newIndex =
        emplaceBackRegexOrError(rules, firstUseLocs, std::move(regex->patt));
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

static auto parseSkipPoint(const vector<ExprToken>& linetoks,
                           InputDiagsRef ctx) -> optional<SkipPoint> {
  auto* seg = get_if_in_bound<WholeSegment>(linetoks, 3, ctx);
  if(!seg) return nullopt;
  bool withinLine;
  if(**seg == "withinLine") withinLine = true;
  else if(**seg == "acrossLines") withinLine = false;
  else return Error(ctx, *seg, "Expected either 'withinLine' or 'acrossLines'");
  if(!requireEol(linetoks, 4, ctx)) return nullopt;
  return SkipPoint{.stayWithinLine = withinLine, .skip = &oalexSkip};
}

static void parseBnfRule(vector<ExprToken> linetoks,
                         InputDiagsRef ctx,
                         vector<Rule>& rules,
                         vector<pair<ssize_t, ssize_t>>& firstUseLocs) {
  const optional<string> ident = getIfIdent(linetoks[0]);
  if(!ident.has_value()) {
    Error(ctx, linetoks[0], "Identifier expected");
    return;
  }
  const size_t ruleIndex = defineIdent(
      ctx, rules, firstUseLocs, *ident, posPair(linetoks[0])
  );
  if(ruleIndex == npos) return;
  if(linetoks.size() < 3) {
    Error(ctx, linetoks[1], "Rule's right-hand side missing");
    return;
  }
  if(const auto* literal = get_if<GluedString>(&linetoks[2])) {
    if(!requireEol(linetoks, 3, ctx)) return;
    assignLiteralOrError(rules, firstUseLocs, ruleIndex, *ident, *literal);
    return;
  }else if(auto* regex = get_if<RegexPattern>(&linetoks[2])) {
    if(!requireEol(linetoks, 3, ctx)) return;
    assignRegexOrError(rules, firstUseLocs, ruleIndex, *ident,
                       std::move(*regex));
    return;
  }else if(isToken(linetoks[2], "Concat")) {
    if(optional<ConcatRule> c =
        parseConcatRule(std::move(linetoks), ctx, rules, firstUseLocs)) {
      rules[ruleIndex] = Rule(std::move(*c), *ident);
    }
    return;
  }else if(isToken(linetoks[2], "SkipPoint")) {
    if(optional<SkipPoint> sp = parseSkipPoint(linetoks, ctx)) {
      rules[ruleIndex] = Rule(std::move(*sp), *ident);
    }
    return;
  }else {
    Error(ctx, linetoks[2], "Expected string literal");
    return;
  }
}

// Returns true iff tokens is a sequence of WholeSegments matching
// expectations. Empty elements in expectations are treated as wildcards,
// and they will match anything, even other ExprToken types.
// But thsoe wildcards do need *some* token to match against, so we return false
// if tokens.size() is too small.
static bool matchesTokens(const vector<ExprToken>& tokens,
                          const vector<string_view>& expectations) {
  if(tokens.size() < expectations.size()) return false;
  for(size_t i=0; i<expectations.size(); ++i)
    if(!expectations[i].empty() && !isToken(tokens[i], expectations[i]))
      return false;
  return true;
}


static WholeSegment indent_of(const Input& input, const ExprToken& tok) {
  ssize_t bol = input.bol(stPos(tok));
  ssize_t indent_end = oalexWSkip.withinLine(input, bol);
  return WholeSegment(bol, indent_end, input);
}

static bool goodIndent(InputDiags& ctx, const WholeSegment& indent1,
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

static const LexDirective& defaultLexopts() {
  static const auto* var = new LexDirective{
    parseRegexCharSet("[_a-zA-Z]"),
    Skipper{ {{"/*","*/"},{"//","\n"}}, {} },
    false
  };
  return *var;
}

// TODO: fix names: append vs emplace
// TODO: fix parameter order
static ssize_t appendLiteralOrError(
    vector<Rule>& rules,
    vector<pair<ssize_t,ssize_t>>& firstUseLocs,
    string_view ident, string_view literal) {
  ssize_t newIndex = rules.size();
  emplaceBackAnonRule(rules, firstUseLocs, monostate{});
  assignLiteralOrError(rules, firstUseLocs, newIndex, ident, literal);
  return newIndex;
}

// This function returns ConcatFlatRule::Commponent::outputPlaceholder
// for ConcatFlatRule components. It must be empty for any pattern that is
// mapped into a map-returning rule.
static string patternName(const Pattern& patt) {
  if(auto* ident = get_if_unique<Ident>(&patt)) return ident->preserveCase();
  else return {};
}

static ssize_t appendPatternRule(InputDiags& ctx,
    const Pattern& patt, vector<Rule>& rules,
    vector<pair<ssize_t,ssize_t>>& firstUseLocs) {
  if(auto* word = get_if_unique<WordToken>(&patt)) {
    return emplaceBackWordOrError(rules, firstUseLocs, **word);
  }else if(auto* oper = get_if_unique<OperToken>(&patt)) {
    return appendLiteralOrError(rules, firstUseLocs, {}, **oper);
  }else if(get_if_unique<NewlineChar>(&patt)) {
    return appendLiteralOrError(rules, firstUseLocs, {}, "\n");
  }else if(auto* ident = get_if_unique<Ident>(&patt)) {
    return findOrAppendIdent(rules, firstUseLocs, ident->preserveCase(), {});
  }else if(auto* concatPatt = get_if_unique<PatternConcat>(&patt)) {
    ConcatFlatRule concatRule{ .comps{} };
    for(ssize_t i = 0; i < (ssize_t)concatPatt->parts.size(); ++i) {
      if(i > 0) {
        // Intersperse concat components with SkipPoint components.
        concatRule.comps.push_back({(ssize_t)rules.size(), {}});
        emplaceBackAnonRule(rules, firstUseLocs,
                            SkipPoint{.stayWithinLine = false,
                                      .skip = &oalexSkip});
      }
      const Pattern& child = concatPatt->parts[i];
      ssize_t j = appendPatternRule(ctx, child, rules, firstUseLocs);
      concatRule.comps.push_back({j, patternName(child)});
    }
    emplaceBackAnonRule(rules, firstUseLocs, std::move(concatRule));
    return rules.size()-1;
  }else {
    Unimplemented("Pattern compilation of index {}", patt.index());
  }
}

// We can later add where-stanza arguments for extracting partPatterns
static auto makePartPatterns(InputDiags& ctx, const JsonLoc& jsloc)
  -> map<Ident,PartPattern> {
  if(holds_alternative<JsonLoc::Vector>(jsloc))
    Unimplemented("Directly outputting list not encased in a map");
  const JsonLoc::Map* jslocmap = get_if<JsonLoc::Map>(&jsloc);
  if(jslocmap == nullptr)
    Bug("parseJsonLocFromBracketGroup() returned something strange");

  map<Ident, PartPattern> rv;
  for(const auto& [p, j] : jsloc.allPlaceholders()) {
    WholeSegment seg(j->stPos, j->enPos, p);
    GluedString gs(ctx, std::move(seg));
    size_t i = 0;
    Ident id = Ident::parse(gs, i);
    if (i != gs.size()) {
      Error(gs, 0, gs.size(), "Not a valid identifier");
      continue;
    }
    rv.insert({id, std::move(gs)});
  }
  return rv;
}

static void registerLocations(
    vector<Rule>& rules, vector<pair<ssize_t,ssize_t>>& firstUseLocs,
    const Ident& id) {
  findOrAppendIdent(rules, firstUseLocs, id.preserveCase(),
                    {id.stPos(), id.enPos()});
}

// Once we have extracted everything we need from InputDiags,
// this is where we compile the extracted string fragments into a rule.
// InputDiags is still used as a destination for error messages.
static void appendPatternRules(
    InputDiags& ctx,
    string_view ident, GluedString patt_string, JsonLoc jsloc,
    vector<Rule>& rules, vector<pair<ssize_t,ssize_t>>& firstUseLocs) {
  map<Ident,PartPattern> partPatterns = makePartPatterns(ctx, jsloc);
  for(auto& [id, pp] : partPatterns) registerLocations(rules, firstUseLocs, id);

  optional<Pattern> patt =
    parsePattern(ctx, tokenizePattern(patt_string, partPatterns,
                                      defaultLexopts()));
  if(!patt.has_value()) return;

  // TODO check if child rule is guaranteed to produce JsonLoc::Map if
  // outputTmpl.substitutionsOk() == false.
  ssize_t newIndex = appendPatternRule(ctx, *patt, rules, firstUseLocs);
  emplaceBackAnonRule(rules, firstUseLocs, OutputTmpl{
      .childidx = newIndex, .outputTmpl = std::move(jsloc)
  });
  rules.back().name(ident);
}

// Assumes colonPos > 0, since the error message
// is attached to the previous token in linetok.
static bool requireColonEol(const vector<ExprToken>& linetoks,
                            size_t colonPos, InputDiags& ctx) {
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
static auto parseIndentedBlock(InputDiags& ctx, size_t& i,
                               const WholeSegment& refIndent,
                               string_view blockName)
  -> optional<GluedString> {
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
static auto parseOutputBraces(vector<ExprToken> linetoks, InputDiagsRef ctx)
  -> optional<JsonLoc> {
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
static bool resemblesRule(const vector<ExprToken>& linetoks) {
  return linetoks.size() >= 2 && isToken(linetoks[0], "rule")
         && getIfIdent(linetoks[1]).has_value();
}

// Assumes i == ctx.input.bol(i), as we just finished lexNextLine().
static void parseRule(vector<ExprToken> linetoks,
                      InputDiags& ctx, size_t& i, vector<Rule>& rules,
                      vector<pair<ssize_t,ssize_t>>& firstUseLocs) {
  if(!requireColonEol(linetoks, 2, ctx)) return;
  optional<GluedString> patt =
      parseIndentedBlock(ctx, i, indent_of(ctx.input, linetoks[0]),
                         "pattern");
  if(!patt.has_value()) return;
  // Guaranteed to succeed by resemblesRule().
  string ident = *getIfIdent(linetoks[1]);

  // Consume next line for the outputs stanza.
  if(auto opt = lexNextLine(ctx, i);
     opt.has_value() && matchesTokens(*opt, {"outputs"}))
    linetoks = std::move(*opt);
  else {
    Error(ctx, i, format("outputs stanza missing in rule {}", ident));
    return;
  }

  if(linetoks.size() < 2 || !isToken(linetoks[1], ":")) {
    Error(ctx, enPos(linetoks[0]), "Expected ':' after 'outputs'");
    return;
  }

  optional<JsonLoc> jsloc = parseOutputBraces<2>(std::move(linetoks), ctx);
  if(!jsloc.has_value()) return;
  appendPatternRules(ctx, ident, std::move(*patt),
                     std::move(*jsloc), rules, firstUseLocs);
}

// Checks second token just so it is not a BNF rule of the form
// `example :=`. We want to avoid requiring too many reserved keywords
// if possible.
static bool resemblesExample(const vector<ExprToken>& linetoks) {
  return linetoks.size() >= 2 && isToken(linetoks[0], "example")
         && getIfIdent(linetoks[1]).has_value();
}

// Assumes i == ctx.input.bol(i), as we just finished lexNextLine().
static auto parseExample(vector<ExprToken> linetoks,
                         InputDiags& ctx, size_t& i) -> optional<Example> {
  if(!requireColonEol(linetoks, 2, ctx)) return nullopt;
  Example rv;
  // Guaranteed to succeed by resemblesExample().
  rv.mappedPos = {.line = ctx.input.rowCol(stPos(linetoks[0])).first};
  rv.ruleName = *getIfIdent(linetoks[1]);

  if(auto sampleInput =
      parseIndentedBlock(ctx, i, indent_of(ctx.input, linetoks[0]),
                         "example input")) {
    rv.sampleInput = std::move(*sampleInput);
  }else return nullopt;

  vector<ExprToken> linetoks2;
  if(auto opt = lexNextLine(ctx, i)) linetoks2 = std::move(*opt);
  else return nullopt;

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

static bool hasUndefinedRules(
    const vector<Rule>& rules,
    const vector<pair<ssize_t, ssize_t>>& firstUseLocs, InputDiags& ctx) {
  if(rules.size() != firstUseLocs.size())
    Bug("rules.size() == {} != {} == firstUseLocs.size(). "
        "The two vectors must always be appended in sync",
        rules.size(), firstUseLocs.size());
  for(size_t i=0; i<rules.size(); ++i)
    if(holds_alternative<monostate>(rules[i])) {
      optional<string> name = rules[i].name();
      if(!name.has_value()) Bug("Anonymous rules should always be initialized");
      const auto [st, en] = firstUseLocs[i];
      Error(ctx, st, en, format("Rule '{}' was used but never defined", *name));
      return true;
    }
  return false;
}

static const JsonLoc* getTemplate(const Rule& rule) {
  if(auto* concat = get_if<ConcatRule>(&rule)) return &concat->outputTmpl;
  if(auto* tmpl   = get_if<OutputTmpl>(&rule)) return &tmpl->outputTmpl;
  return nullptr;
}

static auto hasDuplicatePlaceholders(const JsonLoc& tmpl)
  -> optional<tuple<const JsonLoc*, const JsonLoc*, string>> {
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
static bool hasDuplicatePlaceholders(
    const vector<Rule>& rules, InputDiags& ctx) {
  for(const Rule& rule : rules) if(const JsonLoc* tmpl = getTemplate(rule)) {
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
static void fillInNames(vector<Rule>& rules) {
  size_t nc = 1;
  for(auto& rule : rules) if(rule.needsName() && !rule.name().has_value()) {
    rule.name("rule" + itos(nc++));
  }
}

static bool hasError(const vector<Diag>& diags) {
  for(const auto& d : diags) if(d.severity == Diag::error) return true;
  return false;
}

auto parseOalexSource(InputDiags& ctx) -> optional<ParsedSource> {
  static const auto* userRegexOpts = new RegexOptions{
    // Do not use user-supplied input. See regex_io.h for details.
    .word = parseRegexCharSet("[0-9A-Za-z_]")
  };

  size_t i = 0;
  RuleSet rs{{}, *userRegexOpts};
  vector<Example> examples;
  vector<pair<ssize_t, ssize_t>> firstUseLocs;
  while(ctx.input.sizeGt(i)) {
    if(i != ctx.input.bol(i))
      FatalBug(ctx, i, "Rules must start at bol()");
    optional<vector<ExprToken>> linetoks_opt = lexNextLine(ctx, i);
    if(!linetoks_opt.has_value()) return nullopt;
    auto& linetoks = *linetoks_opt;

    // Any `continue` after this point results in the following lines
    // being processed.
    if(linetoks.empty()) {
      if(ctx.input.sizeGt(i))
        FatalBug(ctx, i, "lexNextLine() returned empty before EOF");
      else break;
    }
    if(resemblesBnfRule(linetoks)) {
      parseBnfRule(std::move(linetoks), ctx, rs.rules, firstUseLocs);
    }else if(resemblesPolitenessDirective(linetoks)) {
      parsePolitenessDirective(linetoks, ctx,
                               rs.rules, firstUseLocs, examples);
    }else if(resemblesExample(linetoks)) {
      if(auto ex = parseExample(std::move(linetoks), ctx, i))
        examples.push_back(std::move(*ex));
    }else if(resemblesRule(linetoks)) {
      parseRule(std::move(linetoks), ctx, i, rs.rules, firstUseLocs);
    }else
      return Error(ctx, linetoks[0],
                   format("Unexpected '{}', was expecting 'example' or "
                          "'require_politeness'",
                          debug(linetoks[0])));
  }
  if(rs.rules.empty()) return Error(ctx, 0, "Doesn't insist on politeness");
  if(hasUndefinedRules(rs.rules, firstUseLocs, ctx) ||
     hasDuplicatePlaceholders(rs.rules, ctx) ||
     hasError(ctx.diags) ||
     false) return nullopt;
  fillInNames(rs.rules);
  return ParsedSource{std::move(rs), std::move(examples)};
}

// TODO make this nicer. Escape with dquoted() on single-line outputs,
// indent on multi-line.
string describeTestFailure(const Example& ex, bool succeeded) {
  string_view input = ex.sampleInput;
  if(!input.empty() && input.back() == '\n') input.remove_suffix(1);

  if(auto msg = ex.expectation.isForErrorSubstr()) {
    if(succeeded) {
      return format("Test failed at {}\n"
                    "Was expecting {} to fail on input '{}'. "
                    "Succeeded unexpectedly.", string(ex.mappedPos),
                    ex.ruleName, input);
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
                    string(ex.mappedPos), ex.ruleName, input);
    }
  }
}

}  // namespace oalex
