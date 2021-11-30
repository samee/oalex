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
#include <memory>
#include <optional>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>
#include "fmt/core.h"

#include "compiler.h"
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
using oalex::PartPattern;
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
using std::holds_alternative;
using std::make_unique;
using std::nullopt;
using std::optional;
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
        concat.comps.push_back({rl.findOrAppendIdent(ctx, id), argname});
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

// Dev-note: technically speaking, we could have cached this index found here
// and reused it later for defineIdent(). In practice, if that really becomes
// an issue, I'd be much better off using a hashmap in RulesWithLocs.
bool
requireUndefined(DiagsDest ctx, const RulesWithLocs& rl, const Ident& ident) {
  ssize_t i = rl.findIdent(ctx, ident);
  if(i == -1) return true;
  if(dynamic_cast<const UnassignedRule*>(&rl[i])) return true;
  Error(ctx, ident.stPos(), ident.enPos(),
        format("'{}' has multiple definitions", ident.preserveCase()));
  return false;
}

void
parseBnfRule(vector<ExprToken> linetoks, DiagsDest ctx, RulesWithLocs& rl) {
  const Ident ident = Ident::parse(ctx, std::get<WholeSegment>(linetoks[0]));
  if(!ident) {
    Error(ctx, linetoks[0], "Identifier expected");
    return;
  }
  if(!requireUndefined(ctx, rl, ident)) return;
  ssize_t orig_size = rl.ssize();
  if(linetoks.size() < 3) {
    Error(ctx, linetoks[1], "Rule's right-hand side missing");
  }else if(const auto* literal = get_if<GluedString>(&linetoks[2])) {
    if(!requireEol(linetoks, 3, ctx)) return;
    const ssize_t ruleIndex = rl.defineIdent(ctx, ident);
    assignLiteralOrError(rl, ruleIndex, *literal);
  }else if(auto* regex = get_if<RegexPattern>(&linetoks[2])) {
    if(!requireEol(linetoks, 3, ctx)) return;
    const ssize_t ruleIndex = rl.defineIdent(ctx, ident);
    string errmsg = format("Expected {}", ident.preserveCase());
    assignRegexOrError(rl, ruleIndex, std::move(errmsg),
                       std::move(regex->patt));
  }else if(isToken(linetoks[2], "Concat")) {
    if(optional<ConcatRule> c = parseConcatRule(std::move(linetoks),ctx,rl)) {
      const ssize_t ruleIndex = rl.defineIdent(ctx, ident);
      rl.deferred_assign(ruleIndex, std::move(*c));
    }else rl.resize_down(orig_size);
  }else if(isToken(linetoks[2], "SkipPoint")) {
    if(optional<SkipPoint> sp = parseSkipPoint(linetoks, ctx)) {
      const ssize_t ruleIndex = rl.defineIdent(ctx, ident);
      rl.deferred_assign(ruleIndex, std::move(*sp));
    }else rl.resize_down(orig_size);
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

// TODO
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
GluedString
trimNewlines(GluedString gs) {
  size_t i, j;
  for(i=0; i<gs.size(); ++i) if(gs[i] != '\n') break;
  if(i == gs.size()) return gs.subqstr(i, 0);
  for(j=gs.size()-1; i<j; --j) if(gs[j] != '\n') break;
  return gs.subqstr(i, j-i+1);
}


const LexDirective&
defaultLexopts() {
  static const auto* var = new LexDirective{
    parseRegexCharSet("[_a-zA-Z]"),
    Skipper{ {{"/*","*/"},{"//","\n"}}, {} },
    false
  };
  return *var;
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
  else rv = trimNewlines(std::move(*rv));
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

// Strips out a comma-separated WordSegment prefix.
// Modifies linetoks. Assumes non-empty linetoks.
// (otherwise we don't have a good way to put LocPair on diags).
// On success, returns non-empty vector and removes items and commas
// from linetoks. On failure, returns empty vector and
// leaves linetoks unchanged.
vector<Ident>
consumeIdentList(DiagsDest ctx, vector<ExprToken>& linetoks,
                 string_view word_desc) {
  vector<Ident> rv;
  size_t i;

  for(i=0; i<linetoks.size(); i+=2) {
    auto* item = get_if<WholeSegment>(&linetoks[i]);
    Ident id;
    if(item) id = Ident::parse(ctx, *item);
    if(!id) {
      Error(ctx, linetoks[i], format("Expected {}", word_desc));
      break;
    }
    rv.push_back(std::move(id));
    if(!matchesTokens(linetoks, i+1, {","})) {
      linetoks.erase(linetoks.begin(), linetoks.begin()+i+1);
      return rv;
    }
  }
  // If we are here, we probably have a trailing comma.
  return {};
}

// Outcomes:
//   No ellipsis  --> returns 0
//   One ellipsis --> returns 1 and sets stPos, enPos (relative to gs)
//   Bad ellipsis --> returns -1
int
findEllipsis(DiagsDest ctx, GluedString gs, size_t& stPos, size_t& enPos) {
  string_view ellipsis = "...";  // TODO change with lexopts/metachars
  size_t i = gs.find(ellipsis);
  if(i == string::npos) return 0;
  size_t i2 = gs.find(ellipsis, i+1);
  if(i2 == string::npos) {
    stPos = i; enPos = i+ellipsis.size();
    return 1;
  }
  size_t i2i = gs.inputPos(i2);
  if(i2 > i + ellipsis.size()) Error(ctx, i2i, i2i+ellipsis.size(),
                                     "Only one omission allowed in a pattern");
  else Error(ctx, gs.inputPos(i), i2i+ellipsis.size(),
             "Too many consecutive dots");
  return -1;
}

optional<PartPattern>
parsePartPattern(DiagsDest ctx, GluedString gs) {
  size_t stPos, enPos;
  ssize_t ec = findEllipsis(ctx, gs, stPos, enPos);
  if(ec < 0) return nullopt;
  if(ec == 0) return PartPattern{std::move(gs)};
  else return DelimPair{
    .st = gs.subqstr(0, stPos),
    .en = gs.subqstr(enPos, string::npos)
  };
}

// On error, doesn't modify p2rule.
// On success, one new definition is appended.
void
parseSingleAliasedLocalDecl(DiagsDest ctx, vector<ExprToken> line,
                            vector<PatternToRuleBinding>& p2rule) {
  auto* ps = get_if<GluedString>(&line[0]);
  if(!ps) {
    Error(ctx, line[0], "Expected a quoted string");
    return;
  }
  optional<PartPattern> patt = parsePartPattern(ctx, *ps);
  if(!patt) return;
  auto* ws = line.size() >= 3 ? get_if<WholeSegment>(&line[2]) : nullptr;
  Ident outkey;
  if(ws) outkey = Ident::parse(ctx, *ws);
  else Error(ctx, line[2], "Expected an output key");
  if(!outkey) return;

  if(!matchesTokens(line, 3, {"~"})) {
    Error(ctx, line[2], "Expected '~' after this");
    return;
  }
  ws = line.size() >= 5 ? get_if<WholeSegment>(&line[4]) : nullptr;
  Ident rulename;
  if(ws) rulename = Ident::parse(ctx, *ws);
  else Error(ctx, line[4], "Expected rule name");
  if(!rulename) return;
  p2rule.push_back(PatternToRuleBinding{
    .pp{std::move(*patt)}, .outTmplKey{std::move(outkey)},
    .ruleName{std::move(rulename)}
  });
  requireEol(line, 5, ctx);
}

// On error, doesn't modify p2rule. On success, new definitions are appended.
void
parseUnaliasedLocalDeclList(DiagsDest ctx, vector<ExprToken> line,
                            vector<PatternToRuleBinding>& p2rule) {
  vector<Ident> lhs = consumeIdentList(ctx, line, "pattern name");
  const WholeSegment* rhs
    = (line.size() >= 2 ? get_if<WholeSegment>(&line[1]) : nullptr);
  if(lhs.empty()) { /* do nothing, consumeIdentList already emitted errors */ }
  else if(!matchesTokens(line, 0, {"~"}))
    Error(ctx, lhs.back().enPos(), "Expected '~' after this");
  else if(rhs == nullptr)
    Error(ctx, enPos(line[0]), "Expected rule name after this");
  else {
    for(const auto& elt : lhs) {
      PatternToRuleBinding binding{
        .pp{GluedString{
          WholeSegment{elt.stPos(), elt.enPos(), elt.preserveCase()}
        }},
        .outTmplKey{elt},
        .ruleName{Ident::parse(ctx, *rhs)},
      };
      if(requireUniqueBinding(p2rule, binding, ctx))
        p2rule.push_back(std::move(binding));
    }
    requireEol(line, 2, ctx);
  }
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
    // We can assume !line.empty()
    const WholeSegment* tok0 = get_if<WholeSegment>(&line[0]);
    bool goodtok0 = false;
    if(tok0 && Ident::parse(ctx, *tok0)) goodtok0 = true;
    else if(holds_alternative<GluedString>(line[0])) goodtok0 = true;

    if(!goodtok0) Error(ctx, line[0], "Expected a part of the pattern");
    else if(line.size() == 1)
      Error(ctx, enPos(line[0]), "Unexpected end of line");
    else if(isToken(line[1], "as"))
      parseSingleAliasedLocalDecl(ctx, std::move(line), rv);
    else if(isToken(line[1], "~") || isToken(line[1], ","))
      parseUnaliasedLocalDeclList(ctx, std::move(line), rv);
    else Error(ctx, enPos(line[1]), "Expected '~' after this");

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
    if(sawWhereKw)
      appendPatternRules(ctx, ident, std::move(*patt), defaultLexopts(),
                         std::move(local_decls), rl);
    else
      Error(ctx, i, format("outputs stanza missing in rule {}",
                           ident.preserveCase()));
    return;
  }

  if(!jstmpl.has_value()) return;
  appendPatternRules(ctx, ident, std::move(*patt), defaultLexopts(),
                     std::move(local_decls), std::move(*jstmpl), rl);
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
  return rl.findOrAppendIdent(ctx, lookId);
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
        .lookidx = lookidx, .parseidx = rl.findOrAppendIdent(ctx, parseId),
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

const Skipper&
defaultSkip() { return defaultLexopts().skip; }

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
