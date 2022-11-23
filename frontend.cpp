/*  Copyright 2020-2022 The oalex authors.

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

#include <memory>
#include <optional>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>
#include "fmt/core.h"

#include "compiler.h"
#include "frontend_pieces.h"
#include "lexer.h"
#include "pattern.h"
#include "regex_io.h"
#include "jsontmpl_parsers.h"
#include "runtime/indent.h"
#include "runtime/util.h"

using fmt::format;
using oalex::DiagsDest;
using oalex::IndentCmp;
using oalex::LexDirective;
using oalex::Note;
using oalex::OutputTmpl;
using oalex::parseJsonTmplFromBracketGroup;
using oalex::parseRegexCharSet;
using oalex::PartPattern;
using oalex::passthroughTmpl;
using oalex::WholeSegment;
using oalex::lex::appendDiags;
using oalex::lex::BracketGroup;
using oalex::lex::BracketType;
using oalex::lex::enPos;
using oalex::lex::ExprToken;
using oalex::lex::fromSegment;
using oalex::lex::gluedCtx;
using oalex::lex::GluedString;
using oalex::lex::isToken;
using oalex::lex::lexIndentedSource;
using oalex::lex::lexListEntries;
using oalex::lex::lexNextLine;
using oalex::lex::lookahead;
using oalex::lex::lookaheadParIndent;
using oalex::lex::oalexWSkip;
using oalex::lex::RegexPattern;
using oalex::lex::skipBlankLines;
using oalex::lex::stPos;
using std::get_if;
using std::holds_alternative;
using std::make_unique;
using std::min;
using std::nullopt;
using std::optional;
using std::pair;
using std::string;
using std::string_view;
using std::tuple;
using std::unique_ptr;
using std::vector;

// TODO generate namespaces in frontend_pieces.{cpp,h}
using ::parseExternRule;

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
    default: Bug("Unknown Expectation type {}", int{matchType_});
  }
  return false;
}

namespace {

// Forward decl
unique_ptr<const RuleExpr>
makeRuleExpr(const ExprToken& tok, DiagsDest ctx);

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
  for(char ch : s) if(!isalnum(ch) && ch != '_') return false;
  return true;
}

Ident
requireIdent(const ExprToken& x, DiagsDest ctx) {
  const WholeSegment* seg = get_if<WholeSegment>(&x);
  if(!seg || (*seg)->empty() || isdigit((*seg)->at(0)) || !resemblesIdent(x)) {
    Error(ctx, x, "Identifier expected");
    return Ident{};
  }
  return Ident::parse(ctx, std::get<WholeSegment>(x));
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
resemblesExternRule(InputDiags& ctx, size_t i) {
  const InputPiece& input = ctx.input();
  if(input.bol(i) != i) return false;
  optional<WholeSegment> tokopt = lookahead(ctx, i);
  return tokopt && **tokopt == "extern";
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

StringLoc
indent_of(const InputPiece& input, const ExprToken& tok) {
  return indent_of(input, stPos(tok));
}

// TODO: move to a more appropriate header. E.g. parser_helpers.h ?
std::nullopt_t
Error(DiagsDest ctx, const StringLoc& s, std::string msg) {
  return Error(ctx, s.stPos(), s.enPos(), std::move(msg));
}

bool
requireCompatIndent(IndentCmp cmpres, DiagsDest ctx,
                    const StringLoc& nextLineIndent) {
  if(cmpres == IndentCmp::bad) {
    Error(ctx, nextLineIndent,
          "Bad mix of spaces and tabs compared to the previous line");
    return false;
  }
  return true;
}

bool
requireGoodIndent(DiagsDest ctx, string_view desc,
                  const StringLoc& indent1, const StringLoc& indent2) {
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
// As a corollary, we shouldn't call trimBlankLines() on fenced inputs.
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
trimBlankLines(GluedString gs) {
  size_t i, j;
  for(i=0; i<gs.size(); ++i) if(gs[i] != '\n') break;
  if(i == gs.size()) return gs.subqstr(i, 0);
  if(gs[gs.size()-1] != '\n')
    Bug("indented input should always end in newlines");
  // Now we know that gs.size() >= i+2 holds
  for(j=gs.size()-2; i+1<j; --j) if(gs[j] != '\n') break;
  return gs.subqstr(i, j-i+2);
}


const LexDirective&
defaultLexopts() {
  static const auto* var = new LexDirective{
    parseRegexCharSet("[_a-zA-Z0-9]"),
    Skipper{ {}, {} },
    .tailcont = false,
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
parseIndentedBlock(InputDiags& ctx, size_t& i, const StringLoc& refIndent,
                   string_view blockName) {
  optional<GluedString> rv;
  if(optional<WholeSegment> ind = lookaheadParIndent(ctx, i)) {
    // Consume input the next block even if it is not indented enough.
    rv = lexIndentedSource(ctx, i, **ind);
    if(!requireGoodIndent(ctx, "Code block", refIndent, fromSegment(*ind)))
      return nullopt;
  }
  if(!rv.has_value())
    Error(ctx, i, format("No indented {} follows", blockName));
  else rv = trimBlankLines(std::move(*rv));
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
  const InputPiece& input = ctx.input();
  while(true) {
    while(input.sizeGt(i) && input.bol(i) != i) ++i;
    i = oalexWSkip.next(input, i);
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
    StringLoc leaderIndent = indent_of(ctx.input(), stanzaLeader);
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
  for(auto& c : collected) if(c.localName == found.localName) {
    Error(ctx, found.localName.stPos(), found.localName.enPos(),
          format("Duplicate definition for placeholder '{}'",
                 found.localName.preserveCase()));
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
  size_t stPos=0, enPos=0;
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
  if(line.size() < 5)
    Error(ctx, enPos(line.back()), "Expected a rule expression");
  else {
    unique_ptr<const RuleExpr> rxpr = makeRuleExpr(line[4], ctx);
    if(rxpr) p2rule.push_back(PatternToRuleBinding{
        .pp{std::move(*patt)}, .localName{std::move(outkey)},
        .ruleExpr{std::move(rxpr)},
      });
  }
  requireEol(line, 5, ctx);
}

// On error, doesn't modify p2rule. On success, new definitions are appended.
void
parseUnaliasedLocalDeclList(DiagsDest ctx, vector<ExprToken> line,
                            vector<PatternToRuleBinding>& p2rule) {
  vector<Ident> lhs = consumeIdentList(ctx, line, "pattern name");
  if(lhs.empty()) { /* do nothing, consumeIdentList already emitted errors */ }
  else if(!matchesTokens(line, 0, {"~"}))
    Error(ctx, lhs.back().enPos(), "Expected '~' after this");
  else if(line.size() < 2)
    Error(ctx, enPos(line.back()), "Expected rule expression after this");
  else {
    for(const auto& elt : lhs) {
      unique_ptr<const RuleExpr> rxpr = makeRuleExpr(line[1], ctx);
      if(!rxpr) continue;
      PatternToRuleBinding binding{
        .pp{GluedString{
          ctx,
          WholeSegment{elt.stPos(), elt.enPos(), elt.preserveCase()}
        }},
        .localName{elt},
        .ruleExpr{std::move(rxpr)},
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
                    const StringLoc& leaderIndent) {
  size_t j = i;
  vector<PatternToRuleBinding> rv;

  vector<ExprToken> line = lexNextLine(ctx, j);
  if(line.empty()) return rv;
  StringLoc lineIndent = indent_of(ctx.input(), line[0]);
  if(!requireGoodIndent(ctx, "Local declaration", leaderIndent, lineIndent))
    return rv;
  StringLoc refIndent = lineIndent;
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
    lineIndent = indent_of(ctx.input(), line[0]);
    cmpres = indentCmp(*refIndent, *lineIndent);
    if(cmpres == IndentCmp::lt)
      Error(ctx, stPos(line[0]), "This line is indented too deep");
  }while(cmpres == IndentCmp::eq || cmpres == IndentCmp::lt);
  if(!requireCompatIndent(cmpres, ctx, lineIndent)) return {};

  return rv;
}

vector<GluedString>
splitLines(const GluedString& block) {
  size_t i=0, j;
  vector<GluedString> rv;
  while((j=block.find('\n', i)) != string::npos) {
    rv.push_back(block.subqstr(i, j-i));
    i = j+1;
  }
  rv.push_back(block.subqstr(i, block.size()-i));
  return rv;
}
GluedString
trim(GluedString s) {
  size_t i=0, j;
  for(i=0; i<s.size(); ++i) if(s[i] != ' ' && s[i] != '\t') break;
  for(j=s.size(); j>0; --j) if(s[j-1] != ' ' && s[j-1] != '\t') break;
  return s.subqstr(i, j-i);
}
optional<pair<string,string>>
commentDelims(DiagsDest ctx, const GluedString& line) {
  string_view comment_kw = "comment";
  size_t bodySt = line.find(comment_kw);
  if(bodySt == string::npos)
    return Error(ctx, line.stPos, line.enPos, "Not a valid comment specifier");
  for(size_t i=0; i<comment_kw.size(); ++i)
    if(isalnum(line[i]) && !(i>=bodySt && i<bodySt+comment_kw.size()))
      return Error(ctx, line.inputPos(i),
                   "Comment delimitter cannot be alpha-numeric");
  GluedString begin = trim(line.subqstr(0, bodySt));
  GluedString end = trim(line.subqstr(bodySt+comment_kw.size(), line.size()));
  if(end.empty()) return pair{string(begin), "\n"};
  else return pair{string(begin), string(end)};
}

// TODO: Change this type from a GluedString to InputPiece. For both these
// functions. We should be able to translate locations with InputPiece alone,
// given how we do it for rowCol() already. That said, I do recall trying to
// get rid of inputPos() altogether.
void
transloc_in_place(const GluedString& s, size_t& loc)
  { loc = s.inputPos(loc); }

vector<ExprToken>
transloc(const GluedString& s, vector<ExprToken> toks) {
  for(auto& tok:toks) {
    transloc_in_place(s, segment(tok).stPos);
    transloc_in_place(s, segment(tok).enPos);
  }
  return toks;
}

vector<ExprToken>
lexGluedLine(DiagsDest ctx, const GluedString& s, string_view desc) {
  InputDiags linectx = gluedCtx(s);
  size_t i = 0;
  vector<ExprToken> rv = transloc(s, lexNextLine(linectx, i));
  if(rv.empty() && !linectx.input().sizeGt(i))
    Error(ctx, s.stPos, s.enPos,
          format("Line for {} unexpectedly empty", desc));
  appendDiags(ctx, std::move(linectx));
  return rv;
}

optional<Skipper::Newlines>
from_string(string_view s) {
  if(s == "ignore_all")   return Skipper::Newlines::ignore_all;
  if(s == "keep_all")     return Skipper::Newlines::keep_all;
  if(s == "ignore_blank") return Skipper::Newlines::ignore_blank;
  if(s == "keep_para")    return Skipper::Newlines::keep_para;
  return nullopt;
}

optional<pair<Skipper::Newlines, bool>>
parseNewlinesDirective(DiagsDest ctx, const GluedString& s) {
  vector<ExprToken> linetoks = lexGluedLine(ctx, s, "newlines directive");
  if(!matchesTokens(linetoks, 0, {"newlines", ":"}))
    FatalBug(ctx, s.stPos, s.enPos, "This line has no newlines directive");
  bool tailcont = false, errors_emitted = false;
  optional<Skipper::Newlines> nl;
  for(size_t i=2; i<linetoks.size(); ++i) {
    const auto* word = get_if<WholeSegment>(&linetoks[i]);
    optional<Skipper::Newlines> wordnl = word ? from_string(word->data)
                                              : nullopt;
    bool istailcont = (word && word->data == "tailcont");
    if(wordnl && !nl) nl = wordnl;
    else if(istailcont && !tailcont) tailcont = true;
    else {  // We know it's an error. Classify it.
      errors_emitted = true;
      if(wordnl)
        Error(ctx, *word, "Multiple newline-handling on the same line");
      else if(istailcont) Error(ctx, *word, "tailcont already set");
      else {
        // Neither wordnl nor istailcont is true. So it's a bad keyword.
        // Suggest fixes and return.
        if(!nl)
          return Error(ctx, linetoks[i], "Expected ignore_all, keep_all, "
                                         "ignore_blank, or keep_para");
        else if(!tailcont)
          return Error(ctx, linetoks[i], "Expected tailcont or end of line");
        else return Error(ctx, linetoks[i], "Expected end of line");
      }
    }
  }
  if(!nl) {
    if(!errors_emitted)
      Error(ctx, enPos(linetoks[1]), "Newline-handling expected");
    return nullopt;
  }else if(nl == Skipper::Newlines::ignore_all && tailcont)
    Error(ctx, linetoks[2],
          "tailcont is only useful for newline-sensitive patterns");
  return pair{*nl, tailcont};
}

const RegexCharSet*
extractCharSet(const Regex* regex) {
  auto* rep = dynamic_cast<const RegexRepeat*>(regex);
  if(!rep) return nullptr;
  return dynamic_cast<const RegexCharSet*>(rep->part.get());
}

optional<RegexCharSet>
parseWordCharsDirective(DiagsDest ctx, const GluedString& s) {
  GluedString rhs = trim(s.subqstr(sizeof("words:")-1, s.size()));
  InputDiags rhsctx = gluedCtx(rhs);
  size_t pos = 0;
  unique_ptr<const Regex> rv = parseRegex(rhsctx, pos);
  // No comment support yet. TODO skipper
  if(rhsctx.input().sizeGt(pos)) Error(rhsctx, pos, "Expected end of line");
  appendDiags(ctx, std::move(rhsctx));
  if(!rv) return nullopt;
  else if(auto* cset = extractCharSet(rv.get())) {
    if(string err = validateWordChars(*cset); !err.empty())
      return Error(ctx, rhs, err);
    return *cset;
  }
  else return Error(ctx, rhs, "'words:' can only use /[some set]+/");
}

// Assumes linetoks.size() >= 1
optional<LexDirective>
parseLexicalStanza(InputDiags& ctx, size_t& i, vector<ExprToken> linetoks) {
  if(!matchesTokens(linetoks, 1, {":"}))
    Error(ctx, enPos(linetoks[0]), "Expected ':'"); // continue parsing
  requireEol(linetoks, 2, ctx);  // continue parsing even on error
  optional<GluedString> lexblock = parseIndentedBlock(ctx, i,
      indent_of(ctx.input(), linetoks[0]), "lexical block");
  if(!lexblock) return nullopt;
  LexDirective rv = LexDirective{
    parseRegexCharSet("[_a-zA-Z0-9]"),
    Skipper{ {}, {} },
    .tailcont = false,
  };
  vector<GluedString> lines = splitLines(*lexblock);
  for(auto& line : lines) {
    line = trim(line);
    if(line.empty()) continue;
    if(line.hasPrefix(0, "newlines:")) {// TODO: tolerate wspace between tokens
      if(auto nl = parseNewlinesDirective(ctx, line)) {
        rv.skip.newlines = nl->first;
        rv.tailcont = nl->second;
      }
      continue;
    }else if(line.hasPrefix(0, "word:")) {
      if(auto cs = parseWordCharsDirective(ctx, line)) rv.wordChars = *cs;
      continue;
    }
    optional<pair<string, string>> delims = commentDelims(ctx, line);
    if(!delims) continue;
    if(delims->second.back() == '\n')
      rv.skip.unnestedComments.push_back(*delims);
    else if(!rv.skip.nestedComment) rv.skip.nestedComment = *delims;
    else Error(ctx, line, "We support only one type of block comments now");
  }
  if(optional<string> err = rv.skip.valid()) Error(ctx, linetoks[0], *err);
  return rv;
}

// A complex component in this case is anything that cannot appear as a
// component of RuleExprRepeat (either as .part or as .glue).
size_t
findComplexComponent(const vector<ExprToken>& toks) {
  for(size_t i=0; i<toks.size(); ++i) {
    // continue on "simple" tokens, return on everything else.
    if(holds_alternative<WholeSegment>(toks[i])) {
      if(isToken(toks[i], "...") || resemblesIdent(toks[i])) continue;
    }else if(const auto* gs = get_if<GluedString>(&toks[i])) {
      if(gs->ctor() == GluedString::Ctor::squoted) continue;
    }
    return i;
  }
  return toks.size();  // Nothing complex found.
}
tuple<size_t,size_t>
findAndCountEllipsis(const vector<ExprToken>& toks) {
  size_t count=0, pos = toks.size();
  for(size_t i=0; i<toks.size(); ++i) if(isToken(toks[i], "...")) {
    ++count; pos=i;
  }
  return tuple{count, pos};
}

bool
hasAnyTlide(const vector<ExprToken>& toks) {
  for(auto& tok:toks) if(isToken(tok, "~")) return true;
  return false;
}

// This struct stores the result of parsing `where`, `outputs`, `errors`,
// and everything else that can accompany a rule. It's meant to be reused
// between pattern rules and expression rules.
//
// Here, many of the values here have default values, which we use if that
// stanza was missing or had some error. The various `saw*Kw` booleans are
// meant to help with diagnostics: they tell the caller if the corresponding
// keyword was completely missing, or if it was present but had other errors
// that resulted in us using the default value.
//
// TODO: Make this an argument in appendPatternRule, instead of passing in
// all the components separately.
struct RuleStanzas {
  bool sawOutputsKw = false, sawWhereKw = false, sawLexicalKw = false;
  bool sawErrorsKw = false;
  JsonTmpl jstmpl = JsonTmpl::Ellipsis{};
  vector<PatternToRuleBinding> local_decls = {};
  LexDirective lexopts = defaultLexopts();
  JsonLoc errors = JsonLoc::Map{};
};

RuleStanzas
parseRuleStanzas(InputDiags& ctx, size_t& i) {
  RuleStanzas rv;

  while(true) {
    size_t oldi = i;
    auto toks = lexNextLine(ctx, i);
    if(toks.empty()) break;
    auto* leader = get_if<WholeSegment>(&toks[0]);
    if(!leader) { i = oldi; break; }
    else if(**leader == "outputs") {
      if(skipStanzaIfSeen(rv.sawOutputsKw, *leader, ctx, i)) continue;
      optional<JsonTmpl> jstmpl = parseRuleOutput(std::move(toks), ctx);
      if(jstmpl.has_value()) rv.jstmpl = std::move(*jstmpl);
    }else if(**leader == "where") {
      if(skipStanzaIfSeen(rv.sawWhereKw, *leader, ctx, i)) continue;
      StringLoc leaderIndent = indent_of(ctx.input(), toks[0]);
      auto new_local_decls = parseRuleLocalDecls(ctx, i, leaderIndent);
      if(!new_local_decls.empty()) rv.local_decls = std::move(new_local_decls);
      else i = skipToIndentLe(ctx, i, *leaderIndent);
    }else if(**leader == "errors") {
      if(skipStanzaIfSeen(rv.sawErrorsKw, *leader, ctx, i)) continue;
      ssize_t is = leader->stPos;  // Don't use i. That's at the end of line.
      JsonLoc errors = parseErrorStanza(ctx, is);
      if(!errors.holdsErrorValue()) rv.errors = std::move(errors);
      else continue;
      i = skipBlankLines(ctx, is);
    }else if(**leader == "lexical") {
      if(skipStanzaIfSeen(rv.sawLexicalKw, *leader, ctx, i)) continue;
      if(auto opt = parseLexicalStanza(ctx, i, std::move(toks)))
        rv.lexopts = std::move(*opt);
    }else { i = oldi; break; }
  }
  return rv;
}

// Returns non-null if successful. Produces no diags if token is a
// BracketGroup, just returns null. On other token types, returns null and
// produces error.
//
// An "atom" in this case means any pattern that can appear unparenthesized.
// Either as a rule expression all by itself, or on the right hand side of a
// RuleExprMappedIdent. A concatenated sequence of RuleExpr is _not_ an atom,
// for instance.
//
// Top-level caller TODO: requireEol and RuleExprIdent
unique_ptr<const RuleExpr>
makeRuleExprAtom(DiagsDest ctx, const ExprToken& tok) {
  if(auto* regex = get_if<RegexPattern>(&tok)) {
    return make_unique<RuleExprRegex>(regex->patt->clone());
  }else if(auto* gs = get_if<GluedString>(&tok)) {
    if(gs->ctor() != GluedString::Ctor::squoted) {
      if(gs->ctor() == GluedString::Ctor::dquoted)
        Error(ctx, *gs, "Double-quoted patterns not yet implemented");
      else
        Error(ctx, *gs, "Strings need to be 'quoted'");
    }else return make_unique<RuleExprSquoted>(string(*gs));
  }else if(auto* seg = get_if<WholeSegment>(&tok)) {
    if(resemblesIdent(*seg)) {
      Ident id = requireIdent(tok, ctx);
      if(id) return make_unique<RuleExprIdent>(std::move(id));
    }else Error(ctx, *seg, "Not a valid rule definition");
  }else if(!holds_alternative<BracketGroup>(tok))
    Bug("{}: Unexpected token type {}", __func__, exprTagName(tok));
  return nullptr;
}

unique_ptr<const RuleExpr>
makeMappedIdentRuleExpr(DiagsDest ctx, const vector<ExprToken>& toks) {
  if(toks.size() == 0) Bug("hasAnyTlide() returned false on empty vector");
  Ident lhs = requireIdent(toks[0], ctx);
  if(!matchesTokens(toks, 1, {"~"})) {
    Error(ctx, enPos(toks[0]), "Expected '~'");
    return nullptr;
  }
  if(toks.size() <= 2) {
    Error(ctx, enPos(toks[1]), "The right-hand side is missing");
    return nullptr;
  }
  unique_ptr<const RuleExpr> rhs = makeRuleExprAtom(ctx, toks[2]);
  if(!rhs) return nullptr;
  if(toks.size() != 3) {
    Error(ctx, enPos(toks[2]), "Expected ')' after this");
    return nullptr;
  }
  return make_unique<RuleExprMappedIdent>(std::move(lhs), std::move(rhs));
}

unique_ptr<const RuleExpr>
makeRuleExprConcat(const BracketGroup& bg, DiagsDest ctx) {
  const vector<ExprToken>& toks = bg.children;
  if(toks.empty()) {
    Error(ctx, bg, "This cannot be empty");
    return nullptr;
  }
  vector<unique_ptr<const RuleExpr>> rv;
  for(auto& tok: toks) if(auto part = makeRuleExpr(tok, ctx))
    rv.push_back(std::move(part));
  if(rv.empty()) return nullptr;
  else if(rv.size() == 1) return std::move(rv[0]);
  else return make_unique<RuleExprConcat>(std::move(rv));
}

bool
repeatCompEqual(const ExprToken& a, const ExprToken& b) {
  if(exprType(a) != exprType(b)) return false;
  if(auto* seg = get_if<WholeSegment>(&a))
    return **seg == *std::get<WholeSegment>(b);
  else if(auto* s = get_if<GluedString>(&a))
    return string_view(*s) == string_view(std::get<GluedString>(b));
  else Bug("repeatCompEqual() called with invalid tokens: {}",
           exprTagName(a));
}

unique_ptr<const RuleExpr>
makeRuleExprAtomVector(const vector<ExprToken>& toks, size_t st, size_t en,
                       DiagsDest ctx) {
  if(st == en) return nullptr;
  if(st+1 == en) return makeRuleExprAtom(ctx, toks[st]);
  vector<unique_ptr<const RuleExpr>> concat_parts;
  for(size_t i=st; i<en; ++i) if(auto rxpr = makeRuleExprAtom(ctx, toks[i]))
    concat_parts.push_back(std::move(rxpr));
  return make_unique<RuleExprConcat>(std::move(concat_parts));
}

// This function assumes that that toks don't have anything other than
// identifiers, ellipses, and single-quoted strings. The caller should have
// already verified this with findComplexComponent().
unique_ptr<const RuleExpr>
makeRuleExprRepeat(const vector<ExprToken>& toks, size_t ellPos,
                   DiagsDest ctx) {
  size_t validSizeCount = 0, validGlueSize = toks.size();
  size_t glueLimit = min(ellPos, toks.size()-ellPos-1);
  for(size_t glueSize=0; glueSize<glueLimit; ++glueSize) {
    bool partEqual = equal(toks.begin(), toks.begin()+ellPos-glueSize,
                           toks.end()-ellPos+glueSize, repeatCompEqual);
    bool glueEqual = equal(toks.begin()+ellPos-glueSize, toks.begin()+ellPos,
                           toks.end()-ellPos, repeatCompEqual);
    if(partEqual && glueEqual) {
      validGlueSize = glueSize;
      ++validSizeCount;
    }
  }
  if(validSizeCount == 0)
    Error(ctx, toks[ellPos], "No obvious repeating component around ellipsis");
  else if(validSizeCount > 1)  // TODO: be more forgiving, like pattern.cpp
    Error(ctx, toks[ellPos], "Ambiguous ellipsis. Try reducing repeats");
  else {
    auto partxpr = makeRuleExprAtomVector(toks, 0, ellPos-validGlueSize, ctx);
    auto gluexpr = makeRuleExprAtomVector(toks, ellPos-validGlueSize,
                                          ellPos, ctx);
    if(partxpr && (validGlueSize == 0 || gluexpr))
      return make_unique<RuleExprRepeat>(std::move(partxpr),
                                         std::move(gluexpr));
  }
  return nullptr;
}

unique_ptr<const RuleExpr>
makeBracketRuleExpr(const BracketGroup& bg, DiagsDest ctx) {
  if(bg.type != BracketType::paren && bg.type != BracketType::square) {
    Error(ctx, bg, "Not a valid rule definition");
    return nullptr;
  }
  unique_ptr<const RuleExpr> result = nullptr;
  const size_t n = bg.children.size();
  size_t complexChild = findComplexComponent(bg.children);
  auto [ellCount, ellPos] = findAndCountEllipsis(bg.children);

  if(hasAnyTlide(bg.children)) {
    result = makeMappedIdentRuleExpr(ctx, bg.children);
  }
  else if(ellCount > 1) {
    Error(ctx, bg.children[ellPos],
          "Multiple ellipses. Separate them using parenthesis");
    return nullptr;
  }
  else if(ellCount == 1 && complexChild < n) {
    Error(ctx, bg.children[complexChild],
          "Ellipsis expressions can only have simple components");
    return nullptr;
  }else if(ellPos < n) {
    result = makeRuleExprRepeat(bg.children, ellPos, ctx);
  }else {
    result = makeRuleExprConcat(bg, ctx);
  }

  if(!result) return nullptr;
  if(bg.type == BracketType::square)
    return make_unique<RuleExprOptional>(std::move(result));
  else return result;
}

unique_ptr<const RuleExpr>
makeRuleExpr(const ExprToken& tok, DiagsDest ctx) {
  unique_ptr<const RuleExpr> rv = makeRuleExprAtom(ctx, tok);
  if(!rv) {
    if(auto* bg = get_if<BracketGroup>(&tok))
      rv = makeBracketRuleExpr(*bg, ctx);
  }
  return rv;
}

// Assumes linetoks.size() >= 4
void
parseExprRule(const Ident& ruleName, vector<ExprToken> linetoks,
              DiagsDest ctx, RulesWithLocs& rl) {
  requireEol(linetoks, 4, ctx);
  unique_ptr<const RuleExpr> rxpr = makeRuleExpr(linetoks[3], ctx);
  if(!rxpr) return;
  if(auto* gs = get_if<GluedString>(&linetoks[3]);
     gs && gs->ctor() == GluedString::Ctor::dquoted)
    Bug("Need to add context to defineIdent() in parseExprRule()");
  appendExprRule(ctx, ruleName, *rxpr, {}, rl);
}

bool
requireColon(const vector<ExprToken>& linetoks, size_t pos, DiagsDest ctx) {
  if(linetoks.size() > pos && isToken(linetoks[pos], ":")) return true;
  Error(ctx, enPos(linetoks[pos-1]), "Was expecting a ':' after this");
  return false;
}

// Checks second token just so it is not a BNF rule of the form
// `rule :=`. We want to avoid requiring too many reserved keywords
// if possible.
bool
resemblesRule(const vector<ExprToken>& linetoks) {
  return linetoks.size() >= 2 && isToken(linetoks[0], "rule")
         && resemblesIdent(linetoks[1]);
}

// Assumes i == ctx.input().bol(i), as we just finished lexNextLine().
void
parseRule(vector<ExprToken> linetoks, InputDiags& ctx, size_t& i,
          RulesWithLocs& rl) {
  if(!requireColon(linetoks, 2, ctx)) return;
  const Ident ident = requireIdent(linetoks[1], ctx);
  if(linetoks.size() > 3) {
    if(ident) parseExprRule(ident, std::move(linetoks), ctx, rl);
    return;
  }
  optional<GluedString> patt =
      parseIndentedBlock(ctx, i, indent_of(ctx.input(), linetoks[0]),
                         "pattern");
  if(!patt.has_value()) return;
  RuleStanzas stz = parseRuleStanzas(ctx, i);

  if(!ident) return;
  if(!stz.sawOutputsKw && !stz.sawWhereKw) {
    Error(ctx, i, format("outputs stanza missing in rule {}",
                         ident.preserveCase()));
    return;
  }

  appendPatternRule(ctx, ident, std::move(*patt), stz.lexopts,
                    std::move(stz.local_decls), std::move(stz.jstmpl),
                    std::move(stz.errors), rl);
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
parseBranchAction(const vector<ExprToken>& branch,
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
resemblesMultiMatchRule(const vector<ExprToken>& linetoks) {
  return linetoks.size() >= 3 && isToken(linetoks[0], "rule")
         && resemblesIdent(linetoks[1]) && isToken(linetoks[2], "choices");
}

void
parseMultiMatchRule(vector<ExprToken> linetoks,
                    InputDiags& ctx, size_t& i, RulesWithLocs& rl) {
  if(!requireColonEol(linetoks, 3, ctx)) return;
  vector<vector<ExprToken>> branches = lexListEntries(ctx, i, '|');
  if(branches.empty()) {
    Error(ctx, enPos(linetoks.back()),
          "Expected choice branches after this");
    return;
  }
  const Ident ruleName = requireIdent(linetoks[1], ctx);
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
    if(!parseBranchAction(branch, actionPos, lookidx, ctx, orRule, rl))
      continue;
  }
  if(!ruleName) return;
  ssize_t orIndex = rl.defineIdent(ctx, ruleName, rl.defaultSkipper());
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

// Assumes i == ctx.input().bol(i), as we just finished lexNextLine().
optional<Example>
parseExample(vector<ExprToken> linetoks, InputDiags& ctx, size_t& i) {
  if(!requireColonEol(linetoks, 2, ctx)) return nullopt;
  // Guaranteed to succeed by resemblesExample().
  size_t exLine = ctx.input().rowCol(stPos(linetoks[0])).first;
  Ident ruleName = requireIdent(linetoks[1], ctx);
  if(!ruleName) return nullopt;

  optional<GluedString> sampleInput =
      parseIndentedBlock(ctx, i, indent_of(ctx.input(), linetoks[0]),
                         "example input");
  if(!sampleInput) return nullopt;

  vector<ExprToken> linetoks2 = lexNextLine(ctx, i);
  if(linetoks2.empty()) return nullopt;

  Expectation expectation;
  if(matchesTokens(linetoks2, {"outputs", "success"})) {
    if(!requireEol(linetoks2, 2, ctx)) return nullopt;
    expectation = Expectation::Success;
  }
  else if(matchesTokens(linetoks2, {"outputs", "error", "with"})) {
    if(linetoks2.size() < 4)
      return Error(ctx, enPos(linetoks2[2]),
                   "The expected error should be on this line");
    const auto* s = get_if<GluedString>(&linetoks2[3]);
    if(s == nullptr || s->ctor() != GluedString::Ctor::squoted)
      return Error(ctx, stPos(linetoks2[3]),
                   "The expected error should be 'single-quoted'");
    expectation = Expectation::ErrorSubstr{string(*s)};
    if(!requireEol(linetoks2, 4, ctx)) return nullopt;
  }else if(matchesTokens(linetoks2, {"outputs", ":"})) {
    optional<JsonTmpl> jstmpl = parseOutputBraces<2>(std::move(linetoks2), ctx);
    if(!jstmpl.has_value()) return nullopt;
    if(hasEllipsis(*jstmpl)) Unimplemented("Ellipsis in output templates");
    if(jstmpl->substitutionsNeeded())
      return Error(ctx, linetoks[2], "Values need to be properly quoted");
    expectation = Expectation::SuccessWithJson{std::move(*jstmpl)};
  }else if(matchesTokens(linetoks2, {"outputs"}))
    return Error(ctx, enPos(linetoks2[0]),
                 "Was expecting ': {', 'success', or 'error with' after this");
  else return Error(ctx, i, "Was expecting 'outputs'");

  // FIXME invalid escape code error appeared multiple times.
  return Example{.mappedPos{.line = exLine}, .ruleName = std::move(ruleName),
                 .sampleInput = std::move(*sampleInput),
                 .expectation = std::move(expectation) };
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
  rl.defaultSkipper(defaultLexopts().skip);
  while(ctx.input().sizeGt(i)) {
    i = skipBlankLines(ctx, i);
    if(i == string::npos) return nullopt;
    i = ctx.input().bol(i);

    // First try bootstrapped pieces. They don't need linetoks.
    // But they do use ssize_t.
    ssize_t is = i;
    bool tok_needed = false, parse_error = false;
    if(resemblesExternRule(ctx, is)) {
      if(JsonLoc jsloc = parseExternRule(ctx, is))
        appendExternRule(jsloc, ctx, rl);
      else parse_error = true;
    }else {
      tok_needed = true;
    }
    i = is;

    if(!parse_error && !tok_needed) continue;
    vector<ExprToken> linetoks = lexNextLine(ctx, i);
    if(linetoks.empty()) {
      if(ctx.input().sizeGt(i)) return nullopt;  // Error case
      else break;
    }
    if(parse_error) continue;

    if(resemblesExample(linetoks)) {
      if(auto ex = parseExample(std::move(linetoks), ctx, i))
        examples.push_back(std::move(*ex));
    }else if(resemblesMultiMatchRule(linetoks)) {
      parseMultiMatchRule(std::move(linetoks), ctx, i, rl);
    }else if(resemblesRule(linetoks)) {
      parseRule(std::move(linetoks), ctx, i, rl);
    }else
      return Error(ctx, linetoks[0],
                   format("Unexpected '{}', was expecting 'example' or 'rule'",
                          debug(linetoks[0])));
  }
  if(rl.ssize() == 0) return Error(ctx, 0, "File doesn't define any rule");
  if(rl.hasUndefinedRules(ctx)) return nullopt;
  RuleSet rs = rl.releaseRulesWith(*userRegexOpts);
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
