/*  Copyright 2020 Google LLC

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
#include <optional>
#include <string_view>
#include <vector>
#include "fmt/core.h"

#include "lexer.h"
#include "regex_io.h"
#include "jsonloc_io.h"

using fmt::format;
using oalex::InputDiagsRef;
using oalex::parseJsonLocFromBracketGroup;
using oalex::parseRegexCharSet;
using oalex::lex::enPos;
using oalex::lex::BracketGroup;
using oalex::lex::BracketType;
using oalex::lex::ExprToken;
using oalex::lex::GluedString;
using oalex::lex::isToken;
using oalex::lex::lexIndentedSource;
using oalex::lex::lexNextLine;
using oalex::lex::lookaheadParIndent;
using oalex::lex::stPos;
using oalex::lex::WholeSegment;
using std::nullopt;
using std::optional;
using std::string;
using std::string_view;
using std::to_string;
using std::vector;

namespace oalex {

MappedPos::operator string() const {
  return "line " + to_string(this->line);
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

// Assumes ident.empty() == false
static size_t identIndex(vector<Rule>& rules, string_view ident) {
  for(size_t i=0; i<rules.size(); ++i) if(ident == rules[i].name()) return i;
  rules.emplace_back(std::monostate{}, string(ident));
  return rules.size()-1;
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
    vector<Rule>& rules, vector<Example>& examples) {
  const char hello_ident[] = "required_hello";
  ssize_t hello_index = identIndex(rules, hello_ident);
  if(linetoks.size() == 1) {
    rules[hello_index] = Rule{
        MatchOrError{ssize_t(rules.size()), "Failed at politeness test"},
        hello_ident};
    rules.emplace_back("Hello!");
    size_t testLine = ctx.input->rowCol(stPos(linetoks[0])).first;
    examples.push_back(
        Example{testLine, "required_hello", "Hello!", Expectation::Success});
    examples.push_back(
        Example{testLine, "required_hello", "Goodbye!",
                Expectation::ErrorSubstr{"Failed at politeness test"}});
  }else if(linetoks.size() >= 2 && isToken(linetoks[1], "jsonized")) {
    if(linetoks.size() > 2) {
      Error(ctx, stPos(linetoks[2]), "Was expecting end of line");
      return;
    }
    const ssize_t nextRuleIndex = rules.size();
    rules[hello_index] = Rule{
        MatchOrError{nextRuleIndex, "Failed at politeness test"}, hello_ident
    };
    rules.emplace_back("Hello!");
    InputDiags tmplinput{Input{"{msg: msg}"}};
    size_t tmplpos = 0;
    ConcatRule single{
      {{hello_index, "msg"}},
      *parseJsonLoc(tmplinput, tmplpos)
    };
    rules.emplace_back(std::move(single), "required_hello_in_json");
  }else Error(ctx, stPos(linetoks[1]),
              "Was expecting end of line or 'jsonized'");
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
static bool requireSizeLe(const vector<ExprToken>& toks, size_t sz,
                          InputDiagsRef ctx) {
  if(toks.size() > sz) {
    Error(ctx, stPos(toks[sz]), "Was expecting expression to end");
    return false;
  }else return true;
}
// Meant for simple cases where we expect only one token between commas
// Allows optional trailing comma.
static bool requireAlternatingSeparators(
    const BracketGroup& bg, char sep, InputDiagsRef ctx) {
  const char seps[2] = {sep, '\0'};
  for(size_t i=0; i<bg.children.size(); i+=2) {
    if(isToken(bg.children[i], seps)) {
      Error(ctx, stPos(bg.children[i]), "Expected token before ','");
      return false;
    }
    if(i+1 >= bg.children.size()) break;
    if(!isToken(bg.children[i+1], seps)) {
      Error(ctx, stPos(bg.children[i+1]), "Expected ','");
      return false;
    }
  }
  return true;
}
template <class T> const T*
get_if_in_bound(const vector<ExprToken>& toks, size_t i,
                InputDiagsRef ctx) {
  if(i >= toks.size()) {
    Error(ctx, enPos(toks.back()), "Unexpected end of expression");
    return nullptr;
  }
  return get_if<T>(&toks[i]);
}

static bool resemblesBnfRule(const vector<ExprToken>& linetoks) {
  return linetoks.size() >= 2 && isToken(linetoks[1], ":=");
}
static void assignLiteralOrError(vector<Rule>& rules, size_t ruleIndex,
                                 string_view ruleName, string_view literal) {
  rules[ruleIndex] = Rule{
    MatchOrError{ssize_t(rules.size()), format("Expected '{}'", literal)},
    string(ruleName)
  };
  rules.emplace_back(string(literal));
}
static void parseBnfRule(const vector<ExprToken>& linetoks,
                         InputDiagsRef ctx,
                         vector<Rule>& rules) {
  const optional<string> ident = getIfIdent(linetoks[0]);
  if(!ident.has_value()) {
    Error(ctx, stPos(linetoks[0]), enPos(linetoks[0]), "Identifier expected");
    return;
  }
  const size_t ruleIndex = identIndex(rules, *ident);
  if(linetoks.size() < 3) {
    Error(ctx, stPos(linetoks[1]), enPos(linetoks[1]),
          "Rule's right-hand side missing");
    return;
  }
  if(const auto* literal = get_if<GluedString>(&linetoks[2])) {
    if(linetoks.size() > 3) {
      Error(ctx, stPos(linetoks[3]), "Expected end of line");
      return;
    }
    assignLiteralOrError(rules, ruleIndex, *ident, *literal);
    return;
  }else if(isToken(linetoks[2], "Concat")) {
    const auto* bg = get_if_in_bound<BracketGroup>(linetoks, 3, ctx);
    if(!bg) return;
    if(!requireBracketType(*bg, BracketType::square, ctx) ||
       !requireSizeLe(linetoks, 4, ctx)) return;
    if(!requireAlternatingSeparators(*bg, ',', ctx)) return;
    ConcatRule concat{ {}, JsonLoc::Map() };
    for(size_t i=0; i<bg->children.size(); i+=2) {
      if(const auto* tok = get_if<WholeSegment>(&bg->children[i])) {
        concat.comps.push_back({ssize_t(identIndex(rules, **tok)), {}});
      }else if(const auto* s = get_if<GluedString>(&bg->children[i])) {
        if(s->ctor() != GluedString::Ctor::squoted) {
          Error(ctx, s->stPos, s->enPos,
                "Expected strings to be single-quoted");
          return;
        }
        ssize_t newIndex = rules.size();
        rules.emplace_back(std::monostate{});
        assignLiteralOrError(rules, newIndex, {}, *s);
        concat.comps.push_back({newIndex, {}});
      }else {
        Error(ctx, stPos(bg->children[i]), enPos(bg->children[i]),
              "Was expecting a string or an identifier");
        return;
      }
    }
    rules[ruleIndex] = Rule{std::move(concat), *ident};
    return;
  }else {
    Error(ctx, stPos(linetoks[2]), enPos(linetoks[2]),
          "Expected string literal");
    return;
  }
}

static bool matchesTokens(const vector<ExprToken>& tokens,
                          vector<string_view> expectations) {
  if(tokens.size() < expectations.size()) return false;
  for(size_t i=0; i<expectations.size(); ++i)
    if(!isToken(tokens[i], expectations[i])) return false;
  return true;
}

// Checks second token just so it is not a BNF rule of the form
// `example :=`. We want to avoid requiring too many reserved keywords
// if possible.
static bool resemblesExample(const vector<ExprToken>& linetoks) {
  return linetoks.size() >= 2 && isToken(linetoks[0], "example")
         && getIfIdent(linetoks[1]).has_value();
}
// Assumes i == ctx.input.bol(i), as we just finished lexNextLine().
auto parseExample(const vector<ExprToken>& linetoks,
                  InputDiags& ctx, size_t& i) -> optional<Example> {
  if(linetoks.size() < 3 || !isToken(linetoks[2], ":"))
    return Error(ctx, enPos(linetoks[1]), "Was expecting a ':' after this");
  if(linetoks.size() > 3)
    return Error(ctx, stPos(linetoks[3]),
                 "Example input needs to be on the following line");
  Example rv;
  // Guaranteed to succeed by resemblesExample().
  rv.mappedPos = {.line = ctx.input.rowCol(stPos(linetoks[0])).first};
  rv.ruleName = *getIfIdent(linetoks[1]);

  // TODO make sure "example" is indented less than the source, and everything
  // that follows is indented more than the "example" keyword.
  optional<GluedString> sampleInput;
  if(auto ind = lookaheadParIndent(ctx, i))
    sampleInput = lexIndentedSource(ctx, i, *ind);
  if(!sampleInput.has_value())
    return Error(ctx, i, "No indented example input follows");
  rv.sampleInput = std::move(*sampleInput);

  vector<ExprToken> linetoks2;
  if(auto opt = lexNextLine(ctx, i)) linetoks2 = *opt;
  else return nullopt;

  if(matchesTokens(linetoks2, {"outputs", "success"})) {
    if(linetoks2.size() > 2)
      return Error(ctx, stPos(linetoks2[2]), "Expected end of line");
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
    if(linetoks2.size() > 4)
      return Error(ctx, stPos(linetoks2[4]), "Expected end of line");
  }else if(matchesTokens(linetoks2, {"outputs", ":"})) {
    const BracketGroup* bg;
    if(linetoks2.size() < 3 ||
        (bg = get_if<BracketGroup>(&linetoks2[2])) == nullptr )
      return Error(ctx, enPos(linetoks2[1]), "Was expecting '{' on this line");
    optional<JsonLoc> jsloc = parseJsonLocFromBracketGroup(ctx, *bg);
    // If there's an error, parseJsonLocFromBracketGroup() should have already
    // produced diags.
    if(!jsloc.has_value()) return nullopt;
    // TODO improve error location.
    if(!jsloc->supportsEquality())
      return Error(ctx, stPos(linetoks[3]),
                   "Values need to be properly quoted");
    if(linetoks2.size() > 3)
      return Error(ctx, stPos(linetoks[3]), "Was expecting end of line");
    rv.expectation = Expectation::SuccessWithJson{std::move(*jsloc)};
  }else if(matchesTokens(linetoks2, {"outputs"}))
    return Error(ctx, enPos(linetoks2[0]),
                 "Was expecting ': {', 'success', or 'error with' after this");
  else return Error(ctx, i, "Was expecting 'outputs'");

  // FIXME invalid escape code error appeared multiple times.
  return rv;
}

static bool hasError(const vector<Diag>& diags) {
  for(const auto& d : diags) if(d.severity == Diag::error) return true;
  return false;
}

auto parseOalexSource(InputDiags& ctx) -> optional<ParsedSource> {
  static const auto* userSkip = new Skipper{{}, {{"#", "\n"}}};
  static const auto* userRegexOpts = new RegexOptions{
    // Do not use user-supplied input. See regex_io.h for details.
    .word = parseRegexCharSet("[0-9A-Za-z_]")
  };

  size_t i = 0;
  RuleSet rs{{}, *userSkip, *userRegexOpts};
  vector<Example> examples;
  while(ctx.input.sizeGt(i)) {
    if(i != ctx.input.bol(i))
      FatalBug(ctx, i, "Rules must start at bol()");
    const optional<vector<ExprToken>> linetoks_opt = lexNextLine(ctx, i);
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
      parseBnfRule(linetoks, ctx, rs.rules);
    }else if(resemblesPolitenessDirective(linetoks)) {
      parsePolitenessDirective(linetoks, ctx,
                               rs.rules, examples);
    }else if(resemblesExample(linetoks)) {
      if(auto ex = parseExample(linetoks, ctx, i))
        examples.push_back(std::move(*ex));
    }else
      return Error(ctx, stPos(linetoks[0]), enPos(linetoks[0]),
                   format("Unexpected '{}', was expecting 'example' or "
                          "'require_politeness'",
                          debug(linetoks[0])));
  }
  if(rs.rules.empty()) return Error(ctx, 0, "Doesn't insist on politeness");
  // TODO check for duplicate Rule names.
  if(hasError(ctx.diags)) return nullopt;
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
      return format("Test failed\nWas expecting output {}.",
                    jsloc->prettyPrint());
    }else {
      return format("Test failed at {}\n"
                    "Was expecting {} to succeed on input '{}'",
                    string(ex.mappedPos), ex.ruleName, input);
    }
  }
}

}  // namespace oalex
