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
#include <vector>
#include "fmt/format.h"

#include "lexer.h"
#include "regex_io.h"

using fmt::format;
using oalex::InputDiagsRef;
using oalex::parseRegexCharSet;
using oalex::lex::enPos;
using oalex::lex::ExprToken;
using oalex::lex::GluedString;
using oalex::lex::isToken;
using oalex::lex::lexNextLine;
using oalex::lex::stPos;
using oalex::lex::WholeSegment;
using std::back_insert_iterator;
using std::back_inserter;
using std::nullopt;
using std::optional;
using std::string;
using std::to_string;
using std::vector;

namespace oalex {

MappedPos::operator string() const {
  return "line " + to_string(this->line);
}

bool Expectation::matches(bool success, const std::vector<Diag>& diags) const {
  if(success!=success_) return false;
  if(success) return true;  // Ignore errorSubstr_
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
// Using back_insert_iterator just for being explicit in this function's
// API contract. I.e. we don't do any vector mutation but push_back().
static void parsePolitenessDirective(
    const vector<ExprToken>& linetoks, InputDiagsRef ctx,
    back_insert_iterator<vector<Rule>> rules, ssize_t nextRuleIndex,
    back_insert_iterator<vector<Example>> examples) {
  if(linetoks.size() != 1) {
    Error(ctx, stPos(linetoks[1]), "Was expecting end of line");
    return;
  }

  *rules = Rule{MatchOrError{nextRuleIndex+1, "Failed at politeness test"},
                "required_hello"};
  *rules = Rule{"Hello!"};
  size_t testLine = ctx.input->rowCol(stPos(linetoks[0])).first;
  *examples = Example{testLine, "required_hello", "Hello!",
                      Expectation::Succeeds};
  *examples = Example{testLine, "required_hello", "Goodbye!",
                      Expectation::ErrorSubstr{"Failed at politeness test"}};
}

static bool resemblesBnfRule(const vector<ExprToken>& linetoks) {
  return linetoks.size() >= 2 && isToken(linetoks[1], ":=");
}
static auto parseBnfRule(const vector<ExprToken>& linetoks,
                         InputDiagsRef ctx) -> optional<Rule> {

  const optional<string> ident = getIfIdent(linetoks[0]);
  if(!ident.has_value())
    return Error(ctx, stPos(linetoks[0]), enPos(linetoks[0]),
                 "Identifier expected");
  if(linetoks.size() < 3)
    return Error(ctx, stPos(linetoks[1]), enPos(linetoks[1]),
                 "Rule's right-hand side missing");
  const auto* literal = get_if<GluedString>(&linetoks[2]);
  if(!literal)
    return Error(ctx, stPos(linetoks[2]), enPos(linetoks[2]),
                 "Expected string literal");
  if(linetoks.size() > 3)
    return Error(ctx, stPos(linetoks[3]), "Expected end of line");
  return Rule{std::move(*literal), std::move(*ident)};
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
      if(optional<Rule> r = parseBnfRule(linetoks, ctx))
        rs.rules.push_back(std::move(*r));
    }else if(resemblesPolitenessDirective(linetoks)) {
      parsePolitenessDirective(linetoks, ctx,
                               back_inserter(rs.rules), rs.rules.size(),
                               back_inserter(examples));
    }else
      return Error(ctx, stPos(linetoks[0]), enPos(linetoks[0]),
                   format("Unexpected '{}', was expecting require_politeness",
                          debug(linetoks[0])));
  }
  if(rs.rules.empty()) return Error(ctx, 0, "Doesn't insist on politeness");
  return ParsedSource{std::move(rs), std::move(examples)};
}

// TODO make this nicer. Escape with dquoted() on single-line outputs,
// indent on multi-line.
string describeTestFailure(const Example& ex, bool succeeded) {
  if(auto msg = ex.expectation.isForErrorSubstr()) {
    if(succeeded) {
      return format("Test failed at {}\n"
                    "Was expecting {} to fail on input '{}'. "
                    "Succeeded unexpectedly.", string(ex.mappedPos),
                    ex.ruleName, ex.sampleInput);
    }else {
      return format("Test failed at {}\n"
                    "Was expecting failure with substring '{}'.",
                    string(ex.mappedPos), *msg);
    }
  }else {
    return format("Test failed at {}\n"
                  "Was expecting {} to succeed on input '{}'",
                  string(ex.mappedPos), ex.ruleName, ex.sampleInput);
  }
}

}  // namespace oalex
