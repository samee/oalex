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

#include <optional>
#include <vector>
#include "fmt/format.h"

#include "lexer.h"
#include "regex_io.h"

using fmt::format;
using oalex::parseRegexCharSet;
using oalex::lex::enPos;
using oalex::lex::ExprToken;
using oalex::lex::GluedString;
using oalex::lex::isToken;
using oalex::lex::lexNextLine;
using oalex::lex::stPos;
using oalex::lex::WholeSegment;
using std::nullopt;
using std::optional;
using std::string;
using std::vector;

namespace oalex {

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

auto parseOalexSource(InputDiags& ctx) -> optional<RuleSet> {
  static const auto* userSkip = new Skipper{{}, {{"#", "\n"}}};
  static const auto* userRegexOpts = new RegexOptions{
    // Do not use user-supplied input. See regex_io.h for details.
    .word = parseRegexCharSet("[0-9A-Za-z_]")
  };

  size_t i = 0;
  RuleSet rs{{}, *userSkip, *userRegexOpts};
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
    if(linetoks.size() >= 2 && isToken(linetoks[1], ":=")) {
      const optional<string> ident = getIfIdent(linetoks[0]);
      if(!ident.has_value()) {
        Error(ctx, stPos(linetoks[0]), enPos(linetoks[0]),
              "Identifier expected");
        continue;
      }
      if(linetoks.size() < 3) {
        Error(ctx, stPos(linetoks[1]), enPos(linetoks[1]),
              "Rule's right-hand side missing");
        continue;
      }
      const auto* literal = get_if<GluedString>(&linetoks[2]);
      if(!literal) {
        Error(ctx, stPos(linetoks[2]), enPos(linetoks[2]),
              "Expected string literal");
        continue;
      }
      if(linetoks.size() > 3) {
        Error(ctx, stPos(linetoks[3]), "Expected end of line");
        continue;
      }
      rs.rules.push_back(Rule{std::move(*literal), std::move(*ident)});
    }else if(isToken(linetoks[0], "require_politeness")) {
      if(linetoks.size() == 1) rs.rules.push_back(Rule{"Hello!"});
      else Error(ctx, stPos(linetoks[1]), "Was expecting end of line");
    }else
      return Error(ctx, stPos(linetoks[0]), enPos(linetoks[0]),
                   format("Unexpected '{}', was expecting require_politeness",
                          debug(linetoks[0])));
  }
  if(rs.rules.empty()) return Error(ctx, 0, "Doesn't insist on politeness");
  return rs;
}

}  // namespace oalex
