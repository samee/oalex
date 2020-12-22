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

#include <string_view>
#include "fmt/format.h"

#include "lexer.h"
#include "regex_io.h"

using fmt::format;
using oalex::parseRegexCharSet;
using oalex::lex::lookahead;
using oalex::lex::UnquotedToken;
using std::optional;
using std::string_view;

namespace oalex {

auto parseOalexSource(InputDiags& ctx) -> optional<RuleSet> {
  static const auto* userSkip = new Skipper{{}, {{"#", "\n"}}};
  static const auto* userRegexOpts = new RegexOptions{
    // Do not use user-supplied input. See regex_io.h for details.
    .word = parseRegexCharSet("[0-9A-Za-z_]")
  };

  ssize_t i = 0;
  optional<UnquotedToken> next = lookahead(ctx, i);
  if(!next) return Error(ctx, 0, "Doesn't insist on politeness");
  if(**next != "require_politeness")
    return Error(ctx, next->stPos, next->enPos,
                 format("Unexpected '{}', was expecting require_politeness",
                        **next));
  i = next->enPos;
  ssize_t j = oalex::lex::skip.acrossLines(ctx.input, i);
  if(ctx.input.sizeGt(j)) return Error(ctx, j, "Was expecting eof");

  RuleSet rs{{}, *userSkip, *userRegexOpts};
  rs.rules.push_back(Rule{"Hello!"});
  return rs;
}

}  // namespace oalex
