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
#include "regex_io.h"
using oalex::parseRegexCharSet;
using std::optional;

namespace oalex {

auto parseOalexSource(InputDiags& ctx) -> optional<RuleSet> {
  static const auto* userSkip = new Skipper{{}, {{"#", "\n"}}};
  static const auto* userRegexOpts = new RegexOptions{
    // Do not use user-supplied input. See regex_io.h for details.
    .word = parseRegexCharSet("[0-9A-Za-z_]")
  };

  if(ctx.input.hasPrefix(0, "require_politeness\n")) {
    RuleSet rs{{}, *userSkip, *userRegexOpts};
    rs.rules.push_back(Rule{"Hello!"});
    return rs;
  }
  return Error(ctx, 0, "Doesn't insist on politeness\n");
}

}  // namespace oalex
