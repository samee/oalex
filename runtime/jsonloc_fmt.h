/*  Copyright 2019-2024 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

// This is not combined into jsonloc.h since including fmtlib increases
// compile time by quite a bit, and not every file needs it.
#pragma once
#include "jsonloc.h"
#include <format>

template <> class std::formatter<oalex::JsonLoc> {
  unsigned indent_ = 0;
 public:
  auto parse(format_parse_context& ctx) -> decltype(ctx.begin());

  // TODO see if we really need this to be templated.
  template <class FormatContext>
  auto format(const oalex::JsonLoc& jsloc, FormatContext& ctx) const {
    for(char ch : jsloc.prettyPrint(indent_)) *ctx.out() = ch;
    return ctx.out();
  }
};

namespace oalex {

// These two functions are really internals,
// but are reused in JsonTmpl::prettyPrint.
void printJsonLocString(std::string& buf, std::string_view s);
std::string_view assertJsonLocKey(std::string_view ctx, std::string_view s);

}  // namespace oalex
