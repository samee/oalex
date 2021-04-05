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

#pragma once

#include <string>
#include <string_view>
#include "diags.h"
#include "jsonloc.h"
#include "regex.h"

namespace oalex {

JsonLoc match(InputDiags& ctx, ssize_t& i, std::string_view s);
JsonLoc match(InputDiags& ctx, ssize_t& i, const Regex& regex,
              const RegexOptions& ropts);

// This version discards matches if it is appearing to split a word into two.
// Providing an empty wordChars {} here is equivalent to calling it without
// wordChars.
JsonLoc match(InputDiags& ctx, ssize_t& i, const RegexCharSet& wordChars,
              std::string_view s);

inline void mapUnion(JsonLoc::Map& m1, JsonLoc::Map& m2) {
  for(auto& p : m2) if(!m1.insert(std::move(p)).second)
    Bug("maps are supposed to be key-disjoint");
}

// Converts a parser into a resemblance-checker.
using GeneratedParser = JsonLoc(*)(InputDiags& ctx, ssize_t& i);
bool quietMatch(const Input& input, ssize_t i, GeneratedParser parser);

}  // namespace oalex
