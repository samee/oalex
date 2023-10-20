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

#include <optional>
#include <string>
#include <string_view>
#include "diags.h"
#include "jsonloc.h"
#include "regex.h"

namespace oalex {

std::nullopt_t errorValue(DiagsDest ctx, ssize_t i, std::string msg);

std::optional<StringLoc> match(InputDiags& ctx, ssize_t& i, std::string_view s);
std::optional<StringLoc> match(InputDiags& ctx, ssize_t& i, const Regex& regex,
                               const RegexOptions& ropts);

// This version discards matches if it is appearing to split a word into two.
// Providing an empty wordChars {} here is equivalent to calling it without
// wordChars.
bool peekMatch(InputDiags& ctx, ssize_t i, const RegexCharSet& wordChars,
               std::string_view s);

std::optional<StringLoc> match(InputDiags& ctx, ssize_t& i,
                               const RegexCharSet& wordChars,
                               std::string_view s);

std::unique_ptr<InputPiece> unowned(const InputPiece& input);

// Dev-note: It feels wrong to have a small heap allocation in a function
// that is called *more* frequently than normal parsers.
// TODO: consider an RAII-controlled flag in InputDiags that disables
// appending errors temporarily.
template <class ParserCallback>
auto quietMatch(const InputPiece& input, ssize_t& i, ParserCallback parser) {
  InputDiags proxy{unowned(input)};
  return parser(proxy, i);
}

template <class V>
bool holdsErrorValue(const std::optional<V>& v) { return !v.has_value(); }

inline bool holdsErrorValue(const JsonLoc& jsloc)
  { return jsloc.holdsErrorValue(); }

template <class ParserCallback>
bool peekMatch(const InputPiece& input, ssize_t i, ParserCallback parser) {
  // The only difference between this and quietMatch() is that
  // peekMatch() accepts `i` by value and returns a bool.
  return !holdsErrorValue(quietMatch(input, i, parser));
}


void mapAppend(JsonLoc::Map& m1, JsonLoc::Map m2);

}  // namespace oalex
