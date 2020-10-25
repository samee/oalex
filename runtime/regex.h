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

#pragma once
#include <string_view>
#include <memory>
#include <variant>
#include <vector>

#include "runtime/diags.h"
#include "runtime/util.h"

namespace oalex {

// Forward decl.
enum struct RegexAnchor;

using Regex = variant_unique_const<
  struct RegexCharSet,
  std::string,
  RegexAnchor,
  struct RegexConcat,
  struct RegexRepeat,
  struct RegexOptional,
  struct RegexOrList
>;

// Regex primitives. Likely to change if we ever switch to matching JsonLoc.
struct CharRange { unsigned char from, to; };
struct RegexCharSet { std::vector<CharRange> ranges; bool negated = false; };
enum struct RegexAnchor { wordEdge, bol, eol };

struct RegexConcat { std::vector<Regex> parts; };
struct RegexRepeat { Regex part; };
struct RegexOptional { Regex part; };
struct RegexOrList { std::vector<Regex> parts; };

struct RegexOptions {
  RegexCharSet word;  // Used for \b matches.
};

bool matchesRegexCharSet(char ch, const RegexCharSet& cset);

// Used for lookaheads
bool startsWithRegex(const Input& input, size_t i,
                     const Regex& regex, const RegexOptions& opts);

// Not used for lookaheads.
bool consumeGreedily(const Input& input, size_t& i,
                     const Regex& regex, const RegexOptions& opts);

}  // namespace oalex
