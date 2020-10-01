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

// TODO flatten namespace
namespace oalex::regex {

// Forward decl.
enum struct RegexAnchor;

using Regex = std::variant<
  std::unique_ptr<struct RegexCharSet>,
  std::unique_ptr<std::string>,
  std::unique_ptr<RegexAnchor>,
  std::unique_ptr<struct RegexConcat>,
  std::unique_ptr<struct RegexRepeat>,
  std::unique_ptr<struct RegexOptional>,
  std::unique_ptr<struct RegexOrList>
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
bool startsWith(const Input& input, size_t i,
                const Regex& regex, const RegexOptions& opts);

// Not used for lookaheads.
bool consumeGreedily(const Input& input, size_t& i,
                     const Regex& regex, const RegexOptions& opts);

}  // namespace oalex::regex
