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

#include <map>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>
#include "runtime/diags.h"
#include "runtime/skipper.h"
#include "runtime/lookahead_regex.h"
#include "ident.h"
#include "lexer.h"

namespace oalex {

// TODO see if we actually need to own these objects,
//      or if we can just keep pointers.
// This type is currently unused.
struct LexDirective {
  regex::CharSet wordChars;
  Skipper skip;  // Assume skip.valid(), verified at construction.
};

// TODO Use this in Skipper as well.
struct DelimPair { lex::QuotedString st, en; };
using PartPattern = std::variant<lex::QuotedString, DelimPair>;

auto matchAllParts(const PartPattern& patt, const lex::QuotedString& s)
  -> std::optional<std::vector<std::pair<size_t, size_t>>>;

// TODO implement this.
// Params:
//   s: The string that needs to be split up.
//   partPatterns: The user-specified patterns to be replaced with Placeholders.
//
// On error, we return an empty vector. An empty `s` input will produce a
// vector with a single empty QuotedString.
using LabelOrPart = std::variant<lex::QuotedString, Ident>;
auto labelParts(
    const lex::QuotedString& s,
    const std::map<Ident,PartPattern>& partPatterns)
    -> std::vector<LabelOrPart>;
// TODO later steps will catch placeholders-in-comments as error.

}  // namespace oalex
