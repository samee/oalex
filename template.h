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
#include <vector>
#include "runtime/diags.h"
#include "runtime/skipper.h"
#include "runtime/lookahead_regex.h"
#include "ident.h"
#include "jsonloc.h"

namespace oalex {

// TODO see if we actually need to own these objects,
//      or if we can just keep pointers.
// This type is currently unused.
struct LexDirective {
  regex::CharSet wordChars;
  Skipper skip;  // Assume skip.valid(), verified at construction.
};

// TODO Use this in Skipper as well.
struct DelimPair { std::string st, en; };
using PartPattern = std::variant<std::string, DelimPair>;
// TODO: We need something like Diags() constructor, something that can convert
//   from string offsets to rowCol() at least for the pre-semantic parsing
//   passes. Maybe, when extracting a QuotedSegment, we can initialize any
//   bol() --> rowCol() mapping table.
auto matchAllParts(const PartPattern& patt, std::string_view s)
  -> std::optional<std::vector<std::pair<size_t, size_t>>>;

// TODO implement this.
// Params:
//   diags: This is what we use to log errors.
//   quoted.value must have JsonLoc::String, or we Bug() out.
//   partPatterns: The user-specified patterns to be replaced with Placeholders.
//
// On error, we return nullopt. If return.has_value(), it is guaranteed to be a
// JsonLoc::Vector, whose elements will be either JsonLoc::String or
// JsonLoc::Placeholder.
auto labelParts(
    Diag& diags,
    const JsonLoc& quoted,
    const std::map<Ident,PartPattern>& partPatterns)
    -> std::optional<JsonLoc>;
// TODO later steps will catch placeholders-in-comments as error.

}  // namespace oalex
