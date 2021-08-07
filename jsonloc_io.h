/*  Copyright 2019-2021 The oalex authors.

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
#include <utility>
#include "runtime/diags.h"
#include "runtime/jsonloc_fmt.h"

// TODO rename jsonloc_io -> jsontmpl_parsers
namespace oalex {

namespace lex { struct BracketGroup; };

class JsonTmpl;
// Note: this does not support a bare string or keyword all by itself, outside
// of square brackets or braces. Without those indicators, it is hard to know
// when to stop.
std::optional<JsonTmpl> parseJsonTmpl(InputDiags& ctx, size_t& i);
std::optional<JsonTmpl> parseJsonTmpl(std::string_view s);

// These functions are identical to the above for now, except for the
// return type. Eventually, they will be removed once all the callers have
// been migrated over.
std::optional<JsonLoc> parseJsonLoc(std::string_view s);

std::optional<JsonTmpl> parseJsonTmplFromBracketGroup(
    DiagsDest ctx, lex::BracketGroup bg);

// Same as above, but it is more forgiving about how things are quoted. This
// is useful in tests. Later, we may extend it to support actual json, e.g.
// those produced by JsonLoc::prettyPrintJson().
std::optional<JsonTmpl> parseJsonTmplFlexQuote(InputDiags& ctx, size_t& i);

}  // namespace oalex
