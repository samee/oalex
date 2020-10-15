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

// TODO: Move these into an oalex/ directory once we implement `make install`.
#include "diags.h"
#include "input_view.h"
#include "jsonloc.h"
#include "regex.h"
#include "skipper.h"

namespace oalex {

inline static JsonLoc quote(std::string input, size_t stPos, size_t enPos) {
  JsonLoc rv = std::move(input);
  rv.stPos = stPos; rv.enPos = enPos;
  return rv;
}

// We might move these into a separate file if we no longer depend on most
// of the runtime headers.
inline JsonLoc match(InputDiags& ctx, ssize_t& i, const std::string& s) {
  if(!ctx.input.hasPrefix(i, s)) return JsonLoc::ErrorValue{};
  return quote(s, std::exchange(i, i+s.size()), s.size());
}

inline JsonLoc match(InputDiags& ctx, ssize_t& i,
              const Regex& regex, const RegexOptions& ropts) {
  size_t oldi = i;
  if(consumeGreedily(ctx.input, sign_cast<size_t&>(i), regex, ropts))
    return quote(ctx.input.substr(oldi, i-oldi), oldi, i);
  else return JsonLoc::ErrorValue{};
}

}
