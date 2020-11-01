/*  Copyright 2019-2020 Google LLC

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

#include "jsonloc_io.h"
#include "runtime/diags_test_util.h"

namespace oalex {

// Testing convenience.
inline std::optional<JsonLoc> parseJsonLoc(std::string_view s) {
  size_t i = 0;
  InputDiags ctx = testInputDiags(s);
  return parseJsonLoc(ctx,i);
}

}  // namespace oalex