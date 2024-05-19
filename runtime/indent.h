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

#pragma once
#include <string_view>

// This file is linked into the runtime, but not exposed in oalex.h

namespace oalex {

enum class IndentCmp { bad, lt, eq, gt };
IndentCmp indentCmp(std::string_view indent1, std::string_view indent2);

class InputPiece;
class StringLoc;
StringLoc indent_of(const InputPiece& input, size_t i);

}  // namespace oalex
