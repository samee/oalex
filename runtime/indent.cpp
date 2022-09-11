/*  Copyright 2022 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "runtime/indent.h"
#include "runtime/input_view.h"
#include "runtime/jsonloc.h"
#include "runtime/skipper.h"
using std::string_view;

namespace oalex {

IndentCmp indentCmp(string_view indent1, string_view indent2) {
  size_t i = 0;
  while(true) {
    if(i>=indent1.size() && i>=indent2.size()) return IndentCmp::eq;
    if(i>=indent1.size()) return IndentCmp::lt;
    if(i>=indent2.size()) return IndentCmp::gt;
    if(indent1[i]!=indent2[i]) return IndentCmp::bad;
    ++i;
  }
}

StringLoc
indent_of(const InputPiece& input, size_t i) {
  // TODO remove uses of oalexWSkip elsewhere.
  const static Skipper* wskip =
    new Skipper{{}, {}, Skipper::Newlines::keep_all};
  size_t bol = input.bol(i);
  size_t indent_end = wskip->next(input, bol);
  return {input.substr_range(bol, indent_end), bol};
}

}  // namespace oalex
