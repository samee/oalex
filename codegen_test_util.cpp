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

#include "codegen_test_util.h"
#include <string_view>
#include "fmt/format.h"
#include "runtime/test_util.h"
using fmt::format;
using std::string;
using std::string_view;

namespace oalex::test {

void assertJsonLocIsString(string_view testName, const JsonLoc& jsloc,
                           string_view s, size_t stPos, size_t enPos) {
  if(jsloc.holdsError()) Bug("{}: eval() produced error", testName);
  assertEqual(format("{}: eval().stPos", testName), jsloc.stPos, stPos);
  assertEqual(format("{}: eval().enPos", testName), jsloc.enPos, enPos);
  if(const string* t = get_if<string>(&jsloc))
    assertEqual(format("{}: output value", testName), string_view(*t), s);
  else Bug("{}: eval produced a non-string. Index: {}", testName,
           jsloc.value.index());
}

}  // namespace oalex::test
