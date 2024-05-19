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
#include "parser_helpers.cpp"
#include "skipper.h"
#include "test_util.h"
using oalex::assertEqual;
using oalex::Input;
using oalex::InputDiags;
using oalex::JsonLoc;
using oalex::Skipper;
using std::string;

namespace {

void testQuietSkipAtNewline() {
  const string msg = R"(xyz
  abc)";
  InputDiags ctx{Input{msg}};
  ssize_t pos = 3;

  auto skipEval = +[](InputDiags& ctx, ssize_t& i) {
    const Skipper skip{ {{"#","\n"}}, {}, Skipper::Newlines::ignore_blank};
    i = skip.next(ctx.input(), i);
    return JsonLoc{JsonLoc::Map{}};
  };
  skipEval(ctx, pos);
  assertEqual(me("Skip didn't stop at newline"), pos, 3);

  quietMatch(ctx.input(), pos, skipEval);
  assertEqual(me("Quiet skip didn't stop at newline"), pos, 3);
}

}

int main() {
  testQuietSkipAtNewline();
}
