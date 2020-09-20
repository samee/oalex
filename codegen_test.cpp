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

#include "codegen.h"
#include "runtime/diags_test_util.h"
#include "runtime/skipper.h"
#include <iterator>
using std::back_inserter;
using std::size;
using std::string;
using oalex::assertEqual;
using oalex::get_if;
using oalex::JsonLoc;
using oalex::Rule;
using oalex::RuleSet;
using oalex::Skipper;

namespace {

void testSingleStringMatch() {
  const Skipper cskip{ {{"/*","*/"},{"//","\n"}}, {}};
  const string msg = "hello-world";
  auto ctx = testInputDiags(msg);
  ssize_t pos = 0;
  Rule rules[] = {msg};
  RuleSet rs{{}, cskip};
  move(rules, rules+size(rules), back_inserter(rs.rules));
  JsonLoc jsloc = eval(ctx, pos, rs, 0);
  if(jsloc.empty()) BugMe("eval() was empty");
  assertEqual(me("eval().stPos"), jsloc.stPos, size_t(0));
  assertEqual(me("eval().enPos"), jsloc.enPos, msg.size());
  if(string* s = get_if<string>(&jsloc)) {
    assertEqual(me("eval() output value"), *s, msg);
  }else BugMe("eval() produced a non-string. Index: {}", jsloc.value.index());
}

}  // namespace

int main() {
  testSingleStringMatch();
}

