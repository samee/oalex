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
using oalex::SkipPoint;

namespace {

const Skipper cskip{ {{"/*","*/"},{"//","\n"}}, {}};

RuleSet singletonRuleSet(Rule r) {
  RuleSet rs{{}, cskip};
  rs.rules.push_back(std::move(r));
  return rs;
}

void testSingleStringMatch() {
  const string msg = "hello-world";
  auto ctx = testInputDiags(msg);
  ssize_t pos = 0;
  RuleSet rs = singletonRuleSet(msg);
  JsonLoc jsloc = eval(ctx, pos, rs, 0);
  if(jsloc.empty()) BugMe("eval() was empty");
  assertEqual(me("eval().stPos"), jsloc.stPos, size_t(0));
  assertEqual(me("eval().enPos"), jsloc.enPos, msg.size());
  if(string* s = get_if<string>(&jsloc)) {
    assertEqual(me("eval() output value"), *s, msg);
  }else BugMe("eval() produced a non-string. Index: {}", jsloc.value.index());
}

void testSingleStringMismatch() {
  const string msg = "hello-world";
  auto ctx = testInputDiags(msg);
  ssize_t pos = 0;
  RuleSet rs = singletonRuleSet(msg + "!");
  JsonLoc jsloc = eval(ctx, pos, rs, 0);
  if(!jsloc.empty()) BugMe("Was expecting string match to fail");
}

void testSingleSkip() {
  const string msg = "  /* hello */ world";
  auto ctx = testInputDiags(msg);
  ssize_t pos = 0;
  RuleSet rs = singletonRuleSet(SkipPoint{false, &cskip});
  eval(ctx, pos, rs, 0);
  assertEmptyDiags(__func__, ctx.diags);
  assertEqual(me("eval() endpoint"), size_t(pos), msg.find("world"));
}

void testSkipFailsOnUnfinishedComment() {
  const string msg = "  /* hello world";
  auto ctx = testInputDiags(msg);
  ssize_t pos = 0;
  RuleSet rs = singletonRuleSet(SkipPoint{false, &cskip});
  eval(ctx, pos, rs, 0);
  assertHasDiagWithSubstr(__func__, ctx.diags, "Unfinished comment");
}

}  // namespace

int main() {
  testSingleStringMatch();
  testSingleStringMismatch();
  testSingleSkip();
  testSkipFailsOnUnfinishedComment();
}

