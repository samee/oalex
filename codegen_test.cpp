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
#include "regex_io.h"
#include "fmt/format.h"
#include "runtime/diags_test_util.h"
#include "runtime/skipper.h"
#include <iterator>
#include <string_view>
using fmt::format;
using std::back_inserter;
using std::size;
using std::string;
using std::string_view;
using oalex::assertEqual;
using oalex::Bug;
using oalex::get_if;
using oalex::JsonLoc;
using oalex::Rule;
using oalex::RuleSet;
using oalex::Skipper;
using oalex::SkipPoint;
using oalex::regex::Regex;
using oalex::regex::RegexOptions;

namespace {

const Skipper cskip{ {{"/*","*/"},{"//","\n"}}, {}};
const RegexOptions regexOpts{
  .word = oalex::parseRegexCharSet("[0-9A-Za-z_]")
};

RuleSet singletonRuleSet(Rule r) {
  RuleSet rs{{}, cskip, regexOpts};
  rs.rules.push_back(std::move(r));
  return rs;
}

void assertJsonLocIsString(string_view testName, const JsonLoc& jsloc,
                           string_view s, size_t stPos, size_t enPos) {
  if(jsloc.empty()) Bug("{}: eval() was empty", testName);
  assertEqual(format("{}: eval().stPos", testName), jsloc.stPos, stPos);
  assertEqual(format("{}: eval().enPos", testName), jsloc.enPos, enPos);
  if(const string* t = get_if<string>(&jsloc))
    assertEqual(format("{}: output value", testName), string_view(*t), s);
  else Bug("{}: eval produced a non-string. Index: {}", testName,
           jsloc.value.index());
}

void testSingleStringMatch() {
  const string msg = "hello-world";
  auto ctx = testInputDiags(msg);
  ssize_t pos = 0;
  RuleSet rs = singletonRuleSet(msg);
  JsonLoc jsloc = eval(ctx, pos, rs, 0);
  assertJsonLocIsString(__func__, jsloc, msg, 0, msg.size());
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
  assertHasDiagWithSubstrAt(__func__, ctx.diags, "Unfinished comment", 2);
}

void testRegexMatch() {
  auto regex_input = testInputDiags("/[a-z]+/");
  size_t pos = 0;
  RuleSet rs = singletonRuleSet(*oalex::parseRegex(regex_input, pos));
  ssize_t spos = 0;
  auto ctx = testInputDiags("hello world");
  JsonLoc jsloc = eval(ctx, spos, rs, 0);
  assertJsonLocIsString(__func__, jsloc, "hello", 0, sizeof("hello")-1);

  spos = 0;
  auto ctx2 = testInputDiags("123");
  jsloc = eval(ctx2, spos, rs, 0);
  if(!jsloc.empty()) BugMe("Was expecting regex match to fail");
}

}  // namespace

int main() {
  testSingleStringMatch();
  testSingleStringMismatch();
  testSingleSkip();
  testSkipFailsOnUnfinishedComment();
  testRegexMatch();
}

