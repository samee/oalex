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
#include "jsonloc_io_test.h"
#include "regex_io.h"
#include "fmt/format.h"
#include "runtime/diags_test_util.h"
#include "runtime/skipper.h"
#include <iterator>
#include <string_view>
#include <vector>
using fmt::format;
using std::back_inserter;
using std::optional;
using std::pair;
using std::size;
using std::string;
using std::string_view;
using std::vector;
using oalex::assertEqual;
using oalex::Bug;
using oalex::ConcatRule;
using oalex::get_if;
using oalex::JsonLoc;
using oalex::makeVector;
using oalex::parseJsonLoc;
using oalex::Regex;
using oalex::RegexOptions;
using oalex::Rule;
using oalex::RuleSet;
using oalex::Skipper;
using oalex::SkipPoint;

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
  if(jsloc.holdsError()) Bug("{}: eval() produced error", testName);
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
  if(!jsloc.holdsError()) BugMe("Was expecting string match to fail");
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
  if(!jsloc.holdsError()) BugMe("Was expecting regex match to fail");
}

// TODO move to some test_util.h
Regex parseRegex(string_view s) {
  size_t i = 0;
  auto ctx = testInputDiags(s);
  optional<Regex> rv = parseRegex(ctx, i);
  if(!rv.has_value()) Bug("{} is not a valid regex", s);
  else return std::move(*rv);
}

void testConcatMatch() {
  RuleSet rs{
    .rules = makeVector<Rule>(parseRegex("/[a-zA-Z]+/"), "=",
                              parseRegex("/[0-9]+/"), ";"),
    .skip{cskip},
    .regexOpts{regexOpts},
  };
  rs.rules.push_back(SkipPoint{false, &rs.skip});
  rs.rules.push_back(ConcatRule{"asgn", {
      {0, "lhs"}, {4, ""}, {1, ""}, {4, ""}, {2, "rhs"}, {4, ""}, {3, ""}
    }, *parseJsonLoc(R"({ stmt: "asgn", lhs, rhs })")
  });
  ssize_t pos = 0;
  auto ctx = testInputDiags("orangeCount = 5; ignored_bits;");
  JsonLoc expected = *parseJsonLoc(R"({
    stmt: "asgn", lhs: "orangeCount", rhs: "5"
  })");
  JsonLoc observed = eval(ctx, pos, rs, rs.rules.size()-1);
  assertEqual(__func__, expected.prettyPrint(), observed.prettyPrint());
}

}  // namespace

int main() {
  testSingleStringMatch();
  testSingleStringMismatch();
  testSingleSkip();
  testSkipFailsOnUnfinishedComment();
  testRegexMatch();
  testConcatMatch();
}

