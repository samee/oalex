/*  Copyright 2019-2021 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "jsontmpl_parsers.h"

#include <cctype>
#include <optional>
#include <map>
#include <string>
#include <vector>

#include "jsontmpl.h"
#include "runtime/jsonloc.h"
#include "runtime/test_util.h"
#include "runtime/util.h"
using std::get_if;
using std::isalnum;
using std::isdigit;
using std::map;
using std::nullopt;
using std::optional;
using std::string;
using std::string_view;
using std::vector;
using namespace std::string_literals;
using oalex::assertEqual;
using oalex::Bug;
using oalex::BugWarn;
using oalex::Input;
using oalex::InputDiags;
using oalex::JsonLoc;
using oalex::JsonTmpl;
using oalex::parseJsonLoc;
using oalex::parseJsonTmpl;
using oalex::parseJsonTmplFlexQuote;
using oalex::Resetter;
using oalex::showDiags;

namespace {

JsonTmpl assertValidJsonTmpl(const char testName[],
                             const char input[], size_t& i) {
  InputDiags ctx{Input{input}};
  optional<JsonTmpl> res = parseJsonTmplFlexQuote(ctx, i);
  if(!res.has_value() || !ctx.diags.empty()) {
    showDiags(ctx.diags);
    Bug("{}: Got unexpected diags.", testName);
  }
  return *res;
}

JsonTmpl assertValidJsonTmpl(const char testName[], const char input[]) {
  size_t i = 0;
  return assertValidJsonTmpl(testName, input, i);
}

template <class K, class T> std::vector<K>
uniqueKeys(const std::vector<std::pair<K,T>>& m) {
  std::vector<K> v;
  for(const auto& [k,e] : m) if(v.empty()||v.back()!=k) v.push_back(k);
  return v;
}

void testSimpleSuccess() {
  const char input[] = R"( {
    # We support comments and single-quotes, among other divergences from
    # json.org.  Includes a trailing comma.
    input: 'hello world', output: ['hello', 'world',], metadata: metadata,
    underscore_identifier: 'done' } )";
  optional<JsonTmpl> json = parseJsonTmpl(input);
  string output = json->prettyPrint(2);
  const char expected[] = R"({
    input: "hello world",
    metadata: metadata,
    output: [
      "hello",
      "world"
    ],
    underscore_identifier: "done"
  })";
  if(output != expected)
    BugMe("Unexpected output:\n{}", output);
}

void testSubstitution() {
  const char input[] = R"({
    input,
    list: ['item 1', input, 'item 2'],   # Duplicate keyword nestled somewhere.
    input2,  # Lone keyword.
  })";
  optional<JsonTmpl> jstmpl = parseJsonTmpl(input);
  optional<JsonLoc> part1 = parseJsonLoc(R"({key: 'value'})");
  JsonLoc jsloc = jstmpl->substituteAll({{"input", *part1},
                                        {"input2", JsonLoc{"hello"}}});
  string output = jsloc.prettyPrint(2);
  const char expected[] = R"({
    input: {
      key: "value"
    },
    input2: "hello",
    list: [
      "item 1",
      {
        key: "value"
      },
      "item 2"
    ]
  })";
  if(output != expected)
    BugMe("Unexpected output:\n{}", output);
}

void testJsonTmplFailure(const char input[], const char errmsg[]) {
  InputDiags ctx{Input{input}};
  size_t i = 0;
  optional<JsonTmpl> res = parseJsonTmpl(ctx, i);
  if(res.has_value() && ctx.diags.empty())
    BugMe("Was expecting failure with input '{}'. Got this instead: {}",
          input, res->prettyPrint());
  assertHasDiagWithSubstr(__func__, ctx.diags, errmsg);
}

void testJsonTmplPosition(const char input[], size_t endi) {
  InputDiags ctx{Input{input}};
  size_t i = 0;
  optional<JsonTmpl> res = parseJsonTmpl(ctx, i);
  if(!ctx.diags.empty()) {
    for(const auto& d : ctx.diags) BugWarn("{}", string(d));
    Bug("Got unexpected diags.");
  }
  if(i != endi) Bug("For input '{}', expected end position was {} != {}",
                    input, endi, i);
}

void testSubstitutionsNeeded(const char input[], bool expectedRes) {
  JsonTmpl jstmpl = assertValidJsonTmpl(__func__, input);
  if(jstmpl.substitutionsNeeded() != expectedRes)
    Bug("{}: Was expecting substitutions to be {} on input \"{}\"",
        __func__, (expectedRes?"not needed":"needed"), input);
}

void testEquality(const char input1[], const char input2[], bool expectedRes) {
  JsonLoc jsloc1 = assertValidJsonTmpl(__func__, input1).outputIfFilled();
  JsonLoc jsloc2 = assertValidJsonTmpl(__func__, input2).outputIfFilled();
  if((jsloc1 == jsloc2) != expectedRes)
    Bug("{}: Was expecting equality check to {} on inputs:\n"
        "  {}\n  {}", __func__, (expectedRes?"succeed":"fail"),
        input1, input2);
}

void testLocations() {
  JsonTmpl tmpl = assertValidJsonTmpl(__func__,
      "{hello: 'world', goodbye: ['till', 'next', 'time']}");
  assertEqual("testLocations.stPos", tmpl.stPos, size_t(0));
  assertEqual("testLocations.enPos", tmpl.enPos, size_t(51));
  JsonTmpl::Map* m = tmpl.getIfMap();
  ssize_t hi = JsonTmpl::mapLinearFind(*m, "hello");
  ssize_t gi = JsonTmpl::mapLinearFind(*m, "goodbye");
  assertEqual("testLocations.hello.stPos", (*m)[hi].second.stPos, size_t(8));
  assertEqual("testLocations.hello.enPos", (*m)[hi].second.enPos, size_t(15));
  assertEqual("testLocations.goodbye.stPos", (*m)[gi].second.stPos, size_t(26));
  assertEqual("testLocations.goodbye.enPos", (*m)[gi].second.enPos, size_t(50));
}

}  // namespace

// Note: none of these check JsonTmpl::stPos or enPos of parse results, since we
// don't quite know how they will actually be used in practice, or what methods
// are needed to support their use pattern. Avoiding overengineering for now.
int main() {
  testSimpleSuccess();
  testSubstitution();
  testJsonTmplPosition("(a,b)", 0);
  testJsonTmplPosition("foo", 0);
  testJsonTmplPosition("[a, b] foo", "[a, b]"s.size());
  testJsonTmplFailure("[a,,b]", "Unexpected comma");
  testJsonTmplFailure("[a,b,,]", "Unexpected comma");
  testJsonTmplFailure("[(a,b)]", "Unexpected parenthesis");
  testJsonTmplFailure("{[]:[]}", "Was expecting the key to be an identifier");
  testJsonTmplFailure("{a:}", "Value missing after the colon");
  testJsonTmplFailure("{a:b:c}", "Was expecting a comma here");
  testJsonTmplFailure("{a:b,a:c}", "Duplicate key a");
  testJsonTmplFailure("[a b]", "Was expecting a comma");
  testJsonTmplFailure("[123]", "'123' is not a valid identifier");
  testSubstitutionsNeeded("['hello', world]", true);
  testSubstitutionsNeeded("['hello', 'world']", false);
  testSubstitutionsNeeded("{ msg: 'hello' }", false);
  testEquality("{hello: 'world', goodbye: ['till', 'next', 'time']}",
               "{goodbye: ['till', 'next', 'time'], hello: 'world'}",
               true);
  testEquality("{hello: 'world', goodbye: ['till', 'next', 'time']}",
               "{hello: 'world', goodbyee: ['till', 'next', 'time']}",
               false);
  testLocations();
}
