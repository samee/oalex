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

#include "jsonloc_io.h"
#include "jsonloc_io_test.h"

#include <cctype>
#include <optional>
#include <map>
#include <string>
#include <vector>

#include "runtime/diags_test_util.h"
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
using oalex::parseJsonLoc;
using oalex::Resetter;
using oalex::uniqueKeys;

namespace {

void testSimpleSuccess() {
  const char input[] = R"( {
    # We support comments, another divergence from json.org.
    # Includes a trailing comma.
    input: "hello world", output: ["hello", "world",], metadata: metadata,
    underscore_identifier: "done" } )";
  optional<JsonLoc> json = parseJsonLoc(input);
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
    list: ["item 1", input, "item 2"],   # Duplicate keyword nestled somewhere.
    input2,  # Lone keyword.
  })";
  optional<JsonLoc> json = parseJsonLoc(input);
  JsonLoc::PlaceholderMap blanks = json->allPlaceholders();
  const vector<string> expected_blanks{"input","input2"};
  if(uniqueKeys(blanks) != expected_blanks)
    BugMe("Unexpected blank set: [{}] != [{}]", uniqueKeys(blanks),
          expected_blanks);

  // Try one substitution.
  optional<JsonLoc> part1 = parseJsonLoc(R"({key: "value"})");
  json->substitute(blanks, "input", *part1);
  if(json->substitutionsOk())
    BugMe("Unexpectedly okay with substitution after only 1 of 2 subs");

  // Try second substitution.
  json->substitute(blanks, "input2", JsonLoc("hello"));
  if(!json->substitutionsOk())
    BugMe("Even after all substitutions, still not okay");

  string output = json->prettyPrint(2);
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

void testJsonLocFailure(const char input[], const char errmsg[]) {
  InputDiags ctx = testInputDiags(input);
  size_t i = 0;
  optional<JsonLoc> res = parseJsonLoc(ctx, i);
  if(res.has_value() && ctx.diags.empty())
    BugMe("Was expecting failure with input '{}'. Got this instead: {}",
          input, res->prettyPrint());
  assertHasDiagWithSubstr(__func__, ctx.diags, errmsg);
}

void testJsonLocPosition(const char input[], size_t endi) {
  InputDiags ctx = testInputDiags(input);
  size_t i = 0;
  optional<JsonLoc> res = parseJsonLoc(ctx, i);
  if(!ctx.diags.empty()) {
    for(const auto& d : ctx.diags) BugWarn("{}", string(d));
    Bug("Got unexpected diags.");
  }
  if(i != endi) Bug("For input '{}', expected end position was {} != {}",
                    input, endi, i);
}

}  // namespace

// Note: none of these check JsonLoc::stPos or enPos of parse results, since we
// don't quite know how they will actually be used in practice, or what methods
// are needed to support their use pattern. Avoiding overengineering for now.
int main() {
  testSimpleSuccess();
  testSubstitution();
  testJsonLocPosition("(a,b)", 0);
  testJsonLocPosition("foo", 0);
  testJsonLocPosition("[a, b] foo", "[a, b]"s.size());
  testJsonLocFailure("[a,,b]", "Unexpected comma");
  testJsonLocFailure("[a,b,,]", "Unexpected comma");
  testJsonLocFailure("[(a,b)]", "Unexpected parenthesis");
  testJsonLocFailure("{[]:[]}", "Was expecting a key");
  testJsonLocFailure("{a:}", "Value missing after the colon");
  testJsonLocFailure("{a:b:c}", "Was expecting a comma here");
  testJsonLocFailure("{a:b,a:c}", "Duplicate key a");
  testJsonLocFailure("[a b]", "Was expecting a comma");
  testJsonLocFailure("[123]", "'123' is not a valid identifier");
}
