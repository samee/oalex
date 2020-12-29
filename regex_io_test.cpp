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

#include "regex_io.h"
#include <string_view>
#include <tuple>
#include <vector>
#include "runtime/diags_test_util.h"
#include "runtime/test_util.h"
using oalex::assertEqual;
using oalex::Bug;
using oalex::Diag;
using oalex::Input;
using oalex::InputDiags;
using oalex::makeVector;
using oalex::move_to_unique;
using oalex::parseRegexCharSet;
using oalex::parseRegex;
using oalex::prettyPrint;
using oalex::CharRange;
using oalex::Regex;
using oalex::RegexAnchor;
using oalex::RegexCharSet;
using oalex::RegexConcat;
using oalex::RegexOptional;
using oalex::RegexOptions;
using oalex::RegexOrList;
using oalex::RegexRepeat;
using std::make_unique;
using std::optional;
using std::pair;
using std::string;
using std::string_view;
using std::tuple;
using std::unique_ptr;
using std::vector;

namespace {

auto charSet(vector<CharRange> ranges) -> unique_ptr<RegexCharSet> {
  return make_unique<RegexCharSet>(RegexCharSet{std::move(ranges), false});
}

auto negatedSet(vector<CharRange> ranges) -> unique_ptr<RegexCharSet> {
  return make_unique<RegexCharSet>(RegexCharSet{std::move(ranges), true});
}

auto charSingle(unsigned char ch) -> unique_ptr<RegexCharSet> {
  return charSet({{ch,ch}});
}

auto charString(string s) { return make_unique<string>(std::move(s)); }

template <class ... Ts>
auto concat(Ts ... ts) -> unique_ptr<RegexConcat> {
  return move_to_unique(RegexConcat{makeVector<Regex>(std::move(ts)...)});
}

template <class ... Ts>
auto orlist(Ts ... ts) -> unique_ptr<RegexOrList> {
  return move_to_unique(RegexOrList{makeVector<Regex>(std::move(ts)...)});
}

Regex repeat(Regex part, char ch) {
  if(ch == '+') return move_to_unique(RegexRepeat{std::move(part)});
  if(ch == '*') return move_to_unique(RegexOptional{
                         move_to_unique(RegexRepeat{std::move(part)})
                       });
  if(ch == '?') return move_to_unique(RegexOptional{std::move(part)});
  Bug("Don't know how to construct repeats of type {}", ch);
}

void testPrettyPrint() {
  std::pair<Regex,string> testVectors[] = {
    {charSet({CharRange{'0','9'}}), "/[0-9]/"},
    {charSet({CharRange{'A','Z'}}), "/[A-Z]/"},
    {charSet({CharRange{'a','z'}}), "/[a-z]/"},
    {charSet({CharRange{'c','c'}}), "/[c]/"},
    {charSet({CharRange{'\n','\n'}}), "/[\\n]/"},
    {charSet({CharRange{'\\','\\'}}), "/[\\\\]/"},
    {charSet({CharRange{'/','/'}}), "/[\\/]/"},
    {charSet({CharRange{'0','8'},
              CharRange{']',']'},
              CharRange{'A','Z'}}),"/[0-8\\]A-Z]/"},
    {charSet({CharRange{'^','^'},
              CharRange{'!','!'},
              CharRange{':',':'}}), "/[\\^!:]/"},
    {charSet({CharRange{'!','!'},
              CharRange{'^','^'},
              CharRange{':',':'}}), "/[!^:]/"},
    {charSet({CharRange{']',']'},
              CharRange{'x','x'},
              CharRange{'-','-'}}), "/[]x-]/"},
    {charSet({CharRange{'x','x'},
              CharRange{'-','-'},
              CharRange{']',']'}}), "/[x\\-\\]]/"},
    {charSet({CharRange{'a','a'},
              CharRange{'-','-'},
              CharRange{'z','z'}}), "/[a\\-z]/"},
    {negatedSet({}), "/./"},
    {negatedSet({{CharRange{'a','z'},CharRange{'@','@'}}}),"/[^a-z@]/"},
    {negatedSet({{CharRange{'^','^'},CharRange{'a','z'}}}),"/[^^a-z]/"},
    {concat(charSingle('a'), charSingle('b'), charSingle('c')), "/[a][b][c]/"},
    {concat(charSingle('a'), concat(charSingle('b'), charSingle('c'))),
      "/[a]([b][c])/"},
    {concat(charString("hello"), charSingle('u')), "/hello[u]/"},
    {repeat(charString("hello"), '?'), "/(hello)?/"},
    {repeat(charString("hello"), '+'), "/(hello)+/"},
    {repeat(charString("hello"), '*'), "/(hello)*/"},
    {concat(charString("hell"), repeat(charString("o"), '?')), "/hello?/"},
    {repeat(charSet({CharRange{'0','9'}}), '+'), "/[0-9]+/"},
    {repeat(concat(charSingle('a'), charSingle('b'), charSingle('c')), '+'),
      "/([a][b][c])+/"},
    {orlist(), "//"},
    {orlist(charString("hello"), charString("world")), "/hello|world/"},
    {orlist(orlist(charString("hello"), charString("world")),
            charString("goodbye")), "/(hello|world)|goodbye/"},
    {charString("hello?"), "/hello\\?/"},
    {charString("hello\n"), "/hello\\n/"},
    {charString("{in}/[brackets]"), "/\\{in}\\/\\[brackets]/"},
    {charString("\\slashes/"), "/\\\\slashes\\//"},
    {concat(move_to_unique(RegexAnchor::wordEdge), charString("hello"),
            move_to_unique(RegexAnchor::wordEdge)), "/\\bhello\\b/"},
    {concat(move_to_unique(RegexAnchor::bol), charString("hello"),
            move_to_unique(RegexAnchor::eol)), "/^hello$/"},
  };
  const size_t n = sizeof(testVectors)/sizeof(testVectors[0]);
  for(size_t i=0; i<n; ++i) {
    string observed = prettyPrint(testVectors[i].first);
    if(observed != testVectors[i].second)
      BugMe("Regex prettyPrint failed: {} != {}", observed,
            testVectors[i].second);
  }
}

// If direct string comparison ever becomes too simple, try AST comparison
// after a parse-print-parse cycle.
void testParseAndPrint() {
  const vector<string> inputs {
    "/[abc]/", "/[a-z123]/", "/[^a-z@]/", "/[^^a-z]/",
    "/[-abc-]/", "/[]]/", "/[^]]/",
    "/[abc\\x03\\-\\t\\n\\]\\/]/",
    "/[\\xdb]/",
    "/[abc][def][ghi]/", "/[a]([b][c])/",
    "/abc/", "/abc[def]/",
    "/hello?/", "/hello*/", "/hello+/",
    "//", "/hello|world/", "/(hello|world)|goodbye/",
    "/(hello|world|)there/",
    "/a.*a/", "/hello\\nworld/", "/\\{in}\\/\\[brackets]/", "/\\\\slashes\\//",
    "/^hello$/", "/\\bhello\\b/",
    "/()+/",
  };
  for(auto& input : inputs) {
    InputDiags ctx{Input{input}};
    size_t i = 0;
    optional<Regex> parseResult = parseRegex(ctx, i);
    assertEmptyDiags(__func__, ctx.diags);
    if(!parseResult) BugMe("Regex {} silently failed to parse.", input);
    string output = prettyPrint(*parseResult);
    if(input != output)
      BugMe("Regex has changed after pretty-printing: {} became {}",
            input, output);
  }
}

void testParseDiags() {
  using namespace std::literals;
  const vector<pair<string,string>> testVectors{
    {"/[\b]/", "Invalid character"},
    {"/[A-z]/", "Ranges can only span"},
    {"/[x-x]/", "Redundant range"},
    {"/[z-a]/", "Invalid range going backwards"},
    {"/[a-b-c]/", "Character range has no start"},
    {"/[/", "Expected closing ']'"},
    {"/[\\", "Incomplete escape"},
    {"/[\\w]/", "Unknown escape"},
    {"/[\\xwq]/", "Invalid hex code"},
    {"/[\\\0x\\-]/"s, "Unknown escape"},
    {"/)", "Unmatched ')'"},
    {"/(/", "Unmatched '('"},
    {"/"+string(256,'('), "nested too deep"},
    {"/+/", "Nothing to repeat"},
    {"/hello", "Unterminated regex"},
    {"/hello|", "Unterminated regex"},
    {"/a++++++/", "Too many consecutive repeat operators"},
  };
  for(auto& [input, msg] : testVectors) {
    assertProducesDiag(__func__, input, msg, parseRegex);
  }
}

void testStripOuterParens() {
  const vector<string> testVectors{"foo", "[a-z]"};
  for(auto& part: testVectors) {
    string expected = "/"+part+"/";
    string input = "/("+part+")/";
    InputDiags ctx{Input{input}};
    size_t i = 0;
    optional<Regex> parseResult = parseRegex(ctx, i);
    assertEmptyDiags(__func__, ctx.diags);
    if(!parseResult) BugMe("Regex {} silently failed to parse.", input);
    string output = prettyPrint(*parseResult);
    if(expected != output)
      BugMe("Regex is not just sans-parenthesis after printing: "
            "{} became {}", input, output);
  }
}

void testRegexMatches() {
  RegexOptions opts{.word = parseRegexCharSet("[0-9A-Za-z_]")};
  const vector<tuple<string,string,size_t>> testVectors{
    {"//", "foo", 0},
    {"/fo[ox]/", "fox", 3},
    {"/foo?/", "fox", 2},
    {"/(ab?c)+|.[xyz]+/", "abcacacabcfoo", 10},
    {"/(ab?c)+|.[xyz]+/", "wx", 2},
    {"/^foo$/", "foo", 3},
    {"/foo|[a-z]+$/", "foo1", 3},
    {"/\\bfoo\\b/", "foo ", 3},
    {"/$/", "", 0},
    {"/^/", "foo", 0},
  };
  for(auto& [pattern, inputstr, matchLen] : testVectors) {
    InputDiags regex_input{Input{pattern}};
    size_t i = 0;
    Regex regex = *parseRegex(regex_input, i);
    Input input{inputstr};
    if(!startsWithRegex(input, 0, regex, opts))
      BugMe("\"{}\" was expected to startWithRegex() {}", inputstr, pattern);
    i = 0;
    if(!consumeGreedily(input, i, regex, opts))
      BugMe("consumeGreedily(\"{}\", {}) fails even though "
            "startsWithRegex() passes", inputstr, pattern);
    assertEqual("Regex comsumption length", i, matchLen);
  }
  const vector<pair<string,string>> failVectors{
    {"/(ab?c)+|.[xyz]+/", "!abcacacabcfoo"},
    {"/(ab?c)+|.[xyz]+/", "!wx"},
    {"/^foo$/", "foo "},
    {"/\\bfoo\\b/", "food"},
    {"/\\b/", ""},
  };
  for(auto& [pattern, inputstr] : failVectors) {
    InputDiags regex_input{Input{pattern}};
    size_t i = 0;
    Regex regex = *parseRegex(regex_input, i);
    Input input{inputstr};
    if(startsWithRegex(input, 0, regex, opts))
      BugMe("\"{}\" was not expected to startWithRegex() {}",
            inputstr, pattern);
    i = 0;
    if(consumeGreedily(input, i, regex, opts))
      BugMe("\"{}\" was not expected to be consumed by {}", inputstr, pattern);
  };
}

}  // namespace

int main() {
  testPrettyPrint();
  testParseAndPrint();
  testParseDiags();
  testStripOuterParens();
  testRegexMatches();
}
