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

#include "lookahead_regex.h"
#include <iostream>
#include <string_view>
#include <vector>
#include "runtime/diags_test_util.h"
#include "runtime/test_util.h"
using oalex::BugDie;
using oalex::Diag;
using oalex::Input;
using oalex::InputDiags;
using oalex::UserErrorEx;
using oalex::regex::CharRange;
using oalex::regex::CharSet;
using oalex::regex::Concat;
using oalex::regex::makeUniqueRegex;
using oalex::regex::Optional;
using oalex::regex::OrList;
using oalex::regex::Regex;
using oalex::regex::Repeat;
using std::cerr;
using std::endl;
using std::make_unique;
using std::optional;
using std::pair;
using std::string;
using std::string_view;
using std::unique_ptr;
using std::vector;

namespace regex = oalex::regex;

namespace {

CharSet negatedSet(CharSet cs) {
  cs.negated=true;
  return cs;
}
CharSet charSingle(unsigned char ch) {
  return CharSet{{CharRange{ch,ch}}};
}

void vector_helper(vector<Regex>&) {}

template <class T, class ... Ts>
void vector_helper(vector<Regex>& out, T t, Ts ... ts) {
  out.push_back(std::move(t));
  vector_helper(out, std::move(ts)...);
}

template <class ... Ts>
auto concat(Ts ... ts) -> unique_ptr<Concat> {
  auto rv = make_unique<Concat>();
  vector_helper(rv->parts, std::move(ts)...);
  return rv;
}

template <class ... Ts>
auto orlist(Ts ... ts) -> unique_ptr<OrList> {
  auto rv = make_unique<OrList>();
  vector_helper(rv->parts, std::move(ts)...);
  return rv;
}

Regex repeat(Regex part, char ch) {
  if(ch == '+') return makeUniqueRegex<Repeat>({std::move(part)});
  if(ch == '*') return makeUniqueRegex<Optional>({
                         makeUniqueRegex<Repeat>({std::move(part)})
                       });
  if(ch == '?') return makeUniqueRegex<Optional>({std::move(part)});
  BugDie()<<"Don't know how to construct repeats of type "<<ch;
}

void testPrettyPrint() {
  std::pair<Regex,string> testVectors[] = {
    {CharSet{{CharRange{'0','9'}}}, "/[0-9]/"},
    {CharSet{{CharRange{'A','Z'}}}, "/[A-Z]/"},
    {CharSet{{CharRange{'a','z'}}}, "/[a-z]/"},
    {CharSet{{CharRange{'c','c'}}}, "/[c]/"},
    {CharSet{{CharRange{'\n','\n'}}}, "/[\\n]/"},
    {CharSet{{CharRange{'\\','\\'}}}, "/[\\\\]/"},
    {CharSet{{CharRange{'/','/'}}}, "/[\\/]/"},
    {CharSet{{CharRange{'0','8'},
              CharRange{']',']'},
              CharRange{'A','Z'}}},"/[0-8\\]A-Z]/"},
    {CharSet{{CharRange{'^','^'},
              CharRange{'!','!'},
              CharRange{':',':'}}}, "/[\\^!:]/"},
    {CharSet{{CharRange{'!','!'},
              CharRange{'^','^'},
              CharRange{':',':'}}}, "/[!^:]/"},
    {CharSet{{CharRange{']',']'},
              CharRange{'x','x'},
              CharRange{'-','-'}}}, "/[]x-]/"},
    {CharSet{{CharRange{'x','x'},
              CharRange{'-','-'},
              CharRange{']',']'}}}, "/[x\\-\\]]/"},
    {CharSet{{CharRange{'a','a'},
              CharRange{'-','-'},
              CharRange{'z','z'}}}, "/[a\\-z]/"},
    {negatedSet({}), "/./"},
    {negatedSet({{CharRange{'a','z'},CharRange{'@','@'}}}),"/[^a-z@]/"},
    {negatedSet({{CharRange{'^','^'},CharRange{'a','z'}}}),"/[^^a-z]/"},
    {concat(charSingle('a'), charSingle('b'), charSingle('c')), "/[a][b][c]/"},
    {concat(charSingle('a'), concat(charSingle('b'), charSingle('c'))),
      "/[a]([b][c])/"},
    {concat("hello", charSingle('u')), "/hello[u]/"},
    {repeat("hello", '?'), "/(hello)?/"},
    {repeat("hello", '+'), "/(hello)+/"},
    {repeat("hello", '*'), "/(hello)*/"},
    {concat("hell", repeat("o", '?')), "/hello?/"},
    {repeat(CharSet{{CharRange{'0','9'}}}, '+'), "/[0-9]+/"},
    {repeat(concat(charSingle('a'), charSingle('b'), charSingle('c')), '+'),
      "/([a][b][c])+/"},
    {orlist(), "//"},
    {orlist("hello", "world"), "/hello|world/"},
    {orlist(orlist("hello", "world"), "goodbye"), "/(hello|world)|goodbye/"},
  };
  const size_t n = sizeof(testVectors)/sizeof(testVectors[0]);
  for(size_t i=0; i<n; ++i) {
    string observed = prettyPrint(testVectors[i].first);
    if(observed != testVectors[i].second)
      BugMe<<"Regex prettyPrint failed: "<<observed
           <<" != "<<testVectors[i].second;
  }
}

void abortScreaming(string_view testName, const vector<Diag>& diags) {
  for(const auto& d:diags) cerr<<string(d)<<endl;
  BugDie()<<testName<<" had unexpected errors";
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
    "/a.*a/",
  };
  for(auto& input : inputs) {
    InputDiags ctx{Input{input}, {}};
    size_t i = 0;
    optional<Regex> parseResult = regex::parse(ctx, i);
    if(!ctx.diags.empty()) abortScreaming(__func__, ctx.diags);
    if(!parseResult) BugMe<<"Regex "<<input<<" silently failed to parse.";
    string output = regex::prettyPrint(*parseResult);
    if(input != output)
      BugMe<<"Regex has changed after pretty-printing: "
           <<input<<" became "<<output;
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
  };
  for(auto& [input, msg] : testVectors) {
    InputDiags ctx{Input{input}, {}};
    size_t i = 0;
    try {
      optional<Regex> parseResult = regex::parse(ctx, i);
    }catch(UserErrorEx& ex) {
      ctx.Error(0,0,ex.what());  // demote Fatal() error to non-fatal.
    }
    assertHasDiagWithSubstr(__func__, ctx.diags, msg);
  }
}

void testStripOuterParens() {
  const vector<string> testVectors{"foo", "[a-z]"};
  for(auto& part: testVectors) {
    string expected = "/"+part+"/";
    string input = "/("+part+")/";
    InputDiags ctx{Input{input}, {}};
    size_t i = 0;
    optional<Regex> parseResult = regex::parse(ctx, i);
    if(!ctx.diags.empty()) abortScreaming(__func__, ctx.diags);
    if(!parseResult) BugMe<<"Regex "<<input<<" silently failed to parse.";
    string output = regex::prettyPrint(*parseResult);
    if(expected != output)
      BugMe<<"Regex is not just sans-parenthesis after printing: "
           <<input<<" became "<<output;
  }
}

}  // namespace

int main() {
  testPrettyPrint();
  testParseAndPrint();
  testParseDiags();
  testStripOuterParens();
}
