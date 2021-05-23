/*  Copyright 2020 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

/* This file tests functions generated by codegen_gen_test.cpp.
   Every function here of the form run*Test() has a corresponding
   function in codegen_gen_test.cpp named generate*Test().
*/

#include "codegen_generated.h"
#include "codegen_test_util.h"

#include <string>
#include <utility>
#include "lexer.h"
#include "jsonloc_io_test.h"
#include "runtime/test_util.h"
#include "runtime/util.h"
using oalex::assertEqual;
using oalex::Bug;
using oalex::Input;
using oalex::InputDiags;
using oalex::JsonLoc;
using oalex::parseJsonLoc;
using oalex::Skipper;
using oalex::sign_cast;
using oalex::testInputDiags;
using oalex::lex::lexIndentedSource;
using oalex::test::assertJsonLocIsString;
using std::optional;
using std::pair;
using std::string;
using std::tuple;

namespace {

void runSingleStringTest() {
  string msg = "hello";
  auto ctx = testInputDiags(msg);
  ssize_t pos = 0;
  JsonLoc jsloc = parseHelloKeyword(ctx, pos);

  assertJsonLocIsString(__func__, jsloc, msg, 0, msg.size());

  msg = "goodbye";
  ctx = testInputDiags(msg);
  pos = 0;
  jsloc = parseHelloKeyword(ctx, pos);
  if(!jsloc.holdsError()) BugMe("Was expecting string match to fail");
}

void runMatchOrErrorTest(JsonLoc (*parse)(InputDiags&, ssize_t&)) {
  // First, try a success case.
  const string msg = "hello-world";
  auto ctx = testInputDiags(msg);
  ssize_t pos = 0;
  JsonLoc jsloc = parse(ctx, pos);
  assertJsonLocIsString(__func__, jsloc, msg, 0, msg.size());

  // Then, a failure case.
  const string msg2 = "goodbye";
  ctx = testInputDiags(msg2);
  pos = 0;
  parse(ctx, pos);
  assertHasDiagWithSubstrAt(__func__, ctx.diags, "Was expecting a greeting", 0);
}

void runSingleRegexTest() {
  const tuple<string, JsonLoc(*)(InputDiags&, ssize_t&), size_t> inputs[] = {
    {"fox", parseFooOrFox, 3},
    {"foood", parseLongFood, 5},
    {"abcde", parseAbcXyz, 3},
    {"xyyyz", parseAbcXyz, 5},
    {"abc", parseAbcWholeLine, 3},
    {"abc+", parseAbcWord, 3},
  };
  for(auto& [msg, parseMsg, len] : inputs) {
    auto ctx = testInputDiags(msg);
    ssize_t pos = 0;
    JsonLoc jsloc = parseMsg(ctx, pos);
    assertJsonLocIsString(__func__, jsloc, string(msg, 0, len), 0, len);
  }


  const pair<string, JsonLoc(*)(InputDiags&, ssize_t&)> bad_inputs[] = {
    {"fort", parseFooOrFox},
    {"fod", parseFooOrFox},
    {"abxyz", parseAbcXyz},
    {"abcd", parseAbcWholeLine},
    {"abc+", parseAbcWholeLine},
  };
  for(auto& [msg, parseMsg] : bad_inputs) {
    auto ctx = testInputDiags(msg);
    ssize_t pos = 0;
    JsonLoc jsloc = parseFooOrFox(ctx, pos);
    if(!jsloc.holdsError()) BugMe("Was expecting regex match to fail");
  }
}

void runConcatFlatTest() {
  ssize_t pos = 0;
  auto ctx = testInputDiags("var x:int = 5; ignored_bits;");
  JsonLoc observed = parseFlatDefn(ctx, pos);
  JsonLoc expected = *parseJsonLoc("{var_name: 'x', type: 'int', rhs: '5'}");
  assertEqual(__func__, observed, expected);

  pos = 0;
  expected = *parseJsonLoc("{var_name: 'x', "
                            "init_value: {type: 'int', value: '5'}}");
  ctx.diags.clear();
  observed = parseFlatThenAssembled(ctx, pos);
  assertEqual(__func__, observed, expected);

  pos = 0;
  ctx = testInputDiags("var y = 9;");
  observed = parseFlatDefn(ctx, pos);
  if(!observed.holdsError())
    BugMe("Was expecting failure on missing type. Got {}", observed);
}

void runSingleWordTemplate() {
  ssize_t pos = 0;
  auto ctx = testInputDiags("word and ignored");
  JsonLoc observed = parseWordTmpl(ctx, pos);
  JsonLoc expected = JsonLoc::Map{{"keyword", JsonLoc{"word"}}};
  assertEqual(__func__, observed, expected);
}

// Dev-note: do not delete when replacing ConcatRule with ConcatFlatRule.
// Rewrite to make the test instead to make it use ConcatFlatRule.
void runConcatTest() {
  auto ctx = testInputDiags("int x = 5;  // A declaration");
  ssize_t pos = 0;
  JsonLoc jsloc = parseDefinition(ctx, pos);
  string expected = R"({
    id: "x",
    value: "5"
  })";
  if(jsloc.holdsError()) BugMe("parseDefinition() failed");
  assertEqual(__func__, expected, jsloc.prettyPrint(2));

  ctx = testInputDiags("intx = 5;");
  pos = 0;
  jsloc = parseAssignment(ctx, pos);
  if(!jsloc.holdsError()) BugMe("run-on word matched unexpectedly");

  ctx = testInputDiags("y=x;");
  pos = 0;
  jsloc = parseAssignment(ctx, pos);
  expected = R"({
    lhs: "y",
    rhs: "x"
  })";
  if(jsloc.holdsError()) BugMe("parseAssignment() failed");
  assertEqual(__func__, expected, jsloc.prettyPrint(2));
}

}  // namespace

// This one needs to be extern for linking to generated code.
JsonLoc parseIndentedTmpl(InputDiags& ctx, ssize_t& i) {
  static const Skipper *shskip = new Skipper{ {{"#", "\n"}}, {} };
  const Input& input = ctx.input;
  ssize_t j = shskip->whitespace(ctx.input, i);
  size_t bol = input.bol(j);
  if(bol == input.bol(i)) return JsonLoc::ErrorValue{};
  string indent = input.substr(bol, j-bol);
  j = bol;
  optional<string> res = lexIndentedSource(ctx, sign_cast<size_t&>(j), indent);
  if(res.has_value()) return JsonLoc::String{std::move(*res)};
  else return JsonLoc::ErrorValue{};
}

namespace {

void runExternParserDeclaration() {
  const char msg[] =
  R"(let some_text:    # Comment ignored

        loren
        ipsum

  # Tail comment
  )";
  auto ctx = testInputDiags(msg);
  ssize_t pos = 0;
  JsonLoc jsloc = parseExtTmpl(ctx, pos);
  string expected = R"({
    id: "some_text",
    tmpl: "loren\nipsum\n\n"
  })";
  if(jsloc.holdsError()) BugMe("parseExtTmpl() failed");
  assertEqual(__func__, expected, jsloc.prettyPrint(2));
}

void runOrTest() {
  const pair<string, JsonLoc> goodInputOutputPairs[] = {
    {"if", JsonLoc{"if"}}, {"while", JsonLoc{"while"}},
    {"42", *parseJsonLoc(R"({number: '42'})")},
  };
  for(auto& [msg, expected] : goodInputOutputPairs) {
    ssize_t pos = 0;
    auto ctx = testInputDiags(msg);
    JsonLoc observed = parseOneWordOrList(ctx, pos);
    assertEqual(__func__, expected, observed);
  }

  ssize_t pos = 0;
  auto ctx = testInputDiags("do");
  JsonLoc observed = parseOneWordOrList(ctx, pos);
  if(!observed.holdsError())
    BugMe("Was expecting failure on keyword 'do'. Got {}", observed);
}

void runFlattenOnDemand() {
  const pair<string, JsonLoc> unflatData[] = {
    {"let", *parseJsonLoc("{next_token: {keyword: 'let'}}")},
    {"42", *parseJsonLoc("{next_token: {number: '42'}}")},
  };
  for(auto& [msg, expected] : unflatData) {
    ssize_t pos = 0;
    auto ctx = testInputDiags(msg);
    JsonLoc observed = parseUnflattenSingleConcat(ctx, pos);
    assertEqual(__func__, expected, observed);
  }
  const pair<string, JsonLoc> flatData[] = {
    {"let", *parseJsonLoc("{keyword: 'let'}")},
    {"42", *parseJsonLoc("{number: '42'}")},
  };
  for(auto& [msg, expected] : flatData) {
    ssize_t pos = 0;
    auto ctx = testInputDiags(msg);
    JsonLoc observed = parseFlattenSingleConcat(ctx, pos);
    assertEqual(__func__, expected, observed);
  }
}

void runLookaheads() {
  // Pad inputs with an extra space since peekMatch() uses a proxy object
  // that often gets offset-computation wrong that is only exposed if input
  // doesn't start at offset 0.
  const pair<string, JsonLoc> testdata[] = {
    {" var x = y; ignore", *parseJsonLoc("{var: 'x', init_value: 'y'}")},
    {" x = y; ignore", *parseJsonLoc("{lhs: 'x', rhs: 'y'}")},
    {" var = x; ignore", JsonLoc{JsonLoc::ErrorValue{}}},
    {" 4:", *parseJsonLoc("{line_number: '4'}")},
    {" .blastoff", *parseJsonLoc("{directive: 'blastoff'}")},
  };
  for(auto& [msg, expected] : testdata) {
    ssize_t pos = 1;
    auto ctx = testInputDiags(msg);
    JsonLoc observed = parseLookaheadSimpleStmt(ctx, pos);
    if(expected.holdsError()) {
      if(!observed.holdsError())
        BugMe("Expected error on '{}', got {}", msg, observed);
    }else assertEqual(__func__, expected, observed);
  }
}

void runQuietTest() {
  auto ctx = testInputDiags("string2");
  ssize_t pos = 0;
  JsonLoc observed = parseQuietMatchTest(ctx, pos);
  if(observed.holdsError() || !ctx.diags.empty()) {
    if(!ctx.diags.empty()) showDiags(ctx.diags);
    BugMe("Expected to succeed without diags");
  }
}

void runLoopRuleTest() {
  tuple<string, JsonLoc, ssize_t> goodcases[] = {
    {"a + b", *parseJsonLoc("{operand: ['a', 'b'] }"), -1},
    {"a", *parseJsonLoc("{operand: ['a'] }"), -1},
    {"a + b + c", *parseJsonLoc("{operand: ['a', 'b', 'c'] }"), -1},
    {"a + b c", *parseJsonLoc("{operand: ['a', 'b'] }"), 5},
  };
  for(auto& [msg, expectedJsloc, expectedEnd] : goodcases) {
    auto ctx = testInputDiags(msg);
    ssize_t pos = 0;
    JsonLoc observed = parseLoopSum(ctx, pos);
    assertEqual(__func__, expectedJsloc, observed);
    if(expectedEnd == -1) expectedEnd = msg.size();
    assertEqual(__func__, expectedEnd, pos);
  }
  pair<string, string> badcases[] = {
    {"(boo!)", "Expected an identifier"},
    {"a ++", "Expected an identifier"},
  };
  for(auto& [msg, expectedDiag] : badcases) {
    auto ctx = testInputDiags(msg);
    ssize_t pos = 0;
    JsonLoc observed = parseLoopSum(ctx, pos);
    if(!observed.holdsError())
      Bug("Was expecting an error on input '{}'. Got {}", msg, observed);
    assertHasDiagWithSubstr(__func__, ctx.diags, expectedDiag);
  }

  // Parsers for breakBefore == n, since that's special-cased.
  auto ctx = testInputDiags("a, b,");
  ssize_t pos = 0;
  JsonLoc observed = parseListPrefix(ctx, pos);
  if(!ctx.diags.empty()) showDiags(ctx.diags);
  assertEqual(__func__, pos, ssize_t(5));
  assertEqual(__func__, *parseJsonLoc("{elements: ['a', 'b']}"), observed);

  // Flattenable child.
  ctx = testInputDiags("!");
  pos = 0;
  observed = parseListPrefix(ctx, pos);
  if(!observed.holdsError())
    Bug("Was expecting an error on mandatory repeats. Got {}", observed);
  assertHasDiagWithSubstr(__func__, ctx.diags, "Expected an identifier");

  ctx = testInputDiags("+a, -b,");
  pos = 0;
  observed = parseSignedListPrefix(ctx, pos);
  if(!ctx.diags.empty()) showDiags(ctx.diags);
  assertEqual(__func__, pos, ssize_t(7));
  assertEqual(__func__, *parseJsonLoc("{elements: ['a', 'b'],"
                                      " sign: ['+', '-']}"), observed);
}

}  // namespace

int main() {
  if(!goodFunc()) Bug("goodFunc() returned false");
  if(badFunc()) Bug("badFunc() returned true");

  auto ctx = testInputDiags("x = 5;");
  size_t pos = 0;
  JsonLoc res = parseAsgnStmt(ctx, pos);
  if(!res.holdsError())
    Bug("parseAsgn() returning success, but is unimplemented!");

  runSingleStringTest();
  runMatchOrErrorTest(&parseHelloWorldOrError);
  runMatchOrErrorTest(&parseErrorRuleHelloWorld);
  runSingleRegexTest();
  runConcatFlatTest();
  runSingleWordTemplate();
  runConcatTest();
  runExternParserDeclaration();
  runOrTest();
  runFlattenOnDemand();
  runLookaheads();
  runQuietTest();
  runLoopRuleTest();
}
