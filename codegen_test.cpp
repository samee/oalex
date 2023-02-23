/*  Copyright 2020-2021 The oalex authors.

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
#include "codegen_test_util.h"

#include "jsontmpl_parsers.h"
#include "regex_io.h"
#include "fmt/core.h"
#include "runtime/jsonloc_fmt.h"
#include "runtime/test_util.h"
#include <string_view>
#include <tuple>
#include <vector>
using fmt::format;
using std::optional;
using std::pair;
using std::size;
using std::string;
using std::string_view;
using std::tuple;
using std::unique_ptr;
using std::vector;
using namespace std::string_literals;
using oalex::AliasRule;
using oalex::assertEqual;
using oalex::Bug;
using oalex::ConcatFlatRule;
using oalex::ErrorRule;
using oalex::ExternParser;
using oalex::Input;
using oalex::InputDiags;
using oalex::JsonLoc;
using oalex::JsonTmpl;
using oalex::LoopRule;
using oalex::makeVectorUnique;
using oalex::MatchOrError;
using oalex::move_to_unique;
using oalex::OrRule;
using oalex::OutputTmpl;
using oalex::parseJsonLoc;
using oalex::parseJsonTmpl;
using oalex::parseRegexCharSet;
using oalex::passthroughTmpl;
using oalex::populateFlatFields;
using oalex::RegexOptions;
using oalex::QuietMatch;
using oalex::Regex;
using oalex::Rule;
using oalex::RuleField;
using oalex::RuleSet;
using oalex::StringRule;
using oalex::Skipper;
using oalex::SkipPoint;
using oalex::WordPreserving;
using oalex::test::assertJsonLocIsString;
using oalex::test::assertLocPairEqual;
using oalex::test::cskip;
using oalex::test::nmRule;
using oalex::test::regexOpts;
using oalex::test::parseRegexRule;
using oalex::test::singletonRuleSet;

namespace {

void testSingleStringMatch() {
  const string msg = " hello-world";
  InputDiags ctx{Input{msg}};
  ssize_t pos = 1;
  RuleSet rs = singletonRuleSet(StringRule{msg.substr(1)});
  JsonLoc jsloc = eval(ctx, pos, rs, 0);
  assertJsonLocIsString(__func__, jsloc, msg.substr(1), 1, msg.size());
}

void testSingleStringMismatch() {
  const string msg = "hello-world";
  InputDiags ctx{Input{msg}};
  ssize_t pos = 0;
  RuleSet rs = singletonRuleSet(StringRule{msg + "!"});
  JsonLoc jsloc = eval(ctx, pos, rs, 0);
  if(!jsloc.holdsErrorValue()) BugMe("Was expecting string match to fail");
}

void testMatchOrError() {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        StringRule{"hello-world"},
        MatchOrError{0, "Was expecting a greeting"}),
    .skips{},
    .regexOpts = {regexOpts},
  };

  // First, try a success case.
  const string msg = "hello-world";
  InputDiags ctx{Input{msg}};
  ssize_t pos = 0;
  JsonLoc jsloc = eval(ctx, pos, rs, 1);
  assertJsonLocIsString(__func__, jsloc, msg, 0, msg.size());

  // Then, a failure case.
  const string msg2 = "goodbye";
  ctx = InputDiags{Input{msg2}};
  pos = 0;
  eval(ctx, pos, rs, 1);
  assertHasDiagWithSubstrAt(__func__, ctx.diags, "Was expecting a greeting", 0);
}

void testAliasRule() {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(StringRule{"hello-world"}, AliasRule{0}),
    .skips{},
    .regexOpts = {regexOpts},
  };
  const string msg = " hello-world";
  InputDiags ctx{Input{msg}};
  ssize_t pos = 1;
  JsonLoc jsloc = eval(ctx, pos, rs, 1);
  assertJsonLocIsString(__func__, jsloc, msg.substr(1), 1, msg.size());
}

void testErrorRule() {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        StringRule{"hello-world"},
        ErrorRule{"Was expecting a greeting"},
        OrRule{
          {{-1, 0, passthroughTmpl}, {-1, 1, JsonTmpl::String{"ignored"}}},
          /* flattenOnDemand */ false,
        }),
    .skips{},
    .regexOpts = {regexOpts},
  };

  // First, try a success case.
  const string msg = "hello-world";
  InputDiags ctx{Input{msg}};
  ssize_t pos = 0;
  JsonLoc jsloc = eval(ctx, pos, rs, 2);
  assertJsonLocIsString(__func__, jsloc, msg, 0, msg.size());

  // Then, a failure case.
  const string msg2 = "goodbye";
  ctx = InputDiags{Input{msg2}};
  pos = 0;
  eval(ctx, pos, rs, 2);
  assertHasDiagWithSubstrAt(__func__, ctx.diags, "Was expecting a greeting", 0);
}

RuleSet skipPointRuleSet(Skipper skip) {
  RuleSet rs{ {}, {std::move(skip)}, {regexOpts} };
  rs.rules.push_back(move_to_unique(SkipPoint{0}));
  return rs;
}

void testSingleSkip() {
  const string msg = "  /* hello */ world";
  InputDiags ctx{Input{msg}};
  ssize_t pos = 0;
  RuleSet rs = skipPointRuleSet(cskip);
  eval(ctx, pos, rs, 0);
  assertEmptyDiags(__func__, ctx.diags);
  assertEqual(me("eval() endpoint"), size_t(pos), msg.find("world"));
}

void testSkipFailsOnUnfinishedComment() {
  const string msg = "  /* hello world";
  InputDiags ctx{Input{msg}};
  ssize_t pos = 0;
  RuleSet rs = skipPointRuleSet(cskip);
  eval(ctx, pos, rs, 0);
  assertHasDiagWithSubstrAt(__func__, ctx.diags, "Unfinished comment", 2);
}

void testSingleWordPreserving() {
  const string msg = "hello world";
  InputDiags ctx{Input{msg}};
  ssize_t pos = 0;
  RuleSet rs = singletonRuleSet(WordPreserving{"hello", 0});
  JsonLoc jsloc = eval(ctx, pos, rs, 0);
  assertJsonLocIsString(__func__, jsloc, "hello", 0, sizeof("hello")-1);

  ctx = InputDiags{Input{"hello_word"}};
  pos = 0;
  jsloc = eval(ctx, pos, rs, 0);
  if(!jsloc.holdsErrorValue())
    BugMe("Was expecting WordPreserving match to fail");
}

void testWordPreservingWithOverride() {
  // First, try the normal rule. Then, ones with overrides.
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
               WordPreserving{"hello", 0},
               WordPreserving{"hello", 1},
               WordPreserving{"hell", 0},
               WordPreserving{"hell", 2}),
    .skips{},
    .regexOpts = {regexOpts, RegexOptions{parseRegexCharSet("[a-z-]")},
                             RegexOptions{parseRegexCharSet("[e-l]")}
                 },
  };
  vector<string> expectations = {"hello", "", "", "hell"};
  for(size_t i=0; i<expectations.size(); ++i) {
    ssize_t spos = 0;
    InputDiags ctx{Input{"hello-world"}};
    JsonLoc jsloc = eval(ctx, spos, rs, i);
    if(expectations[i].empty()) {
      if(!jsloc.holdsErrorValue())
        BugMe("Was expecting an error on case {}. Got {}.", i,
              jsloc.prettyPrint(0));
    }else assertJsonLocIsString(__func__, jsloc, expectations[i], 0,
                                expectations[i].size());
  }
}

void testRegexMatch() {
  RuleSet rs = singletonRuleSet(parseRegexRule("/[a-z]+/"));
  ssize_t spos = 0;
  InputDiags ctx{Input{"hello world"}};
  JsonLoc jsloc = eval(ctx, spos, rs, 0);
  assertJsonLocIsString(__func__, jsloc, "hello", 0, sizeof("hello")-1);

  spos = 0;
  InputDiags ctx2{Input{"123"}};
  jsloc = eval(ctx2, spos, rs, 0);
  if(!jsloc.holdsErrorValue()) BugMe("Was expecting regex match to fail");
}

void testRegexWordOverride() {
  // First, try the normal rule. Then, ones with overrides.
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
               parseRegexRule("/[a-z]+\\b/"),
               parseRegexRule("/[a-z]+\\b/", 1),
               parseRegexRule("/[a-z]+\\b/", 2)),
    .skips{},
    .regexOpts = {regexOpts, RegexOptions{parseRegexCharSet("[a-z-]")},
                             RegexOptions{parseRegexCharSet("[e-l]")}
                 },
  };

  ssize_t spos = 0;
  InputDiags ctx{Input{"hello-world"}};
  JsonLoc jsloc = eval(ctx, spos, rs, 0);
  assertJsonLocIsString(__func__, jsloc, "hello", 0, sizeof("hello")-1);

  spos = 0;
  InputDiags ctx2{Input{"hello-world"}};
  jsloc = eval(ctx2, spos, rs, 1);
  if(!jsloc.holdsErrorValue())
    BugMe("Was expecting hyphen to be a word. Got {}", jsloc.prettyPrint(0));

  spos = 0;
  InputDiags ctx3{Input{"hello-world"}};
  jsloc = eval(ctx3, spos, rs, 2);
  assertJsonLocIsString(__func__, jsloc, "hell", 0, sizeof("hell")-1);
}

void testConcatMatch() {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
               parseRegexRule("/[a-zA-Z]+/"), StringRule{"="},
               parseRegexRule("/[0-9]+/"), StringRule{";"},
               SkipPoint{0},
               ConcatFlatRule{
                 { {0, "lhs"}, {4, ""}, {1, ""}, {4, ""}, {2, "rhs"}, {4, ""},
                   {3, ""}
                 } },
               nmRule(OutputTmpl{5, {},
                      *parseJsonTmpl("{ stmt: 'asgn', lhs, rhs }")}, "asgn")),
    .skips{cskip},
    .regexOpts = {regexOpts},
  };
  ssize_t concatIndex = rs.rules.size()-1;
  ssize_t pos = 0;
  InputDiags ctx{Input{"orangeCount = 5; ignored_bits;"}};
  JsonLoc expected = *parseJsonLoc(R"({
    stmt: 'asgn', lhs: 'orangeCount', rhs: '5'
  })");
  JsonLoc observed = eval(ctx, pos, rs, concatIndex);
  assertEqual(__func__, expected, observed);
  assertLocPairEqual(__func__, 0, ctx.input().find(';',0)+1, observed);

  pos = 0;
  ctx = InputDiags{Input{"orangeCount = 5 missing-semicolon;"}};
  observed = eval(ctx, pos, rs, concatIndex);
  if(!observed.holdsErrorValue())
    BugMe("Was expecting failure on missing semicolon. Got {:2}", observed);
}

void testConcatFlatMatch() {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
               StringRule{"var"},
               parseRegexRule("/[a-zA-Z]+/"),
               StringRule{":"}, StringRule{"="},
               parseRegexRule("/[0-9]+/"),
               StringRule{";"}, SkipPoint{0}),
    .skips{cskip},
    .regexOpts = {regexOpts},
  };
  rs.rules.push_back(move_to_unique(ConcatFlatRule{{
      {1, "var_name"}, {6, ""}, {2, ""}, {6, ""}, {1, "type"},
  }}));
  ssize_t varTypeIndex = rs.rules.size() - 1;
  rs.rules.push_back(move_to_unique(ConcatFlatRule{{
      {0, ""}, {6, ""}, {varTypeIndex, ""}, {6, ""}, {3, ""},
      {6, ""}, {4, "rhs"}, {6, ""}, {5, ""}
  }}));
  ssize_t declIndex = rs.rules.size() - 1;
  rs.rules.push_back(move_to_unique(OutputTmpl{declIndex, {},
        *parseJsonTmpl("{var_name, init_value: {type, value: rhs}}")
  }));
  ssize_t outIndex = rs.rules.size() - 1;
  ssize_t pos = 0;
  InputDiags ctx{Input{"var x:int = 5; ignored_bits;"}};
  JsonLoc expected = *parseJsonLoc(
      "{var_name: 'x', init_value: {type: 'int', value: '5'}}");
  JsonLoc observed = eval(ctx, pos, rs, outIndex);
  assertEqual(__func__, expected, observed);
  assertLocPairEqual(__func__, 0, ctx.input().find(';',0)+1, observed);
  pos = 0;
  ctx = InputDiags{Input{"var y = 9;"}};
  observed = eval(ctx, pos, rs, outIndex);
  if(!observed.holdsErrorValue())
    BugMe("Was expecting failure on missing type. Got {:2}", observed);
}

void testSingleWordTemplate() {
  JsonTmpl jstmpl = JsonTmpl::Map{{"keyword", JsonTmpl{"word"}}};
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
               StringRule{"word"}, OutputTmpl{0, {}, jstmpl}),
    .skips{},
    .regexOpts = {regexOpts},
  };
  ssize_t pos = 0;
  InputDiags ctx{Input{"word and ignored"}};
  JsonLoc observed = eval(ctx, pos, rs, rs.rules.size()-1);
  assertEqual(__func__, jstmpl.outputIfFilled(), observed);
}

void testKeywordsOrNumber() {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
               StringRule{"if"}, StringRule{"while"},
               parseRegexRule("/[0-9]+/")),
    .skips{},
    .regexOpts = {regexOpts},
  };
  rs.rules.push_back(move_to_unique(OrRule{{
      {-1, 0, JsonTmpl{"if"}}, {-1, 1, JsonTmpl{"while"}},
      {-1, 2, *parseJsonTmpl("{number: child}")},
  }, /* flattenOnDemand */ false}));
  const ssize_t orListIndex = rs.rules.size()-1;

  const pair<string, JsonLoc> goodInputOutputPairs[] = {
    {"if", JsonLoc{"if"}}, {"while", JsonLoc{"while"}},
    {"42", *parseJsonLoc(R"({number: '42'})")},
  };
  for(auto& [msg, expected] : goodInputOutputPairs) {
    ssize_t pos = 0;
    InputDiags ctx{Input{msg}};
    JsonLoc observed = eval(ctx, pos, rs, orListIndex);
    assertEqual(__func__, expected, observed);
    assertEqual(__func__, pos, ssize_t(msg.size()));
    assertLocPairEqual(__func__, 0, msg.size(), observed);
  }

  ssize_t pos = 0;
  InputDiags ctx{Input{"do"}};
  JsonLoc observed = eval(ctx, pos, rs, orListIndex);
  if(!observed.holdsErrorValue())
    BugMe("Was expecting failure on keyword 'do'. Got {:2}", observed);
}

void testFlattenOnDemand() {
  // ConcatFlatRule of a single child is a bit weird, but let's say the
  // frontend is dumb (ahem!)
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        StringRule{"let"}, parseRegexRule("/[0-9]+/"),
        ConcatFlatRule{{ {0, "keyword"} }},
        MatchOrError{2, "Expected keyword 'let'"},
        OrRule{{
          {-1, 3, passthroughTmpl},
          {-1, 1, *parseJsonTmpl("{number: child}")},
        }, /* flattenOnDemand */ false},
        ConcatFlatRule{{{4, "next_token"}}}
      ), .skips{}, .regexOpts = {regexOpts},
  };
  const ssize_t ruleidx = rs.rules.size()-1;

  const tuple<string, bool, JsonLoc> inputOutputPairs[] = {
    {"let", false, *parseJsonLoc("{next_token: {keyword: 'let'}}")},
    {"let", true, *parseJsonLoc("{keyword: 'let'}")},
    {"42", false, *parseJsonLoc("{next_token: {number: '42'}}")},
    {"42", true, *parseJsonLoc("{number: '42'}")},
  };
  for(auto& [msg, fod, expected] : inputOutputPairs) {
    dynamic_cast<OrRule*>(rs.rules[4].get())->flattenOnDemand = fod;
    dynamic_cast<ConcatFlatRule*>(rs.rules[5].get())
      ->comps[0].outputPlaceholder =
      (fod ? "" : "next_token");
    ssize_t pos = 0;
    InputDiags ctx{Input{msg}};
    JsonLoc observed = eval(ctx, pos, rs, ruleidx);
    assertEqual(__func__, expected, observed);
  }
}

void testLookaheads() {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        SkipPoint{0},
        WordPreserving{"var", 0},
        parseRegexRule("/[a-z]+/"),
        StringRule{"="}, StringRule{";"},
        nmRule(ConcatFlatRule{{
          {1, ""}, {0, ""}, {2, "var"}, {0, ""}, {3, ""}, {0, ""},
          {2, "init_value"}, {0, ""}, {4, ""},
        }}, "decl"),
        nmRule(ConcatFlatRule{{
          {2, "lhs"}, {0, ""}, {3, ""}, {0, ""}, {2, "rhs"}, {0, ""}, {4, ""},
        }}, "asgn"),
        nmRule(OrRule{{{1, 5, passthroughTmpl}, {-1, 6, passthroughTmpl}},
                      /* flattenOnDemand */ true}, "simple_stmt")),
    .skips{cskip},
    .regexOpts = {regexOpts},
  };
  const pair<string, JsonLoc> testdata[] = {
    {"var x = y; ignore", *parseJsonLoc("{var: 'x', init_value: 'y'}")},
    {"x = y; ignore", *parseJsonLoc("{lhs: 'x', rhs: 'y'}")},
    {"var = x; ignore", JsonLoc{JsonLoc::ErrorValue{}}},
  };
  for(auto& [msg, expected] : testdata) {
    ssize_t pos = 0;
    InputDiags ctx{Input{msg}};
    JsonLoc observed = eval(ctx, pos, rs, 7);
    if(expected.holdsErrorValue()) {
      if(!observed.holdsErrorValue())
        BugMe("Expected error on '{}', got {}", msg, observed);
    }else assertEqual(__func__, expected, observed);
  }
}

void testQuietMatch() {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        StringRule{"string1"}, StringRule{"string2"},
        nmRule(MatchOrError{0, "Expecting 'string1'"}, "string1_or_error"),
        nmRule(QuietMatch{2}, "string1_quiet"),
        nmRule(OrRule{{{-1, 3, passthroughTmpl}, {-1, 1, passthroughTmpl}},
                      /* flattenOnDemand */ false}, "quiet_match_test")),
    .skips{},
    .regexOpts = {regexOpts},
  };
  InputDiags ctx{Input{"string2"}};
  ssize_t pos = 0;
  JsonLoc observed = eval(ctx, pos, rs, 4);
  if(observed.holdsErrorValue() || !ctx.diags.empty()) {
    if(!ctx.diags.empty()) showDiags(ctx.diags);
    BugMe("Expected to succeed without diags");
  }
}

void testMiscFlattening() {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        StringRule{"hello"},
        nmRule(ConcatFlatRule{{ {0, "hello_for_qm"} }}, "hello_flat1"),
        nmRule(QuietMatch{1}, "hello_quiet_passing_thru_concat_flat"),
        nmRule(ConcatFlatRule{{ {0, "hello_for_mor"} }}, "hello_flat1"),
        nmRule(MatchOrError{3, "Expected keyword 'hello'"},
          "match_or_error_passing_thru_concat_flat"),
        nmRule(ConcatFlatRule{{ {2, ""}, {4, ""} }}, "hello_flat2"),
        nmRule(QuietMatch{0}, "hello_quiet_dropped_by_concat_flat"),
        nmRule(MatchOrError{0, "Expected keyword 'hello'"},
          "match_or_error_dropped_by_concat_flat"),
        nmRule(ConcatFlatRule{{ {6, ""}, {7, ""} }}, "hello_flat3")
     ),
    .skips{},
    .regexOpts = {regexOpts},
  };
  string msg = "hellohello";

  // Passthrough test.
  InputDiags ctx{Input{msg}};
  ssize_t pos = 0;
  JsonLoc observed = eval(ctx, pos, rs, 5);
  if(observed.holdsErrorValue() || !ctx.diags.empty()) {
    if(!ctx.diags.empty()) showDiags(ctx.diags);
    BugMe("Expected to succeed without diags");
  }
  assertEqual(__func__, pos, ssize_t(msg.size()));
  assertEqual(__func__, observed, *parseJsonLoc(
        R"({hello_for_qm: 'hello',
            hello_for_mor: 'hello'})"));

  // Drop test.
  ctx.diags.clear();
  pos = 0;
  observed = eval(ctx, pos, rs, 8);
  if(observed.holdsErrorValue() || !ctx.diags.empty()) {
    if(!ctx.diags.empty()) showDiags(ctx.diags);
    BugMe("Expected to succeed without diags");
  }
  assertEqual(__func__, pos, ssize_t(msg.size()));
  assertEqual(__func__, observed, JsonLoc{JsonLoc::Map{}});
}

void testLoopRule() {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        MatchOrError{4, "Expected an identifier"},
        StringRule{"+"}, SkipPoint{0},
        nmRule(LoopRule{{
          .partidx = 0,
          .partname = "operand",
          .glueidx = 5,
          .gluename = "",
          .lookidx = -1,
          .skipidx = 2,
        }}, "sum"),
        parseRegexRule("/[a-z]+/"),
        MatchOrError{1, "Expected operator '+'"},

        // Cases for glueidx == -1
        StringRule{","},
        ConcatFlatRule{{{0, "elements"}, {2, ""}, {6, ""}, {2, ""}}},
        nmRule(LoopRule{{ .partidx = 7, .partname = "",
                          .glueidx = -1, .gluename = "",
                          .lookidx = -1, .skipidx = -1}}, "list_prefix")
    ),
    .skips{cskip},
    .regexOpts = {regexOpts},
  };
  // Some tests have extra trailing space to see if we are properly
  // backtracking from the final SkipPoint
  tuple<string, JsonLoc, ssize_t> goodcases[] = {
    {"a + b ", *parseJsonLoc("{operand: ['a', 'b'] }"), -1},
    {"a /*", *parseJsonLoc("{operand: ['a'] }"), 1},
    {"a + b + c ", *parseJsonLoc("{operand: ['a', 'b', 'c'] }"), -1},
    {"a + b c", *parseJsonLoc("{operand: ['a', 'b'] }"), 5},
  };
  for(auto& [msg, expectedJsloc, expectedEnd] : goodcases) {
    InputDiags ctx{Input{msg}};
    ssize_t pos = 0;
    JsonLoc observed = eval(ctx, pos, rs, 3);
    assertEqual(__func__, expectedJsloc, observed);
    if(expectedEnd == -1) expectedEnd = msg.size()-1;
    assertEqual(format("{}: test case '{}'", __func__, msg), expectedEnd, pos);
    assertLocPairEqual(__func__, 0, expectedEnd, observed);
    assertEmptyDiags(format("{}-with-glue", __func__), ctx.diags);
  }
  pair<string, string> badcases[] = {
    {"(boo!)", "Expected an identifier"},
    {"a ++", "Expected an identifier"},
    {"a + /*", "Unfinished comment"},
  };
  for(auto& [msg, expectedDiag] : badcases) {
    InputDiags ctx{Input{msg}};
    ssize_t pos = 0;
    JsonLoc observed = eval(ctx, pos, rs, 3);
    if(!observed.holdsErrorValue())
      Bug("Was expecting an error on input '{}'. Got {}", msg, observed);
    assertHasDiagWithSubstr(__func__, ctx.diags, expectedDiag);
  }

  InputDiags ctx{Input{"a, b,"}};
  ssize_t pos = 0;
  JsonLoc observed = eval(ctx, pos, rs, 8);
  if(!ctx.diags.empty()) showDiags(ctx.diags);
  assertEqual(__func__ + ": end position on glueless case"s, pos, ssize_t(5));
  assertEqual(__func__ + ": glueless case jsonloc output"s,
              *parseJsonLoc("{elements: ['a', 'b']}"), observed);
  assertEmptyDiags(format("{}-no-glue", __func__), ctx.diags);

  ctx = InputDiags{Input{"!"}};
  pos = 0;
  observed = eval(ctx, pos, rs, 8);
  if(!observed.holdsErrorValue())
    Bug("Was expecting an error on mandatory repeats. Got {}", observed);
  assertHasDiagWithSubstr(__func__, ctx.diags, "Expected an identifier");
}


// Flattenable child is processed on a different branch. Test that too.
void testLoopFlattening() {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        parseRegexRule("/[-+]/"),
        parseRegexRule("/[a-z]+/"),
        MatchOrError{1, "Expected an identifier"},
        ConcatFlatRule{{ {0, "sign"}, {2, "elements"} }},
        SkipPoint{0},
        StringRule{","},
        nmRule(LoopRule{{
          .partidx = 3,
          .partname = "",
          .glueidx = 5,
          .gluename = "",
          .lookidx = -1,
          .skipidx = 4,
        }}, "sum"),
        StringRule{"["}, StringRule{"]"},
        ConcatFlatRule{{ {7, ""}, {6, ""}, {8, ""} }}
    ),
    .skips{cskip},
    .regexOpts = {regexOpts},
  };
  const string msg = "[+a, -b]";
  InputDiags ctx{Input{msg}};
  ssize_t pos = 0;
  JsonLoc observed = eval(ctx, pos, rs, 9);
  if(!ctx.diags.empty()) showDiags(ctx.diags);
  assertEqual(__func__, pos, ssize_t(msg.size()));
  assertEqual(__func__, *parseJsonLoc("{elements: ['a', 'b'],"
                                      " sign: ['+', '-']}"), observed);
}

// Where the "glue" has the more interesting content.
// Test that we still record their names.
void testGluePartSwapped() {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        StringRule{"-"},
        parseRegexRule("/[a-z]+/"),
        ConcatFlatRule{{ { 1, "words" } }},
        LoopRule{{.partidx = 0, .partname = "",
                  .glueidx = 2, .gluename = "",
                  .lookidx = -1, .skipidx = -1 }},
        LoopRule{{.partidx = 0, .partname = "",
                  .glueidx = 1, .gluename = "words",
                  .lookidx = -1, .skipidx = -1 }}
    ),
    .skips{},
    .regexOpts = {regexOpts},
  };
  InputDiags ctx{Input{"-greetings-earth-"}};
  ssize_t pos = 0;
  JsonLoc observed = eval(ctx, pos, rs, 3);
  if(!ctx.diags.empty()) {
    showDiags(ctx.diags);
    BugMe("Expected empty diags");
  }
  assertEqual(__func__, pos, ssize_t(17));
  assertEqual(__func__, *parseJsonLoc("{words: ['greetings', 'earth']}"),
                        observed);

  pos = 0;
  observed = eval(ctx, pos, rs, 4);
  if(!ctx.diags.empty()) {
    showDiags(ctx.diags);
    BugMe("Expected empty diags");
  }
  assertEqual(__func__, pos, ssize_t(17));
  assertEqual(__func__, *parseJsonLoc("{words: ['greetings', 'earth']}"),
                        observed);
}

void testFlatFieldsForNonFlatRules() {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        StringRule{"hello"},
        WordPreserving{"hello", 0},
        ExternParser{"oalexBuiltinHello", {}},
        SkipPoint{0},
        parseRegexRule("/[a-z]+/"),
        OutputTmpl{0, {}, JsonTmpl::Map{}},
        ErrorRule{"Was expecting a hello"}
    ),
    .skips{cskip},
    .regexOpts = {regexOpts},
  };
  populateFlatFields(rs);
  for(const unique_ptr<Rule>& rule: rs.rules)
    assertEqual(__func__, ssize(rule->flatFields()), 0);
}

bool ruleFieldEqual(const RuleField& a, const RuleField& b) {
  return a.field_name == b.field_name &&
         a.schema_source == b.schema_source &&
         a.container == b.container;
}
[[maybe_unused]] string debug_field(const RuleField& rf) {
  string_view c = rf.container == 0 ? "single"
                : rf.container == 1 ? "optional"
                : rf.container == 2 ? "vector"
                : "garbage";
  return format("{{ .field_name = {}, .schema_source = {}, .container = {} }}",
                rf.field_name, rf.schema_source, c);
}

// This test case is trying to implement this oalex grammar:
//
//   rule element choices:
//   | '(' -> paren_group
//   | word
//
//   rule word: /[a-z]+/
//
//   rule paren_group:
//     ( [ items, ... , items ] )
//   where:
//     items ~ element
void testFlatFieldsForNestedList() {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        parseRegexRule("/[a-z]+/"),  // [0]
        StringRule{","},
        StringRule{"("},
        StringRule{")"},
        StringRule{""},
        LoopRule{{.partidx = 9, .partname = "items", // [5]. items, items, ...
                  .glueidx = 1, .gluename = "",
                  .lookidx = -1, .skipidx = -1 }},
        OrRule{{{-1,5,passthroughTmpl}, {-1,4,passthroughTmpl}},
               true /* flattenOnDemand */ },  // [items, items, ... ]
        ConcatFlatRule{{ {2,""}, {6,""}, {3,""} }},
        OutputTmpl{7, {}, *parseJsonTmpl("{items}")},  // paren_group
        OrRule{{ {2,5,passthroughTmpl},
                 {-1,0,*parseJsonTmpl("{word: child}")}},
               false /* flattenOnDemand */ }  // element :: JsonLike
    ),
    .skips{cskip},
    .regexOpts = {regexOpts},
  };
  populateFlatFields(rs);
  vector<vector<RuleField>> expected(rs.rules.size());
  expected[5] = expected[6] = expected[7] = vector {
    RuleField{
      .field_name = "items",
      .schema_source = 9,
      .container = RuleField::vector,
    }
  };
  for(ssize_t i=0; i<ssize(rs.rules); ++i) {
    const vector<RuleField>& observed = rs.rules[i]->flatFields();
    if(expected[i].size() != observed.size() ||
       !equal(expected[i].begin(), expected[i].end(),
              observed.begin(), ruleFieldEqual))
      BugMe("Mismatch at index {}", i);
  }
}

void testFlatWrappers() {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        AliasRule{1},
        WordPreserving{"hello", 0},
        ConcatFlatRule{{ {0, "kw"} }},
        ErrorRule{"Was expecting a hello"},
        OrRule{{ {-1, 2, passthroughTmpl}, {-1, 3, passthroughTmpl} },
               true /* flattenOnDemand */ },
        OrRule{{ {-1, 0, JsonTmpl::Map{ std::pair{"s", passthroughTmpl} } },
                 {-1, 3, passthroughTmpl} },
               true /* flattenOnDemand */ }
    ),
    .skips{cskip},
    .regexOpts = {regexOpts},
  };
  populateFlatFields(rs);
  for(const RuleField& f: rs.rules[4]->flatFields()) {
    if(dynamic_cast<const AliasRule*>(rs.rules[f.schema_source].get()))
      Bug("Rule 4 points to flat wrapper AliasRule at position {}",
          f.schema_source);
  assertEqual("Rule 5 needs a field", rs.rules[5]->flatFields().size(), 1u);
  }
}

}  // namespace

int main() {
  testSingleStringMatch();
  testSingleStringMismatch();
  testMatchOrError();
  testAliasRule();
  testErrorRule();
  testSingleSkip();
  testSingleWordPreserving();
  testWordPreservingWithOverride();
  testSkipFailsOnUnfinishedComment();
  testRegexMatch();
  testRegexWordOverride();
  testConcatFlatMatch();
  testSingleWordTemplate();
  testConcatMatch();
  testKeywordsOrNumber();
  testFlattenOnDemand();
  testLookaheads();
  testQuietMatch();
  testMiscFlattening();
  testLoopRule();
  testLoopFlattening();
  testGluePartSwapped();
  testFlatFieldsForNonFlatRules();
  testFlatFieldsForNestedList();
  testFlatWrappers();
}

