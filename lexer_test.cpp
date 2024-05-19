/*  Copyright 2019-2024 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "lexer.h"
#include "lexer_matcher.h"

#include <iterator>
#include <string>
#include <string_view>
#include <tuple>

#include "fmt/format.h"

#include "runtime/indent.h"
#include "runtime/test_util.h"
#include "runtime/util.h"
using fmt::format;
using fmt::print;
using std::back_insert_iterator;
using std::get;
using std::get_if;
using std::nullopt;
using std::optional;
using std::pair;
using std::string;
using std::string_view;
using std::tuple;
using std::vector;
using namespace std::string_literals;
using oalex::assertEqual;
using oalex::Bug;
using oalex::Diag;
using oalex::IndentCmp;
using oalex::indentCmp;
using oalex::Input;
using oalex::InputDiags;
using oalex::WholeSegment;
using oalex::lex::BracketGroup;
using oalex::lex::BracketType;
using oalex::lex::ExprToken;
using oalex::lex::GluedString;
using oalex::lex::lexBracketGroup;
using oalex::lex::lexFencedSource;
using oalex::lex::lexIndentedSource;
using oalex::lex::lexListEntries;
using oalex::lex::lexNextLine;
using oalex::lex::lexQuotedString;
using oalex::lex::lexSectionHeader;
using oalex::lex::lookahead;
using oalex::lex::NewlineChar;
using oalex::lex::skipBlankLines;
using oalex::lex::matcher::braces;
using oalex::lex::matcher::BracketGroupMatcher;
using oalex::lex::matcher::ExprMatcher;
using oalex::lex::matcher::glued;
using oalex::lex::matcher::matchvec;
using oalex::lex::matcher::parens;
using oalex::lex::matcher::RegexPatternMatcher;
using oalex::lex::matcher::squareBrackets;
namespace matcher = oalex::lex::matcher;

template <> struct fmt::formatter<IndentCmp>: formatter<string_view> {
  template <typename FormatContext>
  auto format(IndentCmp res, FormatContext& ctx) {
    string_view name = "unknown";
    switch (res) {
      case IndentCmp::lt: name = "IndentCmp::lt"; break;
      case IndentCmp::eq: name = "IndentCmp::eq"; break;
      case IndentCmp::gt: name = "IndentCmp::gt"; break;
      case IndentCmp::bad: name = "IndentCmp::bad"; break;
    }
    return formatter<string_view>::format(name, ctx);
  }
};

namespace {

const char goodHeader1[] =
R"(Header at top  # comments
----------   # comments
)";

const char goodHeader2[] = R"(
# comment

Header in the middle
--------------------
)";

const char headerWithComma[] = R"(

Not a header, has comma
-----------------------

)";

const char headerWithExtraBlank[] = R"(
Not a header             # Followed by blank lines.

----------------------   # Just a line of dashes.
)";

const char headerIndented[] = R"(
  Not a header   # Content line has a space
--------------
)";

const char headerDashIndented[] = R"(
Not a header
  ------------   # Dash line has a space
)";

void headerSuccessImpl(const char testInput[], const char testName[],
    vector<string> expected) {
  InputDiags ctx{Input{testInput}};
  size_t i = 0;
  vector<WholeSegment> res = lexSectionHeader(ctx, i);
  if(res.empty() || !ctx.diags.empty()) {
    for(const auto& d:ctx.diags) print(stderr, "{}\n", string(d));
    Bug("{} failed", testName);
  }else {
    vector<string> observed;
    for(const WholeSegment& t : res) observed.push_back(*t);
    if(expected != observed)
      Bug("{}: {} != {}", testName, expected, observed);
  }
}

void headerFailureImpl(const char testInput[], const char testName[],
    const string& expectedDiag) {
  InputDiags ctx{Input{testInput}};
  size_t i = 0;
  vector<WholeSegment> res = lexSectionHeader(ctx, i);
  if(!res.empty() && ctx.diags.empty())
    Bug("Test {} succeeded unexpectedly", testName);
  if(!expectedDiag.empty())
    assertHasDiagWithSubstr(testName, ctx.diags, expectedDiag);
}

const char goodString[] = "\"Hello world\"";
const char goodStringWithEscapes[] = R"("Hello world \n \" \t \\ \x41")";
const char stringDoesntEnd[] = "\"Foo";
const char multiLineString[] = "\"Foo\nBar\"";
const char incompleteEscape[] = "\"Foo\\";
const char invalidEscape[] = "\"\\&\"";
const char incompleteHex[] = R"("Foo\x")";
const char invalidHex[] = R"("\xag")";

void stringSuccessImpl(const char testInput[], const char testName[],
                       string_view expected) {
  InputDiags ctx{Input{testInput}};
  size_t i = 0;
  optional<GluedString> res = lexQuotedString(ctx, i);
  if(!res || !ctx.diags.empty()) {
    for(const auto& d:ctx.diags) print(stderr, "{}\n", string(d));
    Bug("{} failed", testName);
  }else if(expected != *res)
    Bug("{}: {} != {}", testName, expected, string_view(*res));

  // This behavior is a bit arbitrary.
  assertEqual(testInput, size_t{0}, res->stPos);
  assertEqual(testInput, string_view(testInput).size(), res->enPos);
  GluedString res2 = res->subqstr(0, res->size());
  for(size_t i=0; i<res->size(); ++i) {
    if(res->inputPos(i) != res2.inputPos(i))
      Bug("{}: inputPos changed in substring at sourcePos {}: {} != {}",
          testName, i, res->inputPos(i), res2.inputPos(i));
    if(res->rowCol(i) != res2.rowCol(i)) {
      auto [l1,c1] = res->rowCol(i);
      auto [l2,c2] = res2.rowCol(i);
      Bug("{}: rowCol changed in substring at sourcePos{}: ({},{}) != ({},{})",
          testName, i, l1, c1, l2, c2);
    }
  }
}

void stringFailureImpl(const char testInput[], const char testName[],
                       string_view expectedDiag) {
  InputDiags ctx{Input{testInput}};
  size_t i = 0;
  optional<GluedString> res = lexQuotedString(ctx, i);
  if(res && ctx.diags.empty())
    Bug("Test {} succeeded unexpectedly", testName);
  assertHasDiagWithSubstr(testName, ctx.diags, expectedDiag);
}

string debug(const pair<size_t,size_t>& p) {
  return fmt::format("{}:{}", p.first, p.second);
}

// TODO: Replace with the test_util.h version
template <class T>
void assertEq(string_view errmsg, const T& a, const T& b) {
  if(a != b) Bug("{}: {} != {}", errmsg, debug(a), debug(b));
}

void compareSubqstrIndexPos(const GluedString& a, size_t pos, size_t len) {
  const GluedString b = a.subqstr(pos, len);
  for(size_t i=0; i<=b.size(); ++i) if(a.inputPos(i+pos) != b.inputPos(i))
    Bug("Substring inputPos mismatch between '{}' and '{}': "
           "a.inputPos({}) = {}  !=  b.inputPos({}) = {}", string_view(a),
           string_view(b), i+pos, a.inputPos(i+pos), i, b.inputPos(i));
}

void stringPosMap() {
  const char testInput[] = R"("foo\nbar")";
  InputDiags ctx{Input{testInput}};
  size_t i = 0;
  optional<GluedString> res = lexQuotedString(ctx, i);
  if(!res) Bug("Parsing {} failed in {}", testInput, __func__);
  assertEq(__func__ + " rowCol mismatch"s, res->rowCol(0),
           pair<size_t,size_t>(1, 2));
  assertEq(__func__ + " rowCol mismatch"s, res->rowCol(4),
           pair<size_t,size_t>(1, 7));
  assertEq(__func__ + " rowCol mismatch"s, res->rowCol(7),
           pair<size_t,size_t>(1, 10));

  compareSubqstrIndexPos(*res, 0, string::npos);
  compareSubqstrIndexPos(*res, 1, string::npos);
  compareSubqstrIndexPos(*res, 0, 4);
  compareSubqstrIndexPos(*res, 1, 3);
  compareSubqstrIndexPos(*res, 1, 4);
  compareSubqstrIndexPos(*res, 1, 1);
  compareSubqstrIndexPos(*res, 1, 0);
}

void getSegmentTest(const char testInput[], bool expectedSuccess) {
  InputDiags ctx{Input{testInput}};
  size_t i = 0;
  optional<GluedString> res = lexQuotedString(ctx, i);
  if(!res) Bug("Parsing {} failed in {}", testInput, __func__);
  if(expectedSuccess && !res->getSegment())
    BugMe("getSegment() return nullopt for {}", testInput);
  if(!expectedSuccess && res->getSegment())
    BugMe("getSegment() succeeded unexpectedly for {}", testInput);
}

const char fencedSourceBlock[] = R"(```
I can write whatever here I want to. Including
``` as long as
it's not on the only thing on the line.
```)";

const char fencedSourceBlockCustom[] = R"(```foo
Even this
```
Doesn't end the string
```foo)";

const char badFencedStart[] = "```foo#\n";

size_t fenceSize(string_view s) { return s.find('\n'); }

void fencedSourceBlockSuccessImpl(string_view testInput,
                                  const char testName[]) {
  // substr() counts the newline after the starting fence.
  size_t dsize = fenceSize(testInput);
  string_view expected = testInput.substr(dsize+1, testInput.size()-2*dsize-1);

  InputDiags ctx{Input{testInput}};
  size_t i = 0;
  optional<GluedString> res = lexFencedSource(ctx, i);
  if(!res || !ctx.diags.empty()) {
    for(const auto& d:ctx.diags) print("{}\n", string(d));
    Bug("{} failed", testName);
  }else {
    if(expected != *res) {
      Bug("{}: {} != {}", testName, expected, string_view(*res));
    }
  }

  size_t lastLineNumber = ctx.input().rowCol(i).first;

  if(res->rowCol(0).first != 2)
    Bug("{}: Was expecting fenced string body to start on "
        "the second line. Found it on {}. Input:\n{}",
        testName, res->rowCol(0).first, testInput);
  for(size_t i=0; i<res->size(); ++i) if(res->rowCol(i+1).second == 1) {
    if(res->rowCol(i).first+1 != res->rowCol(i+1).first)
      Bug("{}: Line numbers are not sequential "
          " at the start of line {}. Input:\n{}",
          testName, res->rowCol(i+1).first, testInput);
    auto expected = ctx.input().rowCol(i + 1 + dsize);
    if(res->rowCol(i) != expected)
      Bug("{}: Location info changed after parsing: "
          "{} became {}", testName, debug(expected), debug(res->rowCol(i)));
  }
  if(res->rowCol(res->size()) != pair(lastLineNumber, size_t(1)))
    Bug("{}: Was expecting the input to end on {}:1, instead found {}. "
        "Input\n", testName, lastLineNumber, debug(res->rowCol(res->size())),
        testInput);
}

void fencedSourceBlockFailureImpl(const char testInput[], const char testName[],
                                  string_view expectedDiag) {
  assertProducesDiag(testName, testInput, expectedDiag,
                     OALEX_VOIDIFY(lexFencedSource));
}

const char goodIndent[] = R"(
  foo

  bar:
    baz
 outside
)";
const char goodIndentParsed[] = "\nfoo\n\nbar:\n  baz\n";

const char allBlank[] = "\n  \n\t\n";

size_t lineCount(string_view s) {
  size_t c = 1; for(char ch : s) if(ch == '\n') ++c; return c;
}

void indentedSourceBlockSuccessImpl(
    string_view testInput, const char testName[],
    optional<string> expectedResult) {
  InputDiags ctx{Input{testInput}};
  size_t i = 0;
  optional<GluedString> res = lexIndentedSource(ctx, i, "  ");
  if(res.has_value() != expectedResult.has_value() || !ctx.diags.empty()) {
    for(const auto& d:ctx.diags) print(stderr, "{}\n", string(d));
    Bug("{} failed", testName);
  }
  if(!res.has_value()) return;  // Don't check *res if it's not valid.

  if(string_view(*res) != *expectedResult)
    Bug("{}: '{}' != '{}'", testName, string_view(*res), *expectedResult);

  if(testInput.substr(i, 2) == "  ")
    Bug("{}: indented block parsing stopped too early", testName);

  size_t lc = lineCount(testInput.substr(0, i));
  if(res->rowCol(0).first != 1)
    Bug("{}: Parsed value does not start at row 1", testName);

  if(res->rowCol(res->size()).first != lc)
    Bug("{}: Parsed value does not end on line {}, input:\n{}",
        testName, lc, testInput);

  string_view testOutput  = *res;
  for(size_t i = 0; i < res->size(); ++i)
    if(res->rowCol(i).first != res->rowCol(i+1).first) {
      size_t r1 = res->rowCol(i).first, r2 = res->rowCol(i+1).first;
      if(r1+1 != r2)
        Bug("{}: line numbers are not sequential after {}: found {}",
            testName, r1, r2);
      if(testOutput[i] != '\n')
        Bug("{} starts a new line without a newline char at {}",
            testName, debug(res->rowCol(i+1)));
      size_t c2 = res->rowCol(i+1).second;
      char next = (res->sizeGt(i+1) ? testOutput[i+1] : '\n');
      if(c2 == 1) {
        if(next != '\n')
          Bug("{} maps {} to column 1, even though it is "
              "not a blank line.", testName, i+1);
      }else if(c2 == 3) {
        if(next == '\n')
         Bug("{} maps {} to column 3, even though it is a blank line.",
             testName, i+1);
      }else
        Bug("{} maps the start of line {} to column {}, "
            "was expecting column 1 or 3.", testName, r2, c2);
    }else if(testOutput[i] == '\n')
      Bug("{} doesn't start a new line in spite of a \\n char at {}",
          testName, debug(res->rowCol(i)));

  for(size_t i=0; res->sizeGt(i); ++i) {
    if(i == 0 || testOutput[i-1] == '\n') {
      if(res->bol(i) != i)
        BugMe("Expected bol({0}) == {0}, but found {1} in parsed result:\n{2}",
              i, res->bol(i), testOutput);
    }else if(res->bol(i) != res->bol(i-1)) {
      BugMe("bol() changed without newline: bol({}) == {}, bol({}) == {}",
            i, res->bol(i), i-1, res->bol(i-1));
    }
  }
}

const char tabSpaceMix[] = "  foo\n\tbar";

void indentedSourceBlockFailureImpl(
    const char testInput[], const char testName[],
    string_view expectedDiag) {
  InputDiags ctx{Input{testInput}};
  size_t i = 0;
  optional<GluedString> res = lexIndentedSource(ctx, i, "  ");
  if(res && ctx.diags.empty())
    Bug("Test {} succeeded unexpectedly", testName);
  assertHasDiagWithSubstr(testName, ctx.diags, expectedDiag);
}

void lookaheadsSuccess() {
  string inputs[] = {"   \n foo bar", "x  foo", "  []", "  ..., x", "  :"};
  string expecteds[] = {"foo","foo","[","...",":"};
  static_assert(sizeof(inputs)==sizeof(expecteds));
  for(size_t i=0; i<sizeof(inputs)/sizeof(*inputs); ++i) {
    InputDiags ctx{Input{inputs[i]}};
    if(optional<WholeSegment> tok = lookahead(ctx,1)) {
      if(tok->data!=expecteds[i])
        BugMe("Test case {} failed with \"{}\" != \"{}\"", i,
              tok->data, expecteds[i]);
    }else BugMe("Test case {} was supposed to succeed", i);
  }
}

void lookaheadNulloptOnEof() {
  string input = "foo     # hello \n\t\n";
  InputDiags ctx{Input{input}};
  if(optional<WholeSegment> tok = lookahead(ctx,3))
    BugMe("Succeeded unexpectedly, got {}", tok->data);
}

void lookaheadThrowsOnInvalidChar() {
  string input = "\b";
  assertProducesDiag(__func__, "\b", "Unexpected character",
                     OALEX_VOIDIFY(lookahead));
}

// Within brackets, we ignore all indentation.
void bracketGroupSuccess() {
  const string input =
    "{A := (B.C word [#comment\n\"hello\" \"world\"] /a*b/)}";
  const BracketGroupMatcher expected = braces("A", ":=",
        parens("B", ".", "C", "word",
               squareBrackets(glued("hello"), glued("world")),
               RegexPatternMatcher{}));
  InputDiags ctx{Input{input}};
  size_t i = 0;
  optional<BracketGroup> bgopt = lexBracketGroup(ctx,i);

  assertEqual("stPos in bracketGroupSuccess()", bgopt->stPos, size_t{0});
  assertEqual("enPos in bracketGroupSuccess()", bgopt->enPos, input.size());

  if(bgopt.has_value() && ctx.diags.empty()) {
    if(auto err = matcher::match(expected, std::move(*bgopt)))
      BugMe("Failed: {}", *err);
  }else {
    for(const auto& d:ctx.diags) print(stderr, "{}\n", string(d));
    BugMe("Failed");
  }
}

string debugMatcher(BracketType bt) {
  switch(bt) {
    case BracketType::square: return "squareBrackets";
    case BracketType::brace: return "braces";
    case BracketType::paren: return "parens";
    default: return format("Unknown BracketType {}", int(bt));
  }
}

void debug(fmt::memory_buffer& buf, const BracketGroup& bg);

void debug(fmt::memory_buffer& buf, const ExprToken& expr) {
  back_insert_iterator buf_app{buf};
  if(const auto* tok = get_if<WholeSegment>(&expr)) {
    format_to(buf_app, "{}", tok->data);
    return;
  }
  if(const auto* qs = get_if<GluedString>(&expr)) {
    format_to(buf_app, "glued({})", string_view(*qs));
    return;
  }
  debug(buf, get<BracketGroup>(expr));
}

void debug(fmt::memory_buffer& buf, const BracketGroup& bg) {
  back_insert_iterator buf_app{buf};
  format_to(buf_app, "{}(", debugMatcher(bg.type));
  bool first_child = true;
  for(const ExprToken& x : bg.children) {
    if(!first_child) format_to(buf_app, ", ");
    first_child = false;
    debug(buf, x);
  }
}

string debug(const BracketGroup& bg) {
  fmt::memory_buffer buf;
  debug(buf, bg);
  return fmt::to_string(buf);
}

void bracketGroupFailureImpl(const char testName[], string input,
    optional<string> expectedDiag) {
  InputDiags ctx{Input{input}};
  size_t i = 0;
  optional<BracketGroup> bgopt = lexBracketGroup(ctx,i);
  if(!expectedDiag.has_value()) {
    if(bgopt.has_value())
      BugMe("Was expecting nullopt, got {}", debug(*bgopt));
    return;
  }
  assertHasDiagWithSubstr(testName, ctx.diags, *expectedDiag);
}

void bracketGroupThrows(string input, string expectedDiag) {
  assertProducesDiag(__func__, input, expectedDiag,
                     OALEX_VOIDIFY(lexBracketGroup));
}

void newlinePositionIsCorrect() {
  InputDiags ctx{Input{R"(12345"foo\n")"}};
  size_t i = string_view("12345").size();
  size_t j = string_view("12345\"foo").size();
  if(ctx.input().substr(j,2) != "\\n")
    BugMe("Input doesn't have '\\n' where expected");

  size_t temp = i;
  optional<GluedString> s = lexQuotedString(ctx, temp);
  if(!s.has_value()) BugMe("Couldn't parse string literal");
  if((*s)[j-i-1] != '\n')
    BugMe("Parsed literal doesn't have '\\n' at position {}: '{}'",
          j-i-1, string_view(*s));

  NewlineChar ch(*s, j-i-1);
  if(ch.stPos != j)
    BugMe("Newline was recorded at the wrong position: {} != {}", ch.stPos, j);
}

const char goodLine[] = R"(
# Comment to be skipped

var := "Hello!"
more stuff ignored
)";

const char goodLineWithBrackets[] = R"(
  output: {
    key1: "value1",
    key2: "value2"
  }
  something else)";

const char badLine[] = R"(
var "unfinished

)";

void testSkipBlankLines() {
  const tuple<string_view, size_t, size_t> test_cases[] = {
    {"Foo bar", 0, 0},
    {"\n\n  Foo bar", 0, 4},
    {"foo bar", 3, string::npos},
    {"line1 \n  line2", 5, 9},
  };
  size_t test_index = 0;
  for(auto& [msg, inpos, expected]: test_cases) {
    test_index++;
    InputDiags ctx{Input{msg}};
    size_t observed = skipBlankLines(ctx, inpos);
    assertEqual(format("{} test case {}", __func__, test_index),
                observed, expected);
  }
}

// Failed lexQuotedString() commits if input starts with a quote character,
// and tries pretty hard to recover. In those cases, we should not try
// an alternate parsing assuming the cursor is still unchanged.
const char badEagerRecovery[] = "\"\\x0\"die\n";

const char invalidCharInput[] = "hello \x01 world";

void nextLineSuccessImpl(
    string_view testInput, string_view testName,
    vector<ExprMatcher> expectedResult) {
  InputDiags ctx{Input{testInput}};
  size_t pos = 0;
  vector<ExprToken> observedResult = lexNextLine(ctx, pos);
  if(observedResult.empty()) {
    showDiags(ctx.diags);
    Bug("{} failed: Couldn't process input:\n{}", testName, testInput);
  }
  if(observedResult.size() != expectedResult.size())
    Bug("{} failed: output size unexpected: {} != {}", testName,
        observedResult.size(), expectedResult.size());
  assertEqual(__func__, pos, ctx.input().bol(pos));
  for(size_t i=0; i<observedResult.size(); ++i) {
    if(auto err = matcher::match(expectedResult[i], observedResult.at(i)))
      Bug("{} failed at result index {}: {}", testName, i, *err);
  }
}

void nextLineFailureImpl(
    const char testInput[], const char testName[],
    string_view expectedDiag) {
  assertProducesDiag(testName, testInput, expectedDiag,
                     OALEX_VOIDIFY(lexNextLine));
}

void lexListEntriesSuccess() {
  const string input = R"(
    | a b c -> list entry 1
    | d e f -> list entry 2 | (
        things continued
        in brackets
      )
      | indented bullets
    | g h i
    all but ignored in the end)";
  const vector<vector<ExprMatcher>> expected{
    matchvec("|", "a", "b", "c", "->", "list", "entry", "1"),
    matchvec("|", "d", "e", "f", "->", "list", "entry", "2", "+",
             parens("things", "continued", "in", "brackets"),
             "|", "indented", "bullets"),
    matchvec("|", "g", "h", "i"),
  };
  InputDiags ctx{Input{input}};
  size_t i = ctx.input().find('\n', 0) + 1;
  vector<vector<ExprToken>> observed = lexListEntries(ctx, i, '|');
  if(observed.empty() || !ctx.diags.empty()) {
    showDiags(ctx.diags);
    BugMe("lexListEntries failed");
  }
  assertEqual("lexListEntries end position", input.substr(i, 11),
                                             "    all but"s);
  assertEqual(__func__, observed.size(), expected.size());
  for(size_t j = 0; j < observed.size(); ++j) {
    assertEqual(__func__, expected[j].size(), observed[j].size());
    for(size_t k = 0; k < observed.size(); ++k)
      if(auto err = matcher::match(expected[j][k], observed[j][k]))
        BugMe("Failed: {}", *err);
  }
}

void lexListEntriesFailure(string_view testName, string input,
                           string_view expectedDiag) {
  InputDiags ctx{Input{input}};
  size_t i = ctx.input().find('\n', 0) + 1;
  vector<vector<ExprToken>> observed = lexListEntries(ctx, i, '|');
  if(expectedDiag.empty()) {
    if(!observed.empty()) BugMe("Was expecting empty result");
    return;
  }
  assertHasDiagWithSubstrOnce(testName, ctx.diags, expectedDiag);
}

void testIndentCmp() {
  assertEqual(__func__ + string("lt"), indentCmp("\t ", "\t  "), IndentCmp::lt);
  assertEqual(__func__ + string("eq"), indentCmp("\t ", "\t "), IndentCmp::eq);
  assertEqual(__func__ + string("gt"), indentCmp("\t  ", "\t "), IndentCmp::gt);
  assertEqual(__func__ + string("bad"), indentCmp("\t ", " \t"),
                                        IndentCmp::bad);
}

}  // namespace

#define headerSuccess(test, expected) \
  headerSuccessImpl(test, #test "()", expected)
#define headerFailure(test, expectedDiag) \
  headerFailureImpl(test, #test, expectedDiag)
#define stringSuccess(test, expected) \
  stringSuccessImpl(test, #test "()", expected)
#define stringFailure(test, expected) \
  stringFailureImpl(test, #test "()", expected)
#define fencedSourceBlockSuccess(test) \
  fencedSourceBlockSuccessImpl(test, #test "()")
#define fencedSourceBlockFailure(test, expected) \
  fencedSourceBlockFailureImpl(test, #test "()", expected)
#define indentedSourceBlockSuccess(test, expected) \
  indentedSourceBlockSuccessImpl(test, #test, expected)
#define indentedSourceBlockFailure(test, expected) \
  indentedSourceBlockFailureImpl(test, #test, expected)
#define bracketGroupFailure(test, input, expected) \
  bracketGroupFailureImpl("bracketGroupFailure" #test, input, expected)
#define nextLineSuccess(test, expected) \
  nextLineSuccessImpl(test, #test, expected)
#define nextLineFailure(test, expected) \
  nextLineFailureImpl(test, #test, expected)

int main() {
  headerSuccess(goodHeader1, (vector<string>{"Header", "at", "top"}));
  headerSuccess(goodHeader2, (vector<string>{"Header", "in", "the", "middle"}));
  headerFailure(headerWithComma, "");
  headerFailure(headerWithExtraBlank, "");
  headerFailure(headerIndented, "Section headers must not be indented");
  headerFailure(headerDashIndented,
      "Dashes in a section header must not be indented");

  stringSuccess(goodString, "Hello world");
  stringSuccess(goodStringWithEscapes, "Hello world \n \" \t \\ A");
  stringFailure(stringDoesntEnd, "Unexpected end of line");
  stringFailure(multiLineString, "Unexpected end of line");
  stringFailure(incompleteEscape, "Incomplete escape");
  stringFailure(invalidEscape, "Invalid escape");
  stringFailure(incompleteHex, "Incomplete hex");
  stringFailure(invalidHex, "Invalid hex");
  stringPosMap();

  getSegmentTest(R"("Hello world")", true);
  getSegmentTest(R"("Hello\nworld")", false);

  fencedSourceBlockSuccess(fencedSourceBlock);
  fencedSourceBlockSuccess(fencedSourceBlockCustom);
  fencedSourceBlockFailure(badFencedStart, "Fences must be alphanumeric");

  indentedSourceBlockSuccess(goodIndent, goodIndentParsed);
  indentedSourceBlockSuccess(allBlank, nullopt);
  indentedSourceBlockFailure(tabSpaceMix, "mixes tabs and spaces");

  lookaheadsSuccess();
  lookaheadNulloptOnEof();
  lookaheadThrowsOnInvalidChar();

  bracketGroupSuccess();
  bracketGroupFailure(Eof,"",nullopt);
  bracketGroupFailure(NotBracket,"ABC",nullopt);
  bracketGroupFailure(Unmatched,"[abc","Match not found for '['.");
  bracketGroupFailure(BadString,R"({"abc\xt" ab})", "Invalid hex code");
  bracketGroupFailure(Mismatched,"[abc)",
      "Match not found for '[', found ')' instead.");
  bracketGroupFailure(BadRegex,"{var :: /(a-d/}", "Unmatched '('");
  bracketGroupThrows("@","Unexpected character '@'");

  newlinePositionIsCorrect();

  testSkipBlankLines();

  nextLineSuccess(goodLine, (vector<ExprMatcher>{
        "var", ":=", glued("Hello!")}));
  nextLineSuccess(goodLineWithBrackets,(vector<ExprMatcher>{
        "output", ":", braces("key1", ":", glued("value1"), ",",
                              "key2", ":", glued("value2"))}));
  nextLineFailure(badLine, "Unexpected end of line");
  nextLineFailure(badEagerRecovery, "Invalid hex code");
  nextLineFailure(invalidCharInput, "Unexpected character");

  lexListEntriesSuccess();
  lexListEntriesFailure("Eof", "", "");
  lexListEntriesFailure("Offside", R"(
    | pancake [
    whipped cream ] syrup
    )", "Needs to be indented further");
  lexListEntriesFailure("Bad indent", "\n  | pancake\n \t| bacon\n",
                        "mixes tabs and spaces");

  testIndentCmp();
}
