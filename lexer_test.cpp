/*  Copyright 2019 Google LLC

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

#include <string>
#include <string_view>

#include "fmt/format.h"

#include "runtime/diags_test_util.h"
#include "runtime/test_util.h"
#include "runtime/util.h"
using fmt::format;
using fmt::print;
using std::endl;
using std::get;
using std::get_if;
using std::nullopt;
using std::optional;
using std::pair;
using std::string;
using std::string_view;
using std::vector;
using namespace std::string_literals;
using oalex::Bug;
using oalex::Diag;
using oalex::Input;
using oalex::InputDiags;
using oalex::UserErrorEx;
using oalex::lex::BracketGroup;
using oalex::lex::BracketType;
using oalex::lex::ExprToken;
using oalex::lex::NewlineChar;
using oalex::lex::QuotedString;
using oalex::lex::UnquotedToken;
using oalex::lex::lexBracketGroup;
using oalex::lex::lexDelimitedSource;
using oalex::lex::lexIndentedSource;
using oalex::lex::lexQuotedString;
using oalex::lex::lexSectionHeader;
using oalex::lex::lookahead;
using oalex::lex::matcher::braces;
using oalex::lex::matcher::BracketGroupMatcher;
using oalex::lex::matcher::parens;
using oalex::lex::matcher::quoted;
using oalex::lex::matcher::squareBrackets;
namespace matcher = oalex::lex::matcher;

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
  InputDiags ctx{testInputDiags(testInput)};
  size_t i = 0;
  optional<vector<UnquotedToken>> res = lexSectionHeader(ctx, i);
  if(!res || !ctx.diags.empty()) {
    for(const auto& d:ctx.diags) print(stderr, "{}\n", string(d));
    Bug("{} failed", testName);
  }else {
    vector<string> observed;
    for(const UnquotedToken& t : *res) observed.push_back(*t);
    if(expected != observed)
      Bug("{}: {} != {}", testName, expected, observed);
  }
}

void headerFailureImpl(const char testInput[], const char testName[],
    const string& expectedDiag) {
  InputDiags ctx{testInputDiags(testInput)};
  size_t i = 0;
  optional<vector<UnquotedToken>> res = lexSectionHeader(ctx, i);
  if(res && ctx.diags.empty())
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
const char incompleteHex[] = R"("Foo\xF)";
const char invalidHex[] = R"("\xag")";

void stringSuccessImpl(const char testInput[], const char testName[],
                       string_view expected) {
  InputDiags ctx{testInputDiags(testInput)};
  size_t i = 0;
  optional<QuotedString> res = lexQuotedString(ctx, i);
  if(!res || !ctx.diags.empty()) {
    for(const auto& d:ctx.diags) print(stderr, "{}\n", string(d));
    Bug("{} failed", testName);
  }else {
    if(expected != *res)
      Bug("{}: {} != {}", testName, expected, string_view(*res));
  }
}

void stringFailureImpl(const char testInput[], const char testName[],
                       string_view expectedDiag) {
  InputDiags ctx{testInputDiags(testInput)};
  size_t i = 0;
  optional<QuotedString> res = lexQuotedString(ctx, i);
  if(res && ctx.diags.empty())
    Bug("Test {} succeeded unexpectedly", testName);
  assertHasDiagWithSubstr(testName, ctx.diags, expectedDiag);
}

string debug(const pair<size_t,size_t>& p) {
  Bug("{}:{}", p.first, p.second);
}

template <class T>
void assertEq(string_view errmsg, const T& a, const T& b) {
  if(a != b) Bug("{}: {} != {}", errmsg, debug(a), debug(b));
}

void compareSubqstrIndexPos(const QuotedString& a, size_t pos, size_t len) {
  const QuotedString b = a.subqstr(pos, len);
  for(size_t i=0; i<=b.size(); ++i) if(a.inputPos(i+pos) != b.inputPos(i))
    Bug("Substring inputPos mismatch between '{}' and '{}': "
           "a.inputPos({}) = {}  !=  b.inputPos({}) = {}", string_view(a),
           string_view(b), i+pos, a.inputPos(i+pos), i, b.inputPos(i));
}

void stringPosMap() {
  const char testInput[] = R"("foo\nbar")";
  InputDiags ctx{testInputDiags(testInput)};
  size_t i = 0;
  optional<QuotedString> res = lexQuotedString(ctx, i);
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

const char delimSourceBlock[] = R"(```
I can write whatever here I want to. Including
``` as long as
it's not on the only thing on the line.
```)";

const char delimSourceBlockCustom[] = R"(```foo
Even this
```
Doesn't end the string
```foo)";

const char badDelimStart[] = "```foo#\n";

size_t delimSize(string_view s) { return s.find('\n'); }

void delimSourceBlockSuccessImpl(string_view testInput, const char testName[]) {
  // substr() counts the newline after the starting delimitter.
  size_t dsize = delimSize(testInput);
  string_view expected = testInput.substr(dsize+1, testInput.size()-2*dsize-1);

  InputDiags ctx{testInputDiags(testInput)};
  size_t i = 0;
  optional<QuotedString> res = lexDelimitedSource(ctx, i);
  if(!res || !ctx.diags.empty()) {
    for(const auto& d:ctx.diags) print("{}\n", string(d));
    Bug("{} failed", testName);
  }else {
    if(expected != *res) {
      Bug("{}: {} != {}", testName, expected, string_view(*res));
    }
  }

  size_t lastLineNumber = ctx.input.rowCol(i).first;

  if(res->rowCol(0).first != 2)
    Bug("{}: Was expecting delimted string body to start on "
        "the second line. Found it on {}. Input:\n{}",
        testName, res->rowCol(0).first, testInput);
  for(size_t i=0; i<res->size(); ++i) if(res->rowCol(i+1).second == 1) {
    if(res->rowCol(i).first+1 != res->rowCol(i+1).first)
      Bug("{}: Line numbers are not sequential "
          " at the start of line {}. Input:\n{}",
          testName, res->rowCol(i+1).first, testInput);
    auto expected = ctx.input.rowCol(i + 1 + dsize);
    if(res->rowCol(i) != expected)
      Bug("{}: Location info changed after parsing: "
          "{} became {}", testName, debug(expected), debug(res->rowCol(i)));
  }
  if(res->rowCol(res->size()) != pair(lastLineNumber, size_t(1)))
    Bug("{}: Was expecting the input to end on {}:1, instead found {}. "
        "Input\n", testName, lastLineNumber, debug(res->rowCol(res->size())),
        testInput);
}

void delimSourceBlockFailureImpl(const char testInput[], const char testName[],
    string_view expectedDiag) {
  assertProducesDiag(testName, testInput, expectedDiag, lexDelimitedSource);
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
  InputDiags ctx{testInputDiags(testInput)};
  size_t i = 0;
  optional<QuotedString> res = lexIndentedSource(ctx, i, "  ");
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
  InputDiags ctx{testInputDiags(testInput)};
  size_t i = 0;
  optional<QuotedString> res = lexIndentedSource(ctx, i, "  ");
  if(res && ctx.diags.empty())
    Bug("Test {} succeeded unexpectedly", testName);
  assertHasDiagWithSubstr(testName, ctx.diags, expectedDiag);
}

void lookaheadsSuccess() {
  string inputs[] = {"   \n foo bar", "x  foo", "  []", "  ..., x", "  :"};
  string expecteds[] = {"foo","foo","[","...",":"};
  static_assert(sizeof(inputs)==sizeof(expecteds));
  for(size_t i=0; i<sizeof(inputs)/sizeof(*inputs); ++i) {
    InputDiags ctx{testInputDiags(inputs[i])};
    if(optional<UnquotedToken> tok = lookahead(ctx,1)) {
      if(tok->token!=expecteds[i])
        BugMe("Test case {} failed with \"{}\" != \"{}\"", i,
              tok->token, expecteds[i]);
    }else BugMe("Test case {} was supposed to succeed", i);
  }
}

void lookaheadNulloptOnEof() {
  string input = "foo     # hello \n\t\n";
  InputDiags ctx{testInputDiags(input)};
  if(optional<UnquotedToken> tok = lookahead(ctx,3))
    BugMe("Succeeded unexpectedly, got {}", tok->token);
}

void lookaheadThrowsOnInvalidChar() {
  string input = "\b";
  InputDiags ctx{testInputDiags(input)};
  try {
    auto tok = lookahead(ctx,0);
    BugMe("Succeeded unexpectedly, got {}", (tok?tok->token:"<nullopt>"s));
  }catch(const UserErrorEx& ex) {
    if(string(ex.what()).find("Unexpected character") == string::npos)
      BugMe("Not the expected Fatal() error: {}", ex.what());
  }
}

// Within brackets, we ignore all indentation.
void bracketGroupSuccess() {
  const string input = "{A := (B.C word [#comment\n\"hello\" \"world\"])}";
  const BracketGroupMatcher expected = braces("A", ":=",
        parens("B", ".", "C", "word",
               squareBrackets(quoted("hello"), quoted("world"))));
  InputDiags ctx{testInputDiags(input)};
  size_t i = 0;
  optional<BracketGroup> bgopt = lexBracketGroup(ctx,i);

  if(bgopt.has_value() && ctx.diags.empty()) {
    if(auto err = matcher::match(expected,*bgopt))
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

void debug(fmt::memory_buffer& buf, const ExprToken& expr) {
  if(const auto* tok = get_if<UnquotedToken>(&expr)) {
    format_to(buf, "{}", tok->token);
    return;
  }
  if(const auto* qs = get_if<QuotedString>(&expr)) {
    format_to(buf, "quoted({})", string_view(*qs));
    return;
  }
  const BracketGroup& bg = get<BracketGroup>(expr);
  format_to(buf, "{}(", debugMatcher(bg.type));
  bool first_child = true;
  for(const ExprToken& x : bg.children) {
    if(!first_child) format_to(buf, ", ");
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
  InputDiags ctx{testInputDiags(input)};
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
  InputDiags ctx{testInputDiags(input)};
  size_t i = 0;
  try {
    auto bg = lexBracketGroup(ctx,i);
    BugMe("Succeeded unexpectedly, got {}", (bg?debug(*bg):"nullopt"));
  }catch(const UserErrorEx& ex) {
    if(string(ex.what()).find(expectedDiag) == string::npos)
      BugMe("Not the expected Fatal() error: {}", ex.what());
  }
}

void newlinePositionIsCorrect() {
  InputDiags ctx{testInputDiags(R"(12345"foo\n")")};
  size_t i = string_view("12345").size();
  size_t j = string_view("12345\"foo").size();
  if(ctx.input.substr(j,2) != "\\n")
    BugMe("Input doesn't have '\\n' where expected");

  size_t temp = i;
  optional<QuotedString> s = lexQuotedString(ctx, temp);
  if(!s.has_value()) BugMe("Couldn't parse string literal");
  if((*s)[j-i-1] != '\n')
    BugMe("Parsed literal doesn't have '\\n' at position {}: '{}'",
          j-i-1, string_view(*s));

  NewlineChar ch(*s, j-i-1);
  if(ch.stPos != j)
    BugMe("Newline was recorded at the wrong position: {} != {}", ch.stPos, j);
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
#define delimSourceBlockSuccess(test) \
  delimSourceBlockSuccessImpl(test, #test "()")
#define delimSourceBlockFailure(test, expected) \
  delimSourceBlockFailureImpl(test, #test "()", expected)
#define indentedSourceBlockSuccess(test, expected) \
  indentedSourceBlockSuccessImpl(test, #test, expected)
#define indentedSourceBlockFailure(test, expected) \
  indentedSourceBlockFailureImpl(test, #test, expected)
#define bracketGroupFailure(test, input, expected) \
  bracketGroupFailureImpl("bracketGroupFailure" #test, input, expected)

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

  delimSourceBlockSuccess(delimSourceBlock);
  delimSourceBlockSuccess(delimSourceBlockCustom);
  delimSourceBlockFailure(badDelimStart, "Delimiters must be alphanumeric");

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
  bracketGroupFailure(Mismatched,"[abc)",
      "Match not found for '[', found ')' instead.");
  bracketGroupThrows("@","Unexpected character '@'");

  newlinePositionIsCorrect();
}
