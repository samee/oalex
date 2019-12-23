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

#include "diags_test_util.h"
#include "test_util.h"
#include "runtime/util.h"
using std::cerr;
using std::endl;
using std::get;
using std::get_if;
using std::nullopt;
using std::optional;
using std::ostream;
using std::ostringstream;
using std::string;
using std::string_view;
using std::vector;
using namespace std::string_literals;
using oalex::operator<<;
using oalex::BugDie;
using oalex::Diag;
using oalex::GetFromString;
using oalex::Input;
using oalex::InputDiags;
using oalex::Str;
using oalex::UserErrorEx;
using oalex::lex::BracketGroup;
using oalex::lex::BracketType;
using oalex::lex::ExprToken;
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
----------   # comments)";

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
  InputDiags lex{Input(GetFromString(testInput)),{}};
  size_t i = 0;
  optional<vector<UnquotedToken>> res = lexSectionHeader(lex, i);
  if(!res || !lex.diags.empty()) {
    for(const auto& d:lex.diags) cerr<<string(d)<<endl;
    BugDie()<<testName<<" failed";
  }else {
    vector<string> observed;
    for(const UnquotedToken& t : *res) observed.push_back(*t);
    if(expected != observed)
      BugDie()<<testName<<": "<<expected<<" != "<<observed;
  }
}

void headerFailureImpl(const char testInput[], const char testName[],
    const string& expectedDiag) {
  InputDiags lex{Input(GetFromString(testInput)),{}};
  size_t i = 0;
  optional<vector<UnquotedToken>> res = lexSectionHeader(lex, i);
  if(res && lex.diags.empty())
    BugDie()<<"Test "<<testName<<" succeeded unexpectedly";
  assertHasDiagWithSubstr(testName, lex.diags, expectedDiag);
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
  InputDiags lex{Input(GetFromString(testInput)),{}};
  size_t i = 0;
  optional<QuotedString> res = lexQuotedString(lex, i);
  if(!res || !lex.diags.empty()) {
    for(const auto& d:lex.diags) cerr<<string(d)<<endl;
    BugDie()<<testName<<" failed";
  }else {
    if(expected != res->s)
      BugDie()<<testName<<": "<<expected<<" != "<<res->s;
  }
}

void stringFailureImpl(const char testInput[], const char testName[],
    string_view expectedDiag) {
  InputDiags lex{Input(GetFromString(testInput)),{}};
  size_t i = 0;
  optional<QuotedString> res = lexQuotedString(lex, i);
  if(res && lex.diags.empty())
    BugDie()<<"Test "<<testName<<" succeeded unexpectedly";
  assertHasDiagWithSubstr(testName, lex.diags, expectedDiag);
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

size_t delimSize(string_view s) { return s.find('\n'); }

void delimSourceBlockSuccessImpl(string_view testInput, const char testName[]) {
  // substr() counts the newline after the starting delimitter.
  size_t dsize = delimSize(testInput);
  string_view expected = testInput.substr(dsize+1, testInput.size()-2*dsize-1);

  InputDiags lex{Input(GetFromString(testInput)),{}};
  size_t i = 0;
  optional<QuotedString> res = lexDelimitedSource(lex, i);
  if(!res || !lex.diags.empty()) {
    for(const auto& d:lex.diags) cerr<<string(d)<<endl;
    BugDie()<<testName<<" failed";
  }else {
    if(expected != res->s) {
      oalex::Debug()<<expected.size()<<" "<<res->s.size();
      BugDie()<<testName<<": "<<expected<<" != "<<res->s;
    }
  }
}

const char goodIndent[] = R"(
  foo

  bar:
    baz
 outside
)";
const char goodIndentParsed[] = "\nfoo\n\nbar:\n  baz\n";

const char allBlank[] = "\n  \n\t\n";

const char noTrailingNewline[] = "  foo";
const char noTrailingNewlineParsed[] = "foo\n";

void indentedSourceBlockSuccessImpl(
    const char testInput[], const char testName[],
    optional<string> expectedResult) {
  InputDiags lex{Input(GetFromString(testInput)),{}};
  size_t i = 0;
  optional<QuotedString> res = lexIndentedSource(lex, i, "  ");
  if(res.has_value() != expectedResult.has_value() || !lex.diags.empty()) {
    for(const auto& d:lex.diags) cerr<<string(d)<<endl;
    BugDie()<<testName<<" failed";
  }
  if(res.has_value() && res->s != expectedResult)
    BugDie()<<testName<<" '"<<res->s<<"' != '"<<*expectedResult<<"'";
}

const char tabSpaceMix[] = "  foo\n\tbar";

void indentedSourceBlockFailureImpl(
    const char testInput[], const char testName[],
    string_view expectedDiag) {
  InputDiags lex{Input(GetFromString(testInput)),{}};
  size_t i = 0;
  optional<QuotedString> res = lexIndentedSource(lex, i, "  ");
  if(res && lex.diags.empty())
    BugDie()<<"Test "<<testName<<" succeeded unexpectedly";
  assertHasDiagWithSubstr(testName, lex.diags, expectedDiag);
}

void lookaheadsSuccess() {
  string inputs[] = {"   \n foo bar", "x  foo", "  []", "  ..., x", "  :"};
  string expecteds[] = {"foo","foo","[","...",":"};
  static_assert(sizeof(inputs)==sizeof(expecteds));
  for(size_t i=0; i<sizeof(inputs)/sizeof(*inputs); ++i) {
    InputDiags lex{Input(GetFromString(inputs[i])),{}};
    if(optional<UnquotedToken> tok = lookahead(lex,1)) {
      if(tok->token!=expecteds[i])
        BugMe<<"Test case "<<i<<" failed with \""
             <<tok->token<<"\" != \""<<expecteds[i]<<'"';
    }else BugMe<<"Test case "<<i<<" was supposed to succeed";
  }
}

void lookaheadNulloptOnEof() {
  string input = "foo     # hello \n\t\n";
  InputDiags lex{Input(GetFromString(input)),{}};
  if(optional<UnquotedToken> tok = lookahead(lex,3))
    BugMe<<"Succeeded unexpectedly, got "<<tok->token;
}

void lookaheadThrowsOnInvalidChar() {
  string input = "\b";
  InputDiags lex{Input(GetFromString(input)),{}};
  try {
    auto tok = lookahead(lex,0);
    BugMe<<"Succeeded unexpectedly, got "<<(tok?tok->token:"<nullopt>"s);
  }catch(const UserErrorEx& ex) {
    if(string(ex.what()).find("Unexpected character") == string::npos)
      BugMe<<"Not the expected Fatal() error: "<<ex.what();
  }
}

// Within brackets, we ignore all indentation.
void bracketGroupSuccess() {
  const string input = "{A := (B.C word [#comment\n\"hello\" \"world\"])}";
  const BracketGroupMatcher expected = braces("A", ":=",
        parens("B", ".", "C", "word",
               squareBrackets(quoted("hello"), quoted("world"))));
  InputDiags lex{Input(GetFromString(input)),{}};
  size_t i = 0;
  optional<BracketGroup> bgopt = lexBracketGroup(lex,i);

  if(bgopt.has_value() && lex.diags.empty()) {
    if(auto err = matcher::match(expected,*bgopt))
      BugMe<<"Failed: "<<*err;
  }else {
    for(const auto& d:lex.diags) cerr<<string(d)<<endl;
    BugMe<<"Failed";
  }
}

string debugMatcher(BracketType bt) {
  switch(bt) {
    case BracketType::square: return "squareBrackets";
    case BracketType::brace: return "braces";
    case BracketType::paren: return "parens";
    default: return Str()<<"Unknown BracketType "<<int(bt);
  }
}

void debug(ostream& os,const ExprToken& expr) {
  if(const auto* tok = get_if<UnquotedToken>(&expr)) {
    os<<tok->token;
    return;
  }
  if(const auto* qs = get_if<QuotedString>(&expr)) {
    os<<"quoted("<<qs->s<<")";
    return;
  }
  const BracketGroup& bg = get<BracketGroup>(expr);
  os<<debugMatcher(bg.type)<<'(';
  bool first_child = true;
  for(const ExprToken& x : bg.children) {
    if(!first_child) os<<", ";
    first_child = false;
    debug(os, x);
  }
}

string debug(const BracketGroup& bg) {
  ostringstream os;
  debug(os, bg);
  return os.str();
}

void bracketGroupFailureImpl(const char testName[], string input,
    optional<string> expectedDiag) {
  InputDiags lex{Input(GetFromString(input)),{}};
  size_t i = 0;
  optional<BracketGroup> bgopt = lexBracketGroup(lex,i);
  if(!expectedDiag.has_value()) {
    if(bgopt.has_value())
      cerr<<testName<<": Was expecting nullopt, got "<<debug(*bgopt);
    return;
  }
  assertHasDiagWithSubstr(testName, lex.diags, *expectedDiag);
}

void bracketGroupThrows(string input, string expectedDiag) {
  InputDiags lex{Input(GetFromString(input)),{}};
  size_t i = 0;
  try {
    auto bg = lexBracketGroup(lex,i);
    BugMe<<"Succeeded unexpectedly, got "<<(bg?debug(*bg):"nullopt");
  }catch(const UserErrorEx& ex) {
    if(string(ex.what()).find(expectedDiag) == string::npos)
      BugMe<<"Not the expected Fatal() error: "<<ex.what();
  }
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

  delimSourceBlockSuccess(delimSourceBlock);
  delimSourceBlockSuccess(delimSourceBlockCustom);

  indentedSourceBlockSuccess(goodIndent, goodIndentParsed);
  indentedSourceBlockSuccess(allBlank, nullopt);
  indentedSourceBlockSuccess(noTrailingNewline, noTrailingNewlineParsed);
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
}
