#include "lexer.h"

#include <string>
#include <string_view>

#include "test_util.h"
#include "util.h"
using std::cerr;
using std::endl;
using std::nullopt;
using std::optional;
using std::string;
using std::string_view;
using std::vector;
using namespace std::string_literals;
using oalex::operator<<;
using oalex::BugDie;
using oalex::GetFromString;
using oalex::Input;
using oalex::UserErrorEx;
using oalex::lex::Diag;
using oalex::lex::Lexer;
using oalex::lex::QuotedString;
using oalex::lex::UnquotedToken;
using oalex::lex::lexSectionHeader;

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
  Lexer lex{Input(GetFromString(testInput)),{}};
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

bool isSubstr(string_view s, string_view t) {
  return t.find(s) != string::npos;
}

void assertHasDiagWithSubstr(const char testName[], const vector<Diag>& diags,
                             string_view expectedDiag) {
  if(expectedDiag.empty()) return;  // Test succeeds even if we have no diags.
  for(const Diag& d : diags) if(isSubstr(expectedDiag, d.msg)) return;
  cerr<<"Got diags:\n";
  for(const Diag& d : diags) cerr<<" "<<string(d)<<endl;
  BugDie()<<testName<<" didn't get the expected diag: "<<expectedDiag;
}

void headerFailureImpl(const char testInput[], const char testName[],
    const string& expectedDiag) {
  Lexer lex{Input(GetFromString(testInput)),{}};
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
  Lexer lex{Input(GetFromString(testInput)),{}};
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
  Lexer lex{Input(GetFromString(testInput)),{}};
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

  Lexer lex{Input(GetFromString(testInput)),{}};
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
  Lexer lex{Input(GetFromString(testInput)),{}};
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
  Lexer lex{Input(GetFromString(testInput)),{}};
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
    Lexer lex{Input(GetFromString(inputs[i])),{}};
    if(optional<UnquotedToken> tok = lookahead(lex,1)) {
      if(tok->token!=expecteds[i])
        BugMe<<"Test case "<<i<<" failed with \""
             <<tok->token<<"\" != \""<<expecteds[i]<<'"';
    }else BugMe<<"Test case "<<i<<" was supposed to succeed";
  }
}

void lookaheadNulloptOnEof() {
  string input = "foo     # hello \n\t\n";
  Lexer lex{Input(GetFromString(input)),{}};
  if(optional<UnquotedToken> tok = lookahead(lex,3))
    BugMe<<"Succeeded unexpectedly, got "<<tok->token;
}

void lookaheadThrowsOnInvalidChar() {
  string input = "\b";
  Lexer lex{Input(GetFromString(input)),{}};
  try {
    auto tok = lookahead(lex,0);
    BugMe<<"Succeeded unexpectedly, got "<<(tok?tok->token:"<nullopt>"s);
  }catch(const UserErrorEx& ex) {
    if(string(ex.what()).find("Unexpected character") == string::npos)
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
}
