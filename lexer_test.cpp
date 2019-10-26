#include "lexer.h"
#include "test_util.h"
#include "util.h"
using std::cerr;
using std::endl;
using std::optional;
using std::string;
using std::vector;
using oalex::operator<<;
using oalex::BugDie;
using oalex::GetFromString;
using oalex::Input;
using oalex::lex::AlnumToken;
using oalex::lex::Diag;
using oalex::lex::Lexer;
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
  Not a header   # Each line has a space
--------------
)";

const char headerDashIndented[] = R"(
Not a header   # Each line has a space
  ------------
)";

void testSuccessImpl(const char testInput[], const char testName[],
    vector<string> expected) {
  Lexer lex{Input(GetFromString(testInput)),{}};
  size_t i = 0;
  optional<vector<AlnumToken>> res = lexSectionHeader(lex, i);
  if(!res || !lex.diags.empty()) {
    for(const auto& d:lex.diags) cerr<<string(d)<<endl;
    BugDie()<<testName<<" failed";
  }else {
    vector<string> observed;
    for(const AlnumToken& t : *res) observed.push_back(*t);
    if(expected != observed)
      BugDie()<<testName<<": "<<expected<<" != "<<observed;
  }
}

bool isSubstr(const string& s, const string& t) {
  return t.find(s) != string::npos;
}

void testFailureImpl(const char testInput[], const char testName[],
    const string& expectedDiag) {
  Lexer lex{Input(GetFromString(testInput)),{}};
  size_t i = 0;
  optional<vector<AlnumToken>> res = lexSectionHeader(lex, i);
  if(res && lex.diags.empty())
    BugDie()<<"Test "<<testName<<" succeeded unexpectedly";
  if(expectedDiag.empty()) return;  // Test succeeds even if we have no diags.
  for(const Diag& d : lex.diags) {
    if(isSubstr(expectedDiag, d.msg)) return;
  }
  cerr<<"Got diags:\n";
  for(const Diag& d : lex.diags) cerr<<"  "<<string(d)<<endl;
  BugDie()<<testName<<" didn't get the expected diag: "<<expectedDiag;
}

}  // namespace

#define testSuccess(test, expected) testSuccessImpl(test, #test, expected)
#define testFailure(test, expectedDiag) \
  testFailureImpl(test, #test, expectedDiag)

int main() {
  testSuccess(goodHeader1, (vector<string>{"Header", "at", "top"}));
  testSuccess(goodHeader2, (vector<string>{"Header", "in", "the", "middle"}));
  testFailure(headerWithComma, "");
  testFailure(headerWithExtraBlank, "");
  testFailure(headerIndented, "Section headers must not be indented");
  testFailure(headerDashIndented,
      "Dashes in a section header must not be indented");
}
