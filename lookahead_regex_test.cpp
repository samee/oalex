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
using oalex::regex::Regex;
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

void concat_helper(unique_ptr<Concat>&) {}

template <class T, class ... Ts>
void concat_helper(unique_ptr<Concat>& out, T t, Ts ... ts) {
  out->parts.push_back(std::move(t));
  concat_helper(out, std::move(ts)...);
}

template <class ... Ts>
auto concat(Ts ... ts) -> unique_ptr<Concat> {
  auto rv = make_unique<Concat>();
  concat_helper(rv, std::move(ts)...);
  return rv;
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
    {negatedSet({{CharRange{'a','z'},CharRange{'@','@'}}}),"/[^a-z@]/"},
    {negatedSet({{CharRange{'^','^'},CharRange{'a','z'}}}),"/[^^a-z]/"},
    {concat(charSingle('a'), charSingle('b'), charSingle('c')), "/[a][b][c]/"},
    {concat(charSingle('a'), concat(charSingle('b'), charSingle('c'))),
      "/[a]([b][c])/"},
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
    "/[abc][def][ghi]/", "/[a]([b][c])/"
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

}  // namespace

int main() {
  testPrettyPrint();
  testParseAndPrint();
  testParseDiags();
}
