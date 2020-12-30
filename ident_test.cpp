#include"ident.h"
#include<set>
#include<unordered_set>
#include<string_view>
#include"runtime/test_util.h"
#include"runtime/diags_test_util.h"
using namespace oalex;
using namespace std;

namespace {

Ident fromString(string_view testName, string_view input) {
  InputDiags ctx{Input{string(input)}};
  size_t i = 0;
  auto rv = Ident::parse(ctx, i);
  assertEmptyDiags(testName, ctx.diags);
  if(i < input.size()) Bug("{}: '{}' has trailing chars: '{}'", testName,
                           input, input.substr(i));
  return rv;
}

void testParseErrors() {
  pair<string,string> tests[] = {
    {string(101,'x'), "Identifier too long"},
    {"_", "must have a digit or letter"},
    {"_a", "leading"},
    {"a_", "trailing"},
    {"foo__bar", "Consecutive"},
    {"", "must have a digit or letter"},
    {"1world", "cannot start with a digit"},
  };
  for(const auto& [s,err] : tests) {
    assertProducesDiag(__func__, s, err, OALEX_VOIDIFY(Ident::parse));
  }
}

void testStopsAtTrail() {
  InputDiags ctx{Input{"foo-"}};
  size_t i = 0;
  auto rv = Ident::parse(ctx, i);
  assertEmptyDiags(__func__, ctx.diags);
  if(i != 3) BugMe("Ident parsing of 'foo-' was expected to end "
                   "at position 3, not {}", i);
}

void testEqualities() {
  pair<string,string> equals[] = {
    {"foo_bar", "fooBar"},
    {"foo_bar_42", "fooBar42"},
    {"bitEnum", "biteNum"},
  };
  for(const auto& [a,b] : equals) {
    Ident ai = fromString(__func__, a), bi = fromString(__func__, b);
    if(ai != bi)
      BugMe("Identifiers {} and {} don't compare equal.", a, b);
  }
  pair<string,string> not_equals[] = {
    {"foo_barx", "fooBar"},
    {"foo_bar_42", "fooBar4"},
  };
  for(const auto& [a,b] : not_equals) {
    Ident ai = fromString(__func__, a), bi = fromString(__func__, b);
    if(!ai || !bi) BugMe("Ident failed to parse");
    if(ai == bi)
      BugMe("Identifiers {} and {} weren't expected to compare equal.", a, b);
  }
}

void testCaseChange() {
  tuple<string, string, string, string> tests[] = {
    {"foo", "foo", "Foo", "foo"},
    {"foo_bar", "foo_bar", "FooBar", "fooBar"},
    {"fooBar", "foo_bar", "FooBar", "fooBar"},
    {"FOO_BAR", "foo_bar", "FooBar", "fooBar"},
    {"toLCase", "to_l_case", "ToLCase", "toLCase"},
    {"me42you", "me_42_you", "Me42You", "me42You"},
  };
  for(const auto& [a,snake,ucamel,lcamel] : tests) {
    Ident ai = fromString(__func__, a);
    if(!ai) BugMe("Ident failed to parse");
    string snakeo = ai.toSnakeCase();
    if(snakeo != snake)
      BugMe("Snake case failed. {} became {} instead of {}",
            a, snakeo, snake);
    string ucamelo = ai.toUCamelCase();
    if(ucamelo != ucamel)
      BugMe("Upper case failed. {} became {} instead of {}",
            a, ucamelo, ucamel);
    string lcamelo = ai.toLCamelCase();
    if(lcamelo != lcamel)
      BugMe("Lower camel case failed. {} became {} instead of {}",
            a, lcamelo, lcamel);
    string a2 = ai.preserveCase();
    if(a != a2)
      BugMe("preserveCase() is no longer preserves input: '{}' != '{}'",
            a, a2);
  }
}

void testSet() {
  auto id = [](string_view s) { return fromString("testSet()", s); };
  set<Ident> s{id("foo"), id("fooBar"), id("fooBaz")};
  if(s.insert(id("foo_bar")).second)
    BugMe("std::set was expected to treat foo_bar and fooBar as equal");
  if(!s.insert(id("food")).second)
    BugMe("std::set failed to insert new key");

  unordered_set<Ident> us{id("foo"), id("fooBar"), id("fooBaz")};
  if(us.insert(id("foo_bar")).second)
    BugMe("std::unordered_set was expected to treat "
             "foo_bar and fooBar as equal");
  if(!us.insert(id("food")).second)
    BugMe("std::unordered_set failed to insert new key");
}

}  // namespace

int main() {
  testParseErrors();
  testStopsAtTrail();
  testEqualities();
  testCaseChange();
  testSet();
}
