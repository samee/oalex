#include"ident.h"
#include<set>
#include<unordered_set>
#include<string_view>
#include"runtime/diags_test_util.h"
using namespace oalex;
using namespace std;

namespace {

Ident fromString(string_view testName, string_view input) {
  InputDiags ctx{Input{string(input)}};
  size_t i = 0;
  auto rv = Ident::parse(ctx, i);
  assertEmptyDiags(testName, ctx.diags);
  if(i < input.size())
    Bug()<<testName<<": '"<<input<<"' has trailing chars: '"
         <<input.substr(i)<<"'";
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
    assertProducesDiag(__func__, s, err, Ident::parse);
  }
}

void testStopsAtTrail() {
  InputDiags ctx{Input{"foo-"}};
  size_t i = 0;
  auto rv = Ident::parse(ctx, i);
  assertEmptyDiags(__func__, ctx.diags);
  if(i != 3)
    Bug()<<__func__<<": Ident parsing of 'foo-' was expected to end "
                     "at position 3, not "<<i;
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
      Bug()<<"Identifiers "<<a<<" and "<<b<<" don't compare equal.";
  }
  pair<string,string> not_equals[] = {
    {"foo_barx", "fooBar"},
    {"foo_bar_42", "fooBar4"},
  };
  for(const auto& [a,b] : not_equals) {
    Ident ai = fromString(__func__, a), bi = fromString(__func__, b);
    if(!ai || !bi) Bug()<<"Ident failed to parse";
    if(ai == bi)
      Bug()<<"Identifiers "<<a<<" and "<<b
              <<" weren't expected to compare equal.";
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
    if(!ai) Bug()<<"Ident failed to parse";
    string snakeo = ai.toSnakeCase();
    if(snakeo != snake)
      Bug()<<"Snake case failed. "<<a<<" became "<<snakeo
           <<" instead of "<<snake;
    string ucamelo = ai.toUCamelCase();
    if(ucamelo != ucamel)
      Bug()<<"Upper case failed. "<<a<<" became "<<ucamelo
           <<" instead of "<<ucamel;
    string lcamelo = ai.toLCamelCase();
    if(lcamelo != lcamel)
      Bug()<<"Lower camel case failed. "<<a<<" became "<<lcamelo
              <<" instead of "<<lcamel;
    string a2 = ai.preserveCase();
    if(a != a2)
      Bug()<<"preserveCase() is no longer preserves input: '"
           <<a<<"' != '"<<a2<<"'";
  }
}

void testSet() {
  auto id = [](string_view s) { return fromString("testSet()", s); };
  set<Ident> s{id("foo"), id("fooBar"), id("fooBaz")};
  if(s.insert(id("foo_bar")).second)
    Bug()<<"std::set was expected to treat foo_bar and fooBar as equal";
  if(!s.insert(id("food")).second)
    Bug()<<"std::set failed to insert new key";

  unordered_set<Ident> us{id("foo"), id("fooBar"), id("fooBaz")};
  if(us.insert(id("foo_bar")).second)
    Bug()<<"std::unordered_set was expected to treat "
           "foo_bar and fooBar as equal";
  if(!us.insert(id("food")).second)
    Bug()<<"std::unordered_set failed to insert new key";
}

}  // namespace

int main() {
  testParseErrors();
  testStopsAtTrail();
  testEqualities();
  testCaseChange();
  testSet();
}
