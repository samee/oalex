#include"ident.h"
#include<string_view>
#include"runtime/diags_test_util.h"
using namespace oalex;
using namespace std;

namespace {

Ident fromString(string_view testName, string_view input) {
  InputDiags ctx{Input{string(input)}, {}};
  size_t i = 0;
  auto rv = Ident::parse(ctx, i);
  assertEmptyDiags(testName, ctx.diags);
  return rv;
}

void testParseErrors() {
  pair<string,string> tests[] = {
    {string(101,'x'), "Identifier too long"},
    {"_", "must have a digit or letter"},
    {"_a", "leading"},
    {"a_", "trailing"},
    {"foo__bar", "Consecutive"},
  };
  for(const auto& [s,err] : tests) {
    assertProducesDiag(__func__, s, err, Ident::parse);
  }
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
      BugDie()<<"Identifiers "<<a<<" and "<<b<<" don't compare equal.";
  }
  pair<string,string> not_equals[] = {
    {"foo_barx", "fooBar"},
    {"foo_bar_42", "fooBar4"},
  };
  for(const auto& [a,b] : not_equals) {
    Ident ai = fromString(__func__, a), bi = fromString(__func__, b);
    if(ai == bi)
      BugDie()<<"Identifiers "<<a<<" and "<<b
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
    string snakeo = ai.toSnakeCase();
    if(snakeo != snake)
      BugDie()<<"Snake case failed. "<<a<<" became "<<snakeo
              <<" instead of "<<snake;
    string ucamelo = ai.toUCamelCase();
    if(ucamelo != ucamel)
      BugDie()<<"Upper case failed. "<<a<<" became "<<ucamelo
              <<" instead of "<<ucamel;
    string lcamelo = ai.toLCamelCase();
    if(lcamelo != lcamel)
      BugDie()<<"Lower camel case failed. "<<a<<" became "<<lcamelo
              <<" instead of "<<lcamel;
  }
}

}  // namespace

int main() {
  testParseErrors();
  testEqualities();
  testCaseChange();
}
