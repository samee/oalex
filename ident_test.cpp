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

}  // namespace

int main() {
  testEqualities();
}
