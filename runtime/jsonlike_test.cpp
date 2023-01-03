#include "jsonlike.h"

#include <string>
#include "test_util.h"
using oalex::assertEqual;
using oalex::Bug;
using oalex::JsonLike;
using oalex::JsonLoc;
using std::string;

struct ParsedTestStruct {
  string value;
  operator JsonLoc() const {
    return JsonLoc::Map{ { {"key", JsonLoc{value} } } };
  }
};

static_assert(std::is_aggregate_v<ParsedTestStruct>);

int main() {
  // Test initialization and assignment.
  JsonLike js = ParsedTestStruct{"value1"};
  js = ParsedTestStruct{"value2"};
  assertEqual("typeid test", js.type().name(), typeid(ParsedTestStruct).name());

  JsonLike js2;  // default-init compiles.
  js2 = js;  // Copy works.

  if(JsonLoc{js2} != JsonLoc{js})
    Bug("Copy-assignment didn't work. {} != {}", JsonLoc{js2}.prettyPrint(2),
        JsonLoc{js}.prettyPrint(2));

  js2.try_cast<ParsedTestStruct>()->value = "value3";

  if(JsonLoc{js2} == JsonLoc{js})
    Bug("Values aliased even after copying");

  // Test that actual JsonLoc also works, without conversion.
  js = JsonLoc{"testvalue"};
  if(js.try_cast<ParsedTestStruct>() != nullptr)
    Bug("JsonLoc unexpectedly cast back to struct");
}

