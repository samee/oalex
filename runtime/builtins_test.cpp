/*  Copyright 2022 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "runtime/builtins.h"

#include "runtime/diags.h"
#include "runtime/jsonloc.h"
#include "runtime/parser_helpers.h"
#include "runtime/test_util.h"
#include <string_view>
using oalex::assertEqual;
using oalex::Input;
using oalex::InputDiags;
using oalex::JsonLoc;
using oalex::match;
using oalex::Parser;
using oalex::showDiags;

namespace {

// Use this syntax for function declaration to ensure that the
// signature matches the expected one in oalex::Parser.
const Parser simpleLeader = [](InputDiags& ctx, ssize_t& i) -> JsonLoc {
  return match(ctx, i, "my_list:");
};
const Parser simpleItem = [](InputDiags& ctx, ssize_t& i) -> JsonLoc {
  JsonLoc res = match(ctx, i, "item");
  if(res.holdsErrorValue()) {
    Error(ctx, i, "Expected 'item'");
  }
  return res;
};
const Parser leaderBadLocation = [](InputDiags& ctx, ssize_t& i) -> JsonLoc {
  JsonLoc rv = match(ctx, i, "my_list:");
  rv.stPos = rv.enPos = Input::npos;
  return rv;
};
void testSimpleList() {
  const char msg[] = R"(  my_list:
    item
    item
    item
End of list
    more stuff)";
  InputDiags ctx{Input{msg}};
  ssize_t pos = 2;
  JsonLoc observed =
    oalexBuiltinIndentedList(ctx, pos, simpleLeader, simpleItem);
  JsonLoc expected = JsonLoc::Map{
    {"leader", JsonLoc::String{"my_list:"}},
    {"items", JsonLoc::Vector{3, JsonLoc::String{"item"}}},
  };
  if(!ctx.diags.empty()) showDiags(ctx.diags);
  if(observed != expected) {
    BugMe("Output: {}", observed.prettyPrint(2));
  }

  // See that we don't depend on diag location.
  pos = 2;
  observed =
    oalexBuiltinIndentedList(ctx, pos, leaderBadLocation, simpleItem);
  if(!ctx.diags.empty()) showDiags(ctx.diags);
  if(observed != expected) {
    BugMe("Parsing depends on diag location. Output: {}",
          observed.prettyPrint(2));
  }
}
void testBadLeaderResets() {
  const char msg[] = "no_list";
  InputDiags ctx{Input{msg}};
  ssize_t pos = 0;
  JsonLoc observed =
    oalexBuiltinIndentedList(ctx, pos, simpleLeader, simpleItem);
  if(!observed.holdsErrorValue()) BugMe("Expected error on bad list leader");
  assertEmptyDiags(me("No superfluous diags should be inserted"),
                   ctx.diags);
  if(pos != 0) BugMe("Inputs should not be consumed on bad leader");
}

void testSimpleListFailure(const char input[], ssize_t stPos,
                           const char errmsg[]) {
  InputDiags ctx{Input{input}};
  ssize_t pos = stPos;
  JsonLoc observed =
    oalexBuiltinIndentedList(ctx, pos, simpleLeader, simpleItem);
  if(!observed.holdsErrorValue()) BugMe("Expected error");
  assertHasDiagWithSubstr(__func__, ctx.diags, errmsg);
  if(pos != stPos) BugMe("Inputs should not be consumed on {}", errmsg);
}
void testSimpleListSucceedsWithDiag(const char input[], ssize_t stPos,
                                    const char errmsg[], JsonLoc expected) {
  InputDiags ctx{Input{input}};
  ssize_t pos = stPos;
  JsonLoc observed =
    oalexBuiltinIndentedList(ctx, pos, simpleLeader, simpleItem);
  assertHasDiagWithSubstr(__func__, ctx.diags, errmsg);
  if(observed != expected) {
    BugMe("Output: {} != {}", observed.prettyPrint(2),
                              expected.prettyPrint(2));
  }
}
}  // namespace

int main() {
  testSimpleList();
  testBadLeaderResets();
  testSimpleListFailure("my_list:", 0, "Expected list items after this");
  testSimpleListFailure("my_list: item", 0, "Expected end of line");
  testSimpleListFailure("my_list:\nno_item", 0,
                        "should have been indented more");
  testSimpleListFailure(" my_list:\n\titem", 1, "Bad indentation");
  testSimpleListFailure("my_list:\n\tthing", 0, "Expected 'item'");
  testSimpleListSucceedsWithDiag("my_list:\n item\n\titem", 0,
      "Bad indentation", JsonLoc::Map{
        {"leader", JsonLoc::String{"my_list:"}},
        {"items", JsonLoc::Vector{2, JsonLoc::String{"item"}}},
      });
  testSimpleListSucceedsWithDiag(R"(
    my_list:
      item
      item
     something else
  )", 5, "end of the list doesn't match", JsonLoc::Map{
    {"leader", JsonLoc::String{"my_list:"}},
    {"items", JsonLoc::Vector{2, JsonLoc::String{"item"}}},
  });
}
