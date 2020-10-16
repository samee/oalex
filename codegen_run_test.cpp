/*  Copyright 2020 Google LLC

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "codegen_generated.h"
#include "codegen_test_util.h"

#include "runtime/diags_test_util.h"
#include "runtime/util.h"
using oalex::Bug;
using oalex::JsonLoc;
using oalex::test::assertJsonLocIsString;
using std::string;

namespace {

void runSingleStringTest() {
  string msg = "hello";
  auto ctx = testInputDiags(msg);
  ssize_t pos = 0;
  JsonLoc jsloc = start(ctx, pos);

  assertJsonLocIsString(__func__, jsloc, msg, 0, msg.size());

  msg = "goodbye";
  ctx = testInputDiags(msg);
  pos = 0;
  jsloc = start(ctx, pos);
  if(!jsloc.holdsError()) BugMe("Was expecting regex match to fail");
}

}  // namespace

int main() {
  if(!goodFunc()) Bug("goodFunc() returned false");
  if(badFunc()) Bug("badFunc() returned true");

  auto ctx = testInputDiags("x = 5;");
  size_t pos = 0;
  JsonLoc res = parseAsgnStmt(ctx, pos);
  if(!res.holdsError())
    Bug("parseAsgn() returning success, but is unimplemented!");

  runSingleStringTest();
}
