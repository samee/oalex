#include "compiler.h"
#include <string_view>
#include "ident.h"
#include "runtime/test_util.h"
using oalex::assertEqual;
using oalex::assertWhatHasSubstr;
using oalex::Bug;
using oalex::Ident;
using oalex::RulesWithLocs;
using std::string_view;

namespace {

void testFindOrAppendNormalOperations() {
  // TODO cleanup: long int isn't always the same as ssize_t
  auto exprid = Ident::parseGenerated("expr");
  auto stmtid = Ident::parseGenerated("stmt");

  RulesWithLocs rl;
  assertEqual("RulesWithLocs initial size", rl.ssize(), 0l);
  assertEqual("1st findOrAppendIdent() result",
              rl.findOrAppendIdent(exprid), 0l);
  assertEqual("RulesWithLocs size after referencing 'expr'", rl.ssize(), 1l);
  assertEqual("2nd findOrAppendIdent() result",
              rl.findOrAppendIdent(stmtid), 1l);
  assertEqual("RulesWithLocs size after referencing 'stmt'", rl.ssize(), 2l);
  assertEqual("Position for 'expr' after another insert",
              rl.findOrAppendIdent(exprid), 0l);
  assertEqual("Position for 'block' without prior insert",
              rl.findOrAppendIdent(Ident::parseGenerated("block")), 2l);
  assertEqual("RulesWithLocs size after referencing 'block'", rl.ssize(), 3l);
}

void testFindOrAppendEmptyIdentFails() {
  RulesWithLocs rl;
  try {
    rl.findOrAppendIdent(Ident{});
    BugMe("Was expecting findOrAppendIdent() to fail on null parameter");
  }catch(const std::logic_error& ex) {
    assertWhatHasSubstr(__func__, ex,
                        "findOrAppendIdent() invoked with empty Ident");
  }
}

}  // namespace

int main() {
  testFindOrAppendNormalOperations();
  testFindOrAppendEmptyIdentFails();
}
