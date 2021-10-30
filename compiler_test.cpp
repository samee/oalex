#include "compiler.h"
#include <string_view>
#include "ident.h"
#include "runtime/diags.h"
#include "runtime/test_util.h"
using oalex::assertEmptyDiags;
using oalex::assertEqual;
using oalex::assertHasDiagWithSubstr;
using oalex::assertWhatHasSubstr;
using oalex::Bug;
using oalex::DefinitionInProgress;
using oalex::Ident;
using oalex::Input;
using oalex::InputDiags;
using oalex::RulesWithLocs;
using std::string_view;

namespace {

void testFindOrAppendNormalOperations() {
  auto exprid = Ident::parseGenerated("expr");
  auto stmtid = Ident::parseGenerated("stmt");

  RulesWithLocs rl;
  assertEqual("RulesWithLocs initial size", rl.ssize(), 0);
  assertEqual("1st findOrAppendIdent() result",
              rl.findOrAppendIdent(exprid), 0);
  assertEqual("RulesWithLocs size after referencing 'expr'", rl.ssize(), 1);
  assertEqual("2nd findOrAppendIdent() result",
              rl.findOrAppendIdent(stmtid), 1);
  assertEqual("RulesWithLocs size after referencing 'stmt'", rl.ssize(), 2);
  assertEqual("Position for 'expr' after another insert",
              rl.findOrAppendIdent(exprid), 0);
  assertEqual("Position for 'block' without prior insert",
              rl.findOrAppendIdent(Ident::parseGenerated("block")), 2);
  assertEqual("RulesWithLocs size after referencing 'block'", rl.ssize(), 3);
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

void testDefineIdentNormal() {
  InputDiags ctx{Input{"foo foo"}};
  RulesWithLocs rl;
  assertEqual("Initial size", rl.ssize(), 0);
  size_t pos = 0;

  Ident v1 = Ident::parse(ctx, pos);
  assertEqual("First ident end", pos, 3u);
  ssize_t v1_index = rl.defineIdent(ctx, v1);
  assertEqual("Size after a define", rl.ssize(), 1);
  assertEqual("'foo' index in RulesWithLocs", v1_index, 0);
  assertEqual("Returned index name",
              rl[v1_index].nameOrNull()->preserveCase(), "foo");
  assertEqual("stPos", rl[v1_index].nameOrNull()->stPos(), 0u);
  assertEqual("enPos", rl[v1_index].nameOrNull()->enPos(), 3u);
  if(!dynamic_cast<DefinitionInProgress*>(&rl[v1_index]))
    BugMe("defineIdent() should have started the definition process. Found {}",
          rl[v1_index].specifics_typename());
  assertEmptyDiags(__func__, ctx.diags);

  ++pos;
  Ident v2 = Ident::parse(ctx, pos);
  assertEqual("Second ident", v1.preserveCase(), v2.preserveCase());
  ssize_t v2_index = rl.defineIdent(ctx, v2);
  assertHasDiagWithSubstr(__func__, ctx.diags,
                          "'foo' has multiple definitions");
  assertEqual("Failure indicator", -1, v2_index);
}

}  // namespace

int main() {
  testFindOrAppendNormalOperations();
  testFindOrAppendEmptyIdentFails();
  testDefineIdentNormal();
}
