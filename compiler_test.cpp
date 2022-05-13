/*  Copyright 2021 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "compiler.h"
#include <string_view>
#include "codegen_test_util.h"
#include "ident.h"
#include "frontend_pieces.h"
#include "regex_io.h"
#include "runtime/diags.h"
#include "runtime/test_util.h"
using fmt::format;
using oalex::assertEmptyDiags;
using oalex::assertEqual;
using oalex::assertHasDiagWithSubstr;
using oalex::assertWhatHasSubstr;
using oalex::Bug;
using oalex::ConcatFlatRule;
using oalex::DefinitionInProgress;
using oalex::Ident;
using oalex::Input;
using oalex::InputDiags;
using oalex::JsonTmpl;
using oalex::makeVectorUnique;
using oalex::MatchOrError;
using oalex::OutputTmpl;
using oalex::prettyPrint;
using oalex::Regex;
using oalex::RegexRule;
using oalex::Rule;
using oalex::RuleExpr;
using oalex::RuleExprMappedIdent;
using oalex::RuleExprRegex;
using oalex::RuleExprSquoted;
using oalex::RulesWithLocs;
using oalex::ssize;
using oalex::StringRule;
using oalex::UnassignedRule;
using oalex::test::nmRule;
using oalex::test::parseRegex;
using std::pair;
using std::string;
using std::string_view;
using std::unique_ptr;
using std::vector;

namespace {

void assertUnassignedRule(string_view msg, const Rule& rule) {
  if(dynamic_cast<const UnassignedRule*>(&rule)) return;
  Bug("{}. Expected UnassignedRule, found {}", msg, rule.specifics_typename());
}

void assertDefinitionInProgress(string_view msg, const Rule& rule) {
  if(dynamic_cast<const DefinitionInProgress*>(&rule)) return;
  Bug("{}. Expected DefinitionInProgress, found {}", msg,
      rule.specifics_typename());
}

void assertNoDuplicateNames(
    string_view msg, const vector<unique_ptr<Rule>>& rules) {
  for(ssize_t i=0; i<ssize(rules); ++i)
    if(const Ident* nm_i = rules[i]->nameOrNull())
      for(ssize_t j=i+1; j<ssize(rules); ++j)
        if(const Ident* nm_j = rules[j]->nameOrNull())
          if(*nm_i == *nm_j)
            Bug("{}. Name '{}' was defined twice", msg, nm_i->preserveCase());
}

void assertEqualJsonTmpl(
  string_view msg, const JsonTmpl& a, const JsonTmpl& b) {
  if(a.tag() != b.tag())
    Bug("{}. OutputTmpl tag mismatch. {} != {}", msg, a.tagName(), b.tagName());
  if(auto* ma = a.getIfMap()) {
    auto* mb = b.getIfMap();
    assertEqual(format("{}. Map sizes", msg), ssize(*ma), ssize(*mb));
    for(auto& [ka,va] : *ma)
      assertEqualJsonTmpl(msg, va, *JsonTmpl::mapScanForValue(*mb, ka));
  }else if(auto* pa = a.getIfPlaceholder()) {
    auto* pb = b.getIfPlaceholder();
    assertEqual(msg, pa->key, pb->key);
  // Add more JsonTmpl variants if we ever use them.
  }else {
    Bug("Unknown tag {} in {}", a.tagName(), __func__);
  }
}

// Computes the "easy" mappings between a[i] and some b[j]. Easy meaning, the
// ones that are explicitly named.
// Returns rv such that: a[rv[i].first] maps to b[rv[i].second],
vector<pair<ssize_t,ssize_t>>
namedRuleMappings(string_view msg, const vector<unique_ptr<Rule>>& a,
                                   const vector<unique_ptr<Rule>>& b) {
  assertNoDuplicateNames(msg, a);
  assertNoDuplicateNames(msg, b);
  ssize_t i, j;
  vector<pair<ssize_t,ssize_t>> rv;
  vector<bool> used_j(ssize(b), false);
  for(i=0; i<ssize(a); ++i) if(const Ident* nm_a = a[i]->nameOrNull()) {
    for(j=0; j<ssize(b); ++j) if(const Ident* nm_b = b[j]->nameOrNull())
      if(*nm_a == *nm_b) {
        used_j[j] = true;
        rv.push_back({i,j});
        break;
      }
    if(j==ssize(b))
      Bug("{}. No match was found for rule '{}'", msg, nm_a->preserveCase());
  }
  for(j=0; j<ssize(b); ++j) if(const Ident* nm_b = b[j]->nameOrNull())
    if(!used_j[j]) {
      Bug("{}. No match was found for rule '{}'", msg, nm_b->preserveCase());
    }
  return rv;
}

void assertValidAndEqualRuleList(string_view msg,
    const vector<unique_ptr<Rule>>& a, const vector<unique_ptr<Rule>>& b) {
  if(ssize(a) != ssize(b))
    Bug("{}. Rule vectors have different sizes: {} != {}",
        msg, ssize(a), ssize(b));

  ssize_t i, j;
  vector<pair<ssize_t,ssize_t>> stk = namedRuleMappings(msg, a, b);
  vector<ssize_t> mapping_a2b(ssize(a), -1);

  while(!stk.empty()) {
    std::tie(i,j) = stk.back();
    stk.pop_back();
    if((i==-1) != (j==-1)) Bug("{}. Rule component is missing", msg);
    else if(i==-1 && j==-1) continue;
    else if(mapping_a2b[i] == j) continue;
    else if(mapping_a2b[i] == -1) mapping_a2b[i] = j;
    else Bug("{}. Rule vector mismatch", msg);

    const Rule *ar = a[i].get(), *br = b[j].get();
    if(typeid(*ar) != typeid(*br))
      Bug("{}. Rules are of different types: {} != {}", msg,
          typeid(*ar).name(), typeid(*br).name());

    // Type-specific comparison
    if(auto* acat = dynamic_cast<const ConcatFlatRule*>(ar)) {
      auto* bcat = static_cast<const ConcatFlatRule*>(br);
      if(ssize(acat->comps) != ssize(bcat->comps))
        Bug("{}. ConcatFlatRule has unequal component count: {} != {}",
            msg, ssize(acat->comps), ssize(bcat->comps));
      for(ssize_t k=0; k<ssize(acat->comps); ++k) {
        assertEqual(msg, acat->comps[k].outputPlaceholder,
                         bcat->comps[k].outputPlaceholder);
        stk.push_back({acat->comps[k].idx, bcat->comps[k].idx});
      }
    }else if(auto* aout = dynamic_cast<const OutputTmpl*>(ar)) {
      auto* bout = static_cast<const OutputTmpl*>(br);
      assertEqual(msg, aout->childName, bout->childName);
      stk.push_back({aout->childidx, bout->childidx});
      assertEqualJsonTmpl(msg, aout->outputTmpl, bout->outputTmpl);
    }else if(auto* amoe = dynamic_cast<const MatchOrError*>(ar)) {
      auto* bmoe = static_cast<const MatchOrError*>(br);
      assertEqual(msg, amoe->errmsg, bmoe->errmsg);
      stk.push_back({amoe->compidx, bmoe->compidx});
    }else if(auto* aregex = dynamic_cast<const RegexRule*>(ar)) {
      auto* bregex = static_cast<const RegexRule*>(br);
      assertEqual(msg, prettyPrint(*aregex->patt), prettyPrint(*bregex->patt));
    }else if(auto* as = dynamic_cast<const StringRule*>(ar)) {
      auto* bs = static_cast<const StringRule*>(br);
      assertEqual(msg, as->val, bs->val);
    }else {
      Bug("{}: unknown Rule type {}", __func__, ar->specifics_typename());
    }
  }
}

void testFindOrAppendNormalOperations() {
  InputDiags ctx{Input{""}};
  auto exprid = Ident::parseGenerated("expr");
  auto stmtid = Ident::parseGenerated("stmt");

  RulesWithLocs rl;
  assertEqual("RulesWithLocs initial size", rl.ssize(), 0);
  assertEqual("1st findOrAppendIdent() result",
              rl.findOrAppendIdent(ctx, exprid), 0);
  assertEqual("RulesWithLocs size after referencing 'expr'", rl.ssize(), 1);
  assertEqual("2nd findOrAppendIdent() result",
              rl.findOrAppendIdent(ctx, stmtid), 1);
  assertEqual("RulesWithLocs size after referencing 'stmt'", rl.ssize(), 2);
  assertEqual("Position for 'expr' after another insert",
              rl.findOrAppendIdent(ctx, exprid), 0);
  assertEqual("Position for 'block' without prior insert",
              rl.findOrAppendIdent(ctx, Ident::parseGenerated("block")), 2);
  assertEqual("RulesWithLocs size after referencing 'block'", rl.ssize(), 3);
}

void testFindOrAppendEmptyIdentFails() {
  InputDiags ctx{Input{""}};
  RulesWithLocs rl;
  try {
    rl.findOrAppendIdent(ctx, Ident{});
    BugMe("Was expecting findOrAppendIdent() to fail on null parameter");
  }catch(const std::logic_error& ex) {
    assertWhatHasSubstr(__func__, ex,
                        "findIdent() invoked with empty Ident");
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
  assertDefinitionInProgress(
      "defineIdent() should have started the definition process", rl[v1_index]);
  assertEmptyDiags(__func__, ctx.diags);

  ++pos;
  Ident v2 = Ident::parse(ctx, pos);
  assertEqual("Second ident", v1.preserveCase(), v2.preserveCase());
  ssize_t v2_index = rl.defineIdent(ctx, v2);
  assertHasDiagWithSubstr(__func__, ctx.diags,
                          "'foo' has multiple definitions");
  assertEqual("Failure indicator", -1, v2_index);
}

void testDefineIdentEmptyIdentFails() {
  RulesWithLocs rl;
  try {
    InputDiags ctx{Input{"ignored input"}};
    rl.defineIdent(ctx, Ident{});
    BugMe("Was expecting defineIdent() to fail on null parameter");
  }catch(const std::logic_error& ex) {
    assertWhatHasSubstr(__func__, ex,
                        "defineIdent() invoked with empty Ident");
  }
}

void testFindThenDefine() {
  InputDiags ctx{Input{"foo bar baz bar"}};
  RulesWithLocs rl;
  size_t pos = 0;
  Ident foo = Ident::parse(ctx, pos); ++pos;
  Ident bar = Ident::parse(ctx, pos); ++pos;
  Ident baz = Ident::parse(ctx, pos); ++pos;
  Ident bar_defn = Ident::parse(ctx, pos);
  ssize_t i;
  i = rl.findOrAppendIdent(ctx, foo);
  assertEqual("foo index", i, 0);
  i = rl.findOrAppendIdent(ctx, bar);
  assertEqual("bar index", i, 1);
  i = rl.findOrAppendIdent(ctx, baz);
  assertEqual("baz index", i, 2);
  assertUnassignedRule("bar should be unassigned before definition", rl[1]);
  i = rl.defineIdent(ctx, bar_defn);
  assertEqual(me("defineIdent() is different from earlier findOrAppendIdent()"),
              i, 1);
  assertDefinitionInProgress("bar definition didn't take", rl[1]);
}

void testCaseConflicts() {
  InputDiags ctx{Input{"fooBar foo_bar foobar"}};
  size_t pos = 0;
  Ident v1 = Ident::parse(ctx, pos); ++pos;
  Ident v2 = Ident::parse(ctx, pos); ++pos;
  Ident v3 = Ident::parse(ctx, pos);

  RulesWithLocs rl;
  rl.findOrAppendIdent(ctx, v1);
  rl.findOrAppendIdent(ctx, v2);
  assertHasDiagWithSubstr(__func__, ctx.diags, "case-conflicts");

  ctx.diags.clear();
  rl.defineIdent(ctx, v3);
  assertHasDiagWithSubstr(__func__, ctx.diags, "case-conflicts");
}

void testReserveLocalNameEmptyIdentFails() {
  RulesWithLocs rl;
  try {
    InputDiags ctx{Input{"ignored input"}};
    rl.reserveLocalName(ctx, Ident{});
    BugMe("Was expecting reserveLocalName() to fail on null parameter");
  }catch(const std::logic_error& ex) {
    assertWhatHasSubstr(__func__, ex,
                        "reserveLocalName() invoked with empty Ident");
  }
}

void testReserveLocalName() {
  InputDiags ctx{Input{"foo"}};
  RulesWithLocs rl;
  size_t pos = 0;
  Ident foo = Ident::parse(ctx, pos);

  assertEqual("No initial definitions", rl.ssize(), 0);
  // Use 'foo' lots of times as a local variable.
  rl.reserveLocalName(ctx, foo);
  rl.reserveLocalName(ctx, foo);
  rl.reserveLocalName(ctx, foo);
  if(rl.findReservedLocalIdent(foo) != foo)
    BugMe("findReservedLocalIdent() should return a reserved local");
  assertEqual("reserving a local name shouldn't mark it as used as a global",
              rl.ssize(), 0);
  assertEmptyDiags(__func__, ctx.diags);
  if(rl.findReservedLocalIdent(Ident::parseGenerated("bar")))
    BugMe("Found identifier that was never reserved");
}

void testDefineAndReserveProducesError() {
  InputDiags ctx{Input{"foo"}};
  size_t pos = 0;
  Ident foo = Ident::parse(ctx, pos);

  {
    RulesWithLocs rl;
    assertEmptyDiags(me("No initial diags expected"), ctx.diags);
    rl.defineIdent(ctx, foo);
    rl.reserveLocalName(ctx, foo);
    assertHasDiagWithSubstr(me("define-then-reserve"), ctx.diags,
        "Local variable name 'foo' conflicts with a global name");
  }
  {
    RulesWithLocs rl;
    ctx.diags.clear();
    assertEmptyDiags(me("No initial diags expected"), ctx.diags);
    rl.defineIdent(ctx, Ident::parseGenerated("Foo"));
    rl.reserveLocalName(ctx, foo);
    assertHasDiagWithSubstr(me("define-then-reserve-case-conflict"), ctx.diags,
        "Local variable name 'foo' conflicts with the global name 'Foo'");
  }
  {
    RulesWithLocs rl;
    ctx.diags.clear();
    assertEmptyDiags(me("No initial diags expected"), ctx.diags);
    rl.reserveLocalName(ctx, foo);
    rl.defineIdent(ctx, foo);
    assertHasDiagWithSubstr(me("reserve-then-define"), ctx.diags,
        "Local variable name 'foo' conflicts with a global name");
  }

}

void testDestructureErrors() {
  InputDiags ctx{Input{R"(errors after failing:
    id1: "msg1"
    id2: "msg2")"}};
  ssize_t pos = 0;
  vector<pair<Ident,string>> observed
    = destructureErrors(ctx, parseErrorStanza(ctx, pos));
  vector<pair<Ident,string>> expected = {
    {Ident::parseGenerated("id1"), "msg1"},
    {Ident::parseGenerated("id2"), "msg2"},
  };
  // assertEqual() won't work without an fmtlib formatter
  if(observed != expected) {
    fmt::print(stderr, "observed output: {{\n");
    for(auto& [id, s] : observed) {
      fmt::print(stderr, "  {{ {}, {} }}\n", id.preserveCase(), s);
    }
    fmt::print(stderr, "}}\n");
    BugMe("{}", "expected output: { {id1, msg1}, {id2, msg2} }");
  }
}

void testRuleExprCompilation() {
  const char* keyword_fn_name = "keyword_fn";
  RuleExprSquoted keyword_fn_rule{"fn"};
  auto keyword_fn_expected = makeVectorUnique<Rule>(
    StringRule{"fn"},
    nmRule(MatchOrError{0, "Expected 'fn'"}, "keyword_fn")
  );

  const char* string_literal_name = "string_literal";
  unique_ptr<const Regex> string_literal_regex
    = parseRegex(R"(/"([^"\\]|\\.)*"/)");
  RuleExprRegex string_literal_rule{string_literal_regex->clone()};
  auto string_literal_expected = makeVectorUnique<Rule>(
    RegexRule(string_literal_regex->clone()),
    nmRule(MatchOrError{0, "Does not match expected pattern"},
           "string_literal")
  );

  const char* newvar_name = "new_var_name";
  unique_ptr<const Regex> ident_regex
    = parseRegex(R"(/[a-zA-Z_][a-zA-Z0-9_]*/)");
  RuleExprMappedIdent newvar_rule{
    Ident::parseGenerated("new_name"),
    move_to_unique(RuleExprRegex{ident_regex->clone()}),
  };
  auto newvar_expected = makeVectorUnique<Rule>(
    RegexRule{ident_regex->clone()},
    MatchOrError{0, "Does not match expected pattern"},
    ConcatFlatRule{{ {1, "new_name"} }},
    nmRule(OutputTmpl{2, {}, JsonTmpl{JsonTmpl::Map{
      {"new_name", JsonTmpl::Placeholder{"new_name"}}
    }}}, "new_var_name")
  );

  struct TestCase {
    vector<pair<const char*, const RuleExpr*>> rxprs;
    const vector<unique_ptr<Rule>>* expected;
  };
  TestCase cases[] = {
    {.rxprs{{keyword_fn_name, &keyword_fn_rule}},
     .expected = &keyword_fn_expected },
    {.rxprs{{string_literal_name, &string_literal_rule}},
     .expected = &string_literal_expected },
    {.rxprs{{newvar_name, &newvar_rule}},
     .expected = &newvar_expected },
    // TODO: Add more test cases
  };
  ssize_t casei = 0;
  for(const TestCase& testcase : cases) {
    InputDiags ctx{Input{""}};
    RulesWithLocs rl;
    for(auto& [name, rxpr] : testcase.rxprs) {
      ssize_t identi = rl.defineIdent(ctx, Ident::parseGenerated(name));
      assignRuleExpr(ctx, *rxpr, rl, identi);
    }
    assertValidAndEqualRuleList(
      format("{}: cases[{}]", __func__, ++casei),
      rl.releaseRules(), *testcase.expected);
  }
}

}  // namespace

int main() {
  testFindOrAppendNormalOperations();
  testFindOrAppendEmptyIdentFails();
  testDefineIdentNormal();
  testDefineIdentEmptyIdentFails();
  testReserveLocalNameEmptyIdentFails();
  testFindThenDefine();
  testCaseConflicts();
  testReserveLocalName();
  testDefineAndReserveProducesError();
  testDestructureErrors();
  testRuleExprCompilation();
}
