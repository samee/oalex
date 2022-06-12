/*  Copyright 2021-2022 The oalex authors.

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
#include "jsontmpl_parsers.h"
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
using oalex::JsonLoc;
using oalex::JsonTmpl;
using oalex::LoopRule;
using oalex::makeVectorUnique;
using oalex::MatchOrError;
using oalex::OrRule;
using oalex::OutputTmpl;
using oalex::parseJsonLoc;
using oalex::parseRegexCharSet;
using oalex::prettyPrint;
using oalex::QuietMatch;
using oalex::Regex;
using oalex::RegexOptions;
using oalex::RegexRule;
using oalex::Rule;
using oalex::RuleExpr;
using oalex::RuleExprConcat;
using oalex::RuleExprIdent;
using oalex::RuleExprMappedIdent;
using oalex::RuleExprOptional;
using oalex::RuleExprRegex;
using oalex::RuleExprRepeat;
using oalex::RuleExprSquoted;
using oalex::RuleSet;
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

[[maybe_unused]] void
ruleListDebugPrint(const vector<unique_ptr<Rule>>& rl) {
  ssize_t i = 0;
  for(auto& rule_ptr: rl) {
    const Ident* nm_id = rule_ptr->nameOrNull();
    string nm = nm_id ? nm_id->preserveCase() : "";
    string extra;
    if(auto* moe = dynamic_cast<const MatchOrError*>(rule_ptr.get())) {
      extra = format("{{{}}}", moe->compidx);
    }else if(auto* regex = dynamic_cast<const RegexRule*>(rule_ptr.get())) {
      extra = format("{{{}}}", prettyPrint(*regex->patt));
    }else if(auto* tmpl = dynamic_cast<const OutputTmpl*>(rule_ptr.get())) {
      extra = format("{{{}, \"{}\"}}", tmpl->childidx, tmpl->childName);
    }else if(auto* cat = dynamic_cast<const ConcatFlatRule*>(rule_ptr.get())) {
      extra = "{ ";
      for(auto& [id,p] : cat->comps) extra += format("{{{}, \"{}\"}}, ", id, p);
      extra += "}";
    }
    oalex::Debug("  {: 3d}| {}: {}{}", i++, nm,
                 typeid(*rule_ptr).name(), extra);
  }
}

void assertEqualLoopRule(string_view msg,
    vector<pair<ssize_t,ssize_t>>& stk,
    const LoopRule& arep, const LoopRule& brep) {
  assertEqual(msg, arep.partname, brep.partname);
  assertEqual(msg, arep.gluename, brep.gluename);
  stk.push_back({arep.partidx, brep.partidx});
  stk.push_back({arep.glueidx, brep.glueidx});
  stk.push_back({arep.lookidx, brep.lookidx});
  stk.push_back({arep.skipidx, brep.skipidx});
}
void assertEqualOrRule(string_view msg, vector<pair<ssize_t,ssize_t>>& stk,
                       const OrRule& aors, const OrRule& bors) {
   assertEqual(format("{}. flattenOnDemand mismatch", msg),
               aors.flattenOnDemand, bors.flattenOnDemand);
   if(ssize(aors.comps) != ssize(bors.comps))
     Bug("{}. OrRule has unequal component count: {} != {}",
         msg, ssize(aors.comps), ssize(bors.comps));
   for(ssize_t k=0; k<ssize(aors.comps); ++k) {
     stk.push_back({aors.comps[k].lookidx, bors.comps[k].lookidx});
     stk.push_back({aors.comps[k].parseidx, bors.comps[k].parseidx});
     assertEqualJsonTmpl(msg, aors.comps[k].tmpl, bors.comps[k].tmpl);
   }
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
    }else if(auto* arep = dynamic_cast<const LoopRule*>(ar)) {
      auto* brep = static_cast<const LoopRule*>(br);
      assertEqualLoopRule(msg, stk, *arep, *brep);
    }else if(auto* aors = dynamic_cast<const OrRule*>(ar)) {
      auto* bors = static_cast<const OrRule*>(br);
      assertEqualOrRule(msg, stk, *aors, *bors);
    }else if(auto* aq = dynamic_cast<const QuietMatch*>(ar)) {
      auto* bq = static_cast<const QuietMatch*>(br);
      stk.push_back({aq->compidx, bq->compidx});
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

  // string_body_only ~ '"' (body ~ /([^"\\]|\\.)*/) '"'
  const char* string_body_name = "string_body_only";
  unique_ptr<const Regex> string_body_regex
    = parseRegex(R"(/([^"\\]|\\.)*/)");
  RuleExprConcat string_body_rule{makeVectorUnique<const RuleExpr>(
    RuleExprSquoted{"\""},
    RuleExprMappedIdent{
      Ident::parseGenerated("body"),
      move_to_unique(RuleExprRegex{string_body_regex->clone()})
    },
    RuleExprSquoted{"\""}
  )};
  auto string_body_expected = makeVectorUnique<Rule>(
    RegexRule{string_body_regex->clone()},
    MatchOrError{0, "Does not match expected pattern"},
    ConcatFlatRule{{{1, "body"}}},
    StringRule{"\""},
    MatchOrError{3, "Expected '\"'"},
    StringRule{"\""},
    MatchOrError{5, "Expected '\"'"},
    ConcatFlatRule{{ {4, {}}, {2, {}}, {6, {}} }},
    nmRule(OutputTmpl{7, {}, JsonTmpl{JsonTmpl::Map{
             {"body", JsonTmpl::Placeholder{"body"}}
           }} }, "string_body_only")
  );

  // prefixed_string_literal ~ (literal_prefix quoted_part)
  // literal_prefix ~ /L|u8?|U/
  // quoted_part ~ /"([^"\\]|\\.)*"/
  const char* prefixed_string_prefix_name = "literal_prefix";
  unique_ptr<const Regex> prefixed_string_prefix_regex
    = parseRegex(R"(/L|u8?|U/)");
  RuleExprRegex prefixed_string_prefix_rule{
    prefixed_string_prefix_regex->clone()
  };
  const char* prefixed_string_quoted_part_name = "quoted_part";
  unique_ptr<const Regex> prefixed_string_quoted_part_regex
    = parseRegex(R"(/"([^"\\]|\\.)*"/)");
  RuleExprRegex prefixed_string_quoted_part_rule{
    prefixed_string_quoted_part_regex->clone()
  };
  const char* prefixed_string_name = "prefixed_string_literal";
  RuleExprConcat prefixed_string_rule{makeVectorUnique<const RuleExpr>(
    RuleExprIdent{Ident::parseGenerated(prefixed_string_prefix_name)},
    RuleExprIdent{Ident::parseGenerated(prefixed_string_quoted_part_name)}
  )};
  auto prefixed_string_expected = makeVectorUnique<Rule>(
    RegexRule{prefixed_string_prefix_regex->clone()},
    nmRule(MatchOrError{0, "Does not match expected pattern"},
           "literal_prefix"),
    ConcatFlatRule{{{1, "literal_prefix"}}},
    RegexRule{prefixed_string_quoted_part_regex->clone()},
    nmRule(MatchOrError{3, "Does not match expected pattern"},
           "quoted_part"),
    ConcatFlatRule{{{4, "quoted_part"}}},
    ConcatFlatRule{{ {2, {}}, {5, {}} }},
    nmRule(OutputTmpl{6, {}, JsonTmpl{JsonTmpl::Map{
             {"literal_prefix", JsonTmpl::Placeholder{"literal_prefix"}},
             {"quoted_part", JsonTmpl::Placeholder{"quoted_part"}},
      }} }, "prefixed_string_literal")
  );

  // int_value ~ /[0-9]+/
  // signed_int_value ~ ((sign ~ /-|+/) (value ~ int_value))
  const char* signed_int_int_name = "int_value";
  unique_ptr<const Regex> signed_int_int_regex = parseRegex("/[0-9]+/");
  RuleExprRegex signed_int_int_rule{signed_int_int_regex->clone()};
  const char* signed_int_value_name = "signed_int_value";
  unique_ptr<const Regex> signed_int_value_regex = parseRegex("/-|\\+/");
  RuleExprConcat signed_int_value_rule{makeVectorUnique<const RuleExpr>(
      RuleExprMappedIdent{
        Ident::parseGenerated("sign"),
        move_to_unique(RuleExprRegex{signed_int_value_regex->clone()})},
      RuleExprMappedIdent{
        Ident::parseGenerated("value"),
        move_to_unique(RuleExprIdent{Ident::parseGenerated("int_value")})}
  )};
  auto signed_int_value_expected = makeVectorUnique<Rule>(
    RegexRule{signed_int_int_regex->clone()},
    nmRule(MatchOrError{0, "Does not match expected pattern"}, "int_value"),
    ConcatFlatRule{{{1, "value"}}},
    RegexRule{signed_int_value_regex->clone()},
    MatchOrError{3, "Does not match expected pattern"},
    ConcatFlatRule{{{4, "sign"}}},
    ConcatFlatRule{{ {5, {}}, {2, {}} }},
    nmRule(OutputTmpl{6, {}, JsonTmpl{JsonTmpl::Map{
        {"sign", JsonTmpl::Placeholder{"sign"}},
        {"value", JsonTmpl::Placeholder{"value"}},
      }} }, "signed_int_value")
  );

  // hyphen_ident ~ (ident '-' ... '-' ident)
  const char* hyphen_ident_part_name = "ident";
  unique_ptr<const Regex> hyphen_ident_part_regex
    = parseRegex("/[a-zA-Z]+/");
  RuleExprRegex hyphen_ident_part_rule{hyphen_ident_part_regex->clone()};
  const char* hyphen_ident_name = "hyphen_ident";
  RuleExprRepeat hyphen_ident_rule{
    move_to_unique(RuleExprIdent{
      Ident::parseGenerated(hyphen_ident_part_name)
    }),
    move_to_unique(RuleExprSquoted{"-"})
  };
  auto hyphen_ident_expected = makeVectorUnique<Rule>(
    RegexRule{hyphen_ident_part_regex->clone()},
    nmRule(MatchOrError{0, "Does not match expected pattern"},
           "ident"),
    ConcatFlatRule{{{1, "ident"}}},
    StringRule{"-"},
    MatchOrError{3, "Expected '-'"},
    LoopRule{{.partidx=2, .partname{},
              .glueidx=4, .gluename{}, .lookidx=-1, .skipidx=-1}},
    nmRule(OutputTmpl{5, {}, JsonTmpl{JsonTmpl::Map{
            {"ident", JsonTmpl{JsonTmpl::Placeholder{"ident"}}}
          }}}, "hyphen_ident")
  );

  // term ~ ([keyword_indicator ~ ':'] (word ~ /[a-zA-Z]+/))
  const char* keyword_or_ident_name = "term";
  unique_ptr<const Regex> keyword_or_ident_regex
    = parseRegex("/[a-zA-Z]+/");
  RuleExprConcat keyword_or_ident_rule{makeVectorUnique<const RuleExpr>(
    RuleExprOptional{move_to_unique(
      RuleExprMappedIdent{Ident::parseGenerated("keyword_indicator"),
                          move_to_unique(RuleExprSquoted{":"})}
    )},
    RuleExprMappedIdent{Ident::parseGenerated("word"),
                        move_to_unique(
                            RuleExprRegex{keyword_or_ident_regex->clone()}
                        )}
  )};
  auto keyword_or_ident_expected = makeVectorUnique<Rule>(
    StringRule{":"},
    MatchOrError{0, "Expected ':'"},
    RegexRule{keyword_or_ident_regex->clone()},
    MatchOrError{2, "Does not match expected pattern"},
    ConcatFlatRule{{{1, "keyword_indicator"}}},
    ConcatFlatRule{{{3, "word"}}},
    QuietMatch{4},
    StringRule{{}},
    OrRule{{ {-1, 6, oalex::passthroughTmpl}, {-1, 7, JsonTmpl::Map{}} }, true},
    ConcatFlatRule{{ {8, {}}, {5, {}} }},
    nmRule(OutputTmpl{9, {}, JsonTmpl{JsonTmpl::Map{
            {"keyword_indicator",
              JsonTmpl{JsonTmpl::Placeholder{"keyword_indicator"}}},
            {"word", JsonTmpl{JsonTmpl::Placeholder{"word"}}},
          }}}, "term")
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
    {.rxprs{{string_body_name, &string_body_rule}},
     .expected = &string_body_expected },
    {.rxprs{{prefixed_string_prefix_name, &prefixed_string_prefix_rule},
            {prefixed_string_quoted_part_name,
             &prefixed_string_quoted_part_rule},
            {prefixed_string_name, &prefixed_string_rule}},
     .expected = &prefixed_string_expected },
    {.rxprs{{signed_int_int_name, &signed_int_int_rule},
            {signed_int_value_name, &signed_int_value_rule}},
     .expected = &signed_int_value_expected },
    {.rxprs{{hyphen_ident_part_name, &hyphen_ident_part_rule},
            {hyphen_ident_name, &hyphen_ident_rule}},
     .expected = &hyphen_ident_expected },
    {.rxprs{{keyword_or_ident_name, &keyword_or_ident_rule}},
     .expected = &keyword_or_ident_expected },
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
      rl.releaseRulesWith({}).rules, *testcase.expected);
  }
}

void testRuleExprDuplicateIdent() {
  unique_ptr<const Regex> ident_regex = parseRegex(R"(/[a-z_]+/)");
  RuleExprConcat asgn_rule{makeVectorUnique<const RuleExpr>(
    RuleExprMappedIdent{
      Ident::parseGenerated("varname"),
      move_to_unique(RuleExprRegex{ident_regex->clone()})
    },
    RuleExprSquoted{"="},
    RuleExprMappedIdent{
      Ident::parseGenerated("varname"),
      move_to_unique(RuleExprRegex{ident_regex->clone()})
    }
  )};

  InputDiags ctx{Input{""}};
  RulesWithLocs rl;
  assignRuleExpr(ctx, asgn_rule, rl,
                 rl.defineIdent(ctx, Ident::parseGenerated("asgn")));
  assertHasDiagWithSubstr(__func__, ctx.diags,
                          "Duplicate identifier 'varname'");
}

// Dev-note: we _might_ delete this test once
// we have full testing of rule-list in testdata/.
// But for that, we first need to implement the frontend.
void testRuleExprCompilationAndParsing() {
  string_view ident_part_regex = "/[a-zA-Z]+/";
  InputDiags ctx{Input{""}};
  RulesWithLocs rl;

  RuleExprRegex rxpr_ident{parseRegex(ident_part_regex)};
  auto ident_part_name = Ident::parseGenerated("ident");
  ssize_t identi = rl.defineIdent(ctx, ident_part_name);
  assignRuleExpr(ctx, rxpr_ident, rl, identi);

  RuleExprRepeat rxpr_main{
    move_to_unique(RuleExprIdent{ident_part_name}),
    move_to_unique(RuleExprSquoted{"-"})
  };

  ssize_t maini = rl.defineIdent(ctx, Ident::parseGenerated("hyphen_ident"));
  assignRuleExpr(ctx, rxpr_main, rl, maini);

  RegexOptions regopts{.word = parseRegexCharSet("[0-9A-Za-z]")};
  RuleSet rs = rl.releaseRulesWith(regopts);
  auto expected_ruleset = makeVectorUnique<Rule>(
      RegexRule{parseRegex(ident_part_regex)},
      nmRule(MatchOrError{0, "Does not match expected pattern"},
             "ident"),
      ConcatFlatRule{{{1, "ident"}}},
      StringRule{"-"},
      MatchOrError{3, "Expected '-'"},
      LoopRule{{.partidx=2, .partname{},
                .glueidx=4, .gluename{}, .lookidx=-1, .skipidx=-1}},
      nmRule(OutputTmpl{5, {}, JsonTmpl{JsonTmpl::Map{
              {"ident", JsonTmpl{JsonTmpl::Placeholder{"ident"}}}
            }}}, "hyphen_ident")
  );
  assertValidAndEqualRuleList(__func__, rs.rules, expected_ruleset);

  for(maini=0; maini<ssize(rs.rules); ++maini) {
    const Ident* nm = rs.rules[maini]->nameOrNull();
    if(nm && nm->preserveCase() == "hyphen_ident")
      break;
  }
  if(maini == ssize(rs.rules)) Bug("Rule name didn't persist");
  InputDiags ctx_user{Input{"abc-def-ghi"}};
  ssize_t spos = 0;
  JsonLoc jsloc = eval(ctx_user, spos, rs, maini);
  JsonLoc expected
    = *parseJsonLoc("{ ident: ['abc', 'def', 'ghi'] }");
  if(jsloc != expected) {
    BugMe("Couldn't parse hyphenated identifier: {} != {}",
          jsloc.prettyPrint(0), expected.prettyPrint(0));
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
  testRuleExprDuplicateIdent();
  testRuleExprCompilationAndParsing();
}
