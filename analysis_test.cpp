/*  Copyright 2019-2025 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "analysis.h"

#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "codegen.h"
#include "codegen_test_util.h"
#include "jsontmpl.h"
#include "jsontmpl_parsers.h"
#include "runtime/test_util.h"
using oalex::AliasRule;
using oalex::assertEqual;
using oalex::Bug;
using oalex::ConcatFlatRule;
using oalex::ErrorRule;
using oalex::ExternParser;
using oalex::JsonTmpl;
using oalex::LoopRule;
using oalex::OrRule;
using oalex::OutputTmpl;
using oalex::parseJsonTmpl;
using oalex::passthroughTmpl;
using oalex::populateFlatFields;
using oalex::Rule;
using oalex::RuleField;
using oalex::RuleSet;
using oalex::SkipPoint;
using oalex::StringRule;
using oalex::WordPreserving;
using oalex::test::cskip;
using oalex::test::parseRegexRule;
using oalex::test::regexOpts;
using std::string;
using std::string_view;
using std::unique_ptr;
using std::vector;

namespace {

void testFlatFieldsForNonFlatRules() {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        StringRule{"hello"},
        WordPreserving{"hello", 0},
        ExternParser{"oalexBuiltinHello", {}},
        SkipPoint{0},
        parseRegexRule("/[a-z]+/"),
        OutputTmpl{0, {}, JsonTmpl::Map{}},
        ErrorRule{"Was expecting a hello"}
    ),
    .skips{cskip},
    .regexOpts = {regexOpts},
  };
  resolveWrapperTypes(rs);
  populateFlatFields(rs);
  for(const unique_ptr<Rule>& rule: rs.rules)
    assertEqual(__func__, ssize(rule->flatFields()), 0);
}

bool ruleFieldEqual(const RuleField& a, const RuleField& b) {
  return a.field_name == b.field_name &&
         a.schema_source == b.schema_source &&
         a.container == b.container;
}
[[maybe_unused]] string debug_field(const RuleField& rf) {
  string_view c = rf.container == 0 ? "single"
                : rf.container == 1 ? "optional"
                : rf.container == 2 ? "vector"
                : "garbage";
  return format("{{ .field_name = {}, .schema_source = {}, .container = {} }}",
                rf.field_name, rf.schema_source, c);
}

// This test case is trying to implement this oalex grammar:
//
//   rule element choices:
//   | '(' -> paren_group
//   | word
//
//   rule word: /[a-z]+/
//
//   rule paren_group:
//     ( [ items, ... , items ] )
//   where:
//     items ~ element
void testFlatFieldsForNestedList() {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        parseRegexRule("/[a-z]+/"),  // [0]
        StringRule{","},
        StringRule{"("},
        StringRule{")"},
        StringRule{""},
        LoopRule{{.initidx = 10, .looklen = 1, .loopbody{1, 10}}},  // [5]
        OrRule{{{-1,5,passthroughTmpl}, {-1,4,passthroughTmpl}},
               true /* flattenOnDemand */ },  // [items, items, ... ]
        ConcatFlatRule{{ {2}, {6}, {3} }},
        OutputTmpl{7, {}, *parseJsonTmpl("{items}")},  // paren_group
        OrRule{{ {2,5,passthroughTmpl},
                 {-1,0,*parseJsonTmpl("{word: child}")}},
               false /* flattenOnDemand */ },  // element :: JsonLike
        ConcatFlatRule{{ {9, "items"} }} // [10]. items, items, ...
    ),
    .skips{cskip},
    .regexOpts = {regexOpts},
  };
  resolveWrapperTypes(rs);
  populateFlatFields(rs);
  vector<vector<RuleField>> expected(rs.rules.size());
  expected[5] = expected[6] = expected[7] = vector {
    RuleField{
      .field_name = "items",
      .schema_source = 9,
      .container = RuleField::vector,
    }
  };
  expected[10] = vector {
    RuleField{
      .field_name = "items",
      .schema_source = 9,
      .container = RuleField::single,
    }
  };
  for(ssize_t i=0; i<ssize(rs.rules); ++i) {
    const vector<RuleField>& observed = rs.rules[i]->flatFields();
    if(expected[i].size() != observed.size() ||
       !equal(expected[i].begin(), expected[i].end(),
              observed.begin(), ruleFieldEqual))
      BugMe("Mismatch at index {}", i);
  }
}

void testFlatWrappers() {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        AliasRule{1},
        WordPreserving{"hello", 0},
        ConcatFlatRule{{ {0, "kw"} }},
        ErrorRule{"Was expecting a hello"},
        OrRule{{ {-1, 2, passthroughTmpl}, {-1, 3, passthroughTmpl} },
               true /* flattenOnDemand */ },
        OrRule{{ {-1, 0, JsonTmpl::Map{ std::pair{"s", passthroughTmpl} } },
                 {-1, 3, passthroughTmpl} },
               true /* flattenOnDemand */ }
    ),
    .skips{cskip},
    .regexOpts = {regexOpts},
  };
  resolveWrapperTypes(rs);
  populateFlatFields(rs);
  for(const RuleField& f: rs.rules[4]->flatFields()) {
    if(dynamic_cast<const AliasRule*>(rs.rules[f.schema_source].get()))
      Bug("Rule 4 points to flat wrapper AliasRule at position {}",
          f.schema_source);
  assertEqual("Rule 5 needs a field", rs.rules[5]->flatFields().size(), 1u);
  }
}

}  // namespace

int main() {
  testFlatFieldsForNonFlatRules();
  testFlatFieldsForNestedList();
  testFlatWrappers();
}
