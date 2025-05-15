/*  Copyright 2019-2024 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#pragma once

#include <string>
#include <string_view>
#include <vector>
#include <utility>
#include "ident.h"
#include "jsontmpl.h"

namespace oalex {

// Forward decls.
class Rule;
struct RuleSet;

// A RuleSlot isn't the best name, but it is the return type of dependencyOrder
// computation. The generated code will have rules in this order.
//
// Dev-note: I am sorely missing sum types.
struct RuleSlot {
  enum class Type { definition, forwardDecl };
  ssize_t ruleidx;
  Type slotType;
};
std::vector<RuleSlot>
dependencyOrderForCodegen(const RuleSet& rs);

// Computes the ruleset.rules[].exposure() values for anything not yet set.
// The assumptions are:
//
// * populateFlatFields() has already run,
//   and we can use that to determine field types.
// * top-level rules already has exposure set to `topLevel`. The exposure is how
//   we identify them as top-level here.
// * string rules are also assumed to have their exposure set as such, just
//   for the ease of implementation. It's easy to compute this here as well,
//   if we later decide to.
void computeUserExposureForTypes(RuleSet& ruleset);

class OutputStream {
 public:
  virtual void operator()(std::string_view) const = 0;
  virtual ~OutputStream() {}
};

void codegen(const RuleSet& ruleset, ssize_t ruleIndex,
             const OutputStream& cppos, const OutputStream& hos);
void codegenDefaultRegexOptions(const RuleSet& ruleset,
                                const OutputStream& cppos);
void codegenForwardDecl(const RuleSet& ruleset, ssize_t ruleIndex,
                        const OutputStream& hos);

JsonLoc eval(InputDiags& ctx, ssize_t& i,
             const RuleSet& ruleset, ssize_t ruleIndex);

/* This is the same as eval, but is sandwiched between Skipper calls so as to
   skip over comments and spaces. This only works if:

     ruleset.rules[i].context_skipper() != Rule::helperRuleNoContext

  Dev-note: Right now, there is no analog for this in codegen(). The user can
  always manually call the appropriate skipper (if they know the name). Later,
  we can provide some syntax for explicitly naming these:

    derive rule bigfoo: trim(foo)
    derive rule skipfoo: skip(foo)
*/
JsonLoc trimAndEval(InputDiags& ctx, ssize_t& i,
                    const RuleSet& ruleset, ssize_t ruleIndex);

// TODO: Move to analysis.cpp.
ssize_t flatWrapperTarget(const Rule& rule);
bool resultFlattenableOrError(const RuleSet& rs, ssize_t ruleidx);
ssize_t resolveIfWrapper(const RuleSet& ruleset, ssize_t target);

}  // namespace oalex
