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

#pragma once
#include <vector>

namespace oalex {

struct RuleSet;

// Populates ruleset.rules[].flatFields(). This is used after a RuleSet
// object is completed by the compiler, but before it is passed on to a backend.
// Currently it is only used by the codegen() backend, and not eval().
void populateFlatFields(RuleSet& ruleset);

// Populates WrapperRule::typeSource, after checking that we don't have a
// cycle of wrappers.
void resolveWrapperTypes(RuleSet& ruleset);

// A RuleSlot isn't the best name, but it is the return type of dependencyOrder
// computation. The generated code will have rules in this order.
//
// Dev-note: I am sorely missing sum types.
struct RuleSlot {
  enum class Type { definition, forwardDecl };
  std::ptrdiff_t ruleidx;
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

}  // namespace oalex
