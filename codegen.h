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

#include <functional>
#include <string>
#include <string_view>
#include <vector>
#include <utility>
#include "jsonloc.h"
#include "runtime/lookahead_regex.h"  // TODO rename to regex.h, remove ns.
#include "runtime/skipper.h"

namespace oalex {

// Some of this is not specific to codegen, and should move elsewhere.

// We skip before every component.
struct ConcatRule {
  std::string name;
  std::vector<size_t> components;
  std::vector<std::string> outputPlaceholders;
  Skipper* skip;  // Frequently nullptr, which defaults to RuleSet::skip.
  JsonLoc outputTmpl;
};

// TODO add explicit skip-point, rather than tacking it onto ConcatRule.
using Rule = std::variant<std::string, regex::Regex, ConcatRule>;

struct RuleSet {
  std::vector<Rule> rules;
  Skipper skip;
};

using OutputStream = std::function<void(std::string_view)>;
void codegen(const RuleSet& ruleset, ssize_t ruleIndex, OutputStream& os);

// TODO JsonLoc should move to runtime/, at least the type definition.
//   Maybe not the parsing or printing parts.
JsonLoc eval(InputDiags& ctx, ssize_t& i,
             const RuleSet& ruleset, ssize_t ruleIndex);

}  // namespace oalex
