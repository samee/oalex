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
#include "runtime/jsonloc.h"
#include "runtime/lookahead_regex.h"  // TODO rename to regex.h, remove ns.
#include "runtime/skipper.h"

namespace oalex {

// Some of this is not specific to codegen, and should move elsewhere.

struct ConcatRule {
  std::string name;
  std::vector<size_t> components;
  std::vector<std::string> outputPlaceholders;
  JsonLoc outputTmpl;
};

struct SkipPoint {
  bool stayWithinLine = false;  // If true, skip comments should end in '\n'
  const Skipper* skip;  // usually &RuleSet::skip, but can be overridden.
};

using Rule = std::variant<std::string, regex::Regex, SkipPoint, ConcatRule>;

struct RuleSet {
  std::vector<Rule> rules;
  Skipper skip;
};

using OutputStream = std::function<void(std::string_view)>;
void codegen(const RuleSet& ruleset, ssize_t ruleIndex, OutputStream& os);

JsonLoc eval(InputDiags& ctx, ssize_t& i,
             const RuleSet& ruleset, ssize_t ruleIndex);

}  // namespace oalex
