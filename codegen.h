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

#pragma once

#include <functional>
#include <string>
#include <string_view>
#include <vector>
#include <utility>
#include "runtime/jsonloc.h"
#include "runtime/regex.h"
#include "runtime/skipper.h"

namespace oalex {

// Some of this is not specific to codegen, and should move elsewhere.

struct ConcatRule {
  // empty outputPlaceholder means the component is discarded.
  struct Component { ssize_t idx; std::string outputPlaceholder; };
  std::string name;
  std::vector<Component> comps;
  JsonLoc outputTmpl;
};

struct SkipPoint {
  bool stayWithinLine = false;  // If true, skip comments should end in '\n'
  const Skipper* skip;  // usually &RuleSet::skip, but can be overridden.
};

// TODO other component types like RawString and Callback (with nested
// components).
using Rule = std::variant<std::string, Regex, SkipPoint, ConcatRule>;

struct RuleSet {
  std::vector<Rule> rules;
  Skipper skip;  // TODO use this
  RegexOptions regexOpts;
};

using OutputStream = std::function<void(std::string_view)>;
void codegen(const RuleSet& ruleset, ssize_t ruleIndex, OutputStream& os);

JsonLoc eval(InputDiags& ctx, ssize_t& i,
             const RuleSet& ruleset, ssize_t ruleIndex);

}  // namespace oalex
