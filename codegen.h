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
  std::vector<Component> comps;
  JsonLoc outputTmpl;
};

struct SkipPoint {
  bool stayWithinLine = false;  // If true, skip comments should end in '\n'
  const Skipper* skip;  // usually &RuleSet::skip, but can be overridden.
};

struct Rule {
  // TODO other component types like RawString and Callback (with nested
  // components).
  template <class X> explicit Rule(X x) : specifics_(std::move(x)), name_() {}
  template <class X> Rule(X x, std::string name) :
    specifics_(std::move(x)), name_(std::move(name)) {}
  std::string specifics_typename() const;  // Used for debugging/logging.
  std::optional<std::string> name() const {
    if(name_.empty()) return std::nullopt; else return name_;
  }

  template <class X> friend X* get_if(Rule* rule);
  template <class X> friend const X* get_if(const Rule* rule);
 private:
  std::variant<std::string, Regex, SkipPoint, ConcatRule> specifics_;
  std::string name_;
};

template <class X> X* get_if(Rule* rule) {
  return std::get_if<X>(&rule->specifics_);
}

template <class X> const X* get_if(const Rule* rule) {
  return std::get_if<X>(&rule->specifics_);
}

struct RuleSet {
  std::vector<Rule> rules;
  Skipper skip;  // TODO use this
  RegexOptions regexOpts;
};

using OutputStream = std::function<void(std::string_view)>;
void codegen(const RuleSet& ruleset, ssize_t ruleIndex,
             const OutputStream& cppos, const OutputStream& hos);
void codegenDefaultRegexOptions(const RuleSet& ruleset,
                                const OutputStream& cppos);

JsonLoc eval(InputDiags& ctx, ssize_t& i,
             const RuleSet& ruleset, ssize_t ruleIndex);

}  // namespace oalex
