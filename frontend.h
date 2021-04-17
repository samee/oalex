/*  Copyright 2020-2021 The oalex authors.

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
#include "ident.h"
#include "oalex.h"
#include "codegen.h"

namespace oalex {

// Forward decl
class Example;

// This should be a fairly direct representation of our oalex source file.
// Right now, that has a bunch of ruleSets. As our language evolves, it will
// include lexer directives, let rules, examples, and so on. RuleSet will likely
// not remain a field here forever.
struct ParsedSource {
  RuleSet ruleSet;
  std::vector<Example> examples;
};

// This is the output component of each example.
// Initialized either as Expectation::Success, or as
// Expectation::ErrorSubstr{msg}.
class Expectation {
 public:
  // Constructor tags
  static struct Success_t {} Success;
  struct SuccessWithJson { JsonLoc value; };
  struct ErrorSubstr { std::string msg; };

  Expectation() = default;
  Expectation(Success_t) : success_{true} {}  // implicit ctor
  Expectation(SuccessWithJson jsloc)
    : success_{true}, jsloc_{std::move(jsloc.value)} {}
  Expectation(ErrorSubstr f)  // implicit ctor
    : success_{false}, errorSubstr_{std::move(f.msg)} {}

  bool matches(const JsonLoc& jsloc, const std::vector<Diag>& diags) const;

  bool isForSuccess() const { return success_; }
  std::optional<JsonLoc> jsloc() const {
    if(success_ && jsloc_.supportsEquality()) return jsloc_;
    else return std::nullopt;
  }
  std::optional<std::string> isForErrorSubstr() const {
    if(success_) return std::nullopt;
    else return errorSubstr_;
  }
 private:
  bool success_ = false;
  std::string errorSubstr_;
  JsonLoc jsloc_{JsonLoc::ErrorValue{}};  // jsloc_.supportsEquality() == false
};

// Alternative to storing stPos, if we don't want to carry around InputDiags.
// This is likely to be "heavier", since we will probably add filename and
// start/end fields. Don't rely on these specific fields being stable.
// We might merge this with runtime/diags.h, which needs two of these (start
// and end).
struct MappedPos {
  size_t line;
  explicit operator std::string() const;
};

struct Example {
  MappedPos mappedPos;
  Ident ruleName;
  std::string sampleInput;
  Expectation expectation;
  static bool runSucceeded(const JsonLoc& jsloc,
                           const std::vector<Diag>& diags)
    { return !jsloc.holdsError() && diags.empty(); }
};

std::string describeTestFailure(const Example& ex, bool succeeded);

auto parseOalexSource(InputDiags& ctx) -> std::optional<ParsedSource>;

}  // namespace oalex
