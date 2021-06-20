/*  Copyright 2020 The oalex authors.

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

#include "codegen.h"
#include "regex_io.h"

namespace oalex::test {

inline const Skipper cskip{ {{"/*","*/"},{"//","\n"}}, {}};
inline const RegexOptions regexOpts{
  .word = oalex::parseRegexCharSet("[0-9A-Za-z_]")
};

template <class X>
RuleVariant nmRule(X x, std::string s) {
  return RuleVariant{std::move(x), Ident::parseGenerated(s)};
}

inline RuleSet singletonRuleSet(RuleVariant r) {
  RuleSet rs{{}, regexOpts};
  rs.rules.push_back(move_to_unique(r));
  return rs;
}

// X is expected to be one of the variants of Rule::specifics.
template <class X> RuleSet singletonRuleSet(X x) {
  return singletonRuleSet(RuleVariant{std::move(x)});
}

void assertJsonLocIsString(std::string_view testName, const JsonLoc& jsloc,
                           std::string_view s, size_t stPos, size_t enPos);

}  // namespace oalex::test
