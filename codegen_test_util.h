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

#include "codegen.h"
#include "runtime/util.h"
// TODO move function bodies out of this header file, and remove util.h.

namespace oalex::test {

inline const Skipper cskip{ {{"/*","*/"},{"//","\n"}}, {}};
extern const RegexOptions regexOpts;

std::unique_ptr<const Regex> parseRegex(std::string_view s);
RegexRule parseRegexRule(std::string_view s);

inline StringRule nmRule(const char* s, std::string name) {
  return {s, Ident::parseGenerated(name)};
}

template <class X> X nmRule(X x, std::string s) {
  x.deferred_name(Ident::parseGenerated(s));
  return x;
}

inline RuleSet singletonRuleSet(const char* s) {
  RuleSet rs{{}, {}, regexOpts};
  rs.rules.push_back(move_to_unique(StringRule{s}));
  return rs;
}

template <class X> RuleSet singletonRuleSet(X x) {
  RuleSet rs{{}, {}, regexOpts};
  rs.rules.push_back(move_to_unique(x));
  return rs;
}
template <> RuleSet singletonRuleSet<SkipPoint>(SkipPoint sp) = delete;

void assertJsonLocIsString(std::string_view testName, const JsonLoc& jsloc,
                           std::string_view s, size_t stPos, size_t enPos);

// TODO use LocPair as the actual type.
void
assertLocPairEqual(std::string_view msg,
                   size_t stPosExpected, size_t enPosExpected,
                   const JsonLoc& jsloc);

}  // namespace oalex::test
