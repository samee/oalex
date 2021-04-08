/*  Copyright 2019-2021 The oalex authors.

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
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lexer.h"

/*
Intended usage: mostly for tests.

  ExprToken expr = functionUnderTest();
  ExprMatcher pattern = squareBracket("A","*",braces("B","C","D"));
  if(auto error = match(pattern,expr)) return *error;
  else return success;
*/
namespace oalex::lex::matcher {

struct WholeSegmentMatcher;
struct GluedMatcher;
struct RegexPatternMatcher {};
struct BracketGroupMatcher;

using ExprMatcher = std::variant<WholeSegmentMatcher, GluedMatcher,
                                 RegexPatternMatcher, BracketGroupMatcher>;

struct GluedMatcher { std::string s; };
inline GluedMatcher glued(std::string s)
  { return GluedMatcher{std::move(s)}; }

struct WholeSegmentMatcher {
  std::string data;
  WholeSegmentMatcher(const char* s) : data(s) {}  // conversion ctor.
};

struct BracketGroupMatcher {
  BracketType type;
  std::vector<ExprMatcher> children;
};

namespace internal {

// Introduces a dummy type-dependence for use in static_assert.
template <class T> inline const bool false_value = false;

inline ExprMatcher exprMatcher(BracketGroupMatcher bm) { return bm; }
inline ExprMatcher exprMatcher(WholeSegmentMatcher wm) { return wm; }
inline ExprMatcher exprMatcher(RegexPatternMatcher rm) { return rm; }
inline ExprMatcher exprMatcher(GluedMatcher gm) { return gm; }

}  // namespace internal

template <class ... Args> std::vector<ExprMatcher> matchvec(Args&& ... args) {
  return {internal::exprMatcher(std::forward<Args>(args))...};
}

template <class ... Args> BracketGroupMatcher squareBrackets(Args&& ... args) {
  return BracketGroupMatcher{BracketType::square,
                             matchvec(std::forward<Args>(args)...)};
}

template <class ... Args> BracketGroupMatcher braces(Args&& ... args) {
  return BracketGroupMatcher{BracketType::brace,
                             matchvec(std::forward<Args>(args)...)};
}

template <class ... Args> BracketGroupMatcher parens(Args&& ... args) {
  return BracketGroupMatcher{BracketType::paren,
                             matchvec(std::forward<Args>(args)...)};
}

// Returns error. `nullopt` means no error.
std::optional<std::string> match(ExprMatcher pattern, const ExprToken& expr);

// Remove this when we have something in lexer_test.cpp
namespace test {
  inline const ExprMatcher m =
    squareBrackets("a", "+", squareBrackets("b",",",glued("c")));

}  // namespace test

}  // namespace oalex::lex::matcher
