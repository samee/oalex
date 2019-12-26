/*  Copyright 2019 Google LLC

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

struct UnquotedMatcher;
struct QuotedMatcher;
struct BracketGroupMatcher;

using ExprMatcher = std::variant<UnquotedMatcher,
                                 QuotedMatcher, BracketGroupMatcher>;

struct QuotedMatcher { std::string s; };
inline QuotedMatcher quoted(std::string s)
  { return QuotedMatcher{std::move(s)}; }

struct UnquotedMatcher {
  std::string token;
  UnquotedMatcher(const char* s) : token(s) {}  // conversion ctor.
};

struct BracketGroupMatcher {
  BracketType type;
  std::vector<ExprMatcher> children;
};

namespace internal {

// Introduces a dummy type-dependence for use in static_assert.
template <class T> inline const bool false_value = false;

inline ExprMatcher exprMatcher(BracketGroupMatcher bm) { return bm; }
inline ExprMatcher exprMatcher(UnquotedMatcher tokm) { return tokm; }
inline ExprMatcher exprMatcher(QuotedMatcher qm) { return qm; }

}  // namespace internal

template <class ... Args> BracketGroupMatcher squareBrackets(Args ... args) {
  return BracketGroupMatcher{BracketType::square,
                             {internal::exprMatcher(args)...}};
}

template <class ... Args> BracketGroupMatcher braces(Args ... args) {
  return BracketGroupMatcher{BracketType::brace,
                             {internal::exprMatcher(args)...}};
}

template <class ... Args> BracketGroupMatcher parens(Args ... args) {
  return BracketGroupMatcher{BracketType::paren,
                             {internal::exprMatcher(args)...}};
}

// Returns error. `nullopt` means no error.
std::optional<std::string> match(ExprMatcher pattern, ExprToken expr);

// Remove this when we have something in lexer_test.cpp
namespace test {
  inline const ExprMatcher m =
    squareBrackets("a", "+", squareBrackets("b",",",quoted("c")));

}  // namespace test

}  // namespace oalex::lex::matcher