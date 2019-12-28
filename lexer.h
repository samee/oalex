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
#include <optional>
#include <variant>
#include <vector>
#include "segment.h"
#include "runtime/diags.h"
#include "runtime/input_view.h"
#include "runtime/skipper.h"

namespace oalex::lex {

enum class LexSegmentTag {
  unquotedToken = Segment::lastReservedTag + 1,
  section,
  quotedString,
  bracketGroup,
};

// This class is mostly to document which Segment types belong to the lexer.
struct LexSegment : Segment {
  LexSegment(size_t st,size_t en,Segment::tagint_t type_tag)
    : Segment{st,en,type_tag} {}
};

struct UnquotedToken : LexSegment {
  static constexpr auto type_tag = tagint_t(LexSegmentTag::unquotedToken);
  std::string token;
  UnquotedToken(size_t st,size_t en,const Input& input)
    : LexSegment(st,en,type_tag), token(input.substr(st,en-st)) {}
  UnquotedToken(size_t st,size_t en,std::string tok)
    : LexSegment(st,en,type_tag), token(std::move(tok)) {}
  const std::string& operator*() const { return token; }
};

struct QuotedString : LexSegment {
  static constexpr auto type_tag = tagint_t(LexSegmentTag::quotedString);
  std::string s;  // escape codes already interpreted.
  QuotedString(size_t st,size_t en,std::string_view s)
    : LexSegment(st,en,type_tag), s(s) {}
};

struct BracketGroup;

using ExprToken = std::variant<UnquotedToken, QuotedString, BracketGroup>;
enum class ExprType { unquotedToken, quotedString, bracketGroup };

enum class BracketType { square, brace, paren, };

struct BracketGroup : LexSegment {
  static constexpr auto type_tag = tagint_t(LexSegmentTag::bracketGroup);
  BracketType type;
  std::vector<ExprToken> children;
  BracketGroup(size_t st,size_t en,BracketType t)
    : LexSegment(st,en,type_tag), type(t), children() {}
};

inline const LexSegment& lexSegment(const ExprToken& x) {
  return *std::visit([](const auto& x) { return (const LexSegment*)(&x); }, x);
}
inline size_t stPos(const ExprToken& x) { return lexSegment(x).stPos; }
inline size_t enPos(const ExprToken& x) { return lexSegment(x).enPos; }

inline ExprType exprType(const ExprToken& expr)
  { return ExprType(expr.index()); }

inline bool isToken(const ExprToken& x, std::string_view s) {
  if(auto* tok = std::get_if<UnquotedToken>(&x))
    if(tok->token == s) return true;
 return false;
}

inline Skipper skip{{{"#","\n"}}, {}};
inline Skipper wskip{};

std::optional<std::vector<UnquotedToken>>
  lexSectionHeader(InputDiags& lex, size_t& i);
std::optional<QuotedString> lexQuotedString(InputDiags& lex, size_t& i);
std::optional<QuotedString> lexDelimitedSource(InputDiags& lex, size_t& i);
std::optional<QuotedString> lexIndentedSource(InputDiags& lex, size_t& i,
    std::string_view parindent);
std::optional<BracketGroup> lexBracketGroup(InputDiags& lex, size_t& i);

// Returns nullopt on eof. Throws on invalid language character.
std::optional<UnquotedToken> lookahead(InputDiags& lex, size_t i);

}  // namespace oalex::lex
