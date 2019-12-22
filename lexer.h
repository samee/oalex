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
#include "input_view.h"
#include "segment.h"

namespace oalex::lex {

struct Diag {
  enum Severity { error,warning,note } severity;
  size_t stLine, stPos, enLine, enPos;  // These ranges are inclusive.
  std::string msg;
  Diag(const Input& input, size_t st, size_t en, Severity sev, std::string msg)
    : severity(sev), msg(msg) {
    std::tie(stLine,stPos) = input.rowCol(st);
    std::tie(enLine,enPos) = input.rowCol(--en);
  }
  explicit operator std::string() const;
};

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

struct Lexer {
  Input input;
  std::vector<Diag> diags;

  // throws, never returns.
  [[noreturn]] void FatalBug(size_t st,size_t en,std::string msg) const;
  [[noreturn]] void Fatal(size_t st,size_t en,std::string msg) const;

  std::nullopt_t Error(size_t st,size_t en,std::string msg);
  std::nullopt_t Warning(size_t st,size_t en,std::string msg);
  std::nullopt_t Note(size_t st,size_t en,std::string msg);

  [[noreturn]] void FatalBug(size_t pos,std::string msg) const
    { FatalBug(pos, pos+1, std::move(msg)); }
  [[noreturn]] void Fatal(size_t pos,std::string msg) const
    { Fatal(pos, pos+1, std::move(msg)); }
  std::nullopt_t Error(size_t pos,std::string msg)
    { return Error(pos, pos+1, std::move(msg)); }
  std::nullopt_t Warning(size_t pos,std::string msg)
    { return Warning(pos, pos+1, std::move(msg)); }
  std::nullopt_t Note(size_t pos,std::string msg)
    { return Note(pos, pos+1, std::move(msg)); }
};

std::optional<std::vector<UnquotedToken>>
  lexSectionHeader(Lexer& lex, size_t& i);
std::optional<QuotedString> lexQuotedString(Lexer& lex, size_t& i);
std::optional<QuotedString> lexDelimitedSource(Lexer& lex, size_t& i);
std::optional<QuotedString> lexIndentedSource(Lexer& lex, size_t& i,
    std::string_view parindent);
std::optional<BracketGroup> lexBracketGroup(Lexer& lex, size_t& i);

// Returns nullopt on eof. Throws on invalid language character.
std::optional<UnquotedToken> lookahead(const Lexer& lex, size_t i);

}  // namespace oalex::lex
