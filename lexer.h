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
#include <vector>
#include "input_view_manual.h"
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
  bracketed,
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
  const std::string& operator*() const { return token; }
};

struct QuotedString : LexSegment {
  static constexpr auto type_tag = tagint_t(LexSegmentTag::quotedString);
  std::string s;  // escape codes already interpreted.
  QuotedString(size_t st,size_t en,std::string_view s)
    : LexSegment(st,en,type_tag), s(s) {}
};

struct Lexer {
  Input input;
  std::vector<Diag> diags;

  // throws, never returns.
  [[noreturn]] void FatalBug(size_t st,size_t en,std::string msg) const;
  [[noreturn]] void Fatal(size_t st,size_t en,std::string msg) const;

  std::nullopt_t Error(size_t st,size_t en,std::string msg);
  std::nullopt_t Warning(size_t st,size_t en,std::string msg);
  std::nullopt_t Note(size_t st,size_t en,std::string msg);
};

std::optional<std::vector<UnquotedToken>>
  lexSectionHeader(Lexer& lex, size_t& i);
std::optional<QuotedString> lexQuotedString(Lexer& lex, size_t& i);
std::optional<QuotedString> lexDelimitedSource(Lexer& lex, size_t& i);
std::optional<QuotedString> lexIndentedSource(Lexer& lex, size_t& i,
    std::string_view parindent);

// Returns nullopt on eof. Throws on invalid language character.
std::optional<UnquotedToken> lookahead(const Lexer& lex, size_t i);

}  // namespace oalex::lex
