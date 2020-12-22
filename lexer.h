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
#include "regex_io.h"
#include "runtime/diags.h"
#include "runtime/input_view.h"
#include "runtime/skipper.h"

namespace oalex::lex {

class QuotedString;

struct IndexRelation {
  size_t inputPos;
  size_t quotePos;
};

enum class LexSegmentTag {
  unquotedToken = Segment::lastReservedTag + 1,
  section,
  quotedString,
  bracketGroup,
  newlineChar,
};

// This class is mostly to document which Segment types belong to the lexer.
struct LexSegment : Segment {
  LexSegment(size_t st,size_t en,Segment::tagint_t type_tag)
    : Segment{st,en,type_tag} {}
};

struct NewlineChar : LexSegment {
  explicit NewlineChar(const QuotedString& s, size_t pos);
};

// These tokens never have embedded newlines, unless it's a newline all
// by itself. These are meant to be a lightweight string wrapper, and do not
// use complex rowCol() maps. This also disallows any backslash escape sequence
// such as '\n' or '\t'.
struct UnquotedToken : LexSegment {
  static constexpr auto type_tag = tagint_t(LexSegmentTag::unquotedToken);
  std::string token;
  UnquotedToken(size_t st,size_t en,const Input& input)
    : LexSegment(st,en,type_tag), token(input.substr(st,en-st)) {}
  UnquotedToken(size_t st,size_t en,std::string tok)
    : LexSegment(st,en,type_tag), token(std::move(tok)) {}

  // This should only be used for short and simple tokens without newlines or
  // escape codes embedded in it, since that will mess up location-tracking.
  explicit UnquotedToken(const QuotedString& s);
  const std::string& operator*() const { return token; }
  const std::string* operator->() const { return &token; }
};

// These are the factory methods ensure the following invariants on rcmap:
//   * rcmap.pos are provided in strictly increasing order.
//   * rcmap[i].pos < rcmap[j].pos iff
//       (rcmap[i].row, rcmap[i].col) < (rcmap[j].row, rcmap[j].col)
//
// Alternate approaches:
//   Q: Why don't we just keep the entire file in memory, representing strings
//      as just an std::string_view?
//   A: We will still need to keep location information for error messages.
//
//   Q: Okay, then why don't we make our own simple string_view class that uses
//      file indices instead of pointers? We already have rowCol() map for those
//      offsets in class Input.
//   A: QuotedString often represents processed strings, after escape codes
//      and other quoted constructs have been decoded. Bytes in the input file
//      do not always correspond to bytes in a QuotedString.
class QuotedString final : public LexSegment, public InputPiece {
 public:
  static constexpr auto type_tag = tagint_t(LexSegmentTag::quotedString);
  friend auto lexQuotedString(InputDiags& ctx, size_t& i)
    -> std::optional<QuotedString>;
  friend auto lexDelimitedSource(InputDiags& ctx, size_t& i)
    -> std::optional<QuotedString>;
  friend auto lexIndentedSource(InputDiags& ctx, size_t& i,
                                std::string_view parindent)
    -> std::optional<QuotedString>;

  char operator[](size_t pos) const final { return s_[pos]; }
  bool sizeGt(size_t pos) const final { return s_.size() > pos; }
  std::pair<size_t,size_t> rowCol(size_t pos) const final;
  size_t inputPos(size_t pos) const;
  operator std::string_view() const { return s_; }
  operator std::string() const { return s_; }
  bool empty() const { return s_.empty(); }
  size_t size() const { return s_.size(); }
  using InputPiece::hasPrefix;
  bool hasPrefix(size_t pos, std::string_view s) const final
    { return substr(pos, s.size()) == s; }
  std::string_view substr(size_t pos, size_t len) const
    { return std::string_view(s_).substr(pos, len); }
  QuotedString subqstr(size_t pos, size_t len) const;
  size_t find(std::string_view s, size_t st=0) const noexcept
    { return s_.find(s, st); }
  size_t find(char ch, size_t st=0) const noexcept final
    { return s_.find(ch, st); }
  size_t bol(size_t i) const final;

  operator InputDiagsRef() const { return {this, &ctx_->diags}; }  // implicit
 private:
  std::string s_;  // escape codes already interpreted.
  InputDiags* ctx_;  // Used for adding diags and implementing InputPiece.
  std::vector<IndexRelation> index_map_;
  QuotedString(size_t st, size_t en, std::string_view s,
               InputDiags* ctx, std::vector<IndexRelation> imap)
    : LexSegment(st,en,type_tag), s_(s),
      ctx_(ctx), index_map_(std::move(imap)) {}
};

inline bool operator==(const QuotedString& a, const QuotedString& b) {
  return std::string_view(a) == std::string_view(b);
}
inline bool operator!=(const QuotedString& a, const QuotedString& b) {
  return std::string_view(a) != std::string_view(b);
}

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

// Note: right now, we don't worry about unterminated comments in lexer.cpp.
// We will need to change that if we add more comment delimitters here later.
inline Skipper oalexSkip{{{"#","\n"}}, {}};
inline Skipper oalexWSkip{};
inline RegexOptions oalexRegexOpts{
  // Do not use user-supplied input. See regex_io.h for details.
  .word = parseRegexCharSet("[0-9A-Za-z_]")
};

std::optional<char> lexHexCode(InputDiags& ctx, size_t& i);
std::optional<std::vector<UnquotedToken>>
  lexSectionHeader(InputDiags& lex, size_t& i);
std::optional<QuotedString> lexQuotedString(InputDiags& lex, size_t& i);
std::optional<QuotedString> lexDelimitedSource(InputDiags& lex, size_t& i);
std::optional<QuotedString> lexIndentedSource(InputDiags& lex, size_t& i,
    std::string_view parindent);
std::optional<BracketGroup> lexBracketGroup(InputDiags& lex, size_t& i);
std::optional<std::vector<ExprToken>> lexNextLine(InputDiags& lex, size_t& i);

// Returns nullopt on eof. Throws on invalid language character.
std::optional<UnquotedToken> lookahead(InputDiags& lex, size_t i);

}  // namespace oalex::lex
