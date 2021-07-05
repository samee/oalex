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
#include <optional>
#include <variant>
#include <vector>
#include "segment.h"
#include "runtime/regex.h"
#include "runtime/diags.h"
#include "runtime/input_view.h"
#include "runtime/skipper.h"

namespace oalex {

namespace lex {

class GluedString;

struct IndexRelation {
  size_t inputPos;
  size_t quotePos;
};

enum class LexSegmentTag {
  section = Segment::lastReservedTag + 1,
  gluedString,
  bracketGroup,
  newlineChar,
  regexPattern,
};

struct NewlineChar : Segment {
  explicit NewlineChar(const GluedString& s, size_t pos);
};

inline WholeSegment
inputSegment(size_t st, size_t en, const Input& input) {
  return WholeSegment{st, en, input.substr(st, en-st)};
}

// The factory friend function ensure the following invariants on rcmap:
//   * rcmap.pos are provided in strictly increasing order.
//   * rcmap[i].pos < rcmap[j].pos iff
//       (rcmap[i].row, rcmap[i].col) < (rcmap[j].row, rcmap[j].col)
// Being able to calculate all of the above means index_map_ is never empty.
//
// Alternate approaches:
//   Q: Why don't we just keep the entire file in memory, representing strings
//      as just an std::string_view?
//   A: We will still need to keep location information for error messages.
//
//   Q: Okay, then why don't we make our own simple string_view class that uses
//      file indices instead of pointers? We already have rowCol() map for those
//      offsets in class Input.
//   A: GluedString often represents processed strings, after escape codes
//      and other quoted constructs have been decoded. Bytes in the input file
//      do not always correspond to bytes in a GluedString.
class GluedString final : public Segment, public InputPiece {
 public:
  static constexpr auto type_tag = tagint_t(LexSegmentTag::gluedString);
  enum class Ctor { dquoted, squoted, fenced, indented, subqstr, wholeSegment };
  explicit GluedString(WholeSegment s);
  friend auto lexQuotedString(InputDiags& ctx, size_t& i)
    -> std::optional<GluedString>;
  friend auto lexFencedSource(InputDiags& ctx, size_t& i)
    -> std::optional<GluedString>;
  friend auto lexIndentedSource(InputDiags& ctx, size_t& i,
                                std::string_view parindent)
    -> std::optional<GluedString>;

  char operator[](size_t pos) const final { return s_[pos]; }
  bool sizeGt(size_t pos) const final { return s_.size() > pos; }
  size_t inputPos(size_t pos) const;
  operator std::string_view() const { return s_; }
  operator std::string() const { return s_; }
  bool empty() const { return s_.empty(); }
  size_t size() const { return s_.size(); }
  using InputPiece::hasPrefix;
  bool hasPrefix(size_t pos, std::string_view s) const final
    { return substr(pos, s.size()) == s; }
  std::string substr(size_t pos, size_t len) const final
    { return s_.substr(pos, len); }
  GluedString subqstr(size_t pos, size_t len) const;
  size_t find(std::string_view s, size_t st=0) const noexcept
    { return s_.find(s, st); }
  size_t find(char ch, size_t st=0) const noexcept final
    { return s_.find(ch, st); }
  size_t bol(size_t i) const final;

  // Doesn't support trailing newlines yet.
  std::optional<WholeSegment> getSegment() const;
  Ctor ctor() const { return ctor_; }

 private:
  std::string s_;  // escape codes already interpreted.
  Ctor ctor_;  // Records how this object was constructed.
  std::vector<IndexRelation> index_map_;
  GluedString(size_t st, size_t en, std::string_view s, Ctor ctor,
              std::vector<IndexRelation> imap)
    : Segment{st,en,type_tag}, s_(s), ctor_(ctor),
      index_map_(std::move(imap)) {}
  GluedString() = delete;
};

inline bool operator==(const GluedString& a, const GluedString& b) {
  return std::string_view(a) == std::string_view(b);
}
inline bool operator!=(const GluedString& a, const GluedString& b) {
  return std::string_view(a) != std::string_view(b);
}

struct RegexPattern : Segment {
  static constexpr auto type_tag = tagint_t(LexSegmentTag::regexPattern);
  std::unique_ptr<const Regex> patt;
  RegexPattern(size_t st, size_t en, std::unique_ptr<const Regex> patt)
    : Segment{st, en, type_tag}, patt(std::move(patt)) {}
};

struct BracketGroup;

using ExprToken = std::variant<WholeSegment, GluedString,
                               RegexPattern, BracketGroup>;
enum class ExprType { wholeSegment, gluedString, regexPattern, bracketGroup };

enum class BracketType { square, brace, paren, };

struct BracketGroup : Segment {
  static constexpr auto type_tag = tagint_t(LexSegmentTag::bracketGroup);
  BracketType type;
  std::vector<ExprToken> children;
  BracketGroup(size_t st,size_t en,BracketType t)
    : Segment{st,en,type_tag}, type(t), children() {}

  // This type is move-only, because we want to support unique_ptr components.
  BracketGroup(const BracketGroup&) = delete;
  BracketGroup& operator=(const BracketGroup&) = delete;
  BracketGroup(BracketGroup&&) noexcept = default;
  BracketGroup& operator=(BracketGroup&&) noexcept = default;
};

// Identity function, used in diags.h helpers below.
inline const Segment& segment(const Segment& x) { return x; }
inline const Segment& segment(const ExprToken& x) {
  // Use static_cast to disambiguate between overloads.
  using cseg = const Segment;
  return std::visit(static_cast<cseg&(*)(cseg&)>(&segment), x);
}
const char* exprTagName(const ExprToken& t);

inline size_t stPos(const ExprToken& x) { return segment(x).stPos; }
inline size_t enPos(const ExprToken& x) { return segment(x).enPos; }

inline ExprType exprType(const ExprToken& expr)
  { return ExprType(expr.index()); }

inline bool isToken(const ExprToken& x, std::string_view s) {
  if(auto* tok = std::get_if<WholeSegment>(&x))
    if(tok->data == s) return true;
 return false;
}

// Note: right now, we don't worry about unterminated comments in lexer.cpp.
// We will need to change that if we add more comment delimitters here later.
inline Skipper oalexSkip{{{"#","\n"}}, {}};
inline Skipper oalexWSkip{};
extern RegexOptions oalexRegexOpts;

enum class IndentCmp { bad, lt, eq, gt };
IndentCmp indentCmp(std::string_view indent1, std::string_view indent2);

std::optional<char> lexHexCode(InputDiags& ctx, size_t& i);
std::vector<WholeSegment> lexSectionHeader(InputDiags& lex, size_t& i);
std::optional<GluedString> lexQuotedString(InputDiags& lex, size_t& i);
std::optional<GluedString> lexFencedSource(InputDiags& lex, size_t& i);
std::optional<GluedString> lexIndentedSource(InputDiags& lex, size_t& i,
    std::string_view parindent);
std::optional<WholeSegment> lookaheadParIndent(InputDiags& ctx, size_t i);
std::optional<BracketGroup> lexBracketGroup(InputDiags& lex, size_t& i);

// This one is not meant for tentative parsing. Produces error diags if
// anything is wrong. It joins multiple lines together if necessary for
// bracket-matching. Returns empty on errors or on eof (distinguished by
// lex.sizeGt(i)).
// Requires i == lex.input.bol(i) on entry,
// ensures the same condition on exit, so it's safe to call this function
// multiple times in succession.
std::vector<ExprToken> lexNextLine(InputDiags& lex, size_t& i);

// Used, for instance, to lex list of lookaheads,
// with the leader character set to '|'.
// On parse error, it still returns and consumes as many lines as possible.
// No obvious indication is returned to indicate that this has happened,
//   other than diagnostic errors in lex.diags.
// Callers should check for severe errors by checking if the return value is
// empty(), but should otherwise keep parsing from the next line.
std::vector<std::vector<ExprToken>>
lexListEntries(InputDiags& lex, size_t& i, char bullet);

// Returns nullopt on eof. Throws on invalid language character.
std::optional<WholeSegment> lookahead(InputDiags& lex, size_t i);

// This is meant for parsing a comma-separated sequence. As such, it never
// returns empty elements.
// Errors out if it finds one, unless it's the last element. If the last
// element is empty, it is silently discarded to allow a trailing comma in a
// list. Note that this is different from Python's "".split(',') which returns
// a single empty string.
std::vector<std::vector<ExprToken>>
splitCommaNoEmpty(DiagsDest ctx, std::vector<ExprToken> elts);

}  // namespace lex

inline std::nullopt_t
Error(DiagsDest ctx, const Segment& s, std::string msg) {
  return Error(ctx, s.stPos, s.enPos, std::move(msg));
}
inline std::nullopt_t
Error(DiagsDest ctx, const lex::ExprToken& t, std::string msg) {
  const Segment& seg = lex::segment(t);
  return Error(ctx, seg.stPos, seg.enPos, std::move(msg));
}

}  // namespace oalex
