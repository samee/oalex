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
  alnumToken = Segment::lastReservedTag + 1,
  section,
  quotedString,
  bracketed,
};

// This class is mostly to document which Segment types belong to the lexer.
struct LexSegment : Segment {
  LexSegment(size_t st,size_t en,Segment::tagint_t type_tag)
    : Segment{st,en,type_tag} {}
};

struct AlnumToken : LexSegment {
  static constexpr auto type_tag = tagint_t(LexSegmentTag::alnumToken);
  std::string token;
  AlnumToken(size_t st,size_t en,const Input& input)
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
  size_t maxLineLength = 5000;

  std::nullopt_t Error(size_t st,size_t en,std::string msg);
  std::nullopt_t Warning(size_t st,size_t en,std::string msg);
  std::nullopt_t Note(size_t st,size_t en,std::string msg);
};

std::optional<std::vector<AlnumToken>> lexSectionHeader(Lexer& lex, size_t& i);
std::optional<QuotedString> lexQuotedString(Lexer& lex, size_t& i);
std::optional<QuotedString> lexDelimitedSource(Lexer& lex, size_t& i);

}  // namespace oalex::lex
