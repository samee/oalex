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

/*
   13th Oct 2019: initial plans

   I won't really have the usual lexer vs parser division. The initial "lexer"
   phase will still be non-regular. We'll still call them lexer and parser,
   though instead of stage-1 and stage-2. Tasks of our lexer:

     * Ignore comments and non-important whitespaces, still preserving indent
       information where necessary.
     * Output tokens:
         - SectionHeader(string)
         - alnum words
         - operators ( : , := | ~ ... )
         - quoted strings, distinguising between quoting styles.
         - regexes
         - pair-matching (parenthesis, brackets, so on).

    The lexer will be responsible for general look-and-feel of the language,
    concerning itself with what is nested under what, what is quoted, and what
    is commented. It understands the structure and layout of the code, not
    what it means.

    It's output is thus still tree-structured, not a token-stream.
*/

#include "lexer.h"

#include <algorithm>
#include <cstring>
#include <functional>
#include <iterator>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>
#include "fmt/core.h"

#include "regex_io.h"
#include "segment.h"
#include "runtime/indent.h"
#include "runtime/input_view.h"
#include "runtime/jsonloc.h"
#include "runtime/util.h"

using fmt::format;
using std::get_if;
using std::make_move_iterator;
using std::min;
using std::mem_fn;
using std::nullopt;
using std::numeric_limits;
using std::optional;
using std::pair;
using std::remove_if;
using std::stoi;
using std::string;
using std::string_view;
using std::tie;
using std::unique_ptr;
using std::vector;

namespace oalex::lex {

RegexOptions oalexRegexOpts{
  // Do not use user-supplied input. See regex_io.h for details.
  .word = parseRegexCharSet("[0-9A-Za-z_]")
};

const char* exprTagName(const ExprToken& t) {
  auto tag = segment(t).tag;
  if(SegmentTag{tag} == SegmentTag::wholeSegment) return "wholeSegment";
  else switch(LexSegmentTag{tag}) {
    case LexSegmentTag::section: return "section";
    case LexSegmentTag::gluedString: return "gluedString";
    case LexSegmentTag::bracketGroup: return "bracketGroup";
    case LexSegmentTag::newlineChar: return "newlineChar";
    case LexSegmentTag::regexPattern: return "regexPattern";
    default: Bug("Unknown index {}", int(tag));
  }
}

StringLoc fromSegment(WholeSegment s) {
  return StringLoc{std::move(s.data), s.stPos};
}

// For a "\xhh" code, this function assumes "\x" has been consumed, and now we
// are just parsing the "hh" part. `i` points to what should be the first hex
// digit. This is why all errors start at iPos-2:
//
//     \xhh
//     | |
//     | +-- iPos
//     +---- iPos-2

optional<char> lexHexCode(string_view s, size_t& i,
                          DiagsDest ctx, size_t iPos) {
  if(i+1 >= s.size())
    return Error(ctx, iPos-2, iPos+s.size()-i, "Incomplete hex code");
  if(!isxdigit(s[i]) || !isxdigit(s[i+1]))
    return Error(ctx, iPos-2, iPos+2, "Invalid hex code");
  i += 2;
  return stoi(string(s.substr(i-2,2)), nullptr, 16);
}

namespace {

bool isSectionHeaderNonSpace(char ch) {
  return (ch>='0' && ch<='9')
      || (ch>='A' && ch<='Z')
      || (ch>='a' && ch<='z')
      || ch=='_';
}

// TODO use generic word-lexer based on a char set.
optional<WholeSegment> lexHeaderWord(InputDiags& ctx, size_t& i) {
  const InputPiece& input = ctx.input();
  size_t j = i;
  while(input.sizeGt(j) && isSectionHeaderNonSpace(input[j])) ++j;
  if(j == i) return nullopt;
  else return inputSegment(std::exchange(i,j), j, input);
}

// Return conditions:
//   * Success: Return has_value(), `i` has been incremented past the end.
//   * Not recognized: Try parsing it as something else.
//   * We know enough to raise an error. Note: not all diags are errors.
//     But errors should prevent lexing from proceeding to parsing.
//     Fatal errors are just exceptions.
//
// From the above, we can now settle on two different kinds of parsing
// functions:
//   * Ones that are simple: words, tokens, comments.
//       - They will never update diagnostics: they have insufficient context.
//       - Their signature will be: foo(const InputPiece& input,size_t& i);
//       - Return value either optional or bool.
//       - On failure, caller will try parsing it as something else.
//   * Others are invoked from the top-ish level.
//       - They will parse more complicated structures: e.g. section header.
//         parenthesized expressions.
//       - They can produce diagnostics even on successful parsing:
//         e.g. warnings, info.
//       - They can consume inputs even on failure. After some processing, they
//         can decide not to backtrack any more, since it could not have been
//         anything else.
//       - Will likely depend on parser-global bools.
//       - Likely signature: foo(InputDiags&,size_t&);

vector<WholeSegment>
lexSectionHeaderContents(InputDiags& ctx, size_t& i) {
  const InputPiece& input = ctx.input();
  size_t j = i;
  vector<WholeSegment> rv;
  // TODO reduce bsearch overhead.
  for(j = oalexSkip.next(input, j);
      input.bol(j) == input.bol(i);
      j = oalexSkip.next(input, j)) {
    if(isSectionHeaderNonSpace(input[j])) {
      if(optional<WholeSegment> token = lexHeaderWord(ctx,j))
        rv.push_back(*token);
      else return {};
    }else if(input[j] == '\n') break;
    else return {};
  }
  i = j;
  return rv;
}

// Consume it even if the line is indented, so the caller can raise an error.
// On success:
//   Returns the *start* of dashes so caller can in fact check indentation.
//   Modifies i to the beginning of the next line.
optional<size_t> lexDashLine(InputDiags& ctx, size_t& i) {
  const InputPiece& input = ctx.input();
  if(!input.sizeGt(i)) return nullopt;
  size_t j = i;
  size_t dashStart = j;
  while(input.sizeGt(j) && input[j]=='-') ++j;
  i = j;
  return dashStart;
}

// For a backslash code, this function assumes `i` is already at the character
// just after the backslash, inside a string literal.
optional<char> lexQuotedEscape(string_view s, size_t& i,
                               DiagsDest ctx, size_t iPos) {
  if(i >= s.size()) return Error(ctx, iPos-1, "Incomplete escape code");
  char ch;
  // If this changes, please change jsonloc::pringString() as well.
  switch(s[i]) {
    case '\\': ch = '\\'; break;
    case 'n': ch = '\n'; break;
    case 't': ch = '\t'; break;
    case '"': ch = '"'; break;
    case '\'': ch = '\''; break;
    case 'x': ++i; return lexHexCode(s, i, ctx, iPos+1);
    default: return Error(ctx, i-1, "Invalid escape code");
  }
  ++i;
  return ch;
}

// Return value does *not* include trailing newline, if any.  However, i *is*
// incremented past the newline so we are ready to read the next line if one
// exists. We never care about whether or not the last line ends with a newline.
string getline(InputDiags& ctx, size_t& i) {
  size_t eol = i;
  bool nlend = false;
  for(; ctx.input().sizeGt(eol); ++eol) {
    if(ctx.input()[eol]=='\n') { nlend = true; break; }
  }
  string rv = ctx.input().substr(i, eol-i);
  i += rv.size() + nlend;
  return rv;
}

constexpr char badIndentMsg[] = "Indentation mixes tabs and spaces differently "
                                "from the previous line";

// If ctx.input()[i] starts a blank line, return "".
// If the indentation is less than parindent, return nullopt.
// Else, return the source line with parindent stripped out.
//
// If indentation is incompatible with parindent, we still consume the current
// line, but then add a Error() and return "". This allows us a form of error
// recovery: we can still parsing the subsequent lines (since caller ends
// source block on nullopt), while not returning any potential garbage.
optional<string> lexSourceLine(InputDiags& ctx, size_t& i,
                               string_view parindent) {
  static const auto* wskip = new Skipper{{}, {}, Skipper::Newlines::keep_all};
  const InputPiece& input = ctx.input();
  if(!input.sizeGt(i) || i!=input.bol(i)) return nullopt;
  size_t j = wskip->next(input, i);

  // Whitespaces don't matter for blank lines.
  if(input.sizeGt(j) && input[j] == '\n') { i = j+1; return ""; }

  IndentCmp cmp = indentCmp(input.substr(i,j-i), parindent);

  // This is likely past the end of this source block.
  if(cmp == IndentCmp::lt) return nullopt;

  if(cmp == IndentCmp::bad) {
    Error(ctx, i, j, badIndentMsg);
    getline(ctx, i);  // Skip to end of line.
    return "";        // Don't return any of it.
  }

  // We have actual content to be returned.
  i += parindent.size();
  return getline(ctx, i);
}

// Note: '/' starts a regex, and is not an operator in oalex.
bool isquote(char ch) { return ch=='"' || ch=='\''; }
bool isbracket(char ch) { return strchr("(){}[]", ch) != NULL; }
bool isoperch(char ch) { return strchr(":,=|~.-><;", ch) != NULL; }
bool isregex(char ch) { return ch == '/'; }

auto toCtor(char ch) -> GluedString::Ctor {
  switch(ch) {
    case '"':  return GluedString::Ctor::dquoted;
    case '\'': return GluedString::Ctor::squoted;
    default: Bug("toCtor() called with invalid char '{}'", ch);
  }
}

optional<BracketType> lexOpenBracket(const InputPiece& input, size_t& i) {
  switch(input[i]) {
    case '[': ++i; return BracketType::square;
    case '{': ++i; return BracketType::brace;
    case '(': ++i; return BracketType::paren;
    default: return nullopt;
  }
}
optional<BracketType> lexCloseBracket(const InputPiece& input, size_t& i) {
  switch(input[i]) {
    case ']': ++i; return BracketType::square;
    case '}': ++i; return BracketType::brace;
    case ')': ++i; return BracketType::paren;
    default: return nullopt;
  }
}

static size_t
closeBracketStart(const BracketGroup& bg) {
  switch(bg.type) {
    case BracketType::square:
    case BracketType::brace:
    case BracketType::paren:
      return bg.enPos-1;
    default:
      Bug("Unknown bracket type in {}", __func__);
  }
}

string debugChar(char ch) {
  if(isprint(ch)) return format("'{}'", ch);
  else return format("\\x{:02x}", int(ch));
}

bool isident(char ch) { return isalnum(ch) || ch == '_'; }

// Returns false on eof. Throws on invalid language character.
// TODO think more about how next() can forget.
[[nodiscard]] bool lookaheadStart(InputDiags& ctx, size_t& i) {
  const InputPiece& input = ctx.input();
  Skipper skip = oalexSkip; skip.newlines = Skipper::Newlines::ignore_all;
  size_t j = skip.next(input, i);
  if(!input.sizeGt(j)) return false;
  else if(isident(input[j]) || isquote(input[j]) || isbracket(input[j]) ||
          isregex(input[j]) || isoperch(input[j])) { i=j; return true; }
  else Fatal(ctx, j, "Unexpected character " + debugChar(input[j]));
}

static bool isWordChar(char ch) {
  return matchesRegexCharSet(ch, oalexRegexOpts.word);
}

// Careful on numbers: a -12.34e+55 will be decomposed as
//   ["-","12", ".", "34e", "+", "56"]
// But that's okay, we won't support floating-point or signed numerals.
// TODO use generic word-lexing features.
optional<WholeSegment> lexWord(const InputPiece& input, size_t& i) {
  if(!input.sizeGt(i) || !isWordChar(input[i])) return nullopt;
  size_t oldi = i;
  while(input.sizeGt(i) && isWordChar(input[i])) ++i;
  return inputSegment(oldi,i,input);
}

// TODO throw Fatal on .... or ::=.
optional<WholeSegment> lexOperator(const InputPiece& input, size_t& i) {
  if(!input.sizeGt(i) || !isoperch(input[i])) return nullopt;

  // Now we know it is a valid input character, see if it is multichar.
  static const char multichars[][4] = {":=","...", "->"};
  size_t oldi = i;
  for(string_view op : multichars) if(input.substr(i,op.size()) == op) {
    i += op.size();
    return inputSegment(oldi,i,input);
  }
  return inputSegment(oldi,++i,input);
}

char openBracket(BracketType bt) {
  switch(bt) {
    case BracketType::square: return '[';
    case BracketType::brace: return '{';
    case BracketType::paren: return '(';
    default: Bug("Invalid openBracket() type {}", int(bt));
  }
}

char closeBracket(BracketType bt) {
  switch(bt) {
    case BracketType::square: return ']';
    case BracketType::brace: return '}';
    case BracketType::paren: return ')';
    default: Bug("Invalid closeBracket() type {}", int(bt));
  }
}

}  // namespace

size_t
skipBlankLines(InputDiags& ctx, size_t pos) {
  const InputPiece& input = ctx.input();
  size_t rv = oalexSkip.next(input, pos);
  if(input.sizeGt(rv) && input[rv] == '\n')
    rv = oalexSkip.next(input, rv+1);
  if(rv == string::npos) {
    Error(ctx, oalexSkip.whitespace(input, pos), "Unfinished comment");
    return rv;
  }else if(rv == oalexSkip.next(input, input.bol(rv))) return rv;
  else {
    Error(ctx, rv, "Expected end of line");
    return string::npos;
  }
}

NewlineChar::NewlineChar(const GluedString& s, size_t pos)
  : Segment{s.inputPos(pos), s.inputPos(pos)+1,
               tagint_t(LexSegmentTag::newlineChar)} {}

struct IndexRelation {
  size_t inputPos;
  size_t quotePos;
  size_t inputLine;
  size_t inputCol;
};

IndexRelation indexRelation(size_t inputPos, size_t quotePos, DiagsDest rcmap) {
  auto [line, col] = rcmap.rowCol(inputPos);
  return { .inputPos=inputPos, .quotePos=quotePos,
           .inputLine=line, .inputCol=col };
}

// Dev-note: It's weird to have a DiagsDest as an input here, but it's used as
// a location map. This is what we use to initialize IndexRelation, the data
// structure we look up for rowCol(). In future, we can consider this decaying
// into a dedicated type for rcmap. Some super-interface for InputPiece. No, we
// won't accept InputPiece here directly, since we don't want DiagsDest to
// expose InputPiece on the caller side.
GluedString::GluedString(DiagsDest rcmap, WholeSegment s)
  : Segment{s.stPos, s.enPos, type_tag},
    s_(std::move(*s)), ctor_(Ctor::wholeSegment),
    index_map_({indexRelation(s.stPos, 0, rcmap)}) {}

GluedString::GluedString(size_t st, size_t en, std::string_view s, Ctor ctor,
                         std::vector<IndexRelation> imap)
  : Segment{st,en,type_tag}, s_(s), ctor_(ctor),
    index_map_(std::move(imap)) {}

GluedString::GluedString(GluedString&&) noexcept = default;
GluedString::GluedString(const GluedString&) = default;
GluedString::~GluedString() = default;
GluedString& GluedString::operator=(const GluedString&) = default;
GluedString& GluedString::operator=(GluedString&&) noexcept = default;

static bool cmpByQuotePos(const IndexRelation& a, const IndexRelation& b) {
  return a.quotePos < b.quotePos;
}

// Returns index_map.iterator_type, never returns index_map.begin().
static auto upperBound(const vector<IndexRelation>& index_map, size_t qpos) {
  auto it = upper_bound(index_map.begin(), index_map.end(),
                     IndexRelation{.inputPos = 0, .quotePos = qpos,
                                   .inputLine = 0, .inputCol = 0},
                     cmpByQuotePos);
  if(it == index_map.begin()) Bug("Index map doesn't have start entry");
  return it;
}

GluedString GluedString::subqstr(size_t pos, size_t len) const {
  if(pos > size()) Bug("GluedString::subqstr() invoked with invalid pos");
  len = min(len, size() - pos);

  // Find the subrange for the new index_map_.
  auto stit = --upperBound(index_map_, pos);
  auto enit = upperBound(index_map_, pos+len);

  // Construct the new index_map_.
  vector<IndexRelation> imap(stit, enit);  // cannot be empty.
  auto [stLine, stCol] = this->rowCol(pos);
  imap[0] = {.inputPos = this->inputPos(pos), .quotePos = 0,
             .inputLine = stLine, .inputCol = stPos };
  for(size_t i=1; i<imap.size(); ++i) imap[i].quotePos -= pos;
  size_t st = imap[0].inputPos;
  size_t en = imap.back().inputPos + (len - imap.back().quotePos);
  return GluedString(st, en, this->substr(pos, len), Ctor::subqstr,
                     std::move(imap));
}

pair<size_t,size_t> GluedString::rowCol(size_t pos) const {
  auto it = --upperBound(index_map_, pos);
  return pair{it->inputLine, it->inputCol + (pos - it->quotePos)};
}

size_t GluedString::inputPos(size_t pos) const {
  auto it = --upperBound(index_map_, pos);
  return it->inputPos + pos - it->quotePos;
}

size_t GluedString::bol(size_t i) const {
  auto it = upperBound(index_map_, i);
  while(true) {
    --it;
    if(it->quotePos == 0 || s_[it->quotePos-1] == '\n') return it->quotePos;
    if(it == index_map_.begin())
      Bug("Index map's first entry is {} != 0", it->quotePos);
  }
}

optional<WholeSegment> GluedString::getSegment() const {
  if(index_map_.size() == 1) return WholeSegment(this->stPos, this->enPos, s_);
  else if(index_map_.empty())
    Bug("GluedString should never have an empty index");
  else return nullopt;
  // Corner case when s_ ends with a '\n'. FIXME
}

// Returns error-free nullopt if and only if !isregex(input[i]). In all other
// cases, it either returns a good value, or it adds diags to explain the
// problem.  If such diags are added, it consumes input until the next
// unescaped '/'.
static optional<RegexPattern> lexRegexPattern(InputDiags& ctx, size_t& i) {
  const InputPiece& input = ctx.input();
  if(!input.sizeGt(i) || !isregex(input[i])) return nullopt;
  size_t j = i;
  unique_ptr<const Regex> rv = parseRegex(ctx, j);  // TODO: improve naming.
  if(rv == nullptr) {
    // Scan till the next unescaped '/'
    ++j;
    while(input.sizeGt(j)) {
      if(input[j] == '\\') {
        if(input.sizeGt(++j)) ++j;
      }else if(input[j] == '/') {
        i = ++j;
        break;
      }else ++j;
    }
    return nullopt;
  }
  return RegexPattern{std::exchange(i,j), j, std::move(rv)};
}

// This function never returns nullopt silently. It will either return
// a value, or add something to ctx.diags.
static optional<ExprToken> lexSingleToken(InputDiags& ctx, size_t& i) {
  if(!ctx.input().sizeGt(i)) FatalBug(ctx, i, "lexSingleToken() Out of bound");
  else if(isquote(ctx.input()[i])) return lexQuotedString(ctx, i);
  else if(isWordChar(ctx.input()[i])) return lexWord(ctx.input(), i);
  else if(isoperch(ctx.input()[i])) return lexOperator(ctx.input(), i);
  else if(isregex(ctx.input()[i])) return lexRegexPattern(ctx, i);
  else return Error(ctx, i, "Invalid source character");
}

/* Behavior:

   * Throws Fatal() on unexpected characters (outside quotes).
   * On valid source chars, produces silent nullopt on eof.
   * If no open bracket, still silent nullopt.
   * Any failure after this produces diags.
 */
optional<BracketGroup> lexBracketGroup(InputDiags& ctx, size_t& i) {
  const InputPiece& input = ctx.input();
  size_t j = i;
  if(!lookaheadStart(ctx,j)) return nullopt;

  BracketType bt;
  const size_t bstart = j;
  if(auto btOpt = lexOpenBracket(input,j)) bt=*btOpt;
  else return nullopt;

  BracketGroup bg(bstart,Input::npos,bt);
  while(true) {
    if(!lookaheadStart(ctx,j)) {  // lookaheadStart is false on EOF.
      Error(ctx, j, format("Match not found for '{}'.", openBracket(bt)));
      i = Input::npos;
      return nullopt;
    }

    if(auto btOpt = lexCloseBracket(input,j)) {
      BracketType bt2=*btOpt;
      // The only success return path.
      if(bt==bt2) { bg.enPos=i=j; return bg; }
      else {
        // TODO: Examine this '+1' a bit more. It's historical, and suspicious.
        return Error(ctx, std::exchange(i, j+1), j,
                     format("Match not found for '{}', found '{}' instead.",
                            openBracket(bt), closeBracket(bt2)));
      }
    }

    if(isbracket(ctx.input()[j])) {
      if(auto bgopt = lexBracketGroup(ctx,j))
        bg.children.push_back(std::move(*bgopt));
      else ++j;
    }else {
      size_t oldj = j;
      if(auto x = lexSingleToken(ctx, j)) bg.children.push_back(std::move(*x));
      else if(j == oldj) ++j;
    }
  }
}

static string substrFromBol(const InputPiece& input, size_t i) {
  size_t bol = input.bol(i);
  return input.substr(bol, i-bol);
}

static size_t
findOffsideToken(const InputPiece& input, string_view refindent,
                 vector<ExprToken>::const_iterator begin,
                 vector<ExprToken>::const_iterator end) {
  for(auto it = begin; it != end; ++it) {
    const ExprToken& tok = *it;
    if(auto* bg = get_if<BracketGroup>(&tok)) {
      if(auto rv = findOffsideToken(input, refindent,
                                    bg->children.begin(), bg->children.end()))
        return rv;

      // Special-case bracket end char,
      // since we don't check those anywhere else.
      size_t closeSt = closeBracketStart(*bg);
      IndentCmp cmp = indentCmp(substrFromBol(input, closeSt), refindent);
      if(cmp == IndentCmp::lt || cmp == IndentCmp::eq) return closeSt;
    }
    IndentCmp cmp = indentCmp(substrFromBol(input, stPos(tok)), refindent);
    if(cmp == IndentCmp::lt || cmp == IndentCmp::eq) return stPos(tok);
  }
  return Input::npos;
}

// Assumes line is non-empty.
static IndentCmp
firstCharIndent(const InputPiece& input, const vector<ExprToken>& line,
                string_view refindent) {
  return indentCmp(substrFromBol(input, stPos(line[0])), refindent);
}

// Even though return_value itself can be empty,
// every element return_value[i] that it does have are non-empty vectors.
static vector<vector<ExprToken>>
lexLinesIndentedAtLeast(InputDiags& ctx, size_t& i,
                        string_view refindent, char bullet) {
  size_t j = i;
  vector<vector<ExprToken>> rv;
  while(true) {
    vector<ExprToken> line = lexNextLine(ctx, j);
    if(line.empty()) return rv;
    IndentCmp cmp = firstCharIndent(ctx.input(), line, refindent);
    if(cmp == IndentCmp::lt ||
       (cmp == IndentCmp::eq && !isToken(line[0], string_view(&bullet, 1))))
      return rv;
    if(cmp == IndentCmp::bad) {
      size_t char1 = stPos(line[0]);
      Error(ctx, ctx.input().bol(char1), char1, badIndentMsg);
      return rv;
    }
    rv.push_back(std::move(line));
    i = j;
  }
}

// Assumes lines[0] is already indented at refindent.
// Assumes lines[i] is non-empty for all i.
static void
mergeListLines(InputDiags& ctx, vector<vector<ExprToken>>& lines,
               string_view refindent) {
  if(lines.empty()) return;
  size_t i=0;
  for(size_t j=1; j<lines.size(); ++j) {
    IndentCmp cmp = firstCharIndent(ctx.input(), lines[j], refindent);
    if(cmp == IndentCmp::eq) { i=j; continue; }
    lines[i].insert(lines[i].end(), make_move_iterator(lines[j].begin()),
                                    make_move_iterator(lines[j].end()));
    lines[j].clear();
  }
  lines.erase(remove_if(lines.begin(), lines.end(),
                        mem_fn(&vector<ExprToken>::empty)),
              lines.end());
}

vector<vector<ExprToken>>
lexListEntries(InputDiags& ctx, size_t& i, char bullet) {
  const InputPiece& input = ctx.input();
  if(i != input.bol(i))
    FatalBug(ctx, i,
        format("lexListEntries() must start at bol(), got string '{}'",
               debugPrefix(ctx.input(), i)));
  size_t j = i;

  size_t first_char_pos = i;
  if(!lookaheadStart(ctx, first_char_pos)) return {};
  string refindent = substrFromBol(input, first_char_pos);

  vector<vector<ExprToken>> rv =
    lexLinesIndentedAtLeast(ctx, j, refindent, bullet);
  i = j;
  if(rv.empty()) return {};
  mergeListLines(ctx, rv, refindent);

  // Post-process for indent check
  for(const auto& entry : rv) {
    // Skip the bullet token.
    size_t off = findOffsideToken(ctx.input(), refindent,
                                  ++entry.begin(), entry.end());
    if(off != Input::npos) {
      Error(ctx, off, "Needs to be indented further");
      break;
    }
  }
  return rv;
}

// Returns nullopt on eof. Throws on invalid language character.
optional<WholeSegment> lookahead(InputDiags& ctx, size_t i) {
  if(!lookaheadStart(ctx,i)) return nullopt;
  if(auto tok = lexWord(ctx.input(), i)) return tok;
  else if(isbracket(ctx.input()[i]) || isquote(ctx.input()[i]))
    return inputSegment(i,i+1,ctx.input());
  else if(auto op = lexOperator(ctx.input(), i)) return op;
  else Fatal(ctx, i, "Invalid input character");
}

// Input literal is assumed to match /"([^"\\\n]|\\")*"/
optional<GluedString> unquote(const StringLoc& literal, DiagsDest ctx) {
  if(literal->empty() || !isquote(literal->at(0))
                      || !isquote(literal->back()))
    Bug("Expected quoted literal. Received '{}'", *literal);
  vector<IndexRelation> imap;
  string s;
  imap.push_back(indexRelation(literal.stPos()+1, 0, ctx));
  size_t i = 1;
  while(i+1<literal->size()) {
    if(literal->at(i) == '\\') {
      ++i;
      if(optional<char> escres
          = lexQuotedEscape(*literal, i, ctx, literal.stPos()+i)) {
        s += *escres;
        imap.push_back(indexRelation(literal.stPos()+i, s.size(), ctx));
      }else return nullopt;
    }else s += literal->at(i++);
  }
  return GluedString(literal.stPos(), literal.enPos(), s,
                     toCtor(literal->at(0)), std::move(imap));
}

// Returns Input::npos if and only if the line ends
// before the end quote is found.
size_t findMatchingQuote(InputDiags& ctx, size_t i) {
  const InputPiece& input = ctx.input();
  if(!input.sizeGt(i))
    Bug("{}: Position {} is past the end of string", __func__, i);
  const char quote = input[i];
  size_t j = i+1;
  bool skip_next_char = false;
  while(input.sizeGt(j) && input[j] != '\n') {
    if(skip_next_char) skip_next_char = false;
    else if(input[j] == '\\') skip_next_char = true;
    else if(input[j] == quote) return j;
    ++j;
  }
  if(skip_next_char) Error(ctx, j-1, "Incomplete escape code");
  else Error(ctx, j-1, "Unexpected end of line");
  i = j;
  return Input::npos;
}

// It returns an error-free nullopt iff ctx.input()[i] is not a '"', in which
// case the caller should try parsing something else. In all other cases, it
// will either return a valid string, or nullopt with errors added to
// ctx.diags. In this case, increment `i` beyond the end of the next unescaped
// '"', or in case of an unexpected end of line, beyond the next newline.
optional<GluedString> lexQuotedString(InputDiags& ctx, size_t& i) {
  const InputPiece& input = ctx.input();
  if(!input.sizeGt(i) || !isquote(input[i])) return nullopt;

  size_t j = findMatchingQuote(ctx, i);
  if(j == Input::npos) return nullopt;
  auto rv = unquote(StringLoc{input.substr_range(i, j+1), i}, ctx);
  i = j+1;  // Callers expect i to advance even on failure
  return rv;
}

static bool isSourceFence(string_view fence) {
  if(fence.substr(0,3) != "```") return false;
  size_t i;
  for(i=3; i<fence.size(); ++i) if(!isident(fence[i])) break;
  for(   ; i<fence.size(); ++i) if(fence[i]!=' ' && fence[i]!='\t') break;
  return i == fence.size();
}

optional<GluedString> lexFencedSource(InputDiags& ctx, size_t& i) {
  const InputPiece& input = ctx.input();
  if(!input.sizeGt(i) || i!=input.bol(i) || input.substr(i,3)!="```")
    return nullopt;
  size_t j = i;
  vector<IndexRelation> imap;
  string fence = getline(ctx, j);
  if(!isSourceFence(fence))
    Fatal(ctx, i, j, "Fences must be alphanumeric");

  // Valid starting fence, so now we are commited to changing i.
  size_t fenceStart = i;
  size_t inputStart = j;
  i = j;
  imap.push_back(indexRelation(j, 0, ctx));
  while(input.sizeGt(j)) {
    size_t lineStart = j;
    string line = getline(ctx, j);
    if(line == fence) {
      GluedString s(fenceStart, j-1,
                     input.substr(inputStart, lineStart-inputStart),
                     GluedString::Ctor::fenced, std::move(imap));
      i = j;
      return s;
    } else imap.push_back(indexRelation(j, j-inputStart, ctx));
  }
  i = j;
  return Error(ctx, fenceStart, j-1, "Source block ends abruptly");
}

// Can return nullopt if it's only blank lines till a non-blank (or non-error)
// source line, strictly less indented than parindent.
optional<GluedString>
lexIndentedSource(InputDiags& ctx, size_t& i, string_view parindent) {
  const InputPiece& input = ctx.input();
  string rv;
  size_t j = i, start = i;
  vector<IndexRelation> imap;
  bool allblank = true;
  while(input.sizeGt(j)) {
    size_t ls = j;
    optional<string> line = lexSourceLine(ctx,j,parindent);
    if(!line.has_value()) break;
    if(line->empty())
      imap.push_back(indexRelation(ls, rv.size(), ctx));
    else {
      allblank = false;
      imap.push_back(indexRelation(ls+parindent.size(), rv.size(), ctx));
    }
    rv += *line; rv += '\n';
    i = j;
  }
  if(allblank) return nullopt;
  imap.push_back(indexRelation(j, rv.size(), ctx));
  GluedString qs(start, j, std::move(rv), GluedString::Ctor::indented,
                 std::move(imap));
  i = j;
  return qs;
}

// Can return nullopt if it's only blank lines till eof.
// It uses oalexWSkip, since even comments of user language needs
// to be indented. Throws if we are not at the beginning of a line, or
// at the end of the previous line.
optional<WholeSegment>
lookaheadParIndent(InputDiags& ctx, size_t i) {
  const InputPiece& input = ctx.input();
  if(i != input.bol(i))
    FatalBug(ctx, i, "ParIndent computation cannot start mid-line");
  i = oalexWSkip.next(input, i);
  if(!input.sizeGt(i)) return nullopt;
  return inputSegment(input.bol(i), i, input);
}

// TODO utility for checking leader indent.

// Changes `i` iff:
//   * We find a content line followed immediately by a line of just dashes,
//     with no intervening blank line.
//   * Any leading or trailing space or tabs are discarded. So are comments.
// If any of these conditions fail, we promptly return an empty result. A
// content line is one where the only non-comment characters are in
// /[0-9A-Za-z_ \t]+/. Valid content lines are never blank.
//
// Additionally, we produce errors if:
//   * They are indented.
//   * They are neighboring other non-blank lines.
// In these two cases, we still return the header tokens.
// TODO: We should also produce errors if there is an invalid character in
// an otherwise good section header. The same for some odd character in a line
// overwhelmed with dashes.
vector<WholeSegment> lexSectionHeader(InputDiags& ctx, size_t& i) {
  if(i != ctx.input().bol(i)) return {};

  size_t j = i;
  if(size_t k = skipBlankLines(ctx,j); k != string::npos) j = ctx.input().bol(k);
  optional<vector<WholeSegment>> rv = lexSectionHeaderContents(ctx,j);
  if(!rv) return {};
  Skipper skip = oalexSkip; skip.newlines = Skipper::Newlines::keep_all;
  j = skip.next(ctx.input(), j);
  if(!ctx.input().sizeGt(j) || ctx.input()[j]!='\n') return {};
  ++j;
  j = skip.next(ctx.input(), j);
  if(!ctx.input().sizeGt(j) || ctx.input()[j]!='-') return {};
  optional<size_t> stDash=lexDashLine(ctx,j);
  if(!stDash) return {};
  j = skip.next(ctx.input(), j);
  if(!ctx.input().sizeGt(j) || ctx.input()[j]!='\n') return {};
  i = ++j;

  size_t stCont=rv->at(0).stPos;
  const InputPiece& input=ctx.input();
  if(input.bol(stCont) != stCont)
    Error(ctx, input.bol(stCont), stCont-1,
          "Section headers must not be indented");
  if(input.bol(*stDash) != *stDash)
    Error(ctx, input.bol(*stDash), *stDash-1,
          "Dashes in a section header must not be indented");

  return *rv;
}

vector<ExprToken> lexNextLine(InputDiags& ctx, size_t& i) {
  if(i != ctx.input().bol(i))
    FatalBug(ctx, i,
        format("lexNextLine() must start at bol, got '{}'",
               debugPrefix(ctx.input(), i)));

  size_t j = i;
  if(size_t k = skipBlankLines(ctx, j); k != string::npos)
    j = ctx.input().bol(k);

  vector<ExprToken> rv;
  size_t prevBol = j;
  for(j=oalexSkip.next(ctx.input(), prevBol);
      ctx.input().sizeGt(j);
      j=oalexSkip.next(ctx.input(),j)) {
    if(ctx.input()[j] == '\n') { ++j; break; }
    optional<ExprToken> tok = lexBracketGroup(ctx, j);
    if(!tok.has_value()) {
      tok = lexSingleToken(ctx, j);
      if(!tok.has_value()) return {};
    }
    j = enPos(*tok);
    rv.push_back(std::move(*tok));
    prevBol = ctx.input().bol(j);
  }
  i = j;
  return rv;
}

vector<vector<ExprToken>>
splitCommaNoEmpty(DiagsDest ctx, vector<ExprToken> elts) {
  vector<vector<ExprToken>> rv;
  rv.emplace_back();
  for(auto&& elt : elts) {
    if(isToken(elt,",")) {
      if(rv.back().empty()) Error(ctx, elt, "Unexpected comma");
      else rv.emplace_back();
    }
    else rv.back().push_back(std::move(elt));
  }
  if(rv.back().empty()) rv.pop_back();
  return rv;
}

}  // namespace oalex::lex
