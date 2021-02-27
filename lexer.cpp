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
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>
#include "fmt/core.h"

#include "regex_io.h"
#include "segment.h"
#include "runtime/input_view.h"
#include "runtime/util.h"

using fmt::format;
using std::function;
using std::min;
using std::nullopt;
using std::numeric_limits;
using std::optional;
using std::pair;
using std::shared_ptr;
using std::stoi;
using std::string;
using std::string_view;
using std::tie;
using std::unique_ptr;
using std::vector;

namespace oalex::lex {

string_view typeTagName(const LexSegmentTag& tag) {
  switch(tag) {
    case LexSegmentTag::wholeSegment: return "wholeSegment";
    case LexSegmentTag::section: return "section";
    case LexSegmentTag::gluedString: return "gluedString";
    case LexSegmentTag::bracketGroup: return "bracketGroup";
    case LexSegmentTag::newlineChar: return "newlineChar";
    case LexSegmentTag::regexPattern: return "regexPattern";
    default: Bug("Unknown index {}", int(tag));
  }
}

namespace {

bool isSectionHeaderNonSpace(char ch) {
  return (ch>='0' && ch<='9')
      || (ch>='A' && ch<='Z')
      || (ch>='a' && ch<='z')
      || ch=='_';
}

optional<size_t>
skipBlankLine(InputDiags& ctx, size_t i) {
  size_t j = oalexSkip.withinLine(ctx.input, i);
  if(ctx.input.bol(i) == ctx.input.bol(j)) return nullopt;
  else return j;
}

// TODO use generic word-lexer based on a char set.
optional<WholeSegment> lexHeaderWord(InputDiags& ctx, size_t& i) {
  const Input& input = ctx.input;
  Resetter rst(ctx, i);
  while(input.sizeGt(i) && isSectionHeaderNonSpace(input[i])) ++i;
  if(i == rst.start()) return nullopt;
  else {
    rst.markUsed(i);
    return WholeSegment(rst.start(),i,input);
  }
}

// Return conditions:
//   * Success: Return has_value(), `i` has been incremented past the end.
//   * Not recognized: Try parsing it as something else.
//   * We know enough to raise an error. Note: not all diags are errors.
//     But errors should prevent lexing from proceeding to parsing.
//     Fatal errors are just exceptions.
// Never invokes forgetBefore() or oalexSkip.acrossLines().
//
// From the above, we can now settle on two different kinds of parsing
// functions:
//   * Ones that are simple: words, tokens, comments.
//       - They will never update diagnostics: they have insufficient context.
//       - Their signature will be: foo(const Input& input,size_t& i);
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

optional<vector<WholeSegment>>
lexSectionHeaderContents(InputDiags& ctx, size_t& i) {
  const Input& input = ctx.input;
  Resetter rst(ctx, i);
  vector<WholeSegment> rv;
  // TODO reduce bsearch overhead.
  for(i = oalexSkip.withinLine(input, i);
      input.bol(i) == input.bol(rst.start());
      i = oalexSkip.withinLine(input, i)) {
    if(isSectionHeaderNonSpace(input[i])) {
      if(optional<WholeSegment> token = lexHeaderWord(ctx,i))
        rv.push_back(*token);
      else return nullopt;
    }else return nullopt;
  }
  rst.markUsed(i);
  return rv;
}

// Consume it even if the line is indented, so the caller can raise an error.
// On success:
//   Returns the *start* of dashes so caller can in fact check indentation.
//   Modifies i to the beginning of the next line.
optional<size_t> lexDashLine(InputDiags& ctx, size_t& i) {
  const Input& input = ctx.input;
  if(!input.sizeGt(i)) return nullopt;
  Resetter rst(ctx, i);
  i = oalexSkip.withinLine(ctx.input, i);
  if(input.bol(i) != input.bol(rst.start()) || input[i]!='-') return nullopt;
  size_t dashStart = i;
  while(input.sizeGt(i) && input[i]=='-') ++i;
  i = oalexSkip.withinLine(input, i);
  if(input.bol(i) == input.bol(rst.start())) return nullopt;
  else { rst.markUsed(i); return dashStart; }
}

// For a backslash code, this function assumes `i` is already at the character
// just after the backslash, inside a string literal.
optional<char> lexQuotedEscape(InputDiags& ctx, size_t& i) {
  const Input& input = ctx.input;
  if(!input.sizeGt(i)) return Error(ctx, i-1, "Incomplete escape code");
  char ch;
  // If this changes, please change jsonloc::pringString() as well.
  switch(input[i]) {
    case '\\': ch = '\\'; break;
    case 'n': ch = '\n'; break;
    case 't': ch = '\t'; break;
    case '"': ch = '"'; break;
    case '\'': ch = '\''; break;
    case 'x': return lexHexCode(ctx, ++i);
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
  for(; ctx.input.sizeGt(eol); ++eol) {
    if(ctx.input[eol]=='\n') { nlend = true; break; }
  }
  string rv = ctx.input.substr(i, eol-i);
  i += rv.size() + nlend;
  return rv;
}

// If ctx.input[i] starts a blank line, return "".
// If the indentation is less than parindent, return nullopt.
// Else, return the source line with parindent stripped out.
//
// If indentation is incompatible with parindent, we still consume the current
// line, but then add a Error() and return "". This allows us a form of error
// recovery: we can still parsing the subsequent lines (since caller ends
// source block on nullopt), while not returning any potential garbage.
optional<string> lexSourceLine(InputDiags& ctx, size_t& i,
                               string_view parindent) {
  const Input& input = ctx.input;
  if(!input.sizeGt(i) || i!=input.bol(i)) return nullopt;
  size_t j = oalexWSkip.withinLine(input, i);

  // Whitespaces don't matter for blank lines.
  if(input.bol(j) != input.bol(i)) { i = j; return ""; }

  IndentCmp cmp = indentCmp(input.substr(i,j-i), parindent);

  // This is likely past the end of this source block.
  if(cmp == IndentCmp::lt) return nullopt;

  if(cmp == IndentCmp::bad) {
    Error(ctx, i, j, "Indentation mixes tabs and spaces differently "
                     "from the previous line");
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
bool isoperch(char ch) { return strchr(":,=|~.-><", ch) != NULL; }
bool isregex(char ch) { return ch == '/'; }

auto toCtor(char ch) -> GluedString::Ctor {
  switch(ch) {
    case '"':  return GluedString::Ctor::dquoted;
    case '\'': return GluedString::Ctor::squoted;
    default: Bug("toCtor() called with invalid char '{}'", ch);
  }
}

optional<BracketType> lexOpenBracket(const Input& input, size_t& i) {
  switch(input[i]) {
    case '[': ++i; return BracketType::square;
    case '{': ++i; return BracketType::brace;
    case '(': ++i; return BracketType::paren;
    default: return nullopt;
  }
}
optional<BracketType> lexCloseBracket(const Input& input, size_t& i) {
  switch(input[i]) {
    case ']': ++i; return BracketType::square;
    case '}': ++i; return BracketType::brace;
    case ')': ++i; return BracketType::paren;
    default: return nullopt;
  }
}

string debugChar(char ch) {
  if(isprint(ch)) return format("'{}'", ch);
  else return format("\\x{:02x}", int(ch));
}

bool isident(char ch) { return isalnum(ch) || ch == '_'; }

// Returns false on eof. Throws on invalid language character.
// TODO think more about how acrossLines() can forget.
[[nodiscard]] bool lookaheadStart(InputDiags& ctx, size_t& i) {
  const Input& input = ctx.input;
  size_t j = oalexSkip.acrossLines(input, i);
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
optional<WholeSegment> lexWord(const Input& input, size_t& i) {
  if(!input.sizeGt(i) || !isWordChar(input[i])) return nullopt;
  size_t oldi = i;
  while(input.sizeGt(i) && isWordChar(input[i])) ++i;
  return WholeSegment(oldi,i,input);
}

// TODO throw Fatal on .... or ::=.
optional<WholeSegment> lexOperator(const Input& input, size_t& i) {
  if(!input.sizeGt(i) || !isoperch(input[i])) return nullopt;

  // Now we know it is a valid input character, see if it is multichar.
  static const char multichars[][4] = {":=","...", "->"};
  size_t oldi = i;
  for(const string& op : multichars) if(input.substr(i,op.size()) == op) {
    i += op.size();
    return WholeSegment(oldi,i,input);
  }
  return WholeSegment(oldi,++i,input);
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

IndentCmp indentCmp(string_view indent1, string_view indent2) {
  size_t i = 0;
  while(true) {
    if(i>=indent1.size() && i>=indent2.size()) return IndentCmp::eq;
    if(i>=indent1.size()) return IndentCmp::lt;
    if(i>=indent2.size()) return IndentCmp::eq;
    if(indent1[i]!=indent2[i]) return IndentCmp::bad;
    ++i;
  }
}

NewlineChar::NewlineChar(const GluedString& s, size_t pos)
  : LexSegment(s.inputPos(pos), s.inputPos(pos)+1,
               tagint_t(LexSegmentTag::newlineChar)) {}

WholeSegment::WholeSegment(const GluedString& s)
  : LexSegment(s.inputPos(0), s.inputPos(s.size()), type_tag),
    data(string(s)) { }

GluedString::GluedString(InputDiags& ctx, WholeSegment s)
  : LexSegment(s.stPos, s.enPos, type_tag),
    s_(std::move(*s)), ctor_(Ctor::wholeSegment), ctx_(&ctx),
    index_map_({IndexRelation{.inputPos = s.stPos, .quotePos = 0}}) {}

static bool cmpByQuotePos(const IndexRelation& a, const IndexRelation& b) {
  return a.quotePos < b.quotePos;
}

// Returns index_map.iterator_type, never returns index_map.begin().
static auto upperBound(const vector<IndexRelation>& index_map, size_t qpos) {
  auto it = upper_bound(index_map.begin(), index_map.end(),
                     IndexRelation{.inputPos = 0, .quotePos = qpos},
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
  imap[0] = {.inputPos = this->inputPos(pos), .quotePos = 0};
  for(size_t i=1; i<imap.size(); ++i) imap[i].quotePos -= pos;
  size_t st = imap[0].inputPos;
  size_t en = imap.back().inputPos + (len - imap.back().quotePos);
  return GluedString(st, en, this->substr(pos, len), Ctor::subqstr,
                     ctx_, std::move(imap));
}

pair<size_t,size_t> GluedString::rowCol(size_t pos) const {
  return ctx_->input.rowCol(this->inputPos(pos));
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

// For a "\xhh" code, this function assumes "\x" has been consumed, and now we
// are just parsing the "hh" part. `i` points to what should be the first hex
// digit.
optional<char> lexHexCode(InputDiags& ctx, size_t& i) {
  const Input& input = ctx.input;
  if(!input.sizeGt(i+1))
    return Error(ctx, i-2, "Incomplete hex code");
  if(!isxdigit(input[i]) || !isxdigit(input[i+1]))
    return Error(ctx, i-2, "Invalid hex code");
  i += 2;
  return stoi(string(input.substr(i-2,2)),nullptr,16);
}

// Returns error-free nullopt if and only if !isregex(input[i]). In all other
// cases, it either returns a good value, or it adds diags to explain the
// problem.  If such diags are added, it consumes input until the next
// unescaped '/'.
static optional<RegexPattern> lexRegexPattern(InputDiags& ctx, size_t& i) {
  const Input& input = ctx.input;
  if(!input.sizeGt(i) || !isregex(input[i])) return nullopt;
  Resetter rst(ctx, i);
  unique_ptr<const Regex> rv = parseRegex(ctx, i);  // TODO: improve naming.
  //if(rv == nullptr) return nullopt;
  if(rv == nullptr) {
    // Scan till the next unescaped '/'
    ++i;
    while(input.sizeGt(i)) {
      if(input[i] == '\\') {
        if(input.sizeGt(++i)) ++i;
      }else if(input[i] == '/') {
        rst.markUsed(++i);
        break;
      }else ++i;
    }
    return nullopt;
  }
  rst.markUsed(i);
  return RegexPattern{rst.start(), i, std::move(rv)};
}

// This function never returns nullopt silently. It will either return
// a value, or add something to ctx.diags.
static optional<ExprToken> lexSingleToken(InputDiags& ctx, size_t& i) {
  if(!ctx.input.sizeGt(i)) FatalBug(ctx, i, "lexSingleToken() Out of bound");
  else if(isquote(ctx.input[i])) return lexQuotedString(ctx, i);
  else if(isWordChar(ctx.input[i])) return lexWord(ctx.input, i);
  else if(isoperch(ctx.input[i])) return lexOperator(ctx.input, i);
  else if(isregex(ctx.input[i])) return lexRegexPattern(ctx, i);
  else return Error(ctx, i, "Invalid source character");
}

/* Behavior:

   * Throws Fatal() on unexpected characters (outside quotes).
   * On valid source chars, produces silent nullopt on eof.
   * If no open bracket, still silent nullopt.
   * Any failure after this produces diags.
 */
optional<BracketGroup> lexBracketGroup(InputDiags& ctx, size_t& i) {
  const Input& input = ctx.input;
  Resetter rst(ctx, i);
  if(!lookaheadStart(ctx,i)) return nullopt;

  BracketType bt;
  if(auto btOpt = lexOpenBracket(input,i)) bt=*btOpt;
  else return nullopt;

  BracketGroup bg(i,Input::npos,bt);
  while(true) {
    if(!lookaheadStart(ctx,i)) {  // lookaheadStart is false on EOF.
      Error(ctx, i, format("Match not found for '{}'.", openBracket(bt)));
      i = Input::npos;
      return nullopt;
    }

    if(auto btOpt = lexCloseBracket(input,i)) {
      BracketType bt2=*btOpt;
      // The only success return path.
      if(bt==bt2) { bg.enPos=i; rst.markUsed(i); return bg; }
      else {
        rst.markUsed(i+1);
        return Error(ctx, rst.start(), i,
                     format("Match not found for '{}', found '{}' instead.",
                            openBracket(bt), closeBracket(bt2)));
      }
    }

    if(isbracket(ctx.input[i])) {
      if(auto bgopt = lexBracketGroup(ctx,i))
        bg.children.push_back(std::move(*bgopt));
      else ++i;
    }else {
      size_t oldi = i;
      if(auto x = lexSingleToken(ctx, i)) bg.children.push_back(std::move(*x));
      else if(i == oldi) ++i;
    }
  }
}

// Returns nullopt on eof. Throws on invalid language character.
optional<WholeSegment> lookahead(InputDiags& ctx, size_t i) {
  if(!lookaheadStart(ctx,i)) return nullopt;
  if(auto tok = lexWord(ctx.input, i)) return tok;
  else if(isbracket(ctx.input[i]) || isquote(ctx.input[i]))
    return WholeSegment(i,i+1,ctx.input);
  else if(auto op = lexOperator(ctx.input, i)) return op;
  else Fatal(ctx, i, "Invalid input character");
}

// It returns an error-free nullopt iff ctx.input[i] is not a '"', in which case
// the caller should try parsing something else. In all other cases, it will
// either return a valid string, or nullopt with errors added to ctx.diags. In
// this case, increment `i` beyond the end of the next unescaped '"', or in case
// of an unexpected end of line, beyond the next newline.
optional<GluedString> lexQuotedString(InputDiags& ctx, size_t& i) {
  const Input& input = ctx.input;
  if(!input.sizeGt(i) || !isquote(input[i])) return nullopt;
  const char quote = input[i];
  Resetter rst(ctx, i);
  string s;
  vector<IndexRelation> imap;
  bool error = false;
  ++i;
  imap.push_back(IndexRelation{.inputPos = i, .quotePos = 0});
  while(input.sizeGt(i) && input[i] != '\n') {
    if(input[i] == quote) {
      rst.markUsed(++i);
      if(!error)
        return GluedString(rst.start(), i, s, toCtor(quote),
                           &ctx, std::move(imap));
      else return nullopt;
    }else if(input[i] == '\\') {
      if(optional<char> escres = lexQuotedEscape(ctx, ++i)) {
        s += *escres;
        imap.push_back(IndexRelation{.inputPos = i, .quotePos = s.size()});
      } else error = true;
    }else s += input[i++];
  }
  Error(ctx, rst.start(), i, "Unexpected end of line");
  rst.markUsed(++i);
  return nullopt;
}

static bool isSourceFence(string_view fence) {
  if(fence.substr(0,3) != "```") return false;
  size_t i;
  for(i=3; i<fence.size(); ++i) if(!isident(fence[i])) break;
  for(   ; i<fence.size(); ++i) if(fence[i]!=' ' && fence[i]!='\t') break;
  return i == fence.size();
}

optional<GluedString> lexFencedSource(InputDiags& ctx, size_t& i) {
  Input& input = ctx.input;
  if(!input.sizeGt(i) || i!=input.bol(i) || input.substr(i,3)!="```")
    return nullopt;
  Resetter rst(ctx, i);
  vector<IndexRelation> imap;
  string fence = getline(ctx, i);
  if(!isSourceFence(fence))
    Fatal(ctx, rst.start(), i, "Fences must be alphanumeric");

  // Valid starting fence, so now we are commited to changing i.
  size_t fenceStart = rst.start();
  size_t inputStart = i;
  rst.markUsed(i);
  imap.push_back(IndexRelation{.inputPos = i, .quotePos = 0});
  while(input.sizeGt(i)) {
    size_t lineStart = i;
    string line = getline(ctx, i);
    if(line == fence) {
      GluedString s(fenceStart, i-1,
                     input.substr(inputStart, lineStart-inputStart),
                     GluedString::Ctor::fenced, &ctx, std::move(imap));
      rst.markUsed(i);
      return s;
    } else imap.push_back(IndexRelation{.inputPos = i,
                                        .quotePos = i-inputStart});
  }
  rst.markUsed(i);
  return Error(ctx, fenceStart, i-1, "Source block ends abruptly");
}

// Can return nullopt if it's only blank lines till a non-blank (or non-error)
// source line, strictly less indented than parindent.
optional<GluedString>
lexIndentedSource(InputDiags& ctx, size_t& i, string_view parindent) {
  Input& input = ctx.input;
  string rv;
  Resetter rst(ctx,i);
  vector<IndexRelation> imap;
  bool allblank = true;
  while(input.sizeGt(i)) {
    size_t ls = i;
    optional<string> line = lexSourceLine(ctx,i,parindent);
    if(!line.has_value()) break;
    if(line->empty())
      imap.push_back(IndexRelation{.inputPos = ls, .quotePos = rv.size()});
    else {
      allblank = false;
      imap.push_back(IndexRelation{.inputPos = ls+parindent.size(),
                                   .quotePos = rv.size()});
    }
    rv += *line; rv += '\n';
    rst.markUsed(i);
  }
  if(allblank) return nullopt;
  imap.push_back(IndexRelation{.inputPos = i, .quotePos = rv.size()});
  GluedString qs(rst.start(), i, std::move(rv), GluedString::Ctor::indented,
                 &ctx, std::move(imap));
  rst.markUsed(i);
  return qs;
}

// Can return nullopt if it's only blank lines till eof.
// It uses oalexWSkip, since even comments of user language needs
// to be indented. Throws if we are not at the beginning of a line, or
// at the end of the previous line.
optional<WholeSegment>
lookaheadParIndent(InputDiags& ctx, size_t i) {
  Input& input = ctx.input;
  if(i != input.bol(i))
    FatalBug(ctx, i, "ParIndent computation cannot start mid-line");
  i = oalexWSkip.acrossLines(input, i);
  if(!input.sizeGt(i)) return nullopt;
  return WholeSegment(input.bol(i), i, input);
}

// TODO utility for checking leader indent.

// Changes `i` iff:
//   * We find a content line followed immediately by a line of just dashes,
//     with no intervening blank line.
//   * Any leading or trailing space or tabs are discarded. So are comments.
// If any of these conditions fail, we promptly return nullopt. A content line
// is one where the only non-comment characters are in /[0-9A-Za-z_ \t]+/.
//
// Additionally, we produce errors if:
//   * They are indented.
//   * They are neighboring other non-blank lines.
// In these two cases, we still return the header tokens.
// TODO: We should also produce errors if there is an invalid character in
// an otherwise good section header. The same for some odd character in a line
// overwhelmed with dashes.
optional<vector<WholeSegment>> lexSectionHeader(InputDiags& ctx, size_t& i) {
  if(i != ctx.input.bol(i)) return nullopt;

  Resetter rst(ctx, i);
  while(auto j = skipBlankLine(ctx,i)) i = *j;
  optional<vector<WholeSegment>> rv = lexSectionHeaderContents(ctx,i);
  if(!rv) return nullopt;
  optional<size_t> stDash=lexDashLine(ctx,i);
  if(!stDash) return nullopt;
  rst.markUsed(i);

  size_t stCont=rv->at(0).stPos;
  const Input& input=ctx.input;
  if(input.bol(stCont) != stCont)
    Error(ctx, input.bol(stCont), stCont-1,
          "Section headers must not be indented");
  if(input.bol(*stDash) != *stDash)
    Error(ctx, input.bol(*stDash), *stDash-1,
          "Dashes in a section header must not be indented");

  return rv;
}

optional<vector<ExprToken>> lexNextLine(InputDiags& ctx, size_t& i) {
  if(i != ctx.input.bol(i)) FatalBug(ctx, i, "lexNextLine() must start at bol");
  Resetter rst(ctx, i);
  while(optional<size_t> j = skipBlankLine(ctx,i)) i = *j;

  vector<ExprToken> rv;
  size_t prevBol = i;
  for(i=oalexSkip.withinLine(ctx.input, prevBol);
      ctx.input.sizeGt(i) && ctx.input.bol(i) == prevBol;
      i=oalexSkip.withinLine(ctx.input,i)) {
    optional<ExprToken> tok = lexBracketGroup(ctx, i);
    if(!tok.has_value()) {
      tok = lexSingleToken(ctx, i);
      if(!tok.has_value()) return nullopt;
    }
    i = enPos(*tok);
    rv.push_back(std::move(*tok));
    prevBol = ctx.input.bol(i);
  }
  rst.markUsed(i);
  return rv;
}

vector<vector<ExprToken>>
splitCommaNoEmpty(InputDiagsRef ctx, vector<ExprToken> elts) {
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
