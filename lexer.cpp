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

#include <cstring>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "segment.h"
#include "runtime/input_view.h"
#include "runtime/util.h"

using std::function;
using std::hex;
using std::nullopt;
using std::numeric_limits;
using std::optional;
using std::shared_ptr;
using std::stoi;
using std::string;
using std::string_view;
using std::tie;
using std::vector;

namespace oalex::lex {

namespace {

bool isSectionHeaderNonSpace(char ch) {
  return (ch>='0' && ch<='9')
      || (ch>='A' && ch<='Z')
      || (ch>='a' && ch<='z')
      || ch=='_';
}

optional<size_t>
skipBlankLine(InputDiags& ctx, size_t i) {
  size_t j = skip.withinLine(ctx, i);
  if(ctx.input.bol(i) == ctx.input.bol(j)) return nullopt;
  else return j;
}

// TODO use generic word-lexer based on a char set.
optional<UnquotedToken> lexHeaderWord(InputDiags& ctx, size_t& i) {
  const Input& input = ctx.input;
  Resetter rst(ctx, i);
  while(input.sizeGt(i) && isSectionHeaderNonSpace(input[i])) ++i;
  if(i == rst.start()) return nullopt;
  else {
    rst.markUsed(i);
    return UnquotedToken(rst.start(),i,input);
  }
}

// Return conditions:
//   * Success: Return has_value(), `i` has been incremented past the end.
//   * Not recognized: Try parsing it as something else.
//   * We know enough to raise an error. Note: not all diags are errors.
//     But errors should prevent lexing from proceeding to parsing.
//     Fatal errors are just exceptions.
// Never invokes forgetBefore() or skip.acrossLines().
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

optional<vector<UnquotedToken>>
lexSectionHeaderContents(InputDiags& ctx, size_t& i) {
  const Input& input = ctx.input;
  Resetter rst(ctx, i);
  vector<UnquotedToken> rv;
  // TODO reduce bsearch overhead.
  for(i = skip.withinLine(ctx, i);
      input.bol(i) == input.bol(rst.start());
      i = skip.withinLine(ctx, i)) {
    if(isSectionHeaderNonSpace(input[i])) {
      if(optional<UnquotedToken> token = lexHeaderWord(ctx,i))
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
  i = skip.withinLine(ctx, i);
  if(input.bol(i) != input.bol(rst.start()) || input[i]!='-') return nullopt;
  size_t dashStart = i;
  while(input.sizeGt(i) && input[i]=='-') ++i;
  i = skip.withinLine(ctx, i);
  if(input.bol(i) == input.bol(rst.start())) return nullopt;
  else { rst.markUsed(i); return dashStart; }
}

// For a backslash code, this function assumes `i` is already at the character
// just after the backslash, inside a string literal.
optional<char> lexQuotedEscape(InputDiags& ctx, size_t& i) {
  const Input& input = ctx.input;
  if(!input.sizeGt(i)) return ctx.Error(i-1,"Incomplete escape code");
  char ch;
  // If this changes, please change jsonloc::pringString() as well.
  switch(input[i]) {
    case '\\': ch = '\\'; break;
    case 'n': ch = '\n'; break;
    case 't': ch = '\t'; break;
    case '"': ch = '"'; break;
    case 'x': return lexHexCode(ctx, ++i);
    default: return ctx.Error(i-1,"Invalid escape code");
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

enum class IndentCmp { bad, lt, eq, gt };

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

// If ctx.input[i] starts a blank line, return "".
// If the indentation is less than parindent, return nullopt.
// Else, return the source line with parindent stripped out.
//
// If indentation is incompatible with parindent, we still consume the current
// line, but then add a ctx.Error() and return "". This allows us a form of
// error recovery: we can still parsing the subsequent lines (since caller ends
// source block on nullopt), while not returning any potential garbage.
optional<string> lexSourceLine(InputDiags& ctx, size_t& i,
                               string_view parindent) {
  const Input& input = ctx.input;
  if(!input.sizeGt(i) || i!=input.bol(i)) return nullopt;
  size_t j = wskip.withinLine(ctx, i);

  // Whitespaces don't matter for blank lines.
  if(input.bol(j) != input.bol(i)) { i = j; return ""; }

  IndentCmp cmp = indentCmp(input.substr(i,j-i), parindent);

  // This is likely past the end of this source block.
  if(cmp == IndentCmp::lt) return nullopt;

  if(cmp == IndentCmp::bad) {
    ctx.Error(i, j, "Indentation mixes tabs and spaces differently "
                    "from the previous line");
    getline(ctx, i);  // Skip to end of line.
    return "";        // Don't return any of it.
  }

  // We have actual content to be returned.
  i += parindent.size();
  return getline(ctx, i);
}

bool isquote(char ch) { return ch=='"'; }
bool isbracket(char ch) { return strchr("(){}[]", ch) != NULL; }
bool isoperch(char ch) { return strchr(":,=|~.", ch) != NULL; }

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
  if(isprint(ch)) return Str()<<'\''<<ch<<'\'';
  else return Str()<<"\\x"<<hex<<int(ch);
}

// Returns false on eof. Throws on invalid language character.
// TODO think more about how acrossLines() can forget.
[[nodiscard]] bool lookaheadStart(InputDiags& ctx, size_t& i) {
  const Input& input = ctx.input;
  size_t j = skip.acrossLines(ctx, i);
  if(!input.sizeGt(j)) return false;
  else if(isalnum(input[j]) || isquote(input[j]) || isbracket(input[j]) ||
          isoperch(input[j])) { i=j; return true; }
  else ctx.Fatal(j,"Unexpected character " + debugChar(input[j]));
}

// Careful on numbers: a -12.34e+55 will be decomposed as
//   ["-","12", ".", "34", "e", "+", "56"]
// But that's okay, we won't support floating-point or signed numerals.
// TODO use generic word-lexing features.
optional<UnquotedToken> lexWord(const Input& input, size_t& i) {
  if(!input.sizeGt(i) || !isalnum(input[i])) return nullopt;
  size_t oldi = i;
  while(input.sizeGt(i) && isalnum(input[i])) ++i;
  return UnquotedToken(oldi,i,input);
}

// TODO throw Fatal on .... or ::=.
optional<UnquotedToken> lexOperator(const Input& input, size_t& i) {
  if(!input.sizeGt(i) || !isoperch(input[i])) return nullopt;

  // Now we know it is a valid input character, see if it is multichar.
  static const char multichars[][4] = {":=","..."};
  size_t oldi = i;
  for(const string& op : multichars) if(input.substr(i,op.size()) == op) {
    i += op.size();
    return UnquotedToken(oldi,i,input);
  }
  return UnquotedToken(oldi,++i,input);
}

char openBracket(BracketType bt) {
  switch(bt) {
    case BracketType::square: return '[';
    case BracketType::brace: return '{';
    case BracketType::paren: return '(';
    default: Bug()<<"Invalid openBracket() type "<<int(bt);
  }
}

char closeBracket(BracketType bt) {
  switch(bt) {
    case BracketType::square: return ']';
    case BracketType::brace: return '}';
    case BracketType::paren: return ')';
    default: Bug()<<"Invalid closeBracket() type "<<int(bt);
  }
}

}  // namespace

// For a "\xhh" code, this function assumes "\x" has been consumed, and now we
// are just parsing the "hh" part. `i` points to what should be the first hex
// digit.
optional<char> lexHexCode(InputDiags& ctx, size_t& i) {
  const Input& input = ctx.input;
  if(!input.sizeGt(i+1))
    return ctx.Error(i-2,"Incomplete hex code");
  if(!isxdigit(input[i]) || !isxdigit(input[i+1]))
    return ctx.Error(i-2,"Invalid hex code");
  i += 2;
  return stoi(string(input.substr(i-2,2)),nullptr,16);
}

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
      ctx.Error(i,Str()<<"Match not found for '"<<openBracket(bt)<<"'.");
      i = Input::npos;
      return nullopt;
    }

    if(auto btOpt = lexCloseBracket(input,i)) {
      BracketType bt2=*btOpt;
      // The only success return path.
      if(bt==bt2) { bg.enPos=i; rst.markUsed(i); return bg; }
      else {
        rst.markUsed(i+1);
        return ctx.Error(rst.start(),i,
          Str()<<"Match not found for '"<<openBracket(bt)<<"', found '"
               <<closeBracket(bt2)<<"' instead.");
      }
    }

    if(auto bgopt = lexBracketGroup(ctx,i))
      bg.children.push_back(std::move(*bgopt));
    else if(auto sopt = lexQuotedString(ctx,i))
      bg.children.push_back(std::move(*sopt));
    else if(auto wordopt = lexWord(input,i))
      bg.children.push_back(std::move(*wordopt));
    else if(auto operopt = lexOperator(input,i))
      bg.children.push_back(std::move(*operopt));
    else ctx.FatalBug(i, "Invalid input character, "
                         "should have been caught by lookaheadStart().");
  }
}

// Returns nullopt on eof. Throws on invalid language character.
optional<UnquotedToken> lookahead(InputDiags& ctx, size_t i) {
  if(!lookaheadStart(ctx,i)) return nullopt;
  if(auto tok = lexWord(ctx.input, i)) return tok;
  else if(isbracket(ctx.input[i]) || isquote(ctx.input[i]))
    return UnquotedToken(i,i+1,ctx.input);
  else if(auto op = lexOperator(ctx.input, i)) return op;
  else ctx.Fatal(i, "Invalid input character");
}

static RowColRelation
makeRowColRelation(const Input& input, size_t inputPos, size_t quotePos) {
  auto [r,c] = input.rowCol(inputPos);
  return {.pos = quotePos, .row = r, .col = c};
}

// It returns an error-free nullopt iff ctx.input[i] is not a '"', in which case
// the caller should try parsing something else. In all other cases, it will
// either return a valid string, or nullopt with errors added to ctx.diags. In
// this case, increment `i` beyond the end of the next unescaped '"', or in case
// of an unexpected end of line, beyond the next newline.
optional<QuotedString> lexQuotedString(InputDiags& ctx, size_t& i) {
  const Input& input = ctx.input;
  if(!input.sizeGt(i) || input[i]!='"') return nullopt;
  Resetter rst(ctx, i);
  string s;
  vector<RowColRelation> rcmap;
  bool error = false;
  ++i;
  rcmap.push_back(makeRowColRelation(input, i, 0));
  while(input.sizeGt(i) && input[i] != '\n') {
    if(input[i] == '"') {
      rst.markUsed(++i);
      if(!error) return QuotedString(rst.start(), i, s, std::move(rcmap));
      else return nullopt;
    }else if(input[i] == '\\') {
      if(optional<char> escres = lexQuotedEscape(ctx, ++i)) {
        s += *escres;
        rcmap.push_back(makeRowColRelation(input, i, s.size()));
      } else error = true;
    }else s += input[i++];
  }
  ctx.Error(rst.start(),i,"Unexpected end of line");
  rst.markUsed(++i);
  return nullopt;
}

static bool isSourceDelim(string_view delim) {
  if(delim.substr(0,3) != "```") return false;
  size_t i;
  for(i=3; i<delim.size(); ++i) if(!isalnum(delim[i])) break;
  for(   ; i<delim.size(); ++i) if(delim[i]!=' ' && delim[i]!='\t') break;
  return i == delim.size();
}

optional<QuotedString> lexDelimitedSource(InputDiags& ctx, size_t& i) {
  Input& input = ctx.input;
  if(!input.sizeGt(i) || i!=input.bol(i) || input.substr(i,3)!="```")
    return nullopt;
  Resetter rst(ctx, i);
  vector<RowColRelation> rcmap;
  string delim = getline(ctx, i);
  if(!isSourceDelim(delim))
    ctx.Fatal(rst.start(), i, "Delimiters must be alphanumeric");

  // Valid starting delimiter, so now we are commited to changing i.
  size_t delimStart = rst.start();
  size_t inputStart = i;
  rst.markUsed(i);
  rcmap.push_back(makeRowColRelation(input, i, 0));
  while(input.sizeGt(i)) {
    size_t lineStart = i;
    string line = getline(ctx, i);
    if(line == delim) {
      QuotedString s(delimStart, i-1,
                     input.substr(inputStart, lineStart-inputStart),
                     std::move(rcmap));
      rst.markUsed(i);
      return s;
    } else rcmap.push_back(makeRowColRelation(input, i, i-inputStart));
  }
  rst.markUsed(i);
  return ctx.Error(delimStart, i-1, "Source block ends abruptly");
}

// Can return nullopt if it's only blank lines till a non-blank (or non-error)
// source line, strictly less indented than parindent.
optional<QuotedString>
lexIndentedSource(InputDiags& ctx, size_t& i, string_view parindent) {
  Input& input = ctx.input;
  string rv;
  Resetter rst(ctx,i);
  vector<RowColRelation> rcmap;
  bool allblank = true;
  while(input.sizeGt(i)) {
    size_t ls = i;
    optional<string> line = lexSourceLine(ctx,i,parindent);
    if(!line.has_value()) break;
    if(line->empty()) rcmap.push_back(makeRowColRelation(input, ls, rv.size()));
    else {
      allblank = false;
      rcmap.push_back(makeRowColRelation(input, ls+parindent.size(),
                                         rv.size()));
    }
    rv += *line; rv += '\n';
    rst.markUsed(i);
  }
  if(allblank) return nullopt;
  QuotedString qs(rst.start(), i, std::move(rv), std::move(rcmap));
  rst.markUsed(i);
  return qs;
}

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
optional<vector<UnquotedToken>> lexSectionHeader(InputDiags& ctx, size_t& i) {
  if(i != ctx.input.bol(i)) return nullopt;

  Resetter rst(ctx, i);
  while(auto j = skipBlankLine(ctx,i)) i = *j;
  optional<vector<UnquotedToken>> rv = lexSectionHeaderContents(ctx,i);
  if(!rv) return nullopt;
  optional<size_t> stDash=lexDashLine(ctx,i);
  if(!stDash) return nullopt;
  rst.markUsed(i);

  size_t stCont=rv->at(0).stPos;
  const Input& input=ctx.input;
  if(input.bol(stCont) != stCont)
    ctx.Error(input.bol(stCont),stCont-1,
        Str() << "Section headers must not be indented");
  if(input.bol(*stDash) != *stDash)
    ctx.Error(input.bol(*stDash),*stDash-1,
        Str() << "Dashes in a section header must not be indented");

  return rv;
}

}  // namespace oalex::lex
