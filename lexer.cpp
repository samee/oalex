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
using std::nullopt_t;
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

// Consumes everything from comment marker upto and including the newline.
[[nodiscard]] bool lexComment(const Input& input, size_t& i) {
  if(!input.sizeGt(i)) return false;
  if(input[i]!='#') return false;
  for(; input.sizeGt(i) && input[i]!='\n'; ++i);
  if(input.sizeGt(i)) ++i;  // Consume trailing newline if any.
  return true;
}

void skipSpaceTab(const Input& input, size_t& i) {
  for(; input.sizeGt(i) && (input[i]==' ' || input[i]=='\t'); ++i);
}

[[nodiscard]] bool lexEol(const Input& input, size_t& i) {
  if(!input.sizeGt(i)) return true;
  else if(input[i]=='\n') { ++i; return true; }
  else return false;
}

// I would have loved to require comments about space. Someday I will.
[[nodiscard]] bool lexSpaceCommentsToLineEnd(const Input& input, size_t& i) {
  size_t j=i;
  skipSpaceTab(input,j);
  if(lexEol(input,j) || lexComment(input,j)) { i=j; return true; }
  else return false;
}

// Blank or comment-only line.
[[nodiscard]] bool lexBlankLine(const Input& input, size_t& i) {
  if(!input.sizeGt(i) || i!=input.bol(i)) return false;
  return lexSpaceCommentsToLineEnd(input,i);
}

optional<UnquotedToken> lexHeaderWord(const Input& input, size_t& i) {
  size_t j=i;
  while(input.sizeGt(j) && isSectionHeaderNonSpace(input[j])) ++j;
  if(i==j) return nullopt;
  else {
    size_t iold=i; i=j;
    return UnquotedToken(iold,j,input);
  }
}

// Return conditions:
//   * Success: Return has_value(), `i` has been incremented past the end.
//   * Not recognized: Try parsing it as something else.
//   * We know enough to raise an error. Note: not all diags are errors.
//     But errors should prevent lexing from proceeding to parsing.
//     Fatal errors are just exceptions.
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
//       - Likely signature: foo(const Lexer&,size_t&);

optional<vector<UnquotedToken>>
lexSectionHeaderContents(const Input& input, size_t& i) {
  size_t j = i;
  vector<UnquotedToken> rv;
  while(input.sizeGt(j)) {
    char ch = input[j];
    if(ch=='\n' || ch=='#') {
      if(lexSpaceCommentsToLineEnd(input,j)) break;
      else BugDie()<<"Couldn't lex space-comments at "<<j;
    }else if(ch==' ' || ch=='\t') skipSpaceTab(input,j);
    else if(isSectionHeaderNonSpace(ch)) {
      if(optional<UnquotedToken> token = lexHeaderWord(input,j))
        rv.push_back(*token);
      else return nullopt;
    }else return nullopt;
  }
  i = j;
  return rv;
}

optional<size_t> lexDashLine(const Input& input, size_t& i) {
  if(!input.sizeGt(i)) return nullopt;
  size_t j=i;
  skipSpaceTab(input,j);
  if(!input.sizeGt(j) || input[j]!='-') return nullopt;
  size_t rv=j;
  while(input.sizeGt(j)  && input[j]=='-') ++j;
  if(!lexSpaceCommentsToLineEnd(input,j)) return nullopt;
  else { i=j; return rv; }
}

// For a "\xhh" code, this function assumes "\x" has been consumed, and now we
// are just parsing the "hh" part. `i` points to what should be the first hex
// digit.
optional<char> lexHexEscape(Lexer& lex, size_t& i) {
  const Input& input = lex.input;
  if(!input.sizeGt(i+1))
    return lex.Error(i-2,"Incomplete hex code");
  if(!isxdigit(input[i]) || !isxdigit(input[i+1]))
    return lex.Error(i-2,"Invalid hex code");
  i += 2;
  return stoi(string(input.substr(i-2,2)),nullptr,16);
}

// For a backslash code, this function assumes `i` is already at the character
// just after the backslash, inside a string literal.
optional<char> lexQuotedEscape(Lexer& lex, size_t& i) {
  const Input& input = lex.input;
  if(!input.sizeGt(i)) return lex.Error(i-1,"Incomplete escape code");
  char ch;
  // If this changes, please change jsonloc::pringString() as well.
  switch(input[i]) {
    case '\\': ch = '\\'; break;
    case 'n': ch = '\n'; break;
    case 't': ch = '\t'; break;
    case '"': ch = '"'; break;
    case 'x': return lexHexEscape(lex, ++i);
    default: return lex.Error(i-1,"Invalid escape code");
  }
  ++i;
  return ch;
}

// Return value does *not* include trailing newline, if any.  However, i *is*
// incremented past the newline so we are ready to read the next line if one
// exists. We never care about whether or not the last line ends with a newline.
string getline(Lexer& lex, size_t& i) {
  size_t eol = i;
  bool nlend = false;
  for(; lex.input.sizeGt(eol); ++eol) {
    if(lex.input[eol]=='\n') { nlend = true; break; }
  }
  string rv = lex.input.substr(i, eol-i);
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

optional<string> lexSourceLine(Lexer& lex, size_t& i, string_view parindent) {
  const Input& input = lex.input;
  if(!input.sizeGt(i) || i!=input.bol(i)) return nullopt;
  size_t j = i;
  skipSpaceTab(input,j);

  // Whitespaces don't matter for blank lines.
  if(lexEol(input,j)) { i = j; return ""; }

  IndentCmp cmp = indentCmp(input.substr(i,j-i), parindent);

  // This is likely past the end of this source block.
  if(cmp == IndentCmp::lt) return nullopt;

  if(cmp == IndentCmp::bad) {
    lex.Error(i, j, "Indentation mixes tabs and spaces differently "
                    "from the previous line");
    getline(lex, i);  // Skip to end of line.
    return "";        // Don't return any of it.
  }

  // We have actual content to be returned.
  i += parindent.size();
  return getline(lex, i);
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
[[nodiscard]] bool lookaheadStart(const Lexer& lex, size_t& i) {
  const Input& input = lex.input;
  size_t j = i;
  while(input.sizeGt(j)) {
    if(input[j] == '#') {
      if(!lexComment(input,j)) lex.FatalBug(j,j+1,"lexComment() is acting up");
    }
    else if(input[j]==' ' || input[j]=='\t') skipSpaceTab(input,j);
    else if(input[j]=='\n') ++j;
    else if(isalnum(input[j]) || isquote(input[j]) || isbracket(input[j]) ||
            isoperch(input[j])) { i=j; return true; }
    else lex.Fatal(j,"Unexpected character " + debugChar(input[j]));
  }
  return false;
}

// Careful on numbers: a -12.34e+55 will be decomposed as
//   ["-","12", ".", "34", "e", "+", "56"]
// But that's okay, we won't support floating-point or signed numerals.
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
    default: BugDie()<<"Invalid openBracket() type "<<int(bt);
  }
}

char closeBracket(BracketType bt) {
  switch(bt) {
    case BracketType::square: return ']';
    case BracketType::brace: return '}';
    case BracketType::paren: return ')';
    default: BugDie()<<"Invalid closeBracket() type "<<int(bt);
  }
}

}  // namespace

optional<BracketGroup> lexBracketGroup(Lexer& lex, size_t& i) {
  const Input& input = lex.input;
  size_t j = i;
  if(!lookaheadStart(lex,j)) return nullopt;

  BracketType bt;
  if(auto btOpt = lexOpenBracket(input,j)) bt=*btOpt;
  else return nullopt;

  BracketGroup bg(i,Input::npos,bt);
  while(true) {
    if(!lookaheadStart(lex,j)) {  // lookaheadStart is false on EOF.
      lex.Error(i,Str()<<"Match not found for '"<<openBracket(bt)<<"'.");
      i = Input::npos;
      return nullopt;
    }

    if(auto btOpt = lexCloseBracket(input,j)) {
      BracketType bt2=*btOpt;
      if(bt==bt2) { bg.enPos=i=j; return bg; }  // The only success return path.
      else {
        size_t oldi = i; i = j+1;
        return lex.Error(oldi,j,
          Str()<<"Match not found for '"<<openBracket(bt)<<"', found '"
               <<closeBracket(bt2)<<"' instead.");
      }
    }

    if(auto bgopt = lexBracketGroup(lex,j))
      bg.children.push_back(std::move(*bgopt));
    else if(auto sopt = lexQuotedString(lex,j))
      bg.children.push_back(std::move(*sopt));
    else if(auto wordopt = lexWord(lex.input,j))
      bg.children.push_back(std::move(*wordopt));
    else if(auto operopt = lexOperator(lex.input,j))
      bg.children.push_back(std::move(*operopt));
    else lex.FatalBug(j,
        "Invalid input character, should have been caught by lookaheadStart().");
  }
}

// Returns nullopt on eof. Throws on invalid language character.
optional<UnquotedToken> lookahead(const Lexer& lex, size_t i) {
  if(!lookaheadStart(lex,i)) return nullopt;
  if(auto tok = lexWord(lex.input, i)) return tok;
  else if(isbracket(lex.input[i]) || isquote(lex.input[i]))
    return UnquotedToken(i,i+1,lex.input);
  else if(auto op = lexOperator(lex.input, i)) return op;
  else lex.Fatal(i, "Invalid input character");
}

// TODO replace all BugDie in this file with this, so they have location.
void Lexer::FatalBug(size_t st, size_t en, string msg) const {
  BugDie()<<string(Diag(this->input, st, en, Diag::error, std::move(msg)));
}

void Lexer::Fatal(size_t st, size_t en, string msg) const {
  UserError()<<string(Diag(this->input, st, en, Diag::error, std::move(msg)));
}

nullopt_t Lexer::Error(size_t st, size_t en, string msg) {
  this->diags.emplace_back(this->input, st, en, Diag::error, std::move(msg));
  return nullopt;
}

nullopt_t Lexer::Warning(size_t st, size_t en, string msg) {
  this->diags.emplace_back(this->input, st, en, Diag::warning, std::move(msg));
  return nullopt;
}

nullopt_t Lexer::Note(size_t st, size_t en, string msg) {
  this->diags.emplace_back(this->input, st, en, Diag::note, std::move(msg));
  return nullopt;
}

// It returns an error-free nullopt iff lex.input[i] is not a '"', in which case
// the caller should try parsing something else. In all other cases, it will
// either return a valid string, or nullopt with errors added to lex.diags. In
// this case, increment `i` beyond the end of the next unescaped '"', or in case
// of an unexpected end of line, beyond the next newline.
optional<QuotedString> lexQuotedString(Lexer& lex, size_t& i) {
  const Input& input = lex.input;
  if(!input.sizeGt(i) || input[i]!='"') return nullopt;
  size_t j = i;
  string s;
  bool error = false;
  ++j;
  while(!lexEol(input, j)) {
    if(input[j] == '"') {
      size_t oldi = i;
      i = ++j;
      if(!error) return QuotedString(oldi,j,s);
      else return nullopt;
    }else if(input[j] == '\\') {
      if(optional<char> escres = lexQuotedEscape(lex, ++j)) s += *escres;
      else error = true;
    }else s += input[j++];
  }
  lex.Error(i,j-1,"Unexpected end of line");
  i = j;
  return nullopt;
}

optional<QuotedString> lexDelimitedSource(Lexer& lex, size_t& i) {
  Input& input = lex.input;
  if(!input.sizeGt(i) || i!=input.bol(i) || input.substr(i,3)!="```")
    return nullopt;
  size_t j = i;
  // TODO only allow alphanumeric and space. No comments or punctuation.
  string delim = getline(lex, j);

  // Valid starting delimiter, so now we are commited to changing i.
  size_t delimStart = i;
  size_t inputStart = i = j;
  while(input.sizeGt(i)) {
    size_t lineStart = i;
    string line = getline(lex, i);
    if(line == delim) {
      QuotedString s(delimStart, i-1,
                     input.substr(inputStart, lineStart-inputStart));
      input.forgetBefore(i);
      return s;
    }
  }
  return lex.Error(delimStart, i-1, "Source block ends abruptly");
}

// Can return nullopt if it's only blank lines till a non-blank (or non-error)
// source line, strictly more indented than parindent.
optional<QuotedString>
lexIndentedSource(Lexer& lex, size_t& i, string_view parindent) {
  Input& input = lex.input;
  string rv;
  size_t j = i;
  bool allblank = true;
  while(input.sizeGt(j)) {
    optional<string> line = lexSourceLine(lex,j,parindent);
    if(!line.has_value()) break;
    if(!line->empty()) allblank = false;
    rv += *line; rv += '\n';
    input.forgetBefore(j);
  }
  if(allblank) return nullopt;
  QuotedString qs(i,j,rv);
  i = j;
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
optional<vector<UnquotedToken>> lexSectionHeader(Lexer& lex, size_t& i) {
  const Input& input=lex.input;
  size_t j=i;

  while(lexBlankLine(input,j));
  optional<vector<UnquotedToken>> rv = lexSectionHeaderContents(input,j);
  if(!rv) return nullopt;
  optional<size_t> stDash=lexDashLine(input,j);
  if(!stDash) return nullopt;
  i = j;

  size_t stCont=rv->at(0).stPos;
  if(!isSectionHeaderNonSpace(input[input.bol(stCont)]))
    lex.Error(input.bol(stCont),stCont-1,
        Str() << "Section headers must not be indented");
  if(input[input.bol(*stDash)] != '-')
    lex.Error(input.bol(*stDash),*stDash-1,
        Str() << "Dashes in a section header must not be indented");

  return rv;
}

}  // namespace oalex::lex
