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

#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "segment.h"
#include "input_view_manual.h"
#include "util.h"

using std::function;
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

// Consumes everything from comment marker upto and including the newline.
bool lexComment(const Input& input, size_t& i) {
  if(i>=input.size()) return false;
  if(input[i]!='#') return false;
  for(; i<input.size() && input[i]!='\n'; ++i);
  if(i<input.size()) ++i;  // Consume trailing newline if any.
  return true;
}

void skipSpaceTab(const Input& input, size_t& i) {
  for(; i<input.size() && (input[i]==' ' || input[i]=='\t'); ++i);
}

// I would have loved to require comments about space. Someday I will.
bool lexSpaceCommentsToLineEnd(const Input& input, size_t& i) {
  size_t j=i;
  skipSpaceTab(input,j);
  if(j>=input.size() || lexComment(input,j)) i=j;
  else if(input[j]=='\n') i=j+1;  // all blanks, no comment.
  else return false;
  return true;
}

// Blank or comment-only line.
bool lexBlankLine(const Input& input, size_t& i) {
  if(i>=input.size() || i!=input.bol(i)) return false;
  return lexSpaceCommentsToLineEnd(input,i);
}

optional<AlnumToken> lexHeaderWord(const Input& input, size_t& i) {
  size_t j=i;
  while(j<input.size() && isSectionHeaderNonSpace(input[j])) ++j;
  if(i==j) return nullopt;
  else {
    size_t iold=i; i=j;
    return AlnumToken(iold,j,input);
  }
}

// TODO distinguish between hard and soft return false.
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

optional<vector<AlnumToken>>
lexSectionHeaderContents(const Input& input, size_t& i) {
  size_t j = i;
  vector<AlnumToken> rv;
  while(j < input.size()) {
    char ch = input[j];
    if(ch=='\n' || ch=='#') {
      if(lexSpaceCommentsToLineEnd(input,j)) break;
      else BugDie()<<"Couldn't lex space-comments at "<<j;
    }else if(ch==' ' || ch=='\t') skipSpaceTab(input,j);
    else if(isSectionHeaderNonSpace(ch)) {
      if(optional<AlnumToken> token = lexHeaderWord(input,j))
        rv.push_back(*token);
      else return nullopt;
    }else return nullopt;
  }
  i = j;
  return rv;
}

optional<size_t> lexDashLine(const Input& input, size_t& i) {
  if(i>=input.size()) return nullopt;
  size_t j=i;
  skipSpaceTab(input,j);
  if(j>=input.size() || input[j]!='-') return nullopt;
  size_t rv=j;
  while(j<input.size() && input[j]=='-') ++j;
  if(!lexSpaceCommentsToLineEnd(input,j)) return nullopt;
  else { i=j; return rv; }
}

string locationString(const Diag& diag) {
  if(diag.stLine != diag.enLine) return Str()<<diag.stLine<<'-'<<diag.enLine;
  else if(diag.stPos != diag.enPos)
    return Str()<<diag.stLine<<':'<<diag.stPos<<'-'<<diag.enPos;
  else return Str()<<diag.stLine<<':'<<diag.stPos;
}

string severityString(Diag::Severity sev) {
  switch(sev) {
    case Diag::error: return "error";
    case Diag::warning: return "warning";
    case Diag::note: return "note";
    default: BugDie()<<"Diagnostics has a strange severity: "<<sev;
  }
}

optional<char> lexHexEscape(Lexer& lex, size_t& i) {
  const Input& input = lex.input;
  string_view code = input.substr(i, 2);  // Force read before comparing size.
  if(code.size() < 2)
    return lex.Error(i-2,input.size(),"Incomplete hex code");
  if(!isxdigit(code[0]) || !isxdigit(code[1]))
    return lex.Error(i,i+2,"Invalid hex code");
  i += 2;
  return stoi(string(code),0,16);
}

optional<char> lexQuotedEscape(Lexer& lex, size_t& i) {
  const Input& input = lex.input;
  if(i>=input.size()) return lex.Error(i-1,i,"Incomplete escape code");
  char ch;
  switch(input[i]) {
    case '\\': ch = '\\'; break;
    case 'n': ch = '\n'; break;
    case 't': ch = '\t'; break;
    case '"': ch = '"'; break;
    case 'x': return lexHexEscape(lex, ++i);
    default: return lex.Error(i-1,i+1,"Invalid escape code");
  }
  ++i;
  return ch;
}

}  // namespace

nullopt_t Lexer::Error(size_t st, size_t en, string msg) {
  this->diags.emplace_back(this->input, st, en, Diag::error, std::move(msg));
  return nullopt_t();
}

nullopt_t Lexer::Warning(size_t st, size_t en, string msg) {
  this->diags.emplace_back(this->input, st, en, Diag::warning, std::move(msg));
  return nullopt_t();
}

nullopt_t Lexer::Note(size_t st, size_t en, string msg) {
  this->diags.emplace_back(this->input, st, en, Diag::note, std::move(msg));
  return nullopt_t();
}

optional<QuotedString> lexQuotedString(Lexer& lex, size_t& i) {
  const Input& input = lex.input;
  if(i>=input.size() || input[i]!='"') return nullopt;
  size_t j = i;
  string s;
  bool error = false;
  ++j;
  while(j < input.size()) {
    if(input[j] == '"') {
      size_t oldi = i;
      i = ++j;
      if(!error) return QuotedString(oldi,j,s);
      else return nullopt;
    }
    else if(input[j] == '\n') {
      lex.Error(i,j,"Unexpected end of line");
      i = ++j;
      return nullopt;
    }else if(input[j] == '\\') {
      if(optional<char> escres = lexQuotedEscape(lex, ++j)) s += *escres;
      else error = true;
    }else s += input[j++];
  }
  lex.Error(i,j,"String literal never ends");
  return nullopt;
}

Diag::operator string() const {
  return locationString(*this) + ": " + severityString(severity) + ": " + msg;
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
optional<vector<AlnumToken>> lexSectionHeader(Lexer& lex, size_t& i) {
  const Input& input=lex.input;
  size_t j=i;

  while(lexBlankLine(input,j));
  optional<vector<AlnumToken>> rv = lexSectionHeaderContents(input,j);
  if(!rv) return nullopt;
  optional<size_t> stDash=lexDashLine(input,j);
  if(!stDash) return nullopt;
  i = j;

  size_t stCont=rv->at(0).stPos;
  if(!isSectionHeaderNonSpace(input[input.bol(stCont)]))
    lex.Error(input.bol(stCont),stCont,
        Str() << "Section headers must not be indented");
  if(input[input.bol(*stDash)] != '-')
    lex.Error(input.bol(*stDash),*stDash,
        Str() << "Dashes in a section header must not be indented");

  return rv;
}

}  // namespace oalex::lex
