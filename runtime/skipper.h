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
#include <string>
#include "diags.h"

namespace oalex {

// Specifies how whitespaces and comments are skipped. Not used for contexts
// where spaces are significant (e.g. inside string literals).
//
// We allow only a single pair of nestedComment delimiters for now, since we
// don't want to support mismatched pairs in user languages.
//
// Usage: Right after we have ended a token, this is used to find the the start
//   of the next one. It does not decide what a token is, since we don't want
//   it to understand string quote escaping. It can't find the end of a token,
//   only the start.
//
// TODO Change lexer.cpp to use this instead of custom skippers.
// TODO figure out how parameters here affect template-to-grammar conversion.
struct Skipper {
  // Begin and end pairs.
  // Single-line comments use "\n" as the end marker, unnested.
  std::vector<std::pair<std::string,std::string>> unnestedComments;
  std::optional<std::pair<std::string,std::string>> nestedComment;
  // Checks prefix-freeness, and non-empty strings. Checks that '\n' only
  // appears at the end of a delimitter.
  // Nested: start cannot be a prefix of end or vice versa.
  bool valid() const;

  /*
  It finds the next non-space, non-comment character in ctx.input, within the
  substring [pos,eof). If not found, it returns some value larger than input
  size (i.e. some x that makes ctx.input.sizeGt(x) false).

  Example:

    Skipper skip{...};
    InputDiags ctx(...);
    for(size_t i=0; ctx.input.sizeGt(i); i=skip.acrossLines(ctx,i)) {
      // It doesn't matter what type 'tok' is, just that it somehow indicates
      // an end position just beyond what is parsed.
      auto tok = processTokenAt(i);
      i = tok.enPos;
    }

  If skip.acrossBlankLines == false, this will also stop at the '\n' just
  before the next blank line. A line is blank if it only has spaces, tabs,
  and comments. In that case, skip.blankLines() will take us past a
  contiguous block of blank lines, to the next ctx.input.bol().

  If skip.acrossBlankLines == true, it never stops at any whitespace.

  If unfinished comments are found, it records an error in ctx.diags.
  It is assumed that Input will terminate each line with a '\n', including the
  last line. That's why we expect a '\n' comment end-delimiter to match eof,
  even though we don't have any special-handling for that here.
  */
  bool acrossBlankLines = true;
  size_t acrossLines(InputDiags& ctx, size_t pos) const;
  size_t blankLines(InputDiags& ctx, size_t pos) const;

  /*
  It finds the next non-space, non-comment character in ctx.input, within a
  single line. A "line" is defined as the substring in range the substring
  [pos,end), where end is the position of the next '\n'. Input is expected to
  terminate each line with a '\n', even the last line.

  If it's all spaces and comments, it returns the beginning of the next line
  (i.e. the next value i such that ctx.input.bol(i) > ctx.input.bol(pos). If
  this is the last line, it returns some value larger than input size (i.e.
  some x that makes ctx.input.sizeGt(x) false).

  Example:

    Skipper skip{...};
    InputDiags ctx(...);
    for(size_t i=skip.withinLine(ctx,lineStart);
        ctx.input.bol(i) == lineStart;
        i=skip.withinLine(ctx,i)) {
      // It doesn't matter what type 'tok' is, just that it somehow indicates
      // an end position just beyond what is parsed.
      auto tok = processTokenAt(i);
      i = tok.enPos;
     }
     // We assume Input::getch() terminates input with a newline.
     if(ctx.input.sizeGt(i)) nextLineStart = i;
     else foundEof = true;

  If comments don't end within the line, it records an error in ctx.diags.  We
  rely on Input terminating all lines with '\n', even the last one. So it is
  okay if we are expecting a newline-terminated comment, but the end-user ends
  input before that: we should still see a newline here in Skipper.

  Dev notes: it is possible that this method is never useful, since input.bol()
    always has enough information to provide the same facilities even with
    acrossLines(). I will keep writing this anyway, since it's just easier to
    implement without having to think about acrossBlankLines.
  */
  size_t withinLine(InputDiags& ctx, size_t pos) const;
};

}  // namespace oalex
