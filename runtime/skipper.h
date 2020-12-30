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
#include <string>
#include <string_view>
#include <vector>
#include "input_view.h"

namespace oalex {

// Specifies how whitespaces and comments are skipped. Not used for contexts
// where spaces are significant (e.g. inside string literals).
//
// We allow only a single pair of nestedComment delimiters for now, since we
// don't have a clear precedent on what to do in case of mismatched pairs,
// like "[ ( ] )". It is unclear if this is even a desirable construct in
// language design, or if it should be encouraged.
//
// Usage: Right after we have ended a token, this is used to find the the start
//   of the next one. It does not decide what a token is, since we don't want
//   it to understand string quote escaping. It can't find the end of a token,
//   only the start.
//
// TODO figure out how parameters here affect template-to-grammar conversion.
struct Skipper {
  // Begin and end pairs.
  // Single-line comments use "\n" as the end marker, unnested.
  std::vector<std::pair<std::string,std::string>> unnestedComments;
  std::optional<std::pair<std::string,std::string>> nestedComment;

  /*
  optional<string> valid():

  Returns an error message if something is wrong with member values.
  std::nullopt means everything is okay, and it is safe to use the other
  methods here.

  It is really meant for debugging, not user output. E.g. the messages may
  have method names in them.  So if members are set based on user inputs,
  we need to provide an separate layer of validation with contextual
  messages.

  Curent checks:
    * None of the delimiters must be empty.
    * All the start delimiters must be prefix-free.
    * Nested start and end delimiters must not be prefixes of each other.
    * They must not start with ' ' or '\t'.
    * A '\n' is only allowed as the last or only character in an end
      delimiter.
  */
  std::optional<std::string> valid() const;

  /*
  size_t acrossLines(input, pos):

  It finds the next non-space, non-comment character in input, within the
  substring [pos,eof). If not found, it returns some value larger than input
  size (i.e. some x that makes input.sizeGt(x) false).

  Example:

    Skipper skip{...};
    Input input(...);
    for(size_t i=0; input.sizeGt(i); i=skip.acrossLines(input,i)) {
      // It doesn't matter what type 'tok' is, just that it somehow indicates
      // an end position just beyond what is parsed.
      auto tok = processTokenAt(i);
      i = tok.enPos;
    }

  If indicateBlankLines == true, this will also stop at some arbitrary '\n'
  character to indicate the presence of blank lines *without* any comments.
  This mostly for languages like LaTeX or Markdown that treats uncommented
  blank lines as paragraph breaks, but collapses multiple lines into one.
  The example loop above will produce a single '\n' to indicate a sequence of
  comment-free blank lines, if indicateBlankLines is set to true. Otherwise,
  it will never produce a '\n'.

  If indicateBlankLines = false, it never stops at any whitespace.

  If and only if unfinished comments are found, it returns Input::npos.

  The caller may want to append an extra '\n' to the input,
  especially if it is important to detect unterminated comments. In particular,
  this can allow eof to match a '\n' in an end delimitter of an unnestedComment.
  This may be important if the caller is expecting both single-line and
  multi-line comments. Unterminated comments will produce Input::npos, while
  proper termination will be indicated by some other value.

  Dev notes: If a crlf translation layer becomes important, move Unixify from
    skipper_test.cpp to this header.
  */
  bool indicateBlankLines = false;
  size_t acrossLines(const InputPiece& input, size_t pos) const;

  /*
  size_t withinLine(input, pos):

  It finds the next non-space, non-comment character in input, within a
  single line. A "line" is defined as the substring in range the substring
  [pos,end), where end is the position of the next '\n' or end-of-string,
  whichever comes first.

  The caller may want to append an extra '\n' to the input,
  especially if it is important to detect unterminated comments. In particular,
  this can allow eof to match a '\n' in an end delimitter of an unnestedComment.
  This will allow uniform detection of end-of-line using bol() as shown below.

  If it's all spaces and comments, it returns the beginning of the next line
  (i.e. the next value i such that input.bol(i) > input.bol(pos). If
  this is the last line, it returns some value larger than input size (i.e.
  some x that makes input.sizeGt(x) false).

  Example:

    Skipper skip{...};
    Input input(...);
    for(size_t i=skip.withinLine(input,lineStart);
        input.bol(i) == lineStart;
        i=skip.withinLine(input,i)) {
      // It doesn't matter what type 'tok' is, just that it somehow indicates
      // an end position just beyond what is parsed.
      auto tok = processTokenAt(i);
      i = tok.enPos;
     }
     // We assume Input::getch() terminates input with a newline.
     if(input.sizeGt(i)) nextLineStart = i;
     else foundEof = true;

  If and only if comments don't end within the line, it returns Input::npos.

  If a crlf translation layer becomes important, move Unixify from
  skipper_test.cpp to this header.

  Dev notes: it is possible that this method is never useful, since input.bol()
    always has enough information to provide the same facilities even with
    acrossLines(). I will keep writing this anyway, since it's just easier to
    implement without having to think about blank line exceptions.
  */
  size_t withinLine(const InputPiece& input, size_t pos) const;

  // Tests if we can start skipping from pos.
  bool canStart(const InputPiece& input, size_t pos) const;

  /*
  Skips only over ' ', '\n', and '\t'. This is often used to find the start
  of an incomplete comment for error-reporting, in case acrossLines() or
  withinLines() returns Input::npos. If it's all whitespace till eof, it returns
  some x that makes input.sizeGt(x) false.
  */
  size_t whitespace(const InputPiece& input, size_t pos) const;
};

// Returns true iff ch is found in s.
inline bool is_in(char ch, std::string_view s) {
  return s.find(ch) != std::string_view::npos;
}

}  // namespace oalex
