/*  Copyright 2019-2024 The oalex authors.

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
  size_t next(input, pos):

  It finds the next non-space, non-comment character in input, within the
  substring [pos,eof). If not found, it returns some value larger than input
  size (i.e. some x that makes input.sizeGt(x) false).

  Example:

    Skipper skip{...};
    Input input(...);
    for(size_t i=0; input.sizeGt(i); i=skip.next(input,i)) {
      // It doesn't matter what type 'tok' is, just that it somehow indicates
      // an end position just beyond what is parsed.
      auto tok = processTokenAt(i);
      i = tok.enPos;
    }

  If newlines == ignore_all, next() never stops at any whitespace.

  If newlines == keep_para, next() will also stop at some arbitrary '\n'
  character to indicate the presence of blank lines *without* any comments.
  This mostly for languages like LaTeX or Markdown that treats uncommented
  blank lines as paragraph breaks, but collapses multiple lines into one.
  The example loop above will produce a single '\n' to indicate a sequence of
  comment-free blank lines.

  If newlines == ignore_blank, next() will ignore blank lines or lines
  with only comments. For other lines (i.e. ones with non-comment text), it
  will stop at the next '\n' character. This is the most common setting for
  newline-sensitive programming languages.

  If newlines == keep_all, all newlines are significant, and none are discarded.

  Both ignore_blank and keep_all consider the first line to be non-blank if we
  start skipping in the middle of the line. The assumption is that we must have
  just finished processing some token. Every value other than ignore_all also
  special-cases the last '\n' of an end delimitter, in that the skippers may
  stop there as instructed instead of skipping over it.

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
  enum class Newlines { ignore_all, keep_all, ignore_blank, keep_para };
  Newlines newlines = Newlines::ignore_all;
  size_t next(const InputPiece& input, size_t pos) const;

  // Tests if we can start skipping from pos.
  bool canStart(const InputPiece& input, size_t pos) const;

  /*
  Skips only over ' ', '\n', and '\t'. This is often used to find the start
  of an incomplete comment for error-reporting, in case next() returns
  Input::npos. If it's all whitespace till eof, it returns some x that makes
  input.sizeGt(x) false.
  */
  size_t whitespace(const InputPiece& input, size_t pos) const;
};

bool operator==(const Skipper& a, const Skipper& b);
inline bool operator!=(const Skipper& a, const Skipper& b) { return !(a==b); }

// Returns true iff ch is found in s.
inline bool is_in(char ch, std::string_view s) {
  return s.find(ch) != std::string_view::npos;
}

// Converts to string, for codegen and for debugging.
std::string_view to_string(Skipper::Newlines newlines);

}  // namespace oalex
