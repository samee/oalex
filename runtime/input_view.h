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
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace oalex {

using LocPair = std::pair<ssize_t,ssize_t>;
constexpr LocPair nrange{-1,-1};

class InputPiece {
 public:
  static constexpr auto npos = size_t(-1);

  virtual char operator[](size_t sz) const = 0;
  virtual bool sizeGt(size_t sz) const = 0;
  virtual std::pair<size_t,size_t> rowCol(size_t i) const = 0;
  virtual bool hasPrefix(size_t pos, std::string_view s) const = 0;
  bool hasPrefix(size_t pos, size_t count, std::string_view s) const
    { return count >= s.size() && hasPrefix(pos,s); }
  virtual size_t find(char ch, size_t pos) const = 0;
  virtual size_t bol(size_t i) const = 0;
  virtual std::string substr(size_t st,  size_t len) const = 0;
  std::string substr_range(size_t st, size_t en) const {
    return substr(st, en-st);
  }
  virtual ~InputPiece() = default;
};

class InputStream {
 public:
  virtual int16_t getch() = 0;
  virtual ~InputStream() {}
};

// input_view_auto_forget.h seemed too complicated for a simple recursive
// descent parser.  We try an alternative abstraction here, where we access it
// like a normal string, but it only keeps a smallish substring in memory. Any
// time we use operator[], it loads new characters into buf_ if necessary. It
// only forgets parts of it when explicitly asked to.
//
// It always tries to keep one extra character beyond what was already read
// with operator[], so that the following loop will work:
//
//   for(size_t i=0; input.sizeGt(i); ++i) { process(input[i]); }
//
// input.size_ will change from npos to the real size as soon as the last
// character is accessed.
//
// Convention: const methods cannot forget, but they can still cause I/O. So
// you can safely pass a `const Input&` to a method knowing that they won't
// cause you to lose parts of the input string you still care about.

class Input final : public InputPiece {
 public:
  explicit Input(InputStream* is) : stream_(is), size_(npos) {}
  explicit Input(std::string_view s);
  Input(const Input&) = delete;
  Input(Input&&) = default;
  Input& operator=(const Input&) = delete;
  Input& operator=(Input&&) = default;

  void forgetBefore(size_t pos);   // Amortized O(1).
  char operator[](size_t sz) const override;  // Amortized O(1).
  bool sizeGt(size_t pos) const override { peekTo(pos); return pos<size_; }

  // Beginning-of-line index. It is okay if characters at position i has
  // already been forgotten. It is still useful for figuring out indentation.
  // E.g. checking i == bol(i), or isspace(input[bol(i) .. i]).
  // If i is beyond eof, it will still lead us to the point right after the
  // last newline in input.
  //
  // O(log k), where k is the number of lines seen so far.
  size_t bol(size_t i) const override;

  // Returns 1-based positions: line number, and offset in that line.
  // Everything else in this lass is 0-based.
  // O(log k), where k is the working window size.
  std::pair<size_t,size_t> rowCol(size_t i) const override;

  // Like std::string::substr, except:
  //   * Throws an out of bound exception if pos has already been forgotten.
  //   * Also throws if pos >= size().
  //   * This doesn't have a default parameter. Defaulting count to npos would
  //     defeat the whole point of this class, since it would have been more
  //     convenient to use std::string instead.
  //
  // Like std::string::substr, if `count` is too large, we silently truncate
  // the returned string.
  //
  // Idea: perhaps use:
  //   struct string_substr { const string* s; uint32_t st,en; };
  // Can also use size_t instead, but that risks increasing sizeof().
  // Unlike std::string_view, it will stay valid even if s is appended to,
  // and undergoes reallocation.
  std::string substr(size_t pos, size_t count) const override;
  using InputPiece::hasPrefix;
  bool hasPrefix(size_t pos, std::string_view s) const override;
  size_t find(char ch, size_t pos) const override;

  static const size_t maxLineLength = 5000;

 private:
  mutable std::string buf_;
  InputStream* stream_;
  size_t start_pos_=0;
  mutable size_t size_;
  mutable std::vector<size_t> newlines_;

  void peekTo(size_t last) const;
  void peekAndBoundCharAt(size_t i) const;
  bool endSeen() const { return size_ < npos; }
};

// Produces a short prefix of the string for debugging.
std::string debugPrefix(const InputPiece& input, size_t i);

}  // namespace oalex
