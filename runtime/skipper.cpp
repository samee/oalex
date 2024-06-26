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

#include "skipper.h"
#include <string_view>
#include <utility>
#include <vector>
#include "util.h"
using std::format;
using std::pair;
using std::nullopt;
using std::optional;
using std::string;
using std::string_view;
using std::vector;
using namespace std::literals::string_view_literals;

namespace oalex {

static optional<string> validPair(string_view st, string_view en) {
    if(st.empty() || en.empty()) return "Comment delimiters cannot be empty";
    if(is_in(st[0], " \t\n"))
      return "Whitespace at the start of comments will be skipped over";
    return nullopt;
}

string_view to_string(Skipper::Newlines newlines) {
  switch(newlines) {
    case Skipper::Newlines::ignore_all: return "ignore_all";
    case Skipper::Newlines::keep_all: return "keep_all";
    case Skipper::Newlines::ignore_blank: return "ignore_blank";
    case Skipper::Newlines::keep_para: return "keep_para";
    default: Bug("Unknown Newlines value {}", int(newlines));
  }
}

// Switch to `= default` in C++20, so we don't forget any new members.
bool operator==(const Skipper& a, const Skipper& b) {
  return a.unnestedComments == b.unnestedComments &&
         a.nestedComment == b.nestedComment &&
         a.newlines == b.newlines;
}

optional<string> Skipper::valid() const {
  for(const auto& [st,en] : unnestedComments)
    if(auto err = validPair(st,en)) return err;
  if(nestedComment.has_value()) {
    if(auto err = validPair(nestedComment->first, nestedComment->second))
      return err;
    if(nestedComment->first.starts_with(nestedComment->second) ||
       nestedComment->second.starts_with(nestedComment->first))
      return "Nested comment delimiter pair can be confused for each other";
  }

  // Check prefix-freeness.
  vector<string> starts;
  for(const auto& [st,en] : unnestedComments) starts.push_back(st);
  if(nestedComment.has_value()) starts.push_back(nestedComment->first);
  for(size_t i=0; i<starts.size(); ++i)
    for(size_t j=0; j<starts.size(); ++j)
      if(i != j && starts[i].starts_with(starts[j]))
        return format("Comment delimiters cannot be prefixes of each other: "
                      "{}, {}", starts[i], starts[j]);
  return nullopt;
}

// Assumes ctx.input().hasPrefix(pos, end-pos, delims.first).
// Returns npos if comment never ends.
// Returns the exact eof position if comment ends exactly at eof.
static size_t skipPastNestedComment(
    const pair<string,string>& delims,
    const InputPiece& input, size_t pos, size_t end) {
  size_t depth=0, i=pos;
  const auto& [stdelim,endelim] = delims;

  if(stdelim.empty() || endelim.empty())
    Bug("Nested comment delimiters shouldn't be empty. '{}', '{}'",
        stdelim, endelim);

  while(input.sizeGt(i) && i<end) {
    if(input.hasPrefix(i, end-i, stdelim)) {
      i += stdelim.size();  // We know delims are non-empty.
      ++depth;
    }else if(input.hasPrefix(i, end-i, endelim)) {
      i += endelim.size();
      --depth;
      if(depth == 0) return i;
    }else ++i;
  }
  return Input::npos;
}

static size_t
skipPastNext(const string& s, const InputPiece& input, size_t pos, size_t end) {
  for(size_t i=pos; input.sizeGt(i) && i+s.size()<=end; ++i)
    if(input.hasPrefix(i,s)) return i+s.size();
  return Input::npos;
}

// Returns true if something was skipped. Modifies st to past the end of it.
// If we skipped all the way to en and didn't find the end of a comment,
// returns true but sets st to Input::npos.
// For all other values at input[st], returns false and leaves st unchanged.
// Corollary: st is always incremented by some amount if return value is true.
static bool skipComments(const Skipper& skip, const InputPiece& input,
    size_t& st, size_t en) {
  if(skip.nestedComment &&
     input.hasPrefix(st,en-st,skip.nestedComment->first)) {
    st = skipPastNestedComment(*skip.nestedComment,input,st,en);
    return true;
  }
  for(const auto& [cst,cen] : skip.unnestedComments)
    if(input.hasPrefix(st,cst)) {
      st = skipPastNext(cen,input,st+cst.size(),en);
      // Special handling. Leaving this unconsumed for next() to process.
      if(st != Input::npos && cen.back() == '\n') --st;
      return true;
    }
  return false;
}

size_t Skipper::next(const InputPiece& input, size_t pos) const {
  size_t i = pos;
  const bool bolstart = (pos == input.bol(pos));
  bool lineBlank = bolstart, anyLineBlank = false;
  while(true) {
    if(!input.sizeGt(i)) return i;
    else if(is_in(input[i], " \t")) ++i;
    else if(skipComments(*this, input, i, Input::npos)) {
      if(i == Input::npos) return i;
      lineBlank = (i == input.bol(i));
    }else if(input[i] == '\n') {
      if(lineBlank) anyLineBlank = true;
      if((this->newlines == Skipper::Newlines::ignore_blank && !bolstart) ||
         (this->newlines == Skipper::Newlines::keep_all)) return i;
      lineBlank = true;
      ++i;
    }else {
      // subtracting 1 is guaranteed safe, because the preceding char is '\n'.
      if(anyLineBlank && this->newlines == Skipper::Newlines::keep_para)
        return input.bol(i)-1;
      else return i;
    }
  }
}

bool Skipper::canStart(const InputPiece& input, size_t pos) const {
  if(!input.sizeGt(pos)) return false;
  return is_in(input[pos], " \t\n")
      || skipComments(*this, input, pos, Input::npos);
}

size_t Skipper::whitespace(const InputPiece& input, size_t pos) const {
  while(input.sizeGt(pos) && is_in(input[pos], " \t\n")) ++pos;
  return pos;
}

}  // namespace oalex
