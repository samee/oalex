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

#include "skipper.h"
#include <string_view>
#include <utility>
#include <vector>
#include "util.h"
using fmt::format;
using std::pair;
using std::nullopt;
using std::optional;
using std::string;
using std::string_view;
using std::vector;
using namespace std::literals::string_view_literals;

namespace oalex {

static bool isin(char ch,string_view s) {
  return s.find(ch) != string_view::npos;
}

// To be replaced with C++20 std::string_view::starts_with.
static bool starts_with(string_view a, string_view b) {
  return a.substr(0,b.size()) == b;
}

static optional<string> validPair(string_view st, string_view en) {
    if(st.empty() || en.empty()) return "Comment delimiters cannot be empty";
    if(isin(st[0], " \t\n"))
      return "Whitespace at the start of comments will be skipped over";
    if(st.find("\n") != string_view::npos ||
       en.substr(0,en.size()-1).find("\n") != string_view::npos)
      return "skip.withinLine() works poorly with delimiters requiring newline";
    return nullopt;
}

optional<string> Skipper::valid() const {
  for(const auto& [st,en] : unnestedComments)
    if(auto err = validPair(st,en)) return err;
  if(nestedComment.has_value()) {
    if(auto err = validPair(nestedComment->first, nestedComment->second))
      return err;
    if(starts_with(nestedComment->first, nestedComment->second) ||
       starts_with(nestedComment->second, nestedComment->first))
      return "Nested comment delimiter pair can be confused for each other";
  }

  // Check prefix-freeness.
  vector<string> starts;
  for(const auto& [st,en] : unnestedComments) starts.push_back(st);
  if(nestedComment.has_value()) starts.push_back(nestedComment->first);
  for(size_t i=0; i<starts.size(); ++i)
    for(size_t j=0; j<starts.size(); ++j)
      if(i != j && starts_with(starts[i], starts[j]))
        return format("Comment delimiters cannot be prefixes of each other: "
                      "{}, {}", starts[i], starts[j]);
  return nullopt;
}

static size_t skipEnd(const Input& input, size_t pos, bool endsBeforeNextLine) {
  size_t end = endsBeforeNextLine ? input.find('\n',pos) : Input::npos;
  if(end != Input::npos) ++end;  // Careful not to overflow npos.
  return end;
}

// Assumes ctx.input.hasPrefix(pos, end-pos, delims.first).
// Returns npos if comment never ends.
// Returns the exact eof position if comment ends exactly at eof.
static size_t skipPastNestedComment(
    const pair<string,string>& delims,
    const Input& input, size_t pos, size_t end) {
  size_t depth=0, i=pos;
  const auto& [stdelim,endelim] = delims;

  if(stdelim.empty() || endelim.empty())
    BugFmt("Nested comment delimiters shouldn't be empty. '{}', '{}'",
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
skipPastNext(const string& s, const Input& input, size_t pos, size_t end) {
  for(size_t i=pos; input.sizeGt(i) && i+s.size()<=end; ++i)
    if(input.hasPrefix(i,s)) return i+s.size();
  return Input::npos;
}

// Returns true if something was skipped. Modifies st to past the end of it.
// If we skipped all the way to en and didn't find the end of a comment,
// returns true but sets st to Input::npos.
// For all other values at input[st], returns false and leaves st unchanged.
// Corollary: st is always incremented by some amount if return value is true.
static bool skipComments(const Skipper& skip, const Input& input,
    size_t& st, size_t en) {
  if(skip.nestedComment &&
     input.hasPrefix(st,st-en,skip.nestedComment->first)) {
    st = skipPastNestedComment(*skip.nestedComment,input,st,en);
    return true;
  }
  for(const auto& [cst,cen] : skip.unnestedComments)
    if(input.hasPrefix(st,cst)) {
      st = skipPastNext(cen,input,st+cst.size(),en);
      return true;
    }
  return false;
}

size_t Skipper::withinLine(InputDiags& ctx, size_t pos) const {
  const Input& input = ctx.input;
  const size_t end = skipEnd(input,pos,true);
  size_t i = pos;
  while(true) {
    // Check if we still have room to skip.
    if(!input.sizeGt(i) || i>=end) return end;
    else if(isin(input[i], " \t\n")) ++i;
    else if(skipComments(*this, input, i, end)) {
      if(i == Input::npos) Error(ctx, i, i+1, "Comment never ends");
    }else return i;
  }
}

size_t Skipper::acrossLines(InputDiags& ctx, size_t pos) const {
  Input& input = ctx.input;
  size_t i = pos;
  bool lineBlank = (pos == input.bol(pos)), anyLineBlank = false;
  while(true) {
    if(!input.sizeGt(i)) return Input::npos;
    else if(isin(input[i], " \t")) ++i;
    else if(skipComments(*this, input, i, Input::npos)) {
      if(i == Input::npos) Error(ctx, i, i+1, "Comment never ends");
      lineBlank = false;
    }else if(input[i] == '\n') {
      if(lineBlank) anyLineBlank = true;
      lineBlank = true;
      input.forgetBefore(i);
      ++i;
    }else {
      // subtracting 1 is guaranteed safe, because the preceding char is '\n'.
      if(anyLineBlank && this->indicateBlankLines) return input.bol(i)-1;
      else return i;
    }
  }
}

}  // namespace oalex
