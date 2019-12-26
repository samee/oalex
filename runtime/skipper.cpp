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
#include "util.h"
using std::pair;
using std::string;
using std::string_view;
using namespace std::literals::string_view_literals;

namespace oalex {

static size_t skipEnd(const Input& input, size_t pos, bool endsBeforeNextLine) {
  size_t end = endsBeforeNextLine ? input.find('\n',pos) : string::npos;
  if(end != string::npos) ++end;  // Careful not to overflow npos.
  return end;
}

// Assumes ctx.input.hasPrefix(pos, end-pos, delims.first).
static size_t skipPastNestedComment(
    const pair<string,string>& delims,
    InputDiags& ctx, size_t pos, size_t end) {
  const Input& input = ctx.input;
  size_t depth=0, i=pos;
  const auto& [stdelim,endelim] = delims;

  if(stdelim.empty() || endelim.empty())
    Bug()<<"Nested comment delimiters shouldn't be empty. '"
         <<stdelim<<"', '"<<endelim<<"'";

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
  ctx.Error(pos, pos+stdelim.size(), "Comment never ends");
  return string::npos;
}

static size_t
skipPastNext(const string& s, const Input& input, size_t pos, size_t end) {
  for(size_t i=pos; input.sizeGt(i) && i+s.size()<=end; ++i)
    if(input.hasPrefix(i,s)) return i+s.size();
  return string::npos;
}

static bool isin(char ch,string_view s) {
  return s.find(ch) != string_view::npos;
}

size_t Skipper::withinLine(InputDiags& ctx, size_t pos) const {
  const Input& input = ctx.input;
  const size_t end = skipEnd(input,pos,true);
  size_t i = pos;
  while(true) {
  keepSkipping:
    // Check if we still have room to skip.
    if(!input.sizeGt(i) || i>=end) return string::npos;
    else if(isin(input[i]," \t")) ++i;
    else if(nestedComment && input.hasPrefix(i,end-i,nestedComment->first))
      i = skipPastNestedComment(*nestedComment,ctx,i,end);
    else {
      for(const auto& [st,en] : unnestedComments) if(input.hasPrefix(i,st)) {
        size_t i2 = skipPastNext(en,input,i+st.size(),end);
        if(i2==string::npos) ctx.Error(i,i+st.size(),"Comment never ends");
        i = i2;
        goto keepSkipping;
      }
      return i;
    }
  }
}

}  // namespace oalex
