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
#include "jsonloc_fmt.h"
#include <iterator>
#include <string_view>
#include "util.h"
using std::back_insert_iterator;
using std::string_view;

namespace oalex {

void printJsonLocString(fmt::memory_buffer& buf, string_view s) {
  back_insert_iterator buf_app{buf};
  format_to(buf_app, "\"");
  // If this changes, please change lexQuotedEscape as well.
  // TODO write test.
  for(char ch : s) {
    if(ch=='"') format_to(buf_app, "\\\"");
    else if(ch=='\\') format_to(buf_app, "\\\\");
    else if(ch=='\n') format_to(buf_app, "\\n");
    else if(ch=='\t') format_to(buf_app, "\\t");
    else if(isprint(ch))
      format_to(buf_app, "{}", ch);  // check this after '"' and '\\'.
    else format_to(buf_app, "\\x{:02x}", ch);
  }
  format_to(buf_app, "\"");
}

string_view assertJsonLocKey(string_view ctx, string_view s) {
  if(s.empty()) Bug("{}: Identifier can't be null.", ctx);
  if(isdigit(s[0])) Bug("{}: Identifier can't start with a digit.", ctx);
  for(size_t i=0; i<s.size(); ++i) if(s[i]!='_' && !isalnum(s[i]))
    Bug("{}: Invalid identifier character at position {}.", ctx, i);
  return s;
}

}  // namespace oalex
