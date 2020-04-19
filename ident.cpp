/*  Copyright 2020 Google LLC

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include"ident.h"
#include<cctype>
using std::string;

namespace oalex {

bool operator==(const Ident& a, const Ident& b) {
  size_t i=0, j=0;
  while(i<a.orig_.size() && j<b.orig_.size()) {
    char ai = a.orig_[i], bj = b.orig_[j];
    if(ai == '_') { ++i; continue; }
    if(bj == '_') { ++j; continue; }
    if(tolower(ai) != tolower(bj)) return false;
    ++i; ++j;
  }
  return i==a.orig_.size() && j==b.orig_.size();
}

const size_t kMaxIdentLen = 100;

Ident parse(InputDiags& ctx, size_t& i) {
  Resetter rst(ctx, i);
  const Input& input = ctx.input;
  bool alluscore = true;
  size_t l;
  for(l=0; input.sizeGt(i+l); ++l) {
    if(l >= kMaxIdentLen) ctx.Fatal(i, i+l, "Identifier too long");
    char ch = input[i+l];
    if(ch!='_' && !isalnum(ch)) break;
    if(isalnum(ch)) alluscore = false;
  }
  Ident rv;
  rv.orig_ = input.substr(i,l);
  size_t o = i;
  rst.markUsed(i+=l);

  if(alluscore)
    return ctx.Error(o, o+l, "Identifier must have a digit or letter");
  if(input[o] == '_' || input[o+l-1] == '_')
    return ctx.Error(o, o+l, "Identifiers with leading or trailing underscores "
                             "are not supported for forward compatibility");
  for(size_t j=o+1; j<o+l; ++j) if(input[j] == '_' && input[j-1] == '_')
    return ctx.Error(j-1, j+1, "Consecutive underscores are not allowed for "
                               "forward compatibility");
  return rv;
}

}  // namespace oalex
