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

#include "codegen.h"
#include "util.h"
using std::get_if;
using std::string;

namespace oalex {

static bool skip(InputDiags& ctx, ssize_t& i,
                 const SkipPoint& sp) {
  const Input& input = ctx.input;
  const ssize_t oldi = i;
  i = sp.stayWithinLine ? sp.skip->withinLine(input, i)
                        : sp.skip->acrossLines(input, i);
  if(i == ssize_t(string::npos)) {
    ssize_t com = oldi;
    while(ctx.input.sizeGt(com) && is_in(ctx.input[com], " \n\t")) ++com;
    if(!ctx.input.sizeGt(com)) Bug("skipper returned npos without a comment");
    Error(ctx, com, "Unfinished comment");
  }
  return i != ssize_t(string::npos);
}

// TODO move to runtime/oalex_runtime.h
JsonLoc match(InputDiags& ctx, ssize_t& i, const string& s) {
  if(!ctx.input.hasPrefix(i, s)) return {};
  // TODO figure out error reporting, based on whether match is tentative.
  JsonLoc rv = s;
  rv.stPos = i;
  i = rv.enPos = i+s.size();
  return rv;
}

JsonLoc eval(InputDiags& ctx, ssize_t& i,
             const RuleSet& ruleset, ssize_t ruleIndex) {
  const Rule& r = ruleset.rules[ruleIndex];
  if(const string* s = get_if<string>(&r)) return match(ctx, i, *s);
  else if(const auto* sp = get_if<SkipPoint>(&r)) {
    skip(ctx, i, *sp);
    return {};
  }
  Unimplemented("eval() for rule {}", r.index());
}

}  // namespace oalex
