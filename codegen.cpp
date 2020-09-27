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
using std::get_if;
using std::string;

namespace oalex {

static const Skipper& skipper(const RuleSet& ruleset, ssize_t ruleIndex) {
  if(auto* cr = get_if<ConcatRule>(&ruleset.rules[ruleIndex])) {
    return cr->skip ? *cr->skip : ruleset.skip;
  }else return ruleset.skip;
}

static bool skip(InputDiags& ctx, ssize_t& i,
                 const SkipPoint& sp) {
  const Input& input = ctx.input;
  const ssize_t oldi = i;
  i = sp.stayWithinLine ? sp.skip->withinLine(input, i)
                        : sp.skip->acrossLines(input, i);
  if(i == ssize_t(string::npos)) {
    ssize_t com = oldi;
    // com = skipToComment(ctx.input, oldi); // TODO
    Error(ctx, com, "Unfinished comment");
  }
  return i != ssize_t(string::npos);
}

// TODO move to runtime/oalex_runtime.h
JsonLoc skipAndMatch(InputDiags& ctx, ssize_t& i,
                     const Skipper& sk, const string& s) {
  if(!skip(ctx, i, SkipPoint{false, &sk})) return {};
  if(!ctx.input.hasPrefix(i, s)) return {};
  JsonLoc rv = s;
  rv.stPos = i;
  i = rv.enPos = i+s.size();
  return rv;
}

JsonLoc eval(InputDiags& ctx, ssize_t& i,
             const RuleSet& ruleset, ssize_t ruleIndex) {
  const Rule& r = ruleset.rules[ruleIndex];
  if(const string* s = get_if<string>(&r)) {
    return skipAndMatch(ctx, i, skipper(ruleset, ruleIndex), *s);
  }
  Unimplemented("eval() for rule {}", r.index());
}

}  // namespace oalex
