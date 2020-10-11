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
#include <type_traits>
#include <utility>
#include "util.h"
using oalex::Regex;
using oalex::RegexOptions;
using std::exchange;
using std::get_if;
using std::string;

namespace oalex {

static JsonLoc skip(InputDiags& ctx, ssize_t& i,
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
    return JsonLoc::ErrorValue{};
  } else return JsonLoc::String();  // Just something non-error.
}

static JsonLoc quote(string input, size_t stPos, size_t enPos) {
  JsonLoc rv = std::move(input);
  rv.stPos = stPos; rv.enPos = enPos;
  return rv;
}

template <class T>
class ReverseSigned {
  using unref = std::remove_reference_t<T>;
 public:
  static_assert(std::is_lvalue_reference_v<T> && std::is_integral_v<unref>,
                "sign_cast only works on integer references");
  using type = std::conditional_t<std::is_signed_v<unref>,
                                  std::make_unsigned_t<unref>,
                                  std::make_signed_t<unref>>;
};

template <class T> T sign_cast(typename ReverseSigned<T>::type& ref) {
  return reinterpret_cast<T>(ref);
}

// TODO move all overloads to runtime/oalex_runtime.h
JsonLoc match(InputDiags& ctx, ssize_t& i, const string& s) {
  if(!ctx.input.hasPrefix(i, s)) return JsonLoc::ErrorValue{};
  return quote(s, exchange(i, i+s.size()), s.size());
}

JsonLoc match(InputDiags& ctx, ssize_t& i,
              const Regex& regex, const RegexOptions& ropts) {
  size_t oldi = i;
  if(consumeGreedily(ctx.input, sign_cast<size_t&>(i), regex, ropts))
    return quote(ctx.input.substr(oldi, i-oldi), oldi, i);
  else return JsonLoc::ErrorValue{};
}

JsonLoc match(InputDiags& ctx, ssize_t& i,
              const ConcatRule& seq, const RuleSet& rs) {
  JsonLoc rv = seq.outputTmpl;
  JsonLoc::PlaceholderMap pmap = rv.allPlaceholders();
  ssize_t j = i;
  for(auto& [idx, outname] : seq.comps) {
    // TODO move this into substitute in the common case.
    JsonLoc out = eval(ctx, j, rs, idx);
    if(out.holdsError()) return out;
    if(!outname.empty()) rv.substitute(pmap, outname, out);
  }
  i = j;
  return rv;
}

JsonLoc eval(InputDiags& ctx, ssize_t& i,
             const RuleSet& ruleset, ssize_t ruleIndex) {
  const Rule& r = ruleset.rules[ruleIndex];
  if(const string* s = get_if<string>(&r)) return match(ctx, i, *s);
  else if(const auto* sp = get_if<SkipPoint>(&r)) return skip(ctx, i, *sp);
  else if(const auto* regex = get_if<Regex>(&r))
    return match(ctx, i, *regex, ruleset.regexOpts);
  else if(const auto* seq = get_if<ConcatRule>(&r))
    return match(ctx,i, *seq, ruleset);
  Unimplemented("eval() for rule {}", r.index());
}

}  // namespace oalex
