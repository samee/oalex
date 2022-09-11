/*  Copyright 2021 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "parser_helpers.h"
#include <memory>
#include "util.h"
using std::exchange;
using std::make_unique;
using std::string;
using std::string_view;
using std::unique_ptr;

namespace oalex {

JsonLoc errorValue(DiagsDest ctx, ssize_t i, string msg) {
  Error(ctx, i, std::move(msg));
  return JsonLoc::ErrorValue{};
}

static JsonLoc quote(string input, size_t stPos, size_t enPos) {
  JsonLoc rv = std::move(input);
  rv.stPos = stPos; rv.enPos = enPos;
  return rv;
}

JsonLoc match(InputDiags& ctx, ssize_t& i, string_view s) {
  if(!ctx.input.hasPrefix(i, s)) return JsonLoc::ErrorValue{};
  ssize_t oldi = exchange(i, i+s.size());
  return quote(string(s), oldi, i);
}

JsonLoc match(InputDiags& ctx, ssize_t& i, const Regex& regex,
              const RegexOptions& ropts) {
  size_t oldi = i;
  if(consumeGreedily(ctx.input, sign_cast<size_t&>(i), regex, ropts))
    return quote(ctx.input.substr(oldi, i-oldi), oldi, i);
  else return JsonLoc::ErrorValue{};
}

bool peekMatch(InputDiags& ctx, ssize_t i, const RegexCharSet& wordChars,
               string_view s) {
  if(s.empty()) return true;  // Frontend should disallow this.
  if(!ctx.input.hasPrefix(i, s)) return false;
  const ssize_t j = i+s.size();
  if(ctx.input.sizeGt(j) && matchesRegexCharSet(ctx.input[j-1], wordChars)
                         && matchesRegexCharSet(ctx.input[j], wordChars))
    return false;
  else return true;
}

JsonLoc match(InputDiags& ctx, ssize_t& i, const RegexCharSet& wordChars,
              string_view s) {
  if(peekMatch(ctx, i, wordChars, s)) {
    const ssize_t j = i+s.size();
    return quote(string(s), std::exchange(i, j), j);
  }else return JsonLoc::ErrorValue();
}

class InputWrapper : public InputStream {
  const InputPiece* input;
  ssize_t i;
 public:
  InputWrapper(const InputPiece& input, ssize_t i) : input{&input}, i{i} {}
  int16_t getch() override
    { return input->sizeGt(i) ? (*input)[i++] : -1; }
};
// It's not static because we reuse this in codegen.cpp:eval(), but it's
// not in the header file since I have no intention of supporting this hack in
// the long run.
// This proxy object both discards diags and defends against
// overly enthusiastic forgetBefore().
unique_ptr<InputStream> substrProxy(const InputPiece& input, ssize_t i) {
  return make_unique<InputWrapper>(input, i);
}

bool peekMatch(const InputPiece& input, ssize_t i, GeneratedParser parser) {
  // The only difference between this and quietMatch() is that
  // peekMatch() accepts `i` by value.
  return !quietMatch(input, i, parser).holdsErrorValue();
}

JsonLoc quietMatch(const InputPiece& input, ssize_t& i,
                   GeneratedParser parser) {
  /* TODO codegen proper resemblance checkers.
     TODO while this is here, add a flag to InputDiags to disable errors
     with an RAII-controlled flag. Although, I'd have to then think about how
     to do this whiel preserving the type signature to accept an Input type
     and not InputDiags or DiagsDest. Speaking of DiagsDest, that should then
     have to support this too, or we should at least forbid DiagsDest
     construction in this state.

  <rant>
  Instead, we are now abusing normal parsers as resemblance checkers,
  using hacks to discard their diagnostics. This is especially egregious
  when one notices that we are needlessly keeping copies of inputs here,
  in a function that is called *more* frequently than normal parsers. At least,
  we should be able to make Input a more lightweight type that doesn't need to
  make input copies.

  Human-written parsers don't usually have such adapter functions.
  We shouldn't have them either.
  </rant>
  */
  // Offset from input.bol(i) instead of i to help ignore_blank properly
  // detect a non-blank line.
  unique_ptr<InputStream> sp = substrProxy(input, input.bol(i));
  InputDiags proxy{Input{sp.get()}};
  ssize_t pos = i-input.bol(i);
  JsonLoc rv = parser(proxy, pos);
  if(!rv.holdsErrorValue()) i = input.bol(i) + pos;
  return rv;
}

void mapAppend(JsonLoc::Map& m1, JsonLoc::Map m2) {
  m1.insert(m1.end(), std::make_move_iterator(m2.begin()),
                      std::make_move_iterator(m2.end()));
}

void mapCreateOrAppend(JsonLoc::Map& m, const std::string& k,
                       JsonLoc v, bool doCreate) {
  if(doCreate) {
    for(auto& [oldk,oldv] : m) if(oldk == k)
      Bug("Collision in loop elements with name '{}'", k);
    m.push_back({k, JsonLoc::Vector{std::move(v)}});
  }else {
    ssize_t i=0;
    for(i=0; i<ssize(m); ++i) if(m[i].first == k) break;
    if(i==ssize(m)) Bug("Need to create element after the first iteration");
    m[i].second.getIfVector()->push_back(std::move(v));
  }
}

void mapCreateOrAppendAllElts(JsonLoc::Map& m1, JsonLoc::Map m2,
                              bool doCreate) {
  for(auto& [k,v] : m2) mapCreateOrAppend(m1, k, std::move(v), doCreate);
}

void assertMap(JsonLoc& jsloc, string_view errctx) {
  auto* m = jsloc.getIfMap();
  if(m == nullptr) Bug("{}: needs a map", errctx);
}

void assertNotNull(void* p, string_view fname, string_view errmsg) {
  if(!p) Bug("{}: {}", fname, errmsg);
}

}  // namespace oalex
