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

JsonLoc::ErrorValue errorValue(DiagsDest ctx, ssize_t i, string msg) {
  Error(ctx, i, std::move(msg));
  return JsonLoc::ErrorValue{};
}

static JsonLoc quote(string input, size_t stPos, size_t enPos) {
  JsonLoc rv = std::move(input);
  rv.stPos = stPos; rv.enPos = enPos;
  return rv;
}

JsonLoc match(InputDiags& ctx, ssize_t& i, string_view s) {
  if(!ctx.input().hasPrefix(i, s)) return JsonLoc::ErrorValue{};
  ssize_t oldi = exchange(i, i+s.size());
  return quote(string(s), oldi, i);
}

JsonLoc match(InputDiags& ctx, ssize_t& i, const Regex& regex,
              const RegexOptions& ropts) {
  size_t oldi = i;
  if(consumeGreedily(ctx.input(), sign_cast<size_t&>(i), regex, ropts))
    return quote(ctx.input().substr(oldi, i-oldi), oldi, i);
  else {
    i = oldi;
    return JsonLoc::ErrorValue{};
  }
}

bool peekMatch(InputDiags& ctx, ssize_t i, const RegexCharSet& wordChars,
               string_view s) {
  if(s.empty()) return true;  // Frontend should disallow this.
  if(!ctx.input().hasPrefix(i, s)) return false;
  const ssize_t j = i+s.size();
  if(ctx.input().sizeGt(j) && matchesRegexCharSet(ctx.input()[j-1], wordChars)
                          && matchesRegexCharSet(ctx.input()[j], wordChars))
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

// InputDiags owns its InputPiece parameter. This class allows us to
// construct InputDiags that don't own the actual input object.
class InputUnowned final : public InputPiece {
  const InputPiece* input_;
 public:
  explicit InputUnowned(const InputPiece& input) : input_{&input} {}

  char operator[](size_t i) const override { return (*input_)[i]; }
  bool sizeGt(size_t sz) const override { return input_->sizeGt(sz); }
  std::pair<size_t,size_t> rowCol(size_t i) const override
    { return input_->rowCol(i); }
  bool hasPrefix(size_t pos, std::string_view s) const override
    { return input_->hasPrefix(pos, s); }
  size_t find(char ch, size_t pos) const override
    { return input_->find(ch, pos); }
  size_t bol(size_t i) const override { return input_->bol(i); }
  std::string substr(size_t st,  size_t len) const override
    { return input_->substr(st, len); }
};

unique_ptr<InputPiece> unowned(const InputPiece& input) {
  return make_unique<InputUnowned>(input);
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
