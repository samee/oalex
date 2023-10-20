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
using std::nullopt;
using std::optional;
using std::string;
using std::string_view;
using std::unique_ptr;

namespace oalex {

std::nullopt_t errorValue(DiagsDest ctx, ssize_t i, string msg) {
  Error(ctx, i, std::move(msg));
  return nullopt;
}

optional<StringLoc> match(InputDiags& ctx, ssize_t& i, string_view s) {
  if(!ctx.input().hasPrefix(i, s)) return nullopt;
  size_t oldi = i;
  i += s.size();
  return StringLoc{string(s), oldi};
}

optional<StringLoc> match(InputDiags& ctx, ssize_t& i, const Regex& regex,
                          const RegexOptions& ropts) {
  size_t oldi = i;
  if(consumeGreedily(ctx.input(), sign_cast<size_t&>(i), regex, ropts))
    return StringLoc{ctx.input().substr(oldi, i-oldi), oldi};
  else {
    i = oldi;
    return nullopt;
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

optional<StringLoc>
match(InputDiags& ctx, ssize_t& i, const RegexCharSet& wordChars,
      string_view s) {
  if(peekMatch(ctx, i, wordChars, s)) {
    size_t oldi = i; i += s.size();
    return StringLoc{string(s), oldi};
  }else return nullopt;
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

}  // namespace oalex
