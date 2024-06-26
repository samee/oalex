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

#include "input_view.h"
#include "util.h"
using std::make_pair;
using std::pair;
using std::string;
using std::string_view;
using std::vector;

namespace oalex {

// `last` will be a valid index after this, unless it is past the end of input.
// Assumes vector<int>::push_back doesn't throw.
void Input::peekTo(size_t last) const {
  while(!endSeen() && last>=start_pos_+buf_.size()) {
    size_t pos = start_pos_+buf_.size();
    int16_t ch = stream_->getch();
    if(ch < 0) {
      size_ = pos;  // endSeen() is now true.
      return;
    }
    if(ch == '\n') newlines_.push_back(pos);
    else {
      size_t lastBol = newlines_.empty() ? 0 : newlines_.back()+1;
      if(pos+1-lastBol > maxLineLength)
        UserError("Line {} is too long", newlines_.size()+1);
    }
    buf_ += ch;
  }
}

// After this, operator[](x) may no longer be valid for x < begin.
void Input::forgetBefore(const size_t pos) {
  peekTo(pos);
  if(endSeen()) return;  // Choosing not to forget.

  // Check if we've already forgotten this part.
  if(pos <= start_pos_) return;

  const size_t rmlen = pos-start_pos_;
  if(2*rmlen < buf_.size()) return;  // Too little gain for too much work.

  // Actually forget things.
  buf_.erase(buf_.begin(), buf_.begin()+rmlen);
  start_pos_ = pos;
}

[[noreturn]] static void bugLostPos(size_t pos) {
  Bug("Out of bounds, already forgoten {}.", pos);
}

// TODO I no longer remember when to use this function, vs directly comparing
// only i < start_pos_. Figure out a a sane logic.
void Input::peekAndBoundCharAt(size_t i) const {
  if(i < start_pos_) bugLostPos(i);
  peekTo(i+1);
  if(i-start_pos_ < buf_.size()) return;
  if(!endSeen()) Bug("peekTo() returned unexpectedly early.");
  Bug("Out of bound error. {} is beyond the end of the input.", i);
}

char Input::operator[](size_t i) const {
  peekAndBoundCharAt(i);
  return buf_[i-start_pos_];
}

string Input::substr(size_t pos, size_t count) const {
  if(pos < start_pos_) bugLostPos(pos);
  peekTo(pos+count);
  return buf_.substr(pos-start_pos_, count);
}

bool Input::hasPrefix(size_t pos, string_view s) const {
  if(pos < start_pos_) bugLostPos(pos);
  peekTo(pos+s.size());
  return buf_.compare(pos-start_pos_, s.size(), s) == 0;
}

size_t Input::find(char ch, size_t pos) const {
  if(pos < start_pos_) bugLostPos(pos);
  for(size_t i=pos; sizeGt(i); ++i) if((*this)[i]==ch) return i;
  return npos;
}

size_t Input::bol(size_t i) const {
  peekTo(i+1);
  if(newlines_.empty() || i<=newlines_[0]) return 0;
  size_t prev_newline = *--lower_bound(newlines_.begin(), newlines_.end(), i);
  return prev_newline+1;
}

pair<size_t,size_t> Input::rowCol(size_t i) const {
  peekTo(i+1);
  if(newlines_.empty() || i<=newlines_[0]) return make_pair(1, 1+i);
  size_t prev = lower_bound(newlines_.begin(), newlines_.end(), i)
                - newlines_.begin() - 1;
  return make_pair(prev+2, i-newlines_[prev]);
}

static vector<size_t>
allNewlines(string_view s) {
  vector<size_t> rv;
  for(size_t i=0; i<s.size(); ++i) if(s[i] == '\n') rv.push_back(i);
  return rv;
}

string debugPrefix(const InputPiece& input, size_t i) {
  constexpr size_t maxsz = 13;
  return input.sizeGt(i+maxsz) ? input.substr(i, maxsz-3) + "..."
                               : input.substr(i, maxsz);
}

Input::Input(string_view s)
    : buf_(string(s)), size_(buf_.size()), newlines_(allNewlines(buf_)) {}

}  // namespace oalex
