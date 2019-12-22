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

#pragma once
#include<algorithm>
#include<string>
#include<string_view>
#include<utility>
#include<vector>

namespace oalex {

// Note added 22nd Dec 2019: unlike the input_view.* files, this one doesn't
// require an explicit 'forget()' call for buffer cleanup. Instead, it tries to
// automate the process by explicitly keeping track of all the pointer objects
// (i.e. input_view) that are still active.

// class input_buffer and input_view. Implements a reference-counted
// string_view of sorts. Not thread-safe. Calls unexpected() on internal bugs.
//
// Multiple input_view objects can all share various substrings of a single
// input_buffer instance that owns the actual bytes. While parsing, this is
// primarily used to store half-parsed pieces of the input data, before they
// are converted into something more persistent. In typical use, it is expected
// to hold relatively small suffixes of the overall input. The internal buffer
// therefore holds a suffix of the overall input, deallocating unowned prefixes
// as they get released.
//
// The other reason for not using std::string_view is that we want it to stay
// valid even as new characters get appended from the input.
//
// Note that there is no way to obtain an arbitrary input_view from the middle
// of the buffer. It has to start with an acquire_tail call, which can then
// grow with .push_back() calls.
//
// acquire_tail returns an opaque handle to an internal position.  This can be
// used by start_at, release, and acquire_copy.

class input_buffer {
  // For invariants, see input_view_test.cpp:input_buffer_test::valid.
  std::string s_;
  size_t off_=0, minnz_=0, starts_del_=0;
  std::vector<std::pair<size_t,size_t>> starts_;  // offset-count pairs.
  friend class input_view;
  friend class input_buffer_test;

  // Undefined behavior unless this part has been acquired.
  // Used only by input_view::operator std::string_view.
  // Exception: string_view::substr can throw iff st-off_ > s_.size().
  //   Note that st and off_ are unsigned types.
  std::string_view substr(size_t st,size_t len) {
    return std::string_view(s_).substr(st-off_,len);
  }
 public:
  class pos_type { size_t to_int; friend class input_buffer; };

  input_buffer() = default;
  input_buffer(const input_buffer&) = delete;
  void push_back(char x) noexcept;
  pos_type acquire_tail() noexcept;
  void acquire_copy(pos_type pos) noexcept;
  size_t start_at(pos_type pos) const noexcept
    { return starts_.at(pos.to_int-starts_del_).first; }
  void release(pos_type pos) noexcept;

  char at(size_t i) const noexcept { return s_.at(i-off_); }
  size_t end_offset() const noexcept { return off_+s_.size(); }
};

// The only way to construct a non-null object is by using grab_tail.
// Typical lifetime:
//   1. grab_tail
//   2. input_buffer::push_back() grows all unbounded input_view.
//   3. stop_growing or remove_prefix shrinks it.
//   4. Eventually released through reset() or dtor.
// It can get copied or moved around while all this happens. Any
// std::string_view obtained here can get invalidated as soon as
// any deallocation or push_back occurs on this->buf. This includes
// the case of other input_views getting destroyed.
class input_view {
  input_buffer* buf;
  input_buffer::pos_type pos;
  size_t len;
 public:
  input_view() noexcept : buf(nullptr) {}
  input_view(const input_view& that) noexcept { *this=that; }
  input_view(input_view&& that) noexcept { *this=std::move(that); }
  input_view& operator=(const input_view& that) noexcept;
  input_view& operator=(input_view&& that) noexcept;
  ~input_view() { if(buf) buf->release(pos); }
  operator bool() const noexcept { return buf; }
  friend input_view grab_tail(input_buffer& buf) noexcept;

  size_t start() const noexcept { return buf->start_at(pos); }
  size_t size() const noexcept
    { return std::min(len,buf->end_offset()-start()); }
  size_t stop() const noexcept { return start()+size(); }
  void remove_suffix(size_t n) noexcept
    { if(size()>n) len=size()-n; else len=0; }
  bool growing() const noexcept { return len==std::string::npos; }
  void stop_growing() noexcept { len=size(); }
  void reset() noexcept { if(buf) { buf->release(pos); buf=nullptr; } }
  char operator[](size_t i) const noexcept { return buf->at(start()+i); }

  explicit operator std::string_view() const noexcept
    { return buf ? buf->substr(start(),len) : std::string_view(); }
  explicit operator std::string() const noexcept
    { return std::string(std::string_view(*this)); }
};

input_view grab_tail(input_buffer& buf) noexcept;

}  // namespace oalex
