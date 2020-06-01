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

#include "input_view_auto_forget.h"
#include "runtime/util.h"
using namespace std;

namespace oalex {

// ----------------------- input_buffer --------------------------------

void input_buffer::push_back(char x) noexcept {
  if(minnz_>=starts_.size()) ++off_;
  else s_.push_back(x);
}

input_buffer::pos_type input_buffer::acquire_tail() noexcept {
  size_t st=end_offset();
  if(starts_.empty()||starts_.back().first!=st) starts_.push_back({st,1});
  else starts_.back().second++;
  pos_type pos;
  pos.to_int=starts_.size()-1+starts_del_;
  return pos;
}

// These BugDie() calls are for catching potential problems in input_view.

// Increments the reference count at st if one already exists.
void input_buffer::acquire_copy(pos_type pos) noexcept {
  size_t i=pos.to_int;
  if(i<starts_del_)
    BugDieFmt("Trying to copy from deleted portions: {} < {}", i, starts_del_);
  i-=starts_del_;
  if(i>=starts_.size())
    BugDieFmt("Out of range in acquire_copy_at: {} >= {}", i, starts_.size());
  if(!starts_[i].second)
    BugDieFmt("Nothing to copy from at {}", i);
  starts_[i].second++;
}

void input_buffer::release(pos_type pos) noexcept {
  size_t i=pos.to_int;
  if(i<starts_del_)
    BugDieFmt("Trying to release already released portion. {} < {}",
              i, starts_del_);
  i-=starts_del_;
  if(i>=starts_.size())
    BugDieFmt("Out of range in release_t: {} >= {}", i, starts_.size());
  if(starts_[i].second==0)
    BugDieFmt("Nothing to release at {}", i);
  starts_[i].second--;

  // See if we can release memory
  if(starts_[i].second>0u || i>minnz_) return;

  // How much memory?
  for(;minnz_<starts_.size();++minnz_) if(starts_[minnz_].second) break;
  size_t doff=s_.size();
  if(minnz_<starts_.size()) {
    if(starts_[minnz_].first<off_)
      BugDieFmt("This part of starts_ should have been released "
                "according to off_");
    doff=starts_[minnz_].first-off_;
  }

  // Is it even worth releasing memory?
  if(s_.size()<32 || doff<s_.size()/2) return;

  // Make it so.
  s_.erase(0,doff);  // strong exception safety.
  starts_.erase(starts_.begin(),starts_.begin()+minnz_);  // same here.
  starts_del_+=minnz_;
  minnz_=0;
  off_+=doff;
}

// ----------------------- input_view ----------------------------------

input_view& input_view::operator=(const input_view& that) noexcept {
  if(!that) { buf=nullptr; return *this; }
  that.buf->acquire_copy(that.pos);
  buf=that.buf;
  pos=that.pos;
  len=that.len;
  return *this;
}

input_view& input_view::operator=(input_view&& that) noexcept {
  buf=that.buf;
  pos=that.pos;
  len=that.len;
  that.buf=nullptr;
  return *this;
}

// Returns an input_view that keeps growing with buf.push_back(), by setting
// the end position to be string::npos. Its growth can later be stunted with
// remove_suffix() or stop_growing().
input_view grab_tail(input_buffer& buf) noexcept {
  input_view rv;
  rv.buf=&buf;
  rv.pos=buf.acquire_tail();
  rv.len=string::npos;
  return rv;
}

}  // namespace oalex
