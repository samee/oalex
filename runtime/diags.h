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
#include "input_view.h"

namespace oalex {

struct Diag {
  enum Severity { error,warning,note } severity;
  size_t stLine, stPos, enLine, enPos;  // These ranges are inclusive.
  std::string msg;
  Diag(const Input& input, size_t st, size_t en, Severity sev, std::string msg)
    : severity(sev), msg(msg) {
    std::tie(stLine,stPos) = input.rowCol(st);
    std::tie(enLine,enPos) = input.rowCol(--en);
  }
  explicit operator std::string() const;
};

struct InputDiags {
  Input input;
  std::vector<Diag> diags;

  InputDiags(Input input, std::vector<Diag> diags)
    : input(std::move(input)), diags(std::move(diags)) {}
  InputDiags(InputDiags&&) = default;
  InputDiags& operator=(InputDiags&&) = default;
  void markUsed(size_t st, size_t en);

  // throws, never returns.
  [[noreturn]] void FatalBug(size_t st,size_t en,std::string msg) const;
  [[noreturn]] void Fatal(size_t st,size_t en,std::string msg) const;

  std::nullopt_t Error(size_t st,size_t en,std::string msg);
  std::nullopt_t Warning(size_t st,size_t en,std::string msg);
  std::nullopt_t Note(size_t st,size_t en,std::string msg);

  [[noreturn]] void FatalBug(size_t pos,std::string msg) const
    { FatalBug(pos, pos+1, std::move(msg)); }
  [[noreturn]] void Fatal(size_t pos,std::string msg) const
    { Fatal(pos, pos+1, std::move(msg)); }
  std::nullopt_t Error(size_t pos,std::string msg)
    { return Error(pos, pos+1, std::move(msg)); }
  std::nullopt_t Warning(size_t pos,std::string msg)
    { return Warning(pos, pos+1, std::move(msg)); }
  std::nullopt_t Note(size_t pos,std::string msg)
    { return Note(pos, pos+1, std::move(msg)); }

 private:
  size_t lastForgotten_ = 0;
};

// Helper for Input::forgetBefore().
// We almost never want to *unconditionally* call forgetBefore().
// TODO remove all explicit forgetBefore() calls.
inline void InputDiags::markUsed(size_t st, size_t en) {
  if(st <= lastForgotten_) {
    input.forgetBefore(en);
    lastForgotten_ = en;
  }
}

class Resetter {
  size_t oldi_, *targeti_;
  InputDiags *ctx_;
 public:
  explicit Resetter(InputDiags& ctx, size_t& i)
    : oldi_(i), targeti_(&i), ctx_(&ctx) {}
  ~Resetter() { if(targeti_) *targeti_ = oldi_; }
  void markUsed(size_t en) { ctx_->markUsed(oldi_, en); targeti_ = nullptr; }
};

}  // namespace oalex
