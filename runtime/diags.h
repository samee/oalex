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
#include "util.h"

namespace oalex {

struct Diag {
  enum Severity { error,warning,note } severity;
  size_t stLine, stPos, enLine, enPos;  // These ranges are inclusive.
  std::string msg;
  Diag(const InputPiece& input, size_t st, size_t en,
       Severity sev, std::string msg)
    : severity(sev), msg(msg) {
    std::tie(stLine,stPos) = input.rowCol(st);
    std::tie(enLine,enPos) = input.rowCol(--en);
  }
  explicit operator std::string() const;
};

struct InputDiagsRef {
  const InputPiece* input;
  std::vector<Diag>* diags;
};

std::string locationString(const InputPiece& input, size_t st, size_t en);

// Typically, we don't expect this to be called directly. This is merely a
// helper for the more convenient Error(), Warning(), and Note().
inline std::nullopt_t
pushDiagReturnNullOpt(InputDiagsRef ctx, size_t st, size_t en,
                      Diag::Severity sev, std::string msg) {
  ctx.diags->emplace_back(*ctx.input, st, en, sev, std::move(msg));
  return std::nullopt;
}

[[noreturn]] inline void
FatalBug(InputDiagsRef ctx, size_t st, size_t en, std::string msg) {
  Bug("{}", std::string(Diag(*ctx.input, st, en, Diag::error, std::move(msg))));
}

[[noreturn]] inline void
Fatal(InputDiagsRef ctx, size_t st, size_t en, std::string msg) {
  UserError("{}", std::string(Diag(*ctx.input, st, en,
                              Diag::error, std::move(msg))));
}

inline std::nullopt_t
Error(InputDiagsRef ctx, size_t st, size_t en, std::string msg) {
  return pushDiagReturnNullOpt(ctx, st, en, Diag::error, std::move(msg));
}

inline std::nullopt_t
Warning(InputDiagsRef ctx, size_t st, size_t en, std::string msg) {
  return pushDiagReturnNullOpt(ctx, st, en, Diag::warning, std::move(msg));
}

inline std::nullopt_t
Note(InputDiagsRef ctx, size_t st, size_t en, std::string msg) {
  return pushDiagReturnNullOpt(ctx, st, en, Diag::note, std::move(msg));
}

[[noreturn]] inline void
FatalBug(InputDiagsRef ctx, size_t st, std::string msg) {
  FatalBug(ctx, st, st+1, std::move(msg));
}

[[noreturn]] inline void
Fatal(InputDiagsRef ctx, size_t st, std::string msg) {
  Fatal(ctx, st, st+1, std::move(msg));
}

inline std::nullopt_t
Error(InputDiagsRef ctx, size_t st, std::string msg) {
  return pushDiagReturnNullOpt(ctx, st, st+1, Diag::error, std::move(msg));
}

inline std::nullopt_t
Warning(InputDiagsRef ctx, size_t st, std::string msg) {
  return pushDiagReturnNullOpt(ctx, st, st+1, Diag::warning, std::move(msg));
}

inline std::nullopt_t
Note(InputDiagsRef ctx, size_t st, std::string msg) {
  return pushDiagReturnNullOpt(ctx, st, st+1, Diag::note, std::move(msg));
}

struct InputDiags {
  Input input;
  std::vector<Diag> diags;

  explicit InputDiags(Input input) : input(std::move(input)) {}
  InputDiags(InputDiags&&) = default;
  InputDiags& operator=(InputDiags&& that) = default;
  InputDiags(const InputDiags&) = delete;
  InputDiags& operator=(const InputDiags&) = delete;

  void markUsed(size_t st, size_t en);

  operator InputDiagsRef() { return {&input, &diags}; }  // implicit, non-const
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
  size_t oldi_, chkpoint_, *targeti_;
  InputDiags *ctx_;
 public:
  explicit Resetter(InputDiags& ctx, size_t& i)
    : oldi_(i), chkpoint_(i), targeti_(&i), ctx_(&ctx) {}
  ~Resetter() { if(targeti_) *targeti_ = chkpoint_; }
  void markUsed(size_t en) { ctx_->markUsed(chkpoint_, en); chkpoint_ = en; }
  size_t start() const { return oldi_; }
};

}  // namespace oalex
