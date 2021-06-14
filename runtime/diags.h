/*  Copyright 2019-2020 The oalex authors.

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
#include <optional>
#include "input_view.h"
#include "util.h"

namespace oalex {

struct Diag {
  enum Severity { error,warning,note } severity;
  size_t stLine, stPos, enLine, enPos;  // These ranges are inclusive.
  std::string msg;
  Diag(const Input& input, size_t st, size_t en,
       Severity sev, std::string msg)
    : severity(sev), msg(msg) {
    std::tie(stLine,stPos) = input.rowCol(st);
    std::tie(enLine,enPos) = input.rowCol(--en);
  }
  explicit operator std::string() const;
};

inline bool hasError(const std::vector<Diag>& diags) {
  for(const auto& d : diags) if(d.severity == Diag::error) return true;
  return false;
}

class DiagsDest {
  const Input* input_;
  std::vector<Diag>* diags_;
 public:
  DiagsDest(struct InputDiags& ctx);  // implicit

  // Typically, we don't expect this to be called directly. This is merely a
  // helper for the more convenient Error(), Warning(), and Note().
  std::nullopt_t pushDiagReturnNullOpt(size_t st, size_t en,
                                       Diag::Severity sev, std::string msg) {
    diags_->emplace_back(*input_, st, en, sev, std::move(msg));
    return std::nullopt;
  }
  Diag makeDiag(size_t st, size_t en,
                Diag::Severity sev, std::string msg) const {
    return Diag(*input_, st, en, sev, std::move(msg));
  }
  std::string locationString(size_t st, size_t en) const;
};

[[noreturn]] inline void
FatalBug(DiagsDest ctx, size_t st, size_t en, std::string msg) {
  Bug("{}", std::string(ctx.makeDiag(st, en, Diag::error, std::move(msg))));
}

[[noreturn]] inline void
Fatal(DiagsDest ctx, size_t st, size_t en, std::string msg) {
  UserError("{}", std::string(ctx.makeDiag(st, en,
                                           Diag::error, std::move(msg))));
}

inline std::nullopt_t
Error(DiagsDest ctx, size_t st, size_t en, std::string msg) {
  return ctx.pushDiagReturnNullOpt(st, en, Diag::error, std::move(msg));
}

inline std::nullopt_t
Warning(DiagsDest ctx, size_t st, size_t en, std::string msg) {
  return ctx.pushDiagReturnNullOpt(st, en, Diag::warning, std::move(msg));
}

inline std::nullopt_t
Note(DiagsDest ctx, size_t st, size_t en, std::string msg) {
  return ctx.pushDiagReturnNullOpt(st, en, Diag::note, std::move(msg));
}

[[noreturn]] inline void
FatalBug(DiagsDest ctx, size_t st, std::string msg) {
  FatalBug(ctx, st, st+1, std::move(msg));
}

[[noreturn]] inline void
Fatal(DiagsDest ctx, size_t st, std::string msg) {
  Fatal(ctx, st, st+1, std::move(msg));
}

inline std::nullopt_t
Error(DiagsDest ctx, size_t st, std::string msg) {
  return ctx.pushDiagReturnNullOpt(st, st+1, Diag::error, std::move(msg));
}

inline std::nullopt_t
Warning(DiagsDest ctx, size_t st, std::string msg) {
  return ctx.pushDiagReturnNullOpt(st, st+1, Diag::warning, std::move(msg));
}

inline std::nullopt_t
Note(DiagsDest ctx, size_t st, std::string msg) {
  return ctx.pushDiagReturnNullOpt(st, st+1, Diag::note, std::move(msg));
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

inline DiagsDest::DiagsDest(struct InputDiags& ctx)
  : input_(&ctx.input), diags_(&ctx.diags) {}

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
