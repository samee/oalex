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
#include <optional>  // for nullopt_t
#include "input_view.h"

namespace oalex {

struct Diag {
  enum Severity { error,warning,note } severity;
  size_t stLine, stPos, enLine, enPos;  // These ranges are inclusive.
  std::string msg;
  Diag(const Input& input, size_t st, size_t en,
       Severity sev, std::string msg);
  explicit operator std::string() const;
};

bool hasError(const std::vector<Diag>& diags);

class DiagsDest {
  const Input* input_;
  std::vector<Diag>* diags_;
 public:
  DiagsDest(struct InputDiags& ctx);  // implicit

  // Typically, we don't expect this to be called directly. This is merely a
  // helper for the more convenient Error(), Warning(), and Note().
  std::nullopt_t pushDiagReturnNullOpt(size_t st, size_t en,
                                       Diag::Severity sev, std::string msg);
  Diag makeDiag(size_t st, size_t en,
                Diag::Severity sev, std::string msg) const;
  std::string locationString(size_t st, size_t en) const;
};

[[noreturn]] void
FatalBug(DiagsDest ctx, size_t st, size_t en, std::string msg);
[[noreturn]] void
Fatal(DiagsDest ctx, size_t st, size_t en, std::string msg);
std::nullopt_t Error(DiagsDest ctx, size_t st, size_t en, std::string msg);
std::nullopt_t Warning(DiagsDest ctx, size_t st, size_t en, std::string msg);
std::nullopt_t Note(DiagsDest ctx, size_t st, size_t en, std::string msg);

[[noreturn]] void FatalBug(DiagsDest ctx, size_t st, std::string msg);
[[noreturn]] void Fatal(DiagsDest ctx, size_t st, std::string msg);
std::nullopt_t Error(DiagsDest ctx, size_t st, std::string msg);
std::nullopt_t Warning(DiagsDest ctx, size_t st, std::string msg);
std::nullopt_t Note(DiagsDest ctx, size_t st, std::string msg);

struct InputDiags {
  Input input;
  std::vector<Diag> diags;

  explicit InputDiags(Input input) : input(std::move(input)) {}
  InputDiags(InputDiags&&) = default;
  InputDiags& operator=(InputDiags&& that) = default;
  InputDiags(const InputDiags&) = delete;
  InputDiags& operator=(const InputDiags&) = delete;
};

inline DiagsDest::DiagsDest(struct InputDiags& ctx)
  : input_(&ctx.input), diags_(&ctx.diags) {}

}  // namespace oalex
