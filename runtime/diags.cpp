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

#include "diags.h"
#include "util.h"
#include "fmt/core.h"
#include <string>
using fmt::format;
using std::string;
using std::nullopt;
using std::nullopt_t;

namespace oalex {

static string severityString(Diag::Severity sev) {
  switch(sev) {
    case Diag::error: return "error";
    case Diag::warning: return "warning";
    case Diag::note: return "note";
    default: Bug("Diagnostics has a strange severity: {}", int{sev});
  }
}

static string locationString(const Diag& diag) {
  if(diag.stLine != diag.enLine)
    return format("{}-{}", diag.stLine, diag.enLine);
  else if(diag.stPos != diag.enPos)
    return format("{}:{}-{}", diag.stLine, diag.stPos, diag.enPos);
  else return format("{}:{}", diag.stLine, diag.stPos);
}

string DiagsDest::locationString(size_t st, size_t en) const {
  return oalex::locationString(Diag{*input_, st, en, Diag::error, string()});
}

Diag::Diag(const Input& input, size_t st, size_t en,
           Severity sev, std::string msg)
  : severity(sev), msg(msg) {
  std::tie(stLine,stPos) = input.rowCol(st);
  std::tie(enLine,enPos) = input.rowCol(--en);
}

Diag::operator string() const {
  return format("{}: {}: {}", locationString(*this),
                severityString(severity), msg);
}

bool hasError(const std::vector<Diag>& diags) {
  for(const auto& d : diags) if(d.severity == Diag::error) return true;
  return false;
}

nullopt_t
DiagsDest::pushDiagReturnNullOpt(size_t st, size_t en,
                                 Diag::Severity sev, std::string msg) {
  diags_->emplace_back(*input_, st, en, sev, std::move(msg));
  return std::nullopt;
}

Diag
DiagsDest::makeDiag(size_t st, size_t en,
                    Diag::Severity sev, std::string msg) const {
  return Diag(*input_, st, en, sev, std::move(msg));
}

void
FatalBug(DiagsDest ctx, size_t st, size_t en, string msg) {
  Bug("{}", string(ctx.makeDiag(st, en, Diag::error, std::move(msg))));
}

void
Fatal(DiagsDest ctx, size_t st, size_t en, string msg) {
  UserError("{}", string(ctx.makeDiag(st, en, Diag::error, std::move(msg))));
}

nullopt_t
Error(DiagsDest ctx, size_t st, size_t en, string msg) {
  return ctx.pushDiagReturnNullOpt(st, en, Diag::error, std::move(msg));
}

nullopt_t
Warning(DiagsDest ctx, size_t st, size_t en, string msg) {
  return ctx.pushDiagReturnNullOpt(st, en, Diag::warning, std::move(msg));
}

nullopt_t
Note(DiagsDest ctx, size_t st, size_t en, string msg) {
  return ctx.pushDiagReturnNullOpt(st, en, Diag::note, std::move(msg));
}

void
FatalBug(DiagsDest ctx, size_t st, string msg) {
  FatalBug(ctx, st, st+1, std::move(msg));
}

void
Fatal(DiagsDest ctx, size_t st, string msg) {
  Fatal(ctx, st, st+1, std::move(msg));
}

nullopt_t
Error(DiagsDest ctx, size_t st, string msg) {
  return ctx.pushDiagReturnNullOpt(st, st+1, Diag::error, std::move(msg));
}

nullopt_t
Warning(DiagsDest ctx, size_t st, string msg) {
  return ctx.pushDiagReturnNullOpt(st, st+1, Diag::warning, std::move(msg));
}

nullopt_t
Note(DiagsDest ctx, size_t st, string msg) {
  return ctx.pushDiagReturnNullOpt(st, st+1, Diag::note, std::move(msg));
}

}  // namespace oalex
