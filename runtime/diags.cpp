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

DiagsDest::DiagsDest(class InputDiags& ctx)
  : input_(&ctx.input), diags_(&ctx.diags) {}

static string severityString(Diag::Severity sev) {
  switch(sev) {
    case Diag::error: return "error";
    case Diag::warning: return "warning";
    case Diag::note: return "note";
    default: Bug("Diagnostics has a strange severity: {}", sev);
  }
}

static string locationString(const Diag& diag) {
  if(diag.stLine != diag.enLine)
    return format("{}-{}", diag.stLine, diag.enLine);
  else if(diag.stPos != diag.enPos)
    return format("{}:{}-{}", diag.stLine, diag.stPos, diag.enPos);
  else return format("{}:{}", diag.stLine, diag.stPos);
}

string locationString(const Input& input, size_t st, size_t en) {
  return locationString(Diag{input, st, en, Diag::error, string()});
}

Diag::operator string() const {
  return format("{}: {}: {}", locationString(*this),
                severityString(severity), msg);
}

}  // namespace oalex
