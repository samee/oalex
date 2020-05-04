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

#include "diags.h"
#include "util.h"
#include <string>
using std::string;
using std::nullopt;
using std::nullopt_t;

namespace oalex {

static string severityString(Diag::Severity sev) {
  switch(sev) {
    case Diag::error: return "error";
    case Diag::warning: return "warning";
    case Diag::note: return "note";
    default: BugDie()<<"Diagnostics has a strange severity: "<<sev;
  }
}

static string locationString(const Diag& diag) {
  if(diag.stLine != diag.enLine) return Str()<<diag.stLine<<'-'<<diag.enLine;
  else if(diag.stPos != diag.enPos)
    return Str()<<diag.stLine<<':'<<diag.stPos<<'-'<<diag.enPos;
  else return Str()<<diag.stLine<<':'<<diag.stPos;
}


Diag::operator string() const {
  return locationString(*this) + ": " + severityString(severity) + ": " + msg;
}

void DiagDest::FatalBug(size_t st, size_t en, string msg) const {
  BugDie()<<string(Diag(row_col_table(), st, en, Diag::error, std::move(msg)));
}

void DiagDest::Fatal(size_t st, size_t en, string msg) const {
  UserError()<<string(Diag(row_col_table(), st, en,
                           Diag::error, std::move(msg)));
}

nullopt_t DiagDest::Error(size_t st, size_t en, string msg) {
  diagDest().emplace_back(row_col_table(), st, en, Diag::error, std::move(msg));
  return nullopt;
}

nullopt_t DiagDest::Warning(size_t st, size_t en, string msg) {
  diagDest().emplace_back(row_col_table(), st, en,
                          Diag::warning, std::move(msg));
  return nullopt;
}

nullopt_t DiagDest::Note(size_t st, size_t en, string msg) {
  diagDest().emplace_back(row_col_table(), st, en, Diag::note, std::move(msg));
  return nullopt;
}

}  // namespace oalex
