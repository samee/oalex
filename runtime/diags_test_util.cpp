/*  Copyright 2019-2020 Google LLC

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "diags_test_util.h"
using oalex::Bug;
using oalex::BugWarn;
using oalex::Diag;
using oalex::GetFromString;
using oalex::Input;
using oalex::InputDiags;
using oalex::isSubstr;
using fmt::format_to;
using fmt::memory_buffer;
using fmt::print;
using fmt::to_string;
using std::string;
using std::string_view;
using std::vector;

void showDiags(const vector<Diag>& diags) {
  memory_buffer buf;
  format_to(buf, "diags:\n");
  for(const auto& d : diags) format_to(buf, "  {}\n", string(d));
  BugWarn("{}", to_string(buf));
}

void assertHasDiagWithSubstr(string_view testName, const vector<Diag>& diags,
                             string_view expectedDiag) {
  for(const Diag& d : diags) if(isSubstr(expectedDiag, d.msg)) return;
  showDiags(diags);
  Bug("{} didn't get the expected diag: {}", testName, expectedDiag);
}

void assertHasDiagWithSubstrAt(string_view testName, const vector<Diag>& diags,
                               string_view expectedDiag, size_t expectedStPos) {
  for(const Diag& d : diags) {
    if(d.stPos != expectedStPos+1) continue;  // The +1 is from Diags() ctor
    if(!isSubstr(expectedDiag, d.msg)) continue;
    return;
  }
  showDiags(diags);
  Bug("{} didn't get the expected diag at position {}: {}",
      testName, expectedStPos, expectedDiag);
}

InputDiags testInputDiags(string_view s) {
  return InputDiags{Input(GetFromString(s))};
}

void assertEmptyDiags(string_view testName, const vector<Diag>& diags) {
  if(diags.empty()) return;
  for(const auto& d:diags) print(stderr, "{}\n", string(d));
  Bug("{} had unexpected errors", testName);
}
