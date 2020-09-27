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
#include <string_view>
#include "fmt/format.h"

#include "diags.h"
#include "test_util.h"

inline void showDiags(const std::vector<oalex::Diag>& diags) {
  fmt::memory_buffer buf;
  fmt::format_to(buf, "diags:\n");
  for(const auto& d : diags) fmt::format_to(buf, "  {}\n", std::string(d));
  oalex::BugWarn("{}", fmt::to_string(buf));
}

inline void assertHasDiagWithSubstr(std::string_view testName,
                                    const std::vector<oalex::Diag>& diags,
                                    std::string_view expectedDiag) {
  using oalex::Diag;
  for(const Diag& d : diags) if(oalex::isSubstr(expectedDiag, d.msg)) return;
  showDiags(diags);
  oalex::Bug("{} didn't get the expected diag: {}",
             testName, expectedDiag);
}

inline oalex::InputDiags testInputDiags(std::string_view s) {
  return oalex::InputDiags{oalex::Input(oalex::GetFromString(s))};
}

inline void assertEmptyDiags(std::string_view testName,
                      const std::vector<oalex::Diag>& diags) {
  if(diags.empty()) return;
  for(const auto& d:diags) fmt::print(stderr, "{}\n", std::string(d));
  oalex::Bug("{} had unexpected errors", testName);
}

template <class Cb>
void assertProducesDiag(std::string_view testName, std::string_view input,
                        std::string_view err, Cb cb) {
  oalex::InputDiags ctx{oalex::Input{std::string(input)}};
  size_t i = 0;
  try {
    cb(ctx, i);
  }catch(oalex::UserErrorEx& ex) {
    Error(ctx,0,0,ex.what());  // demote Fatal() error to non-fatal.
  }
  assertHasDiagWithSubstr(testName, ctx.diags, err);
}
