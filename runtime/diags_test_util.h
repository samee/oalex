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
#include "diags.h"
#include "test_util.h"

inline void showDiags(const std::vector<oalex::Diag>& diags) {
  oalex::BugWarn()<<"diags:";
  for(const auto& d : diags) oalex::BugWarn()<<"  "<<std::string(d);
}

inline void assertHasDiagWithSubstr(std::string_view testName,
                                    const std::vector<oalex::Diag>& diags,
                                    std::string_view expectedDiag) {
  using oalex::Diag;
  if(expectedDiag.empty()) return;  // Test succeeds even if we have no diags.
  for(const Diag& d : diags) if(oalex::isSubstr(expectedDiag, d.msg)) return;
  showDiags(diags);
  oalex::Bug()<<testName<<" didn't get the expected diag: "<<expectedDiag;
}

inline oalex::InputDiags testInputDiags(std::string_view s) {
  return oalex::InputDiags{oalex::Input(oalex::GetFromString(s))};
}

inline void assertEmptyDiags(std::string_view testName,
                      const std::vector<oalex::Diag>& diags) {
  if(diags.empty()) return;
  for(const auto& d:diags) std::cerr<<std::string(d)<<std::endl;
  oalex::Bug()<<testName<<" had unexpected errors";
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
