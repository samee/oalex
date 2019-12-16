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
#include "lexer.h"
#include "test_util.h"

inline void assertHasDiagWithSubstr(const char testName[],
                                    const std::vector<oalex::lex::Diag>& diags,
                                    std::string_view expectedDiag) {
  using oalex::lex::Diag;
  if(expectedDiag.empty()) return;  // Test succeeds even if we have no diags.
  for(const Diag& d : diags) if(oalex::isSubstr(expectedDiag, d.msg)) return;
  std::cerr<<"Got diags:\n";
  for(const Diag& d : diags) std::cerr<<" "<<std::string(d)<<std::endl;
  oalex::BugDie()<<testName<<" didn't get the expected diag: "<<expectedDiag;
}

