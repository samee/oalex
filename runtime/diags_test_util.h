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
#pragma once
#include <string_view>

#include "diags.h"
#include "test_util.h"

void showDiags(const std::vector<oalex::Diag>& diags);

void assertHasDiagWithSubstr(std::string_view testName,
                             const std::vector<oalex::Diag>& diags,
                             std::string_view expectedDiag);

void assertHasDiagWithSubstrAt(std::string_view testName,
                               const std::vector<oalex::Diag>& diags,
                               std::string_view expectedDiag,
                               size_t expectedStPos);

// TODO remove this function if no longer necessary.
// Input{} has string ctor now.
oalex::InputDiags testInputDiags(std::string_view s);

void assertEmptyDiags(std::string_view testName,
                      const std::vector<oalex::Diag>& diags);

// Often, the cb parameter of assertProducesDiags() has an extra return value
// that we don't care about in assertProducesDiag(). Using a macro on the
// caller-side (as opposed to a template here) lets us use vanilla function
// pointers instead of closures that need capture.
#define OALEX_VOIDIFY(fun) (+[](oalex::InputDiags& a, size_t& b) { fun(a, b); })

void assertProducesDiag(std::string_view testName, std::string_view input,
                        std::string_view err,
                        void (*cb)(oalex::InputDiags&, size_t&));
