/*  Copyright 2019-2024 The oalex authors.

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

#include <string>
#include <string_view>
#include <format>
#include "diags.h"
#include "util.h"


template <>
struct std::formatter<std::vector<std::string>> {
  constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

  auto format(const std::vector<std::string>& v, std::format_context& ctx) const
    -> std::format_context::iterator;
};

namespace oalex {

template <class ... Args> [[noreturn]] void
  BugMeImpl(const char testName[], const char* fmt, const Args& ... args) {
    Bug(std::format("{}: {}", testName, fmt).data(), args...);
}

// Consider source_location instead of __func__ for new use cases.
// This pattern is a holdover from pre-C++20 era.

#define BugMe(...) oalex::BugMeImpl(__func__, __VA_ARGS__)
#define me(msg) std::format("{}: {}", __func__, msg)

template <class X, class Y>
void assertEqual(std::string_view msg, const X& a, const Y& b) {
  if(a!=b) Bug("{}: '{}' != '{}'", msg, a, b);
}

void showDiags(const std::vector<oalex::Diag>& diags);

void assertHasDiagWithSubstr(std::string_view testName,
                             const std::vector<oalex::Diag>& diags,
                             std::string_view expectedDiag);

void assertHasDiagWithSubstrOnce(std::string_view testName,
                                 const std::vector<oalex::Diag>& diags,
                                 std::string_view expectedDiag);

void assertHasDiagWithSubstrAt(std::string_view testName,
                               const std::vector<oalex::Diag>& diags,
                               std::string_view expectedDiag,
                               size_t expectedStPos);

void assertEmptyDiags(std::string_view testName,
                      const std::vector<oalex::Diag>& diags);

void assertWhatHasSubstr(std::string_view msg, const std::exception& ex,
                         std::string_view expected_what);

// Often, the cb parameter of assertProducesDiags() has an extra return value
// that we don't care about in assertProducesDiag(). Using a macro on the
// caller-side (as opposed to a template here) lets us use vanilla function
// pointers instead of closures that need capture.
#define OALEX_VOIDIFY(fun) (+[](oalex::InputDiags& a, size_t& b) { fun(a, b); })

void assertProducesDiag(std::string_view testName, std::string_view input,
                        std::string_view err,
                        void (*cb)(oalex::InputDiags&, size_t&));
}  // namespace oalex
