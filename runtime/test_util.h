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

#include <string>
#include <string_view>
#include "fmt/core.h"
#include "diags.h"
#include "util.h"


template <>
struct fmt::formatter<std::vector<std::string>> {
  constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

  auto format(const std::vector<std::string>& v, fmt::format_context& ctx)
    -> decltype(format_to(ctx.out(), ""));
};

namespace oalex {

template <class ... Args> [[noreturn]] void
  BugMeImpl(const char testName[], const char* fmt, const Args& ... args) {
    Bug(fmt::format("{}: {}", testName, fmt).data(), args...);
}

#define BugMe(...) oalex::BugMeImpl(__func__, __VA_ARGS__)
#define me(msg) fmt::format("{}: {}", __func__, msg)

template <class X>
void assertEqual(std::string_view msg, const X& a, const X& b) {
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

// Often, the cb parameter of assertProducesDiags() has an extra return value
// that we don't care about in assertProducesDiag(). Using a macro on the
// caller-side (as opposed to a template here) lets us use vanilla function
// pointers instead of closures that need capture.
#define OALEX_VOIDIFY(fun) (+[](oalex::InputDiags& a, size_t& b) { fun(a, b); })

void assertProducesDiag(std::string_view testName, std::string_view input,
                        std::string_view err,
                        void (*cb)(oalex::InputDiags&, size_t&));
}  // namespace oalex
