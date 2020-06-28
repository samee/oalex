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
#include<cstdio>
#include<memory>
#include<stdexcept>
#include"fmt/format.h"

namespace oalex {

// Usage:
//   if(weird) Bug("Tell me more about {}", x);
//   if(weird) BugDie("Tell me more about {}", x);
//   if(weird) BugWarn("Tell me more about {}", x);
//   if(weird) UserError("Tell me more about {}", x);
//   Debug("More message about {}", x);  // Never in checked-in code.
//
// TODO: adopt std::format() whenever it becomes available and stable.
struct BugEx : std::logic_error {
  BugEx(const std::string& s) : std::logic_error(s) {}
};

template <class ... Args>
[[noreturn]] void Bug(const char* fmt, const Args& ... args) {
  throw BugEx(fmt::format(fmt, args...));
}

template <class ... Args>
[[noreturn]] void BugDie(const char* fmt, const Args& ... args) {
  fmt::print(stderr, fmt, args...);
  std::abort();
}

struct UnimplementedEx : BugEx {
  UnimplementedEx(const std::string& msg) : BugEx(msg) {}
};
template <class ... Args>
[[noreturn]] void Unimplemented(const char* fmt, const Args& ... args) {
  throw UnimplementedEx(fmt::format(fmt, args...));
}

template <class ... Args>
void Debug(const char* fmt, const Args& ... args) {
  fmt::print(stderr, fmt, args...);
  fmt::print(stderr, "\n");
}

struct UserErrorEx : std::runtime_error {
  UserErrorEx(const std::string& s) : std::runtime_error(s) {}
};
template <class ... Args>
[[noreturn]] void UserError(const char* fmt, const Args& ... args) {
  throw UserErrorEx(fmt::format(fmt, args...));
}

template <class ... Args>
void BugWarn(const char* fmt, const Args& ... args) {
  fmt::print(stderr, fmt::format("Bug: {}\n", fmt), args...);
}

// Enables brace-initialization for variants without naming the type twice.
template <class T>
auto move_to_unique(T& t) -> std::unique_ptr<T> {
  return std::make_unique<T>(std::move(t));
}

template <class T>
auto move_to_unique(T&& t) -> std::unique_ptr<T> {
  return std::make_unique<T>(std::move(t));
}

inline bool isSubstr(std::string_view s, std::string_view t) {
  return t.find(s) != std::string_view::npos;
}

}  // namespace oalex
