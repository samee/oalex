/*  Copyright 2019-2021 The oalex authors.

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
#include<charconv>
#include<cstdio>
#include<memory>
#include<variant>
#include"fmt/core.h"

namespace oalex {

// Usage:
//   if(weird) Bug("Tell me more about {}", x);
//   if(weird) BugDie("Tell me more about {}", x);
//   if(weird) BugWarn("Tell me more about {}", x);
//   if(weird) UserError("Tell me more about {}", x);
//   Debug("More message about {}", x);  // Never in checked-in code.
//
// TODO: adopt std::format() whenever it becomes available and stable.

[[noreturn]] void BugImplHelper(const char* fmt, fmt::format_args args);
template <class ... Args>
[[noreturn]] void Bug(const char* fmt, const Args& ... args) {
  BugImplHelper(fmt, fmt::make_format_args(args...));
}

template <class ... Args>
[[noreturn]] void BugDie(const char* fmt, const Args& ... args) {
  fmt::print(stderr, fmt, args...);
  std::abort();
}

[[noreturn]] void
UnimplementedImplHelper(const char* fmt, fmt::format_args args);
template <class ... Args>
[[noreturn]] void Unimplemented(const char* fmt, const Args& ... args) {
  UnimplementedImplHelper(fmt, fmt::make_format_args(args...));
}

template <class ... Args>
void Debug(const char* fmt, const Args& ... args) {
  fmt::print(stderr, fmt, args...);
  fmt::print(stderr, "\n");
}

[[noreturn]] void
UserErrorImplHelper(const char* fmt, fmt::format_args args);
template <class ... Args>
[[noreturn]] void UserError(const char* fmt, const Args& ... args) {
  UserErrorImplHelper(fmt, fmt::make_format_args(args...));
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

// variant utilities, specially geared towards variant<unique_ptr<...>,...>.
template <class ... T> using variant_unique =
  std::variant<std::unique_ptr<T>...>;
template <class ... T> using variant_unique_const =
  std::variant<std::unique_ptr<const T>...>;

template <class ... Ts, class V> bool holds_one_of(const V& v) {
  return (std::holds_alternative<Ts>(v) || ...);
}
template <class ... Ts, class V> bool holds_one_of_unique(const V& v) {
  return holds_one_of<std::unique_ptr<Ts>...>(v);
}
template <class ... Ts, class V> bool holds_one_of_unique_const(const V& v) {
  return holds_one_of<std::unique_ptr<const Ts>...>(v);
}

template <class T, class V, class = std::enable_if_t<!std::is_const_v<V>>>
auto get_if_unique(V* v) -> T* {
  std::unique_ptr<T>* r = std::get_if<std::unique_ptr<T>>(v);
  return r?r->get():nullptr;
}

template <class T, class V>
auto get_if_unique(const V* v) -> const T* {
  const std::unique_ptr<T>* r = std::get_if<std::unique_ptr<T>>(v);
  return r?r->get():nullptr;
}

template <class T, class V, class = std::enable_if_t<!std::is_const_v<V>>>
auto get_unique(V& v) -> T& {
  if(auto& up = std::get<std::unique_ptr<T>>(v)) return *up;
  else throw std::bad_variant_access();
}

template <class T, class V>
auto get_unique(const V& v) -> const T& {
  if(auto& up = std::get<std::unique_ptr<T>>(v)) return *up;
  else throw std::bad_variant_access();
}

inline bool isSubstr(std::string_view s, std::string_view t) {
  return t.find(s) != std::string_view::npos;
}


// sign_cast<int>(). Eqiuvalent to reinterpret_cast, but constrained.

template <class T>
class ReverseSigned {
  using unref = std::remove_reference_t<T>;
 public:
  static_assert(std::is_lvalue_reference_v<T> && std::is_integral_v<unref>,
                "sign_cast only works on integer references");
  using type = std::conditional_t<std::is_signed_v<unref>,
                                  std::make_unsigned_t<unref>,
                                  std::make_signed_t<unref>>;
};

template <class T> T sign_cast(typename ReverseSigned<T>::type& ref) {
  return reinterpret_cast<T>(ref);
}

// makeVector<V>(...). Allows construction of a vector with move-only elements.

template <class V, class ... Args> std::vector<V>
makeVector(Args ... args) {
  std::vector<V> rv;
  (rv.push_back(std::move(args)), ...);
  return rv;
}
template <class V, class ... Args> auto
makeVectorUnique(Args ... args) {
  std::vector<std::unique_ptr<V>> rv;
  (rv.push_back(move_to_unique(args)), ...);
  return rv;
}

// Used for code-generation and debugging.
// Avoid std::to_strings to avoid locales.
template <class Int> std::string
itos(Int x) {
  std::string s(std::numeric_limits<Int>::digits10+3, '\0');
  std::to_chars_result res = std::to_chars(s.data(), s.data()+s.size(), x);
  if(res.ec != std::errc{}) Bug("Need more memory in itos()");
  s.resize(res.ptr-s.data());
  return s;
}

#ifndef __cpp_lib_ssize
// TODO: Remove this when we start requiring C++20
template <class C>
constexpr auto ssize(const C& c)
  -> std::common_type_t<std::ptrdiff_t,
                        std::make_signed_t<decltype(c.size())>> {
  return c.size();
}
#endif

}  // namespace oalex
