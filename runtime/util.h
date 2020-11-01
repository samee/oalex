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
#include<variant>
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

// variant utilities, specially geared towards variant<unique_ptr<...>,...>.
template <class ... T> using variant_unique =
  std::variant<std::unique_ptr<T>...>;
template <class ... T> using variant_unique_const =
  std::variant<std::unique_ptr<const T>...>;

template <class T>
struct is_variant { static constexpr bool value = false; };
template <class ... Ts>
struct is_variant<std::variant<Ts...>> { static constexpr bool value = true; };
template <class T> inline constexpr
bool is_variant_v = is_variant<T>::value;

template <class ... Ts> struct holds_one_of_helper;  // undefined

template <>
struct holds_one_of_helper<> {
  template <class V> static bool check(const V&) { return false; }
};

template <class T, class ... Ts>
struct holds_one_of_helper<T, Ts...> {
  template <class V> static bool check(const V& v) {
    static_assert(is_variant_v<V>);
    return std::holds_alternative<T>(v) || holds_one_of_helper<Ts...>::check(v);
  }
};

template <class ... Ts, class V> bool holds_one_of(const V& v) {
  return holds_one_of_helper<Ts...>::check(v);
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

template <class V, class FirstArg, class ... RestArgs>
void makeVectorImpl(std::vector<V>& v, FirstArg&& fa, RestArgs&& ... ra) {
  v.push_back(std::move(fa));
  makeVectorImpl(v, std::move(ra)...);
}

template <class V>
void makeVectorImpl(std::vector<V>&) {}

template <class V, class ... Args> std::vector<V>
makeVector(Args ... args) {
  std::vector<V> rv;
  makeVectorImpl(rv, std::move(args)...);
  return rv;
}

}  // namespace oalex
