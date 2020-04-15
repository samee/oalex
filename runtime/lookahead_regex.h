/*  Copyright 2020 Google LLC

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
#include <memory>
#include <variant>
#include <vector>

#include "runtime/diags.h"

namespace oalex::regex {

// Forward decl.
enum struct Anchor;

using Regex = std::variant<
  std::unique_ptr<struct CharSet>,
  std::unique_ptr<std::string>,
  std::unique_ptr<Anchor>,
  std::unique_ptr<struct Concat>,
  std::unique_ptr<struct Repeat>,
  std::unique_ptr<struct Optional>,
  std::unique_ptr<struct OrList>
>;

// Regex primitives. Likely to change if we ever switch to matching JsonLoc.
struct CharRange { unsigned char from, to; };
struct CharSet { std::vector<CharRange> ranges; bool negated = false; };
enum struct Anchor { wordEdge, bol, eol };

struct Concat { std::vector<Regex> parts; };
struct Repeat { Regex part; };
struct Optional { Regex part; };
struct OrList { std::vector<Regex> parts; };

// variant utilities, specially geared towards variant<unique_ptr<...>,...>.
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

struct RegexOptions {
  CharSet word;  // Used for \b matches.
};

// Used for lookaheads
bool startsWith(const Input& input, size_t i,
                const Regex& regex, const RegexOptions& opts);

}  // namespace oalex::regex
