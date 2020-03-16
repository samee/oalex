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

using Regex = std::variant<
  std::unique_ptr<struct CharSet>,
  std::unique_ptr<std::string>,
  std::unique_ptr<struct Concat>,
  std::unique_ptr<struct Repeat>,
  std::unique_ptr<struct Optional>,
  std::unique_ptr<struct OrList>
>;

// Regex primitives. Likely to change if we ever switch to matching JsonLoc.
struct CharRange { unsigned char from, to; };
struct CharSet { std::vector<CharRange> ranges; bool negated = false; };

struct Concat { std::vector<Regex> parts; };
struct Repeat { Regex part; };
struct Optional { Regex part; };
struct OrList { std::vector<Regex> parts; };

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

template <class T, class V>
auto get_unique(const V& v) -> const T& {
  return *std::get<std::unique_ptr<T>>(v);
}

// Enables brace-initialization for variants without naming the type twice.
template <class T>
auto makeUniqueRegex(T t) -> Regex {
  return std::make_unique<T>(std::move(t));
}

}  // namespace oalex::regex
