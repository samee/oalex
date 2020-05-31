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

#include <iostream>
#include <map>
#include <string>
#include <string_view>
#include "fmt/format.h"
#include "util.h"

template <>
struct fmt::formatter<std::vector<std::string>> {
  constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const std::vector<std::string>& v, FormatContext& ctx) {
    if(v.empty()) return format_to(ctx.out(), "{{}}");
    format_to(ctx.out(), "{{{}", v[0]);
    for(size_t i=1;i<v.size();++i) format_to(ctx.out(), ", {}", v[i]);
    return format_to(ctx.out(), "}}");
  }
};

namespace oalex {

template <class ... Args> [[noreturn]] void
  BugMeFmtImpl(const char testName[], const char* fmt, const Args& ... args) {
    BugFmt(fmt::format("{}: {}", testName, fmt).data(), args...);
}

#define BugMe oalex::Bug()<<__func__<<": "
#define BugMeFmt(...) oalex::BugMeFmtImpl(__func__, __VA_ARGS__)

// Useful as getch() callbacks in dfa.h and input_view.h.
// Dev note: When reading from files, two things to note:
//   * This is where "\r\n" -> "\n" conversion happens.
//   * If the last character is not "\n", append an extra "\n".
class GetFromString {
  std::string src;
  size_t i=0;
 public:
  explicit GetFromString(std::string_view src):src(src) {}
  int operator()() { return i<src.size()?src[i++]:-1; }
};

inline std::ostream&
operator<<(std::ostream& os,const std::vector<std::string>& v) {
  if(v.empty()) return os<<"{}";
  os<<'{'<<v[0];
  for(size_t i=1;i<v.size();++i) os<<", "<<v[i];
  os<<'}';
  return os;
}

template <class K, class T, class Cmp> std::vector<K>
uniqueKeys(const std::multimap<K,T,Cmp>& m) {
  std::vector<K> v;
  for(const auto& [k,e] : m) if(v.empty()||v.back()!=k) v.push_back(k);
  return v;
}

inline bool isSubstr(std::string_view s, std::string_view t) {
  return t.find(s) != std::string_view::npos;
}

}  // namespace oalex
