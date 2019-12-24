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
#include <string_view>
#include "util.h"

namespace oalex {

// Definition of non-static global variable, not declaration.
bool someError=false;
struct TestErrImpl {
  ~TestErrImpl() { std::cerr<<std::endl; }
  std::ostream& start(const char* file,int line) {
    someError=true;
    return std::cerr<<file<<':'<<line<<": ";
  }
};

#define TestErr TestErrImpl().start(__FILE__,__LINE__)

// If I need to check BugDie later, I'll use std::set_terminate(). This can
// happen if I worry about accidentally disabling checks. But for now, I am not
// testing them since dying is never part of the API contract. They are all
// internal bugs.
#define BugMe oalex::BugDie()<<__func__<<": "

// Useful as getch() callbacks in dfa.h and input_view.h.
// Dev note: When reading from files, two things to note:
//   * This is where "\r\n" -> "\n" conversion happens.
//   * If the last character is not "\n", append an extra "\n".
class GetFromString {
  std::string_view src;
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
