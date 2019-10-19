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

#ifdef TEST_UTIL_H
#error "Do not include more than once." \
 "This should only be included from *_test.cpp with a main()"
#endif
#define TEST_UTIL_H

#include <iostream>
#include <string_view>
#include "util.h"

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

// Useful as getch() callbacks in dfa.h and input_view_manual.h.
class GetFromString {
  std::string_view src;
  size_t i=0;
 public:
  explicit GetFromString(std::string_view src):src(src) {}
  int operator()() { return i<src.size()?src[i++]:-1; }
};

