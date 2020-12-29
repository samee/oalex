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

#include "input_view.h"
#include "diags_test_util.h"
#include "test_util.h"
#include "util.h"

#include <random>
#include "fmt/core.h"
using std::bernoulli_distribution;
using std::default_random_engine;
using std::make_pair;
using std::pair;
using std::string;
using std::uniform_int_distribution;

using oalex::assertEqual;
using oalex::Bug;
using oalex::GetFromString;
using oalex::Input;

namespace {
  // 1. Generate random string with newlines.
  // 2. We can always make O(n) ground-truth functions.
  // 3. Pick average line length k. Make string with P('\n') = 1/k.
  // 4. Scan through it with i, forgetting at j:
  //      ++i with P(advance) = 1 - 1/l
  //      j+=delta with P(forget) = 1/l
  //      where l = 2k.
  //      delta = i-j with P = 1/2l
  //            = (i-j)/2 with p = 1/4l
  //            = (i-j)/2^x with p = 1/(2l*2^x)
  //      verify one last time before forgetting.

string randomString(size_t avgLineSz,size_t sz) {
  const char ichars[]="abcdefghijklmnopqrstuvwxyz";
  string s(sz,' ');
  default_random_engine engine(42);
  uniform_int_distribution<size_t> random_index(0,sizeof(ichars)-2);
  bernoulli_distribution newline_coin(1.0/avgLineSz);
  for(char& ch:s) {
    if(newline_coin(engine)) ch='\n';
    else ch=ichars[random_index(engine)];
  }
  return s;
}

size_t bol(const string& s,size_t i) {
  if(i == 0) return 0;
  for(size_t j=i-1;j>0;--j) if(s[j]=='\n') return j+1;
  return s[0]=='\n';
}

pair<size_t,size_t> rowCol(const string& s,size_t i) {
  size_t row=0, col=0;
  for(size_t j=0;j<i;++j) {
    if(s[j]=='\n') { ++row; col=0; }
    else ++col;
  }
  return make_pair(row+1,col+1);
}

size_t forgetLen(default_random_engine& engine, size_t sz) {
  if(sz == 0) return 0;
  if(bernoulli_distribution(.5)(engine)) return sz;
  return forgetLen(engine, sz/2);
}

void testDataMatchesString(const string& s, size_t avgWindowLen) {
  default_random_engine engine(42);
  bernoulli_distribution forget_coin(1.0/avgWindowLen);
  Input input((GetFromString(s)));
  size_t i=0,j=0;
  while(j<s.size()) {
    if(i >= s.size() || forget_coin(engine)) {
      size_t rmlen=forgetLen(engine, i-j);
      if(s.substr(j,rmlen) != input.substr(j,rmlen))
        Bug("substr mismatch {} != {}", s.substr(j,rmlen),
                                        input.substr(j,rmlen));
      j+=rmlen;
      input.forgetBefore(j);
    }else {
      if(s[i] != input[i])
        Bug("input[{}] mismatch: {} != {}", i, s[i], input[i]);
      if(bol(s,i) != input.bol(i))
        Bug("bol({}) mismatch: {} != {}", i, bol(s,i), input.bol(i));
      pair<size_t,size_t> observed = input.rowCol(i), expected = rowCol(s,i);
      if(observed.first != expected.first)
        Bug("row mismatch: {} != {}", expected.first, observed.first);
      if(observed.second != expected.second)
        Bug("col mismatch: {} != {}", expected.second, observed.second);
      ++i;
    }
  }
}

void testLineTooLong() {
  string s(Input::defaultMaxLineLength+1,'-');
  assertProducesDiag(__func__, s, "Line 1 is too long",
                     +[](oalex::InputDiags& ctx, size_t&)
                       { ctx.input[ctx.input.maxLineLength()]; });
  string t(Input::defaultMaxLineLength, '-');
  Input input2((GetFromString("\n" + t)));
  input2[input2.maxLineLength()];  // No exceptions.
}

// Newline computation is slightly different for Input::Input(getch) and
// Input::Input(string). Other tests already check the getch() version, by using
// GetFromString(). This checks the second version too, since we just
// encountered this bug.
void testAllNewlinesRecorded() {
  Input input("hello\n\nworld");
  assertEqual(__func__, input.rowCol(0).first, size_t{1});
  assertEqual(__func__, input.rowCol(sizeof("hello")).first, size_t{2});
  assertEqual(__func__, input.rowCol(sizeof("hello")+1).first, size_t{3});
}

void testForgottenBol() {
  string s(100,'-');
  Input input{GetFromString(s)};
  input.forgetBefore(80);
  if(input.bol(90)!=0)
    BugMe("first-line bol() is wrong after amnesia. 0 != {}", input.bol(90));
  if(input.bol(50)!=0)
    BugMe("Can't find bol() for forgotten index. 0 != {}", input.bol(50));
}

}

int main() {
  const size_t linelen=50;
  testDataMatchesString(randomString(linelen,1000), 2*linelen);
  testLineTooLong();
  testAllNewlinesRecorded();
  testForgottenBol();
}
