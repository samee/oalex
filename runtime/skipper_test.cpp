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

#include "skipper.h"
#include <functional>
#include <string_view>
#include <vector>
#include "diags.h"
#include "diags_test_util.h"
#include "test_util.h"
using std::function;
using std::vector;
using std::string;
using std::string_view;
using oalex::Bug;
using oalex::GetFromString;
using oalex::Input;
using oalex::InputDiags;
using oalex::Skipper;
using oalex::operator<<;

namespace {

// Takes in an Input::getch_() callback, and produces another that conforms
// to the needs of Skipper. Does a poor-man's conversion from dos-encoded crlf
// streams to Unix-style lf streams. Additionally, it adds an extra '\n'
// character at the end if one doesn't already exist.
//
// Normally, the standard file I/O libraries will take care of this, so we
// shouldn't need this Unixify adaptor. But Skipper really cares about the last
// character being a '\n' for single-line comments.
//
// We try to do this in a way that's idempotent: it won't modify anything if
// the stream already has everything we need. It won't add an extra '\n' in the
// end if one is already there. The hope is there won't be a need to ever
// switch this translation off. If that becomes necessary, we'll probably
// do the simpler thing of just using <stdio.h> streams and adding an extra
// '\n' to everything, instead of using this.
//
// Usage:
//   std::function<int16_t()> newGetch = Unixify{oldGetch};
struct Unixify {
  function<int16_t()> getch;
  int16_t last_char = -1;
  int16_t operator()() {
    int16_t ch;
    do { ch = getch(); } while(ch == '\r');  // filter out '\r'.

    if(ch < 0 && last_char >= 0 && last_char != '\n') return last_char='\n';
    else return last_char=ch;
  }
};

string escape(string_view s) {
  string rv;
  for(char ch : s) switch(ch) {
    case '\r': rv += "\\r"; break;
    case '\n': rv += "\\n"; break;
    default: rv += ch;
  }
  return rv;
}

void testUnixify(const char input[], const char expected_output[]) {
  auto getch = Unixify{GetFromString(input)};
  string output;
  while(true) { char ch = getch(); if(ch < 0) break; output += ch; }
  if(expected_output != output)
    BugMe<<escape(expected_output)<<" != "<<escape(output);
}

void unixifyTests() {
  testUnixify("foo\r\nbar", "foo\nbar\n");
  testUnixify("foo\r\nbar\r\nbaz", "foo\nbar\nbaz\n");
  testUnixify("foo\nbar\n", "foo\nbar\n");
  testUnixify("", "");
  testUnixify("foo", "foo\n");
}

InputDiags unixifiedTestInputDiags(string_view s) {
  return InputDiags{Input(Unixify{GetFromString(s)}), {}};
}

// TODO failure tests.
const Skipper cskip{ {{"/*","*/"},{"//","\n"}}, {}};
const char cinput[] = "hello /* comment */ world // more stuff";

const Skipper pyskip{ {{"#","\n"},{"'''","'''"},{"\"\"\"","\"\"\""}}, {}};
const char pyinput[] = "hello '''comment'''\"\"\"comment\"\"\" world # stuff";

const Skipper ocamlskip{{}, {{"(*","*)"}} };
const char ocamlinput[] = "hello (* more (* nested *) stuff *) world";

const Skipper htmlskip{{{"<!--","-->"}}, {}};
const char htmlinput[] = "hello <!-- comment --> world";

const Skipper haskellskip{{{"--","\n"}}, {{"{-","-}"}}};
const char haskellinput[] = "hello {- a {- b -} c -} world -- stuff";

vector<string> getWords(InputDiags& ctx, const Skipper& skip) {
  const Input& input = ctx.input;
  vector<string> rv;
  for(size_t i = skip.withinLine(ctx,0);
      input.bol(i) == 0; i = skip.withinLine(ctx,i)) {
    if(!input.sizeGt(i)) Bug()<<"Input isn't producing a trailing newline";
    if(input[i] == '\n') break;
    if(!isalpha(input[i]))
      Bug()<<"Found unexpected test character at input["<<i<<"]";
    size_t j;
    for(j=i; input.sizeGt(j) && isalpha(input[j]); ++j);
    rv.push_back(input.substr(i,j-i));
    i=j;
  }
  return rv;
}

void testSingleLineSuccess() {
  const Skipper *skip[] = {&cskip,&pyskip,&ocamlskip,&htmlskip,&haskellskip};
  const char *input[] = {cinput,pyinput,ocamlinput,htmlinput,haskellinput};
  const char langnames[][8] = {"c","python","ocaml","html","haskell"};
  const size_t n = sizeof(skip)/sizeof(skip[0]);
  for(size_t i=0; i<n; ++i) {
    InputDiags ctx = unixifiedTestInputDiags(input[i]);
    vector<string> words = getWords(ctx,*skip[i]);
    if(words != vector<string>{"hello", "world"})
      BugMe<<"Had problems with parsing "<<langnames[i]<<". "<<words
           <<" != {hello, world}";
  }
}

void testLineEndsAtNewline() {
  const string input = "hello world";
  InputDiags ctx = unixifiedTestInputDiags(input + "  \n  ");
  size_t pos1 = input.size();
  size_t pos2 = cskip.withinLine(ctx, pos1);
  if(pos2 <= pos1)
    BugMe<<"Skipper::withinLine() is not moving to the next line. pos = "<<pos2;
  if(!ctx.input.sizeGt(pos2))
    BugMe<<"We kept on skippin on the following line. pos = "<<pos2;
  if(ctx.input[pos2-1] != '\n')
    BugMe<<"We did not stop right after a newline. pos = "<<pos2;
}

void testTabsSkipped() {
  const string input = "\thello \t  /* stuff */\tworld";
  InputDiags ctx = unixifiedTestInputDiags(input);
  vector<string> words = getWords(ctx, cskip);
  if(words != vector<string>{"hello", "world"})
    BugMe<<"{hello, world} != "<<words;
}

}  // namespace

int main() {
  unixifyTests();
  testSingleLineSuccess();
  testLineEndsAtNewline();
  testTabsSkipped();
}
