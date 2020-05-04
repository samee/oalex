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
using std::optional;
using std::vector;
using std::string;
using std::string_view;
using oalex::Bug;
using oalex::BugWarn;
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
  return InputDiags{Input(Unixify{GetFromString(s)})};
}

const Skipper cskip{ {{"/*","*/"},{"//","\n"}}, {}};
const char cinput[] = "hello /* /* comment */ world // more stuff";

const Skipper pyskip{ {{"#","\n"},{"'''","'''"},{"\"\"\"","\"\"\""}}, {}};
const char pyinput[] = "hello '''comment'''\"\"\"comment\"\"\" world # stuff";

const Skipper ocamlskip{{}, {{"(*","*)"}} };
const char ocamlinput[] = "hello (* more (* nested *) stuff *) world";

const Skipper htmlskip{{{"<!--","-->"}}, {}};
const char htmlinput[] = "hello <!-- comment --> world";

const Skipper haskellskip{{{"--","\n"}}, {{"{-","-}"}}};
const char haskellinput[] = "hello {- a {- b -} c -} world -- stuff";

const Skipper *langskip[] = {&cskip,&pyskip,&ocamlskip,&htmlskip,&haskellskip};
const char *langinput[] = {cinput,pyinput,ocamlinput,htmlinput,haskellinput};
const char langnames[][8] = {"c","python","ocaml","html","haskell"};
const size_t lang_n = sizeof(langskip)/sizeof(langskip[0]);

// Assumes i is a valid index. "\n" is a word. Increments i to end of word.
string parseWord(const Input& input, size_t& i) {
  if(input[i] != '\n' && !isalpha(input[i]))
    Bug()<<"Found unexpected test character at input["<<i<<"]";
  if(input[i] == '\n') { ++i; return input.substr(i-1,1); }
  size_t j = i;
  while(input.sizeGt(i) && isalpha(input[i])) ++i;
  return input.substr(j, i-j);
}

vector<string> getLineWords(InputDiags& ctx, const Skipper& skip) {
  const Input& input = ctx.input;
  vector<string> rv;
  for(size_t i = skip.withinLine(ctx,0);
      input.bol(i) == 0; i = skip.withinLine(ctx,i)) {
    if(!input.sizeGt(i)) Bug()<<"Input isn't producing a trailing newline";
    if(input[i] == '\n') break;
    rv.push_back(parseWord(input, i));
  }
  return rv;
}

vector<string> getAllWords(InputDiags& ctx, const Skipper& skip) {
  const Input& input = ctx.input;
  vector<string> rv;
  for(size_t i = skip.acrossLines(ctx,0);
      i != input.npos; i = skip.acrossLines(ctx,i)) {
    if(!input.sizeGt(i)) Bug()<<"Input isn't producing a trailing newline";
    rv.push_back(parseWord(input, i));
  }
  return rv;
}

void testSingleLineSuccess() {
  for(size_t i=0; i<lang_n; ++i) {
    InputDiags ctx = unixifiedTestInputDiags(langinput[i]);
    vector<string> words = getLineWords(ctx, *langskip[i]);
    if(words != vector<string>{"hello", "world"})
      BugMe<<"Had problems with parsing "<<langnames[i]<<". "<<words
           <<" != {hello, world}";
    words = getAllWords(ctx, *langskip[i]);
    if(words != vector<string>{"hello", "world"})
      BugMe<<"skip.acrossLines() failed parsing "<<langnames[i]<<". "<<words
           <<" != {hello, world}";
  }
}

void testMultilineSuccess() {
  const char cmultiline[] = R"(  hello // comment
    world /* foo
    bar  */ again )";
  InputDiags ctx = unixifiedTestInputDiags(cmultiline);
  vector<string> words = getAllWords(ctx, cskip);
  if(words != vector<string>{"hello", "world", "again"})
    BugMe<<"Problem parsing multiline comment "<<words
         <<" != {hello, world, again}";
}

void testBlankLinesMatter() {
  const Skipper mdskipper{{}, {}, true};
  const char mdinput[] = R"( hello world


    goodbye world)";
  InputDiags ctx = unixifiedTestInputDiags(mdinput);
  vector<string> words = getAllWords(ctx, mdskipper);
  vector<string> expected{"hello", "world", "\n", "goodbye", "world"};
  if(words != expected)
    BugMe<<"Markdown parsing problem: "<<words<<" != "<<expected;

  const Skipper ltxskipper{{{"%","\n"}}, {}, true};
  const char ltxinput1[] = R"(hello
    % comment
    world

    % comment

    % more comment

    goodbye world)";
  ctx = unixifiedTestInputDiags(ltxinput1);
  words = getAllWords(ctx, ltxskipper);
  if(words != expected)
    BugMe<<"LaTeX parsing problem: "<<words<<" != "<<expected;

  const char ltxinput2[] = "hello\n%\nworld";
  ctx = unixifiedTestInputDiags(ltxinput2);
  words = getAllWords(ctx, ltxskipper);
  if(words != vector<string>{"hello", "world"})
    BugMe<<"LaTeX parsing problem: "<<words<<" != {hello, world}";
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
  vector<string> words = getLineWords(ctx, cskip);
  if(words != vector<string>{"hello", "world"})
    BugMe<<"{hello, world} != "<<words;
}

void testCommentNeverEnds() {
  const string input = "hello world";
  InputDiags ctx = unixifiedTestInputDiags(input + " /* ");
  size_t pos = cskip.withinLine(ctx, input.size());
  assertHasDiagWithSubstr(__func__, ctx.diags, "Comment never ends");
  // This check is typically used as a loop termination condition.
  if(ctx.input.bol(pos) == 0) BugMe<<"Leaves characters unconsumed";

  // Test that we are not accidentally nesting it.
  ctx = unixifiedTestInputDiags(input + " /* /* */");
  pos = cskip.withinLine(ctx, input.size());
  if(!ctx.diags.empty()) {
    showDiags(ctx.diags);
    BugMe<<"Wasn't expecting problems with properly closed comments";
  }
  if(ctx.input.bol(pos) == 0) BugMe<<"Leaves characters unconsumed";

  // Test again for nested comments.
  ctx = unixifiedTestInputDiags(input + " {- {- -} ");
  pos = haskellskip.withinLine(ctx, input.size());
  assertHasDiagWithSubstr(__func__, ctx.diags, "Comment never ends");
  if(ctx.input.bol(pos) == 0) BugMe<<"Leaves characters unconsumed";

  // Multiline tests.
  ctx = unixifiedTestInputDiags(input + "\n /*");
  pos = cskip.acrossLines(ctx, input.size());
  assertHasDiagWithSubstr(__func__, ctx.diags, "Comment never ends");
  if(ctx.input.sizeGt(pos)) BugMe<<"Leaves characters unconsumed";

  ctx = unixifiedTestInputDiags(input + " /* \n /* */");
  pos = cskip.acrossLines(ctx, input.size());
  if(!ctx.diags.empty()) {
    showDiags(ctx.diags);
    BugMe<<"Wasn't expecting problems with properly closed multiline comments";
  }
  if(ctx.input.sizeGt(pos)) BugMe<<"Leaves characters unconsumed";

  ctx = unixifiedTestInputDiags(input + " {- {- \n  -} ");
  pos = haskellskip.acrossLines(ctx, input.size());
  assertHasDiagWithSubstr(__func__, ctx.diags, "Comment never ends");
  if(ctx.input.sizeGt(pos)) BugMe<<"Leaves characters unconsumed";
}

void assertValidCallReturnsSubstring(string_view testname, const Skipper& skip,
    string_view expected_error) {
  optional<string> err = skip.valid();
  if(!err) Bug()<<testname<<" unexpectedly succeeded";
  if(err->find(expected_error) == string::npos)
    Bug()<<testname<<" didn't find the expected error. '"<<*err
         <<"' does not contain '"<<expected_error<<"'";
}

void testValid() {
  for(size_t i=0; i<lang_n; ++i) if(auto err = langskip[i]->valid())
    Bug()<<langnames[i]<<" skipper has problems: "<<*err;
  const Skipper noComments{{}, {}};
  if(auto err = noComments.valid())
    Bug()<<"Language with no comments has problems: "<<*err;
  assertValidCallReturnsSubstring("empty unnested start delim",
      Skipper{{{"", "*/"}}, {}}, "delimiters cannot be empty");
  assertValidCallReturnsSubstring("empty unnested end delim",
      Skipper{{{"/*", ""}}, {}}, "delimiters cannot be empty");
  assertValidCallReturnsSubstring("empty nested start delim",
      Skipper{{}, {{"", "*/"}}}, "delimiters cannot be empty");
  assertValidCallReturnsSubstring("empty nested end delim",
      Skipper{{}, {{"/*", ""}}}, "delimiters cannot be empty");
  assertValidCallReturnsSubstring("delims start with space",
      Skipper{{{" /*", "*/"}}, {}}, "Whitespace at the start");
  assertValidCallReturnsSubstring("start delim with newline",
      Skipper{{{"/*\n", "*/"}}, {}}, "delimiters requiring newline");
  assertValidCallReturnsSubstring("end delim with newline",
      Skipper{{{"/*", "\n*/"}}, {}}, "delimiters requiring newline");
  assertValidCallReturnsSubstring("prefixed nested delim forward",
      Skipper{{}, {{"''", "'''"}}}, "can be confused for each other");
  assertValidCallReturnsSubstring("prefixed nested delim forward",
      Skipper{{}, {{"'''", "''"}}}, "can be confused for each other");
  assertValidCallReturnsSubstring("prefixed nested delim equal",
      Skipper{{}, {{"'''", "'''"}}}, "can be confused for each other");
  assertValidCallReturnsSubstring("not prefix-free",
      Skipper{{{"---", "\n"}}, {{"--", "->"}}}, "cannot be prefixes");
}

}  // namespace

int main() {
  unixifyTests();
  testSingleLineSuccess();
  testLineEndsAtNewline();
  testTabsSkipped();
  testCommentNeverEnds();
  testValid();
  testMultilineSuccess();
  testBlankLinesMatter();
}
