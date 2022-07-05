/*  Copyright 2019-2022 The oalex authors.

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
#include <string_view>
#include <vector>
#include "input_view.h"
#include "test_util.h"
using std::optional;
using std::vector;
using std::string;
using std::string_view;
using oalex::Bug;
using oalex::Input;
using oalex::Skipper;

namespace {

string unixify(string s) {
  s.erase(std::remove(s.begin(), s.end(), '\r'), s.end());
  if(!s.empty() && s.back()!='\n') s+='\n';
  return s;
}

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
  string output = unixify(input);
  if(output != expected_output)
    BugMe("{} != {}", escape(expected_output), escape(output));
}

void unixifyTests() {
  testUnixify("foo\r\nbar", "foo\nbar\n");
  testUnixify("foo\r\nbar\r\nbaz", "foo\nbar\nbaz\n");
  testUnixify("foo\nbar\n", "foo\nbar\n");
  testUnixify("", "");
  testUnixify("foo", "foo\n");
}

Input unixifiedTestInput(string_view s) {
  return Input(unixify(string{s}));
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
const char haskellinput[] = "{- pref -} hello {- a {- b -} c -} world -- stuff";

const Skipper ltxskip{{{"%","\n"}}, {}, Skipper::Newlines::keep_para};
const char ltxinput[] = "hello world % comment";

const Skipper *langskip[] = {&cskip,&pyskip,&ocamlskip,
                             &htmlskip,&haskellskip,&ltxskip};
const char *langinput[] = {cinput,pyinput,ocamlinput,
                           htmlinput,haskellinput,ltxinput};
const char langnames[][8] = {"c","python","ocaml","html","haskell","latex"};
const size_t lang_n = sizeof(langskip)/sizeof(langskip[0]);

// Assumes i is a valid index. "\n" is a word. Increments i to end of word.
string parseWord(const Input& input, size_t& i) {
  if(input[i] != '\n' && !isalpha(input[i]))
    Bug("Found unexpected test character at input[{}]", i);
  if(input[i] == '\n') { ++i; return input.substr(i-1,1); }
  size_t j = i;
  while(input.sizeGt(i) && isalpha(input[i])) ++i;
  return input.substr(j, i-j);
}

vector<string> getLineWords(const Input& input, const Skipper& skip) {
  vector<string> rv;
  for(size_t i = skip.withinLine(input,0);
      input.bol(i) == 0; i = skip.withinLine(input,i)) {
    if(!input.sizeGt(i)) Bug("Input isn't producing a trailing newline");
    if(input[i] == '\n') break;
    rv.push_back(parseWord(input, i));
  }
  return rv;
}

vector<string> getAllWords(const Input& input, const Skipper& skip) {
  vector<string> rv;
  for(size_t i = skip.acrossLines(input,0);
      input.sizeGt(i); i = skip.acrossLines(input,i)) {
    if(!input.sizeGt(i)) Bug("Input isn't producing a trailing newline");
    rv.push_back(parseWord(input, i));
  }
  return rv;
}

void testSingleLineSuccess() {
  for(size_t i=0; i<lang_n; ++i) {
    Input input = unixifiedTestInput(langinput[i]);
    vector<string> words = getLineWords(input, *langskip[i]);
    if(words != vector<string>{"hello", "world"})
      BugMe("Had problems with parsing {}. {} != {{hello, world}}",
                langnames[i], words);
    words = getAllWords(input, *langskip[i]);
    if(words != vector<string>{"hello", "world"})
      BugMe("skip.acrossLines() failed parsing {}. {} != {{hello, world}}",
               langnames[i], words);
  }
}

void testMultilineSuccess() {
  const char cmultiline[] = R"(  hello // comment
    world /* foo
    bar  */ again )";
  Input input = unixifiedTestInput(cmultiline);
  vector<string> words = getAllWords(input, cskip);
  if(words != vector<string>{"hello", "world", "again"})
    BugMe("Problem parsing multiline comment {} != {{hello, world, again}}",
          words);
}

void testBlankLinesMatter() {
  const Skipper mdskipper{{}, {}, Skipper::Newlines::keep_para};
  const char mdinput[] = R"( hello world


    goodbye world)";
  Input input = unixifiedTestInput(mdinput);
  vector<string> words = getAllWords(input, mdskipper);
  vector<string> expected{"hello", "world", "\n", "goodbye", "world"};
  if(words != expected)
    BugMe("Markdown parsing problem: {} != {}", words, expected);

  const char ltxinput1[] = R"(hello
    % comment
    world

    % comment

    % more comment

    goodbye world)";
  input = unixifiedTestInput(ltxinput1);
  words = getAllWords(input, ltxskip);
  if(words != expected)
    BugMe("LaTeX parsing problem: {} != {}", words, expected);

  const char ltxinput2[] = "hello\n%\nworld";
  input = unixifiedTestInput(ltxinput2);
  words = getAllWords(input, ltxskip);
  if(words != vector<string>{"hello", "world"})
    BugMe("LaTeX parsing problem: {} != {{hello, world}}", words);
}

void testLineEndsAtNewline() {
  const string msg = "hello world";
  Input input = unixifiedTestInput(msg + "  \n  ");
  size_t pos1 = msg.size();
  size_t pos2 = cskip.withinLine(input, pos1);
  if(pos2 <= pos1)
    BugMe("Skipper::withinLine() is not moving to the next line. pos = {}",
          pos2);
  if(!input.sizeGt(pos2))
    BugMe("We kept on skippin on the following line. pos = {}", pos2);
  if(input[pos2-1] != '\n')
    BugMe("We did not stop right after a newline. pos = {}", pos2);
}

void testCommentEndsAtNewline() {
  const string msg = R"(  % comment

  foo)";
  Input input = unixifiedTestInput(msg);
  size_t pos = ltxskip.acrossLines(input, 0);
  if(!input.sizeGt(pos)) BugMe("Was expecting to stop before eos");
  if(msg[pos] != '\n')
    BugMe("Was expecting to stop at blank line. Stopped at '{}' instead",
          msg.substr(pos));
}

void testTabsSkipped() {
  const string msg = "\thello \t  /* stuff */\tworld";
  Input input = unixifiedTestInput(msg);
  vector<string> words = getLineWords(input, cskip);
  if(words != vector<string>{"hello", "world"})
    BugMe("{{hello, world}} != {}", words);
}

void testCommentNeverEnds() {
  const string msg = "hello world";
  Input input = unixifiedTestInput(msg + " /* ");
  size_t pos = cskip.withinLine(input, msg.size());
  if(pos != Input::npos)
    BugMe("Unfinished comment should have produced npos, not {}", pos);
  // This check is typically used as a loop termination condition.
  if(input.bol(pos) == 0) BugMe("Leaves characters unconsumed");

  // Test that we are not accidentally nesting it.
  input = unixifiedTestInput(msg + " /* /* */");
  pos = cskip.withinLine(input, msg.size());
  if(input.bol(pos) == 0) BugMe("Leaves characters unconsumed");

  // Test again for nested comments.
  input = unixifiedTestInput(msg + " {- {- -} ");
  pos = haskellskip.withinLine(input, msg.size());
  if(pos != Input::npos)
    BugMe("Unfinished comment should have produced npos, not {}", pos);
  if(input.bol(pos) == 0) BugMe("Leaves characters unconsumed");

  // Multiline tests.
  input = unixifiedTestInput(msg + "\n /*");
  pos = cskip.acrossLines(input, msg.size());
  if(pos != Input::npos)
    BugMe("Unfinished comment should have produced npos, not {}", pos);
  if(input.sizeGt(pos)) BugMe("Leaves characters unconsumed");

  input = unixifiedTestInput(msg + " /* \n /* */");
  pos = cskip.acrossLines(input, msg.size());
  if(input.sizeGt(pos)) BugMe("Leaves characters unconsumed");

  input = unixifiedTestInput(msg + " {- {- \n  -} ");
  pos = haskellskip.acrossLines(input, msg.size());
  if(pos != Input::npos)
    BugMe("Unfinished comment should have produced npos, not {}", pos);
  if(input.sizeGt(pos)) BugMe("Leaves characters unconsumed");
}

void assertValidCallReturnsSubstring(string_view testname, const Skipper& skip,
    string_view expected_error) {
  optional<string> err = skip.valid();
  if(!err) Bug("{} unexpectedly succeeded", testname);
  if(err->find(expected_error) == string::npos)
    Bug("{} didn't find the expected error. '{}' does not contain '{}'",
        *err, expected_error);
}

void testValid() {
  for(size_t i=0; i<lang_n; ++i) if(auto err = langskip[i]->valid())
    Bug("{} skipper has problems: {}", langnames[i], *err);
  const Skipper noComments{{}, {}};
  if(auto err = noComments.valid())
    Bug("Language with no comments has problems: {}", *err);
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

void testEquality() {
  Skipper a{{{"/*","*/"}, {"//", "\n"}}, {}, Skipper::Newlines::ignore_all}, b;
  if(a != a) BugMe("Every Skipper should be equal to itself");
  if(a == b) BugMe("C skipper should not be the same as the default skipper");
  b = a;
  if(a != b) BugMe("Assignment should have produced a perfect copy");
  b.nestedComment = std::pair{"(*", "*)"};
  if(a == b) BugMe("nestedComment was ignored in comparison");
  b = a;
  b.newlines = Skipper::Newlines::keep_para;
  if(a == b) BugMe("newlines was ignored in comparison");
  b = a;
  b.unnestedComments.pop_back();
  if(a == b) BugMe("unnestedComments was ignored in comparison");
}

}  // namespace

int main() {
  unixifyTests();
  testSingleLineSuccess();
  testLineEndsAtNewline();
  testCommentEndsAtNewline();
  testTabsSkipped();
  testCommentNeverEnds();
  testValid();
  testMultilineSuccess();
  testBlankLinesMatter();
  testEquality();
}
