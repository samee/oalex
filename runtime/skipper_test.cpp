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
#include <vector>
#include "diags.h"
#include "diags_test_util.h"
using std::vector;
using std::string;
using oalex::Bug;
using oalex::Input;
using oalex::InputDiags;
using oalex::Skipper;
using oalex::operator<<;

namespace {

// TODO avoid explicit '\n' in inputs with a custom getch().
// TODO failure tests.
const Skipper cskip{ {{"/*","*/"},{"//","\n"}}, {}};
const char cinput[] = "hello /* comment */ world // more stuff\n";

const Skipper pyskip{ {{"#","\n"},{"'''","'''"},{"\"\"\"","\"\"\""}}, {}};
const char pyinput[] = "hello '''comment'''\"\"\"comment\"\"\" world # stuff\n";

const Skipper ocamlskip{{}, {{"(*","*)"}} };
const char ocamlinput[] = "hello (* more (* nested *) stuff *) world\n";

const Skipper htmlskip{{{"<!--","-->"}}, {}};
const char htmlinput[] = "hello <!-- comment --> world\n";

const Skipper haskellskip{{{"--","\n"}}, {{"{-","-}"}}};
const char haskellinput[] = "hello {- a {- b -} c -} world -- stuff\n";

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
  const char langnames[][7] = {"c","python","ocaml","html","haskell"};
  const size_t n = sizeof(skip)/sizeof(skip[0]);
  for(size_t i=0; i<n; ++i) {
    InputDiags ctx = testInputDiags(input[i]);
    vector<string> words = getWords(ctx,*skip[i]);
    if(words != vector<string>{"hello", "world"})
      BugMe<<"Had problems with parsing "<<langnames[i]<<". "<<words
           <<" != {hello, world}";
  }
}

}  // namespace

int main() {
  testSingleLineSuccess();
}
