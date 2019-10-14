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

/*
   13th Oct 2019: initial plans

   I won't really have the usual lexer vs parser division. The initial "lexer"
   phase will still be non-regular. We'll still call them lexer and parser,
   though instead of stage-1 and stage-2. Tasks of our lexer:

     * Ignore comments and non-important whitespaces, still preserving indent
       information where necessary.
     * Output tokens:
         - SectionHeading(string)
         - alnum words
         - quoted strings, distinguising between quoting styles.
         - regexes
         - pair-matching (parenthesis, brackets, so on).

    The lexer will be responsible for general look-and-feel of the language,
    concerning itself with what is nested under what, what is quoted, and what
    is commented. It understands the structure and layout of the code, not
    what it means.

    It's output is thus still tree-structured.
*/

#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "segment.h"
#include "input_view_manual.h"

using std::function;
using std::numeric_limits;
using std::shared_ptr;
using std::string;
using std::string_view;
using std::vector;

namespace oalex::lex {

enum class LexSegmentTag {
  alnumToken = Segment::lastReservedTag + 1,
  section,
  quotedString,
  bracketed,
};

struct AlnumToken : Segment {
  static constexpr auto type_tag = tagint_t(LexSegmentTag::alnumToken);
  string token;
  AlnumToken(size_t st,size_t en,string_view token)
    : Segment{st,en,type_tag}, token(string(token)) {}
};

// The main lexer. See top of this file for why we use the term lexer for
// non-regular language parsing. See dfa.h for getch() convention.
vector<shared_ptr<Segment>> lex(Input& input) {
  vector<shared_ptr<Segment>> rv;
  // ... continue here.
  return rv;
}

}  // namespace oalex::lex
