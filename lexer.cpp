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

#include <functional>
#include <limits>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "segment.h"
#include "util.h"

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

// input_view.h seemed too complicated for a simple recursive descent parser.
// We try an alternative abstraction here, where we access it like a normal
// string, but it only keeps a smallish substring in memory. Any time we use
// operator[], it loads new characters into buf_ if necessary. It only forgets
// parts of it when explicitly asked to.
//
// It always tries to keep one extra character beyond what was already read with
// operator[], so that the following loop will work:
//
//   for(size_t i=0;i<input.size();++i) { process(input[i]); }
//
// input.size() will change from npos to the real size as soon as the last
// character is accessed.
class Input {
 public:
  static constexpr auto npos = numeric_limits<size_t>::max();
  explicit Input(function<int16_t()> getch)
    : getch_(getch), pos_(0), size_(npos) { peekMore(); }
  void forgetBefore(size_t begin);
  char operator[](size_t sz);

 private:
  string buf_;
  function<int16_t()> getch_;
  size_t pos_,size_;

  void peekMore();
  bool endSeen() const { return size_ < npos; }
};

void Input::peekMore() {
  if(endSeen()) return;
  int16_t ch=getch_();
  if(ch < 0) size_=buf_.size()+pos_;
  else buf_+=ch;
}

// operator[](x) will no longer be valid for x < begin.
void Input::forgetBefore(size_t begin) {
  while(!endSeen() && begin >= pos_+buf_.size()) peekMore();
  // No point saving memory anymore.
  if(endSeen()) return;
  // Check if we've already forgotten this part.
  if(begin <= pos_) return;
  buf_.erase(buf_.begin(),buf_.begin()+(begin-pos_));
  pos_=begin;
}

char Input::operator[](size_t i) {
  if(i < pos_) BugDie()<<"Out of bound error, already forgotten "<<i<<'.';
  while(!endSeen() && i+1-pos_<buf_.size()) peekMore();
  if(i-pos_ < buf_.size()) return buf_[i-pos_];
  BugDie()<<"Out of bound error. "<<i<<" is beyond the end of input.";
}

// The main lexer. See top of this file for why we use the term lexer for
// non-regular language parsing. See dfa.h for getch() convention.
vector<shared_ptr<Segment>> lex(Input& input) {
  vector<shared_ptr<Segment>> rv;
  // ... continue here.
  return rv;
}

}  // namespace oalex::lex
