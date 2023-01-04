/*  Copyright 2022 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "builtins.h"
#include "indent.h"
#include "jsonlike.h"
#include "skipper.h"
#include "util.h"
using oalex::Bug;
using oalex::indent_of;
using oalex::IndentCmp;
using oalex::indentCmp;
using oalex::InputDiags;
using oalex::InputPiece;
using oalex::JsonLike;
using oalex::JsonLoc;
using oalex::LocPair;
using oalex::Parser;
using oalex::Skipper;
using oalex::StringLoc;
using std::vector;

namespace oalex {

ParsedIndentedList::operator JsonLoc() const {
  vector<JsonLoc> items_loc;
  for(auto& item : this->items) items_loc.push_back(JsonLoc{item});
  return JsonLoc::withPos(
      JsonLoc::Map{
        {"leader", JsonLoc{this->leader}},
        {"items", JsonLoc{std::move(items_loc)}},
      },
      this->loc.first,
      this->loc.second);
}

}  // namespace oalex

static size_t skipToNextLine(InputDiags& ctx, size_t i, const Skipper& skip) {
  // TODO: refactor with eval(SkipPoint).
  const InputPiece& input = ctx.input();
  size_t j = skip.next(input, i);
  if(j == input.npos && i != input.npos) {
    // TODO replace oalexWSkip with whitespace().
    ssize_t com = skip.whitespace(input, i);
    if(!ctx.input().sizeGt(com)) Bug("skipper returned npos without a comment");
    Error(ctx, com, "Unfinished comment");
    return j;
  }else if(!input.sizeGt(j)) return j;  // EOF: Let the next parser add errors.

  size_t bi = input.bol(i), bj = input.bol(j);
  if(bi == bj) {
    Error(ctx, j, "Expected end of line");
    return input.npos;
  }else if(bi > bj) Bug("skipper shouldn't move backwards");
  return j;
}

// Consumes malformed list entries until an indent that's less or equal to
// the leader indent. If the leader itself is malformed, nothing is consumed.
// TODO fix error-handling.
JsonLike
oalexBuiltinIndentedList(
    InputDiags& ctx, ssize_t& i,
    const Parser& leader, const Parser& lineItem) {
  const InputPiece& input = ctx.input();
  ssize_t j = i;
  JsonLike jslike_h = leader(ctx, j);
  if(!jslike_h) return jslike_h;

  StringLoc hindent = indent_of(input, i);

  // TODO make this a parameter, instead of hardcoding it. When we do, remember
  // to disallow comments before lineItem() on each non-blank line.
  const Skipper skip{{{"#", "\n"}}, {}};
  if(skip.newlines != Skipper::Newlines::ignore_all)
    oalex::Unimplemented("Skipper::Newlines::{} in oalexBuiltinIndentedList",
                         to_string(skip.newlines));

  vector<JsonLike> lines;
  j = skipToNextLine(ctx, j, skip);
  if(!input.sizeGt(j)) {
    // TODO make this error customizable from oalex source.
    Error(ctx, hindent.enPos(), "Expected list items after this");
    return JsonLoc::ErrorValue{};
  }

  StringLoc first_indent = indent_of(input, j);
  IndentCmp c = indentCmp(*hindent, *first_indent);
  if(c != IndentCmp::lt) {
    if(c == IndentCmp::gt || c == IndentCmp::eq)
      Error(ctx, hindent.enPos(), "Line should have been indented more");
    else
      Error(ctx, first_indent.stPos(), first_indent.enPos(), "Bad indentation");
    return JsonLoc::ErrorValue{};
  }

  while(input.sizeGt(j)) {
    JsonLike item = lineItem(ctx, j);
    if(!item) return item;
    lines.push_back(std::move(item));

    // See if there is another item next, and advance j only if so.
    ssize_t k = skipToNextLine(ctx, j, skip);
    if(!input.sizeGt(k)) break;
    StringLoc curindent = indent_of(input, k);
    c = indentCmp(*first_indent, *curindent);
    if(c == IndentCmp::bad) {
      // Keep going with the error.
      Error(ctx, curindent.stPos(), curindent.enPos(), "Bad indentation");
    }else if(c == IndentCmp::lt)  // Unexpectedly deep indentation.
      k -= curindent->size() - first_indent->size();
    else if(c == IndentCmp::gt) {  // End of list
      c = indentCmp(*hindent, *curindent);
      if(c != IndentCmp::eq && c != IndentCmp::gt)
        Error(ctx, curindent.stPos(), curindent.enPos(),
              "Indentation at the end of the list doesn't match "
              "the one at the beginning");
      break;
    }
    // else if(c == IndentCmp::eq) { all good, keep going }
    j = k;
  }

  LocPair loc{i, j};
  i = j;
  return oalex::ParsedIndentedList{
    .loc = std::move(loc),
    .leader = std::move(jslike_h),
    .items = std::move(lines),
  };
}
