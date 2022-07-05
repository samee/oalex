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
#include "skipper.h"
#include "util.h"
using oalex::Bug;
using oalex::indent_of;
using oalex::IndentCmp;
using oalex::indentCmp;
using oalex::Input;
using oalex::InputDiags;
using oalex::JsonLoc;
using oalex::Parser;
using oalex::Skipper;
using oalex::StringLoc;

static size_t skipToNextLine(InputDiags& ctx, size_t i, const Skipper& skip) {
  // TODO: refactor with eval(SkipPoint).
  const Input& input = ctx.input;
  size_t j = skip.acrossLines(input, i);
  if(j == Input::npos && i != Input::npos) {
    // TODO replace oalexWSkip with whitespace().
    ssize_t com = skip.whitespace(input, i);
    if(!ctx.input.sizeGt(com)) Bug("skipper returned npos without a comment");
    Error(ctx, com, "Unfinished comment");
    return j;
  }else if(!input.sizeGt(j)) return j;  // EOF: Let the next parser add errors.

  size_t bi = input.bol(i), bj = input.bol(j);
  if(bi == bj) {
    Error(ctx, j, "Expected end of line");
    return Input::npos;
  }else if(bi > bj) Bug("skipper shouldn't move backwards");
  return j;
}

// Consumes malformed list entries until an indent that's less or equal to
// the leader indent. If the leader itself is malformed, nothing is consumed.
// TODO fix error-handling.
JsonLoc
oalexBuiltinIndentedList(
    InputDiags& ctx, ssize_t& i,
    const Parser& leader, const Parser& lineItem) {
  const Input& input = ctx.input;
  ssize_t j = i;
  JsonLoc jsloc_h = leader(ctx, j);
  if(jsloc_h.holdsErrorValue()) return jsloc_h;

  StringLoc hindent = indent_of(input, i);

  // TODO make this a parameter, instead of hardcoding it. When we do, remember
  // to disallow comments before lineItem() on each non-blank line.
  // In the normal case, when this skipper is provided, skip over spaces in
  // unexpectedly deep indentation below.
  const Skipper skip{{{"#", "\n"}}, {}};
  if(skip.newlines != Skipper::Newlines::ignore_all)
    oalex::Unimplemented("Skipper::Newlines::{} in oalexBuiltinIndentedList",
                         to_string(skip.newlines));

  JsonLoc::Vector lines;
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
    JsonLoc item = lineItem(ctx, j);
    if(item.holdsErrorValue()) return item;
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

  i = j;
  return JsonLoc::Map{
    {"leader", std::move(jsloc_h)},
    {"items", JsonLoc{std::move(lines)}},
  };
}
