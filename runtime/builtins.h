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

#pragma once

#include "diags.h"
#include "jsonlike.h"

namespace oalex {
  class Parser {
   public:
    virtual JsonLike operator()(InputDiags& ctx, ssize_t& pos) const = 0;
    virtual ~Parser() {}
  };
  class ParserPtr : public Parser {
   public:
    using callback_type = JsonLike (*)(InputDiags&, ssize_t&);
    ParserPtr(callback_type cb) : cb_{cb} {}  // implicit
    JsonLike operator()(InputDiags& ctx, ssize_t& pos) const override {
      return cb_(ctx, pos);
    }
   private:
    callback_type cb_ = nullptr;
  };
}

/* While we have only one function declared below, more can be added later.

   This function is outside of the oalex namespace, to be consistent with other
   extern functions that are not builtin. Hence they start with `oalexBuiltin`
   as their name prefix. */


/*
  Parses an indented list. The first line is expected to be a leader that
  starts the list, followed by list items that are indented strictly more than
  the leader was. For example, if the leader is the string "start:", and each
  item is the string "itemN" for some number N, the following will be a valid
  list:

      start:
        item1  # Comments
        item2  #  allowed
        item3  #  here

  The starting leader can be anything accepted by the callback parser `leader`.
  Each item is anything accepted by `lineItem`. The starting position `i` is
  expected to be at the beginning of something `leader()` will accept, although
  `oalexBuiltinIndentedList()` might have to look back from there to find the
  reference indentation to compare list items with. This value of `i` does not
  have to be at the beginning of a line, it can be in the middle too.

  On success, `i` will be advanced past the last character of the last list
  item. So in the example above, `i` would have landed just after the digit `3`.
  The return value will be a JsonLoc::Map of the following form:

      { leader: <return from leader()>,
        items: [return, values, from, lineItem()] }

  So in the example above, the prettyPrint() of the return value will be:

      { leader: "start", items: ["item1", "item2", "item3"] }

  On error, the response varies:

    * On callback error, either from `leader()` or `lineItem()`, we return
      immediately, propagating the error.
    * If there is a problem with the first lineItem's indentation, we also
      return an error immediately, emitting some diagnostics.
    * If there is a problem with the indentation of later lines, we either keep
      going or we declare the previous line to be the end of the list.

  In all cases, `i` is left unchanged if we ever return an error. Frequently,
  `lineItem()` would need to recover from errors if you want parsing to continue
  past it (e.g. by emiting errors and returning a non-error JsonLoc). It does
  not make sense for lineItem() to accept the empty string, since it will then
  be impossible to define "indentation" of any line.

  This convention of aborting on most errors is to make it consistent with how
  LoopRule works. We can always add more error recovery in the components.

  Comments and blank lines are allowed between any two items or after the leader
  line. Comments can also appear on the same line as leader or lineItem content,
  right after they end. No comments may appear before these lines, however, so
  that indentation level is clearly visible.

  Definition of indent comparison. See indentCmp() in indent.cpp for details.
  Unlike Python indentation, this function does not define an equivalence
  between a tab character and some number of spaces. Instead, we just have a
  rule that works for most real-life code without any such equivalence. We say
  that line 2 is indented deeper than line 1 if and only if: the string of
  spaces and tabs at the start of line 1 is a proper prefix of that at the start
  of line 2. The indentations are equal if the lines start with the exact same
  string of spaces and tabs. If two lines have no prefix relationship between
  their indentation, they are deemed incomparable and we raise an error.
*/
oalex::JsonLike
oalexNewBuiltinIndentedList(
    oalex::InputDiags& ctx, ssize_t& i,
    const oalex::Parser& leader, const oalex::Parser& lineItem);
