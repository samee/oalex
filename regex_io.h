/*  Copyright 2020 Google LLC

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
We have two separate uses for regex, and I'm not entirely sure they match up.
One is in lookaheads, the other is in the actual grammar. It is possible that
we'll allow grammars with more flexibility if they are regular (no lookaheads,
allow negation, use in skipper, so on).

For now though, it looks like we are blocked more on the lookahead part of
things. So let's design something for just that, keeping in mind that we may
have to capture subexpressions at some point.

So what operations do I care about? Match prefix and null intersection:

  startsWith(string, regex) -> bool
  prefixesBoth(regex1, regex2) -> optional<string>

This means I will need to support negative lookaheads, at least in the end.
Well, I will need to support negation, since this whole thing is a lookahead
anyway. Since we are only matching and not parsing, we don't care about
ambiguous disjunctions; we don't care about which branch was taken.

We won't care about greedy-vs-non-greedy repetition either, since I don't care
about how far they match. Only that they do.

Might parameterize primitives, if we ever use it directly on JsonLoc.

  data RegexPrimitive = Char | CharRange

  data Regex = Primitive RegexPrimitive
             | Concat [Regex]
             | Repeat Regex
             | Optional Regex
             | OrList [Regex]

No anchors for now, since it's lookahead. Might add them later. I can always
look ahead and see if the next char is what we expect. And we almost never
lookahead to check for eof.

  -- Add escaping parameters.
  prettyPrint Primitive (Char ch) = ch
  prettyPrint Primitive (CharRange from to) = from ++ "-" ++ to

  prettyPrint Concat [pieces] = concatmap prettyPrint pieces

  -- consider eliminating grouping
  prettyPrint Repeat regex = "(" ++ prettyPrint regex ++ ")..."

  prettyPrint Optional regex = "[" ++ prettyPrint regex ++ "]"


Okay, todo then:
  startsWith(input_view, regex) -> bool
  unmatchedString(regex1, ..., regexn) -> optional<string>
  matchesNull(regex) -> bool

Enforce usual rules about not allowing empty non-terminals being included.
   */
#pragma once
#include <optional>

#include "runtime/regex.h"
#include "runtime/diags.h"

namespace oalex::regex {

auto prettyPrint(const Regex& regex) -> std::string;
auto parse(InputDiags& ctx, size_t& i) -> std::optional<Regex>;

// Used for test only. Dies on invalid input.
CharSet parseCharSet(std::string input);
}  // namespace oalex::regex
