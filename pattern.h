/*  Copyright 2020-2021 The oalex authors.

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
#include <map>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>
#include "runtime/diags.h"
#include "runtime/skipper.h"
#include "runtime/regex.h"
#include "ident.h"
#include "lexer.h"

// TODO s/\<size_t/ssize_t/g
namespace oalex {

/* Dev-note: I would have preferred if tailcont didn't make an appearance in
   pattern.{h,cpp} at all. We really need this only in the frontend.cpp and
   compiler.cpp, where this can be used to trim off the last newline. This
   is needlessly expanding the API surface. */
struct LexDirective {
  RegexCharSet wordChars;
  Skipper skip;  // Assume skip.valid(), verified at construction.
  bool tailcont;
};

// Empty on success, error message on failure. Doesn't accept DiagsDest because
// we've already lost the wordChars don't contain location. And I don't want
// stPos or enPos parameters either, because that would be confusing.
// For now, this just restricts wordChars to isprint() && !isspace().
// This will certainly be insufficient if we ever want to support utf8, but we
// can cross that bridge when we get there.
std::string validateWordChars(const RegexCharSet& wordChars);

// TODO Use this in Skipper as well.
// Dev-note: We cannot use WholeSegment here, since the user does sometimes
// have to use escape characters. Like '\"'. The PartPattern often has to refer
// to this escaped version before they are found in a pattern.
struct DelimPair { lex::GluedString st, en; };
using PartPattern = std::variant<lex::GluedString, DelimPair>;

auto matchAllParts(DiagsDest ctx,
                   const PartPattern& patt, const lex::GluedString& s)
  -> std::optional<std::vector<std::pair<size_t, size_t>>>;

// Params:
//   s: The string that needs to be split up.
//   partPatterns: The user-specified patterns to be replaced with Placeholders.
//   wordChars: To disallow the returned placeholders from splitting up a
//     word into multiple pieces. It can still combine multiple words and other
//     symbols into a single placeholder. Use {} to not perform any checks.
//
// On error, we return an empty vector. An empty `s` input will produce a
// vector with a single empty GluedString.
// Note: this function can be used either on patterns that skip over
// whitespaces, or those who do not.
using LabelOrPart = std::variant<std::pair<size_t,size_t>, Ident>;
auto labelParts(
    DiagsDest ctx,
    const lex::GluedString& s,
    const std::map<Ident,PartPattern>& partPatterns,
    const RegexCharSet& wordChars)
    -> std::vector<LabelOrPart>;

// Strong typedefs. The distinction between a WordToken and an OperToken is
// really about whether they need to be surrounded by word boundaries when
// being matched.
struct WordToken : public WholeSegment {
  explicit WordToken(const WholeSegment& s) : WholeSegment(s) {}
};
struct OperToken : public WholeSegment {
  explicit OperToken(const WholeSegment& s) : WholeSegment(s) {}
};

using TokenOrPart = std::variant<WordToken, OperToken, lex::NewlineChar, Ident>;
// This is unlikely to be used outside of tokenizePattern(), so for now
// it is really only exposed for testing. Never produces a zero-sized token.
// Return value elements never contain an Ident, hence the "WithoutLabel"
// part of the name.
// Expects unixified linefeeds, since it uses skippers.
auto tokenizePatternWithoutLabels(
    DiagsDest ctx, const lex::GluedString& s,
    const std::pair<size_t,size_t>& locpair,
    const LexDirective& opts, std::string_view comment_end_error)
    -> std::vector<TokenOrPart>;

// This function doesn't make sense if we are keeping all spaces.
// Expects unixified linefeeds, since it uses skippers.
auto tokenizePattern(DiagsDest ctx, const lex::GluedString& s,
                     const std::map<Ident,PartPattern>& partPatterns,
                     const LexDirective& lexopts) -> std::vector<TokenOrPart>;

// Exposed for testing only.
bool hasFusedPatternOpers(DiagsDest ctx,
                          const std::vector<TokenOrPart>& tops);

using Pattern = std::variant<
  // TokenOrPart components.
  std::unique_ptr<WordToken>,
  std::unique_ptr<OperToken>,
  std::unique_ptr<lex::NewlineChar>,
  std::unique_ptr<Ident>,
  // Compound components.
  std::unique_ptr<struct PatternConcat>,
  std::unique_ptr<struct PatternOrList>,
  std::unique_ptr<struct PatternOptional>,
  std::unique_ptr<struct PatternRepeat>,
  std::unique_ptr<struct PatternFold>
>;

struct PatternConcat { std::vector<Pattern> parts; };
struct PatternOrList { std::vector<Pattern> parts; };
struct PatternOptional { Pattern part; };
struct PatternRepeat   { Pattern part; };
struct PatternFold     { Pattern part, glue; };

auto parsePattern(DiagsDest ctx, std::vector<TokenOrPart> tops)
  -> std::optional<Pattern>;

// parsePattern() helper, exposed for testing.
struct RolloutEllipsisForTestResult {
  std::string expr, part, glue, err;
};
RolloutEllipsisForTestResult rolloutEllipsisForTest(std::string s);

}  // namespace oalex
