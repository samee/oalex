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

// For now, frontend should restrict wordChars to isprint() && !isspace().
// This will certainly cause problems if we ever want to support utf8, but we
// can cross that bridge when we get there.
struct LexDirective {
  RegexCharSet wordChars;
  Skipper skip;  // Assume skip.valid(), verified at construction.
  bool keepAllNewlines;  // Dev-note: see comment for trimNewlines()
};

// TODO Use this in Skipper as well.
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
using LabelOrPart = std::variant<lex::GluedString, Ident>;
auto labelParts(
    DiagsDest ctx,
    const lex::GluedString& s,
    const std::map<Ident,PartPattern>& partPatterns,
    const RegexCharSet& wordChars)
    -> std::vector<LabelOrPart>;

// Strong typedefs.
struct WordToken : public WholeSegment {
  explicit WordToken(const WholeSegment& s) : WholeSegment(s) {}
};
struct OperToken : public WholeSegment {
  explicit OperToken(const WholeSegment& s) : WholeSegment(s) {}
};

using TokenOrPart = std::variant<WordToken, OperToken, lex::NewlineChar, Ident>;
// This is unlikely to be used outside of tokenizePattern(), so for now
// it is really only exposed for testing.
// A WholeSegment can never be empty. The bool for each element indicates
// whether the token should be surrounded by word-boundary anchors when
// matching.
// Return value elements contain either WordToken or OperToken, never Ident.
// Expects unixified linefeeds, since it uses skippers.
auto tokenizePatternWithoutLabels(
    DiagsDest ctx,
    const lex::GluedString& s, const LexDirective& opts,
    std::string_view comment_end_error)
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
