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

#include <map>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>
#include "runtime/diags.h"
#include "runtime/skipper.h"
#include "runtime/lookahead_regex.h"
#include "ident.h"
#include "lexer.h"

namespace oalex {

// For now, frontend should restrict wordChars to isprint() && !isspace().
// This will certainly cause problems if we ever want to support utf8, but we
// can cross that bridge when we get there.
struct LexDirective {
  regex::CharSet wordChars;
  Skipper skip;  // Assume skip.valid(), verified at construction.
  bool keepAllNewlines;
};

// TODO Use this in Skipper as well.
struct DelimPair { lex::QuotedString st, en; };
using PartPattern = std::variant<lex::QuotedString, DelimPair>;

auto matchAllParts(const PartPattern& patt, const lex::QuotedString& s)
  -> std::optional<std::vector<std::pair<size_t, size_t>>>;

// Params:
//   s: The string that needs to be split up.
//   partPatterns: The user-specified patterns to be replaced with Placeholders.
//
// On error, we return an empty vector. An empty `s` input will produce a
// vector with a single empty QuotedString.
using LabelOrPart = std::variant<lex::QuotedString, Ident>;
auto labelParts(
    const lex::QuotedString& s,
    const std::map<Ident,PartPattern>& partPatterns)
    -> std::vector<LabelOrPart>;

// Strong typedefs.
struct WordToken : public lex::UnquotedToken {
  explicit WordToken(const lex::QuotedString& s) : UnquotedToken(s) {}
};
struct OperToken : public lex::UnquotedToken {
  explicit OperToken(const lex::QuotedString& s) : UnquotedToken(s) {}
};

using TokenOrPart = std::variant<WordToken, OperToken, lex::NewlineChar, Ident>;
// This is unlikely to be used outside of tokenizeTemplate(), so for now
// it is really only exposed for testing.
// A UnquotedToken can never be empty. The bool for each element indicates
// whether the token should be surrounded by word-boundary anchors when
// matching.
// Return value elements contain either WordToken or OperToken, never Ident.
// Expects unixified linefeeds, since it uses skippers.
auto tokenizeTemplateWithoutLabels(
    const lex::QuotedString& s, const LexDirective& opts,
    std::string_view comment_end_error)
    -> std::vector<TokenOrPart>;

// This function doesn't make sense if we are keeping all spaces.
// Expects unixified linefeeds, since it uses skippers.
auto tokenizeTemplate(
    const std::vector<LabelOrPart>& lblParts,
    const LexDirective& lexopts) -> std::vector<TokenOrPart>;

// Exposed for testing only.
bool hasFusedTemplateOpers(InputDiags& ctx,
                           const std::vector<TokenOrPart>& tops);

using Template = std::variant<
  // TokenOrPart components.
  std::unique_ptr<WordToken>,
  std::unique_ptr<OperToken>,
  std::unique_ptr<lex::NewlineChar>,
  std::unique_ptr<Ident>,
  // Compound components.
  std::unique_ptr<struct TemplateConcat>,
  std::unique_ptr<struct TemplateOrList>,
  std::unique_ptr<struct TemplateOptional>,
  std::unique_ptr<struct TemplateRepeat>,
  std::unique_ptr<struct TemplateFold>
>;

struct TemplateConcat { std::vector<Template> parts; };
struct TemplateOrList { std::vector<Template> parts; };
struct TemplateOptional { Template part; };
struct TemplateRepeat   { Template part; };
struct TemplateFold     { Template part, glue; };

auto templatize(InputDiags& ctx, std::vector<TokenOrPart> tops)
  -> std::optional<Template>;

}  // namespace oalex
