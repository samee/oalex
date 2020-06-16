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

#include "template.h"
#include "lexer.h"
#include "lookahead_regex_io.h"
#include "runtime/input_view.h"
#include "runtime/diags_test_util.h"
#include "runtime/test_util.h"
#include <map>
#include <utility>
using std::get_if;
using std::holds_alternative;
using std::make_unique;
using std::map;
using std::optional;
using std::pair;
using std::string;
using std::string_view;
using std::variant;
using std::vector;
using namespace std::literals::string_literals;
using oalex::Bug;
using oalex::DelimPair;
using oalex::Ident;
using oalex::Input;
using oalex::InputDiags;
using oalex::LexDirective;
using oalex::matchAllParts;
using oalex::PartPattern;
using oalex::Skipper;
using oalex::OperToken;
using oalex::TokenOrPart;
using oalex::WordToken;
using oalex::lex::QuotedString;
using oalex::lex::UnquotedToken;
using oalex::lex::lexQuotedString;
using oalex::regex::parseCharSet;

namespace {

void assertHasSubstr(string_view testName, string_view s,
                     size_t st, string_view t) {
  string_view m = s.substr(st, t.size());
  if(m != t)
    Bug("{}: Matched the wrong part. Found '{}' instead of '{}'",
        testName, m, t);
}

// Maybe move this to runtime/input_view.h if this becomes useful.
size_t findSubstr(const Input& input, string_view s) {
  for(size_t i = 0; input.sizeGt(i); ++i) if(input.hasPrefix(i, s)) return i;
  return string::npos;
}

QuotedString findQuote(string_view testName, InputDiags& ctx,
                       string s) {
  s = '"' + s + '"';
  size_t i = findSubstr(ctx.input, s);
  if(i == string::npos)
    Bug("{}: '{}' not found in test input", testName, s);
  size_t j = i;
  auto res = lexQuotedString(ctx, j);
  assertEmptyDiags(testName, ctx.diags);
  if(!res.has_value() || j != i + s.size())
    Bug("{}: findQuote() called with invalid string {}", testName, s);
  return std::move(*res);
}

/*
This function returns a pair of (unique_ptr<InputDiags>, fquote), where:
  * InputDiags: holds fileBody as a string, surrounded by an extra space
    character on both sides.
  * fquote: a lambda that uses findQuote to extract a QuotedString from the
    returned InputDiags.

Input parameters:
  * testName: used only for diagnostic messages in case something goes wrong.
  * fileBody: Usually a concatenation of various string literals used in a test.

Implementation note:
  * We surround fileBody with spaces to prevent internal markUsed() calls from
    invoking forgetBefore(). This is unnecessary if we are using fileBody
    contents in only a single test, but this allows convenient reuse.
  * We cannot return InputDiags by value, since fquote keeps a reference to it.
*/
auto setupMatchTest(string testName, string fileBody) {
  auto ctx = make_unique<InputDiags>(testInputDiags(" " + fileBody + " "));
  auto fquote = [testName, &ctxref = *ctx](string s) {
    return findQuote(testName, ctxref, std::move(s));
  };
  return make_pair(std::move(ctx), fquote);
}

void testMatchAll() {
  auto [ctx, fquote] = setupMatchTest(__func__, R"(
    "if (cond) { ... } else { ... }"
    "cond"
    "{" "}"
  )");

  const QuotedString tmpl = fquote("if (cond) { ... } else { ... }");

  // Test a single match.
  optional<vector<pair<size_t, size_t>>> out
    = matchAllParts(fquote("cond"), tmpl);

  assertEmptyDiags(__func__, ctx->diags);
  if(!out) BugMe("Couldn't find 'cond' in: {}", string(tmpl));
  if(out->size() != 1)
    BugMe("Was expecting a single match. Found {}", out->size());
  assertHasSubstr(__func__, tmpl, out->at(0).first, "cond");

  out = matchAllParts(DelimPair{fquote("{"), fquote("}")}, tmpl);
  assertEmptyDiags(__func__, ctx->diags);
  if(!out) BugMe("Couldn't find '{ ... }' in: {}", string(tmpl));
  if(out->size() != 2)
    BugMe("Was expecting two matches. Found {}", out->size());
  for(auto& bound : *out) {
    assertHasSubstr(__func__, tmpl, bound.first, "{");
    assertHasSubstr(__func__, tmpl, bound.second-"}"s.size(), "}");
  }
}

void testEmptyPatternsFail() {
  auto [ctx, fquote] = setupMatchTest(__func__, R"("foo" "")");
  QuotedString tmpl = fquote("foo");
  QuotedString empty = fquote("");

  auto res1 = matchAllParts(empty, tmpl);
  if(res1) BugMe("succeeded unexpectedly with an empty pattern");
  assertHasDiagWithSubstr(__func__, ctx->diags,
                          "Placeholder pattern cannot be empty");

  ctx->diags.clear();
  auto res2 = matchAllParts(DelimPair{empty, empty}, tmpl);
  if(res2) BugMe("succeeded unexpectedly with a pair of empty patterns");
  assertHasDiagWithSubstr(__func__, ctx->diags,
                          "Placeholder pattern cannot be empty");
}

void testConfusingPatterns() {
  // Test with something like HTML comments, but end marker is truncated
  // to create confusion about whether the starting delimitter is self-closing.
  auto [ctx, fquote] = setupMatchTest(__func__, R"("foo" "<!--" "--")");
  auto res = matchAllParts(DelimPair{fquote("<!--"), fquote("--")},
                           fquote("foo"));
  assertHasDiagWithSubstr(__func__, ctx->diags,
                          "End pattern is a substring of the start pattern");
}

void testMatchAllFailsOnOverlap() {
  auto [ctx, fquote] = setupMatchTest(__func__,
                                      R"("ababa { { } }" "aba" "{" "}")");
  QuotedString tmpl = fquote("ababa { { } }");

  auto res1 = matchAllParts(fquote("aba"), tmpl);
  if(res1) BugMe("succeeded unexpectedly while matching 'aba'");
  assertHasDiagWithSubstr(__func__, ctx->diags,
                          "Pattern 'aba' matches overlapping segments");

  ctx->diags.clear();
  auto res2 = matchAllParts(DelimPair{fquote("{"), fquote("}")}, tmpl);
  if(res2) BugMe("succeeded unexpectedly while matching '{{ ... }}'");
  assertHasDiagWithSubstr(__func__, ctx->diags,
                          "Pattern '{ ... }' matches overlapping segments");
}

void testUnfinishedMatch() {
  auto [ctx, fquote] = setupMatchTest(__func__, R"("{} {" "{" "}")");
  QuotedString tmpl = fquote("{} {");
  auto res = matchAllParts(DelimPair{fquote("{"), fquote("}")}, fquote("{} {"));
  if(!res) BugMe("Was expecting a match in spite of error");
  assertHasDiagWithSubstr(__func__, ctx->diags, "Unterminated segment");
}

Ident findIdent(string_view testName, InputDiags& ctx, string_view id) {
  size_t i = findSubstr(ctx.input, id);
  if(i == string::npos) Bug("{} not found in {} input", id, testName);
  Ident rv = Ident::parse(ctx, i);
  if(rv.preserveCase() != id)
    Bug("findIdent() cannot perform whole-word matches. Found "
        "{} instead of {}", rv.preserveCase(), id);
  return rv;
}

string debug(const variant<QuotedString,Ident>& lp) {
  if(auto* id = get_if<Ident>(&lp)) return id->preserveCase();
  if(auto* q = get_if<QuotedString>(&lp)) return string(*q);
  Bug("LabelOrPart variant with unknown index {}", lp.index());
}

void testLabelParts() {
  char input[] = R"("if (cond) { ... } else { ... }"
                    "cond" "{" "}" condexpr stmt)";
  auto [ctx, fquote] = setupMatchTest(__func__, input);
  QuotedString tmpl = fquote("if (cond) { ... } else { ... }");
  auto fid = [&](string_view s) {
    return findIdent("testLabelParts", *ctx, s);
  };
  map<Ident,PartPattern> partspec{
    {fid("condexpr"), fquote("cond")},
    {fid("stmt"), DelimPair{fquote("{"), fquote("}")}}};
  vector<variant<QuotedString,Ident>> observed = labelParts(tmpl, partspec);
  vector<variant<QuotedString,Ident>> expected{
    tmpl.subqstr(0,4), fid("condexpr"), tmpl.subqstr(8,2),
    fid("stmt"), tmpl.subqstr(17,6), fid("stmt"),
  };
  if(observed.size() != expected.size())
    BugMe("Expected {} parts, found {}", expected.size(), observed.size());
  for(size_t i=0; i<expected.size(); ++i) if(expected[i] != observed[i])
    BugMe("Failed equality at index {}: {} != {}",
          i, debug(expected[i]), debug(observed[i]));
  if(observed != expected) BugMe("Something didn't match");
}

void testCrossLabelOverlapFails() {
  char input[] = R"( "[ ] [" "[" "]" index index2)";
  auto [ctx, fquote] = setupMatchTest(__func__, input);
  QuotedString tmpl = fquote("[ ] [");
  map<Ident,PartPattern> partspec{
    {findIdent(__func__, *ctx, "index"), DelimPair{fquote("["), fquote("]")}},
    {findIdent(__func__, *ctx, "index2"), DelimPair{fquote("]"), fquote("[")}},
  };
  vector<variant<QuotedString,Ident>> observed = labelParts(tmpl, partspec);
  if(!observed.empty()) BugMe("Was expecting an empty vector on error");
  assertHasDiagWithSubstr(__func__, ctx->diags,
                          "Part '] ... [' overlaps with '[ ... ]'");
}

void testNoMatchWarns() {
  auto [ctx, fquote] = setupMatchTest(__func__, R"(" " "foo")");
  QuotedString tmpl = fquote(" ");
  map<Ident,PartPattern> partspec{
    {findIdent(__func__, *ctx, "foo"), fquote("foo")},
  };
  vector<variant<QuotedString,Ident>> observed = labelParts(tmpl, partspec);
  vector<variant<QuotedString,Ident>> expected{tmpl};
  if(observed != expected) BugMe("Didn't get the unsplit string");
  assertHasDiagWithSubstr(__func__, ctx->diags,
                          "No match found for pattern 'foo'");
}

void testEmptySuccess() {
  auto [ctx, fquote] = setupMatchTest(__func__, R"("")");
  vector observed = labelParts(fquote(""), {});
  if(observed.empty()) BugMe("Failed on empty input");
  if(observed.size() != 1)
    BugMe("Split empty string to get {} pieces, was expecting 1",
          observed.size());
  if(debug(observed[0]) != "")
    BugMe("Split empty string to obtain non-empty output: {}",
          debug(observed[0]));
}

string_view token(string_view testName, const TokenOrPart& tok) {
  if(auto* w = get_if<WordToken>(&tok)) return w->token;
  if(auto* o = get_if<OperToken>(&tok)) return o->token;
  Bug("{}: input has index {}, which is neither a word nor a token",
      testName, tok.index());
}

bool isWord(string_view testName, const TokenOrPart& tok) {
  if(holds_alternative<WordToken>(tok)) return true;
  if(holds_alternative<OperToken>(tok)) return false;
  Bug("{}: input has index {}, which is neither a word nor a token",
      testName, tok.index());
}

void testTokenizeNoLabel() {
  LexDirective lexopts{parseCharSet("[_a-zA-Z]"),
                       Skipper{ {{"/*","*/"},{"//","\n"}}, {} }};
  auto [ctx, fquote] = setupMatchTest(__func__, R"("def foo(args): //\n")");
  vector<TokenOrPart> observed =
    tokenizeTemplateWithoutLabels(fquote("def foo(args): //\\n"), lexopts);
  vector<string> expected{"def", "foo", "(", "args", "):"};
  vector<string> observed_strings;
  for(const auto& tok : observed)
    observed_strings.push_back(string(token(__func__, tok)));
  if(expected != observed_strings)
    BugMe("Tokenization produced {} != {}", observed_strings, expected);
  for(const auto& tok : observed) {
    bool isword = isWord(__func__, tok);
    if(isword != matchesCharSet(token(__func__, tok)[0], lexopts.wordChars))
      BugMe("'{}' {} expected to be a word, but it {} found to be so",
            token(__func__, tok),
            isword ? "wasn't" : "was", isword ? "was" : "wasn't");
  }
}

// TODO Test unfinished comment

}  // namespace

int main() {
  testMatchAll();
  testEmptyPatternsFail();
  testConfusingPatterns();
  testMatchAllFailsOnOverlap();
  testUnfinishedMatch();
  testLabelParts();
  testCrossLabelOverlapFails();
  testNoMatchWarns();
  testEmptySuccess();
  testTokenizeNoLabel();
}
