/*  Copyright 2019-2024 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "pattern.h"
#include "lexer.h"
#include "regex_io.h"
#include "runtime/input_view.h"
#include "runtime/test_util.h"
#include "runtime/util.h"
#include <format>
#include <map>
#include <utility>
using std::format;
using std::get_if;
using std::holds_alternative;
using std::make_tuple;
using std::make_unique;
using std::map;
using std::optional;
using std::nullopt;
using std::pair;
using std::size;
using std::string;
using std::string_view;
using std::unique_ptr;
using std::variant;
using std::vector;
using namespace std::literals::string_literals;
using oalex::assertEmptyDiags;
using oalex::assertEqual;
using oalex::assertHasDiagWithSubstr;
using oalex::Bug;
using oalex::DelimPair;
using oalex::get_if_unique;
using oalex::Ident;
using oalex::Input;
using oalex::InputDiags;
using oalex::InputPiece;
using oalex::isSubstr;
using oalex::LabelOrPart;
using oalex::labelParts;
using oalex::LexDirective;
using oalex::matchAllParts;
using oalex::OperToken;
using oalex::parsePattern;
using oalex::parseRegexCharSet;
using oalex::PartPattern;
using oalex::Pattern;
using oalex::PatternConcat;
using oalex::PatternOrList;
using oalex::PatternOptional;
using oalex::PatternRepeat;
using oalex::PatternFold;
using oalex::RegexCharSet;
using oalex::rolloutEllipsisForTest;
using oalex::RolloutEllipsisForTestResult;
using oalex::showDiags;
using oalex::Skipper;
using oalex::TokenOrPart;
using oalex::Unimplemented;
using oalex::WholeSegment;
using oalex::WordToken;
using oalex::lex::lexIndentedSource;
using oalex::lex::NewlineChar;
using oalex::lex::GluedString;
using oalex::lex::lexQuotedString;

namespace {

void assertHasSubstr(string_view testName, string_view s,
                     size_t st, string_view t) {
  string_view m = s.substr(st, t.size());
  if(m != t)
    Bug("{}: Matched the wrong part. Found '{}' instead of '{}'",
        testName, m, t);
}

// Maybe move this to runtime/input_view.h if this becomes useful.
size_t findSubstr(const InputPiece& input, string_view s) {
  for(size_t i = 0; input.sizeGt(i); ++i) if(input.hasPrefix(i, s)) return i;
  return string::npos;
}

GluedString findQuote(string_view testName, InputDiags& ctx,
                       string s) {
  s = '"' + s + '"';
  size_t i = findSubstr(ctx.input(), s);
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
  auto ctx = make_unique<InputDiags>(Input{" " + fileBody + " "});
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

  const GluedString patt = fquote("if (cond) { ... } else { ... }");

  // Test a single match.
  optional<vector<pair<size_t, size_t>>> out
    = matchAllParts(*ctx, fquote("cond"), patt);

  assertEmptyDiags(__func__, ctx->diags);
  if(!out) BugMe("Couldn't find 'cond' in: {}", string(patt));
  if(out->size() != 1)
    BugMe("Was expecting a single match. Found {}", out->size());
  assertHasSubstr(__func__, patt, out->at(0).first, "cond");

  out = matchAllParts(*ctx, DelimPair{fquote("{"), fquote("}")}, patt);
  assertEmptyDiags(__func__, ctx->diags);
  if(!out) BugMe("Couldn't find '{ ... }' in: {}", string(patt));
  if(out->size() != 2)
    BugMe("Was expecting two matches. Found {}", out->size());
  for(auto& bound : *out) {
    assertHasSubstr(__func__, patt, bound.first, "{");
    assertHasSubstr(__func__, patt, bound.second-"}"s.size(), "}");
  }
}

void testEmptyPatternsFail() {
  auto [ctx, fquote] = setupMatchTest(__func__, R"("foo" "")");
  GluedString patt = fquote("foo");
  GluedString empty = fquote("");

  auto res1 = matchAllParts(*ctx, empty, patt);
  if(res1) BugMe("succeeded unexpectedly with an empty pattern");
  assertHasDiagWithSubstr(__func__, ctx->diags,
                          "Placeholder pattern cannot be empty");

  ctx->diags.clear();
  auto res2 = matchAllParts(*ctx, DelimPair{empty, empty}, patt);
  if(res2) BugMe("succeeded unexpectedly with a pair of empty patterns");
  assertHasDiagWithSubstr(__func__, ctx->diags,
                          "Placeholder pattern cannot be empty");
}

void testConfusingPatterns() {
  // Test with something like HTML comments, but end marker is truncated
  // to create confusion about whether the starting delimitter is self-closing.
  auto [ctx, fquote] = setupMatchTest(__func__, R"("foo" "<!--" "--")");
  auto res = matchAllParts(*ctx, DelimPair{fquote("<!--"), fquote("--")},
                           fquote("foo"));
  assertHasDiagWithSubstr(__func__, ctx->diags,
                          "End pattern is a substring of the start pattern");
}

void testMatchAllFailsOnOverlap() {
  auto [ctx, fquote] = setupMatchTest(__func__,
                                      R"("ababa { { } }" "aba" "{" "}")");
  GluedString patt = fquote("ababa { { } }");

  auto res1 = matchAllParts(*ctx, fquote("aba"), patt);
  if(res1) BugMe("succeeded unexpectedly while matching 'aba'");
  assertHasDiagWithSubstr(__func__, ctx->diags,
                          "Pattern 'aba' matches overlapping segments");

  ctx->diags.clear();
  auto res2 = matchAllParts(*ctx, DelimPair{fquote("{"), fquote("}")}, patt);
  if(res2) BugMe("succeeded unexpectedly while matching '{{ ... }}'");
  assertHasDiagWithSubstr(__func__, ctx->diags,
                          "Pattern '{ ... }' matches overlapping segments");
}

void testUnfinishedMatch() {
  auto [ctx, fquote] = setupMatchTest(__func__, R"("{} {" "{" "}")");
  GluedString patt = fquote("{} {");
  auto res = matchAllParts(*ctx, DelimPair{fquote("{"), fquote("}")},
                           fquote("{} {"));
  if(!res) BugMe("Was expecting a match in spite of error");
  assertHasDiagWithSubstr(__func__, ctx->diags, "Unterminated segment");
}

Ident findIdent(string_view testName, InputDiags& ctx, string_view id) {
  for(size_t i=0; ctx.input().sizeGt(i); ++i) if(ctx.input().hasPrefix(i, id)) {
    size_t j = i;
    Ident rv = Ident::parse(ctx, j);
    if(rv.preserveCase() == id) return rv;
  }
  Bug("Word {} not found in {} input", id, testName);
}

auto setupLabelTest(string testName, string fileBody) {
  auto [ctx, fquote] = setupMatchTest(testName, fileBody);
  auto fid = [testName, &ctxref = *ctx](string_view s) {
    return findIdent(testName, ctxref, s);
  };
  return make_tuple(std::move(ctx), fquote, fid);
}

string debug(const LabelOrPart& lp) {
  if(auto* id = get_if<Ident>(&lp)) return id->preserveCase();
  if(auto* q = get_if<pair<size_t,size_t>>(&lp))
    return std::format("{{{}, {}}}", q->first, q->second);
  Bug("LabelOrPart variant with unknown index {}", lp.index());
}

void testLabelParts() {
  char input[] = R"("if (cond) { ... } else { ... }"
                    "cond" "{" "}" condexpr stmt)";
  auto [ctx, fquote, fid] = setupLabelTest(__func__, input);
  GluedString patt = fquote("if (cond) { ... } else { ... }");
  map<Ident,PartPattern> partspec{
    {fid("condexpr"), fquote("cond")},
    {fid("stmt"), DelimPair{fquote("{"), fquote("}")}}};
  vector<LabelOrPart> observed
    = labelParts(*ctx, patt, partspec, {});
  vector<LabelOrPart> expected{
    pair{0,4}, fid("condexpr"), pair{8,10},
    fid("stmt"), pair{17,23}, fid("stmt"),
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
  GluedString patt = fquote("[ ] [");
  map<Ident,PartPattern> partspec{
    {findIdent(__func__, *ctx, "index"), DelimPair{fquote("["), fquote("]")}},
    {findIdent(__func__, *ctx, "index2"), DelimPair{fquote("]"), fquote("[")}},
  };
  vector<LabelOrPart> observed
    = labelParts(*ctx, patt, partspec, {});
  if(!observed.empty()) BugMe("Was expecting an empty vector on error");
  assertHasDiagWithSubstr(__func__, ctx->diags,
                          "Part '] ... [' overlaps with '[ ... ]'");
}

void testEmptySuccess() {
  auto [ctx, fquote] = setupMatchTest(__func__, R"("")");
  vector observed = labelParts(*ctx, fquote(""), {}, {});
  if(observed.empty()) BugMe("Failed on empty input");
  if(observed.size() != 1)
    BugMe("Split empty string to get {} pieces, was expecting 1",
          observed.size());
  if(observed[0] != LabelOrPart{pair{0, 0}})
    BugMe("Split empty string to obtain non-empty output: {}",
          debug(observed[0]));
}

void testNoWordSplit() {
  auto [ctx, fquote, fid] = setupLabelTest(__func__,
      R"("foobar" "w-foo-bar-w" "foo" "bar" "-foo-")");
  GluedString patt = fquote("foobar");
  RegexCharSet wordChars = parseRegexCharSet("[_a-zA-Z]");
  map<Ident,PartPattern> partspec{ {fid("foo"), fquote("foo")} };
  vector<LabelOrPart> observed = labelParts(*ctx, patt, partspec, wordChars);
  vector<LabelOrPart> expected_no_part = { LabelOrPart{pair{0, 6}} };
  assertEmptyDiags(__func__, ctx->diags);
  if(observed != expected_no_part)
    Bug("Word with run-on suffix was not disregarded");

  ctx->diags.clear();
  // patt unchanged
  partspec = { {fid("bar"), fquote("bar")} };
  observed = labelParts(*ctx, patt, partspec, wordChars);
  assertEmptyDiags(__func__, ctx->diags);
  if(observed != expected_no_part)
    Bug("Word with run-on prefix was not disregarded");

  ctx->diags.clear();
  patt = fquote("w-foo-bar-w");
  partspec = { {fid("foo"), fquote("foo")}, {fid("bar"), fquote("bar")} };
  labelParts(*ctx, patt, partspec, wordChars);
  assertEmptyDiags(__func__, ctx->diags);

  ctx->diags.clear();
  // patt unchanged
  partspec = { {fid("foo"), fquote("-foo-")}, {fid("bar"), fquote("bar")} };
  labelParts(*ctx, patt, partspec, wordChars);
  assertEmptyDiags(__func__, ctx->diags);
}

string_view token(string_view testName, const TokenOrPart& tok) {
  if(auto* w = get_if<WordToken>(&tok)) return **w;
  if(auto* o = get_if<OperToken>(&tok)) return **o;
  Bug("{}: input has index {}, which is neither a word nor a token",
      testName, tok.index());
}

bool isWord(string_view testName, const TokenOrPart& tok) {
  if(holds_alternative<WordToken>(tok)) return true;
  if(holds_alternative<OperToken>(tok)) return false;
  Bug("{}: input has index {}, which is neither a word nor a token",
      testName, tok.index());
}

LexDirective mkLineLexOpts(LexDirective lexopts) {
  lexopts.skip.newlines = Skipper::Newlines::keep_all;
  return lexopts;
}
const LexDirective lexopts{.wordChars = parseRegexCharSet("[_a-zA-Z]"),
                           .skip{ {{"/*","*/"},{"//","\n"}}, {} },
                           .tailcont = false};
const LexDirective linelexopts = mkLineLexOpts(lexopts);

void testTokenizeNoLabel() {
  auto [ctx, fquote] = setupMatchTest(__func__, R"("def foo(args): //\n")");
  GluedString patt = fquote("def foo(args): //\\n");
  vector<TokenOrPart> observed = tokenizePatternWithoutLabels(*ctx,
      patt, {0,patt.size()}, lexopts, "");
  vector<string> expected{"def", "foo", "(", "args", "):"};
  vector<string> observed_strings;
  for(const auto& tok : observed)
    observed_strings.push_back(string(token(__func__, tok)));
  if(expected != observed_strings)
    BugMe("Tokenization produced {} != {}", observed_strings, expected);
  for(const auto& tok : observed) {
    bool isword = isWord(__func__, tok);
    if(isword != matchesRegexCharSet(token(__func__, tok)[0],
                                     lexopts.wordChars))
      BugMe("'{}' {} expected to be a word, but it {} found to be so",
            token(__func__, tok),
            isword ? "wasn't" : "was", isword ? "was" : "wasn't");
  }
}

void testTokenizeNoLabelRunoffComment() {
  auto [ctx, fquote] = setupMatchTest(__func__, R"("def foo(args): //\n")");
  GluedString qs = fquote("def foo(args): //\\n");
  qs = qs.subqstr(0, qs.size()-1);  // Remove the last newline
  tokenizePatternWithoutLabels(*ctx, qs, {0, qs.size()}, lexopts,
                               "Missing newline");
  assertHasDiagWithSubstr(__func__, ctx->diags, "Missing newline");
}

vector<string> debugTokens(const vector<TokenOrPart>& tops) {
  vector<string> rv;
  for(const auto& top : tops) {
    if(auto* w = get_if<oalex::WordToken>(&top))
      rv.push_back("word:" + **w);
    else if(auto* op = get_if<oalex::OperToken>(&top))
      rv.push_back("oper:" + **op);
    else if(auto* id = get_if<Ident>(&top))
      rv.push_back("ident:" + id->preserveCase());
    else if(holds_alternative<NewlineChar>(top))
      rv.push_back("newline");
    else BugMe("Unknown TokenOrPart index {}", top.index());
  }
  return rv;
}

void testTokenizeSuccess() {
  char input[] = R"("if (cond) { ... } else { ... }"
                    where:   # This 'where' is actually ignored
                      condexpr: "cond"
                      stmt: "{" "}")";
  auto [ctx, fquote, fid] = setupLabelTest(__func__, input);
  GluedString patt = fquote("if (cond) { ... } else { ... }");
  map<Ident,PartPattern> partspec{
    {fid("condexpr"), fquote("cond")},
    {fid("stmt"), DelimPair{fquote("{"), fquote("}")}}};
  vector<string> observed
    = debugTokens(tokenizePattern(*ctx, patt, partspec, lexopts));
  vector<string> expected {"word:if", "oper:(", "ident:condexpr", "oper:)",
                           "ident:stmt", "word:else", "ident:stmt"};
  if(observed != expected) BugMe("{} != {}", observed, expected);
}

void testTokenizeLabelInComment() {
  char input[] = R"("if (condition) stmt;  // Test 'if' condition"
                     where:
                       condexpr: "condition"
                       stmt: "stmt")";
  auto [ctx, fquote, fid] = setupLabelTest(__func__, input);
  GluedString patt = fquote("if (condition) stmt;  // Test 'if' condition");
  map<Ident,PartPattern> partspec{
    {fid("condexpr"), fquote("condition")},
    {fid("stmt"), fquote("stmt")}};
  tokenizePattern(*ctx, patt, partspec, lexopts);
  assertHasDiagWithSubstr(__func__, ctx->diags,
                          "Placeholders not allowed in comments");

  // Now try with linelexopts.
  ctx->diags.clear();
  tokenizePattern(*ctx, patt, partspec, linelexopts);
  assertHasDiagWithSubstr(__func__, ctx->diags,
                          "Placeholders not allowed in comments");
}

void testTokenizeRunoffComment() {
  char input[] = R"("stmt; // comment")";
  auto [ctx, fquote] = setupMatchTest(__func__, input);
  GluedString patt = fquote("stmt; // comment");
  tokenizePattern(*ctx, patt, {}, lexopts);
  assertHasDiagWithSubstr(__func__, ctx->diags, "Comment never ends");

  // Now try with linelexopts.
  ctx->diags.clear();
  tokenizePattern(*ctx, patt, {}, linelexopts);
  assertHasDiagWithSubstr(__func__, ctx->diags, "Comment never ends");
}

size_t lineStart(size_t lineno, const InputPiece& input) {
  size_t i=0;
  while(lineno--) {
    i = input.find('\n', i);
    if(i == input.npos) return i;
    else ++i;
  }
  return i;
}

GluedString assertSuccessfulParse(string_view testName, InputDiags& ctx,
                                   size_t pos, string_view parindent) {
  auto opt = lexIndentedSource(ctx, pos, parindent);
  if(!opt.has_value()) {
    showDiags(ctx.diags);
    Bug("{}: pattern was not parsed properly", testName);
  }
  return std::move(*opt);
}

void testParaBreaks() {
  LexDirective paralexopts = lexopts;
  paralexopts.skip.newlines = Skipper::Newlines::keep_para;

  char input[] = R"(
  let:     # This 'let' is actually ignored.
    directives

    // some comment

    loren
    // Comment in para
    ipsum
    dolor
  where:   # This is also ignored
    directives: "directives"
  )";
  auto [ctx, fquote, fid] = setupLabelTest(__func__, input);
  GluedString patt
    = assertSuccessfulParse(__func__, *ctx, lineStart(2,ctx->input()), "    ");
  map<Ident,PartPattern> partspec{ {fid("directives"), fquote("directives")} };
  vector<string> observed
    = debugTokens(tokenizePattern(*ctx, patt, partspec, paralexopts));
  vector<string> expected {"ident:directives", "newline", "word:loren",
                           "word:ipsum", "word:dolor"};
  if(observed != expected) BugMe("{} != {}", observed, expected);
}

void testKeepAllNewlines() {
  char input[] = R"(
  let:
    timestamp: <epoch_time>

    // Paid today
    transaction:
      paid_to: landlord
      amount: $1000
  where:
    timestamp_sec: "<epoch_time>"
    recipient: "landlord"
    amount: "$1000"
  )";
  auto [ctx, fquote, fid] = setupLabelTest(__func__, input);
  GluedString patt
    = assertSuccessfulParse(__func__, *ctx, lineStart(2,ctx->input()), "    ");
  map<Ident,PartPattern> partspec{
    {fid("timestamp_sec"), fquote("<epoch_time>")},
    {fid("recipient"), fquote("landlord")},
    {fid("amount"), fquote("$1000")},
  };
  vector<string> observed
    = debugTokens(tokenizePattern(*ctx, patt, partspec, linelexopts));
  vector<string> expected {
    "word:timestamp", "oper::", "ident:timestamp_sec", "newline",
    "newline",  // Consequtive newlines not collapsed.
    "newline",  // Comments correctly ignored.
    "word:transaction", "oper::", "newline",
    "word:paid_to", "oper::", "ident:recipient", "newline",
    "word:amount", "oper::", "ident:amount", "newline",
  };
  if(observed != expected)
    BugMe("\n{}\nis not equal to \n{}", observed, expected);
}

void testTailcont() {
  char input[] = R"(
    {
      lineA
      lineB
      lineC
    }

  )";
  auto [ctx, fquote, fid] = setupLabelTest(__func__, input);
  GluedString patt
    = assertSuccessfulParse(__func__, *ctx, lineStart(1,ctx->input()), "    ");
  vector<string> observed
    = debugTokens(tokenizePattern(*ctx, patt, {}, linelexopts));
  vector<string> expected {
    "oper:{", "newline",
    "word:lineA", "newline",
    "word:lineB", "newline",
    "word:lineC", "newline",
    "oper:}", "newline", "newline",
  };
  if(observed != expected)
    BugMe("\n{}\n is not equal to \n{}", observed, expected);

  assertEqual("Expectation should end in a newline",
              expected.back(), "newline");
  while(expected.back() == "newline") expected.pop_back();
  LexDirective tail_continues = linelexopts;
  tail_continues.tailcont = true;
  observed = debugTokens(tokenizePattern(*ctx, patt, {}, tail_continues));
  if(observed != expected)
    BugMe("\n{}\n is not equal to \n{}", observed, expected);
}

void testNoEndingNewline() {
  char input[] = R"("foo bar /* baz */")";
  auto [ctx, fquote, fid] = setupLabelTest(__func__, input);
  GluedString s = fquote("foo bar /* baz */");
  vector<string> observed
    = debugTokens(tokenizePattern(*ctx, s, {}, linelexopts));
  vector<string> expected{"word:foo", "word:bar"};
  if(observed != expected) BugMe("{} != {}", observed, expected);
}

void testUnmarkedPatternOpers() {
  char input[] = R"(
  let expr:
    [a, ... , elt]                  // Okay, parsed as pattern operators
    [sql| select * from mytable |]  // Error if unmarked.
    expr .... expr                  // Four dots to provoke errors.
  where:
    GhcQuasiQuotes: "[sql|" "|]"
    LongEllipsis: "...."
  )";
  auto [ctx, fquote, fid] = setupLabelTest(__func__, input);
  GluedString patt
    = assertSuccessfulParse(__func__, *ctx, lineStart(2,ctx->input()), "    ");
  map<Ident,PartPattern> partspec{
    {fid("GhcQuasiQuotes"), DelimPair{fquote("[sql|"), fquote("|]")}},
    {fid("LongEllipsis"), fquote("....")},
  };
  vector<TokenOrPart> tops
    = tokenizePattern(*ctx, patt, partspec, lexopts);
  if(hasFusedPatternOpers(*ctx, tops)) {
    showDiags(ctx->diags);
    BugMe("Unexpectedly found fused patterns even with operators marked");
  }

  // Try again with no markings.
  tops = tokenizePattern(*ctx, patt, {}, lexopts);
  if(!hasFusedPatternOpers(*ctx, tops))
    BugMe("Failed to detect pattern operators");
  assertHasDiagWithSubstr(__func__, ctx->diags, "Token '|]' incorporates '|'");
}

class PatternMatcher {
 public:
  enum class Type { leafToken, orList, concat, optional, repeat, fold };
  PatternMatcher() = default;
  PatternMatcher(const char* d) : token_(d) {}  // implicit ctor
  PatternMatcher(string_view d) : token_(d) {}  // implicit ctor
  PatternMatcher(Type t, vector<PatternMatcher> children)
    : type_(t), children_(std::move(children)) {}
  Type type() const { return type_; }
  friend auto match(const PatternMatcher& m, const Pattern& p)
    -> optional<string>;
 private:
  Type type_ = Type::leafToken;
  // Valid iff type_ == leafToken:
  string_view token_;

  // Valid iff type_ != leafToken.
  //   size() is arbitrary but non-zero for type_ == orList or concat.
  //   size() == 1 for type_ == optional or repeat
  //   size() == 2 for type_ == fold
  vector<PatternMatcher> children_;
};

template <class ... Args> PatternMatcher concatMatcher(Args ... args) {
  return PatternMatcher(PatternMatcher::Type::concat, {args...});
}
template <class ... Args> PatternMatcher orListMatcher(Args ... args) {
  return PatternMatcher(PatternMatcher::Type::orList, {args...});
}
PatternMatcher optionalMatcher(PatternMatcher m) {
  return PatternMatcher(PatternMatcher::Type::optional, {m});
}
PatternMatcher repeatMatcher(PatternMatcher m) {
  return PatternMatcher(PatternMatcher::Type::repeat, {m});
}
PatternMatcher foldMatcher(PatternMatcher part, PatternMatcher glue) {
  return PatternMatcher(PatternMatcher::Type::fold, {part,glue});
}

template <class P>
bool hasType(const Pattern& p) { return holds_alternative<unique_ptr<P>>(p); }

string_view debugMatcherType(const PatternMatcher& m) {
  switch(m.type()) {
    case PatternMatcher::Type::leafToken: return "leafToken";
    case PatternMatcher::Type::orList:    return "orList";
    case PatternMatcher::Type::concat:    return "concat";
    case PatternMatcher::Type::optional:  return "optional";
    case PatternMatcher::Type::repeat:    return "repeat";
    case PatternMatcher::Type::fold:      return "fold";
    default: Bug("Unknown matcher type {}", static_cast<int>(m.type()));
  }
}
string_view debugType(const Pattern& p) {
  if(hasType<WordToken>(p))   return "WordToken";
  if(hasType<OperToken>(p))   return "OperToken";
  if(hasType<PatternConcat>(p))   return "PatternConcat";
  if(hasType<PatternOrList>(p))   return "PatternOrList";
  if(hasType<PatternOptional>(p)) return "PatternOptional";
  Bug("Unknown Pattern type of index {}", p.index());
}

auto match(const PatternMatcher& m, const Pattern& p) -> optional<string> {
  // Helpers.
  auto typeError = [&p](string_view expected) {
    return format("Expected {}, received {}", expected, debugType(p));
  };
  auto vectorMatch = [](const vector<PatternMatcher>& vm,
                        const vector<Pattern>& vt) -> optional<string> {
    if(vm.size() != vt.size())
      return format("Expected {} children, got {}", vm.size(), vt.size());
    for(size_t i=0; i<vm.size(); ++i)
      if(auto err = match(vm[i], vt[i])) return err;
    return nullopt;
  };
  auto checkChildCount = [](const PatternMatcher& m,
                            size_t count) -> optional<string> {
    if(m.children_.size() != count)
      return format("{} matcher should have {} children, found {}",
                    debugMatcherType(m), count, m.children_.size());
    else return nullopt;
  };

  if(m.type_ == PatternMatcher::Type::leafToken) {
    const WholeSegment* tok = get_if_unique<WordToken>(&p);
    if(!tok) tok = get_if_unique<OperToken>(&p);
    if(!tok) return typeError("leaf token");
    if(**tok != m.token_)
      return format("Failed to match '{}' with '{}'", m.token_, **tok);
  }else if(m.type_ == PatternMatcher::Type::concat) {
    auto* concat = get_if_unique<PatternConcat>(&p);
    if(!concat) return typeError("concat list");
    return vectorMatch(m.children_, concat->parts);
  }else if(m.type_ == PatternMatcher::Type::orList) {
    auto* orList = get_if_unique<PatternOrList>(&p);
    if(!orList) return typeError("OR list");
    return vectorMatch(m.children_, orList->parts);
  }else if(m.type_ == PatternMatcher::Type::optional) {
    if(auto err = checkChildCount(m, 1)) return err;
    auto* opt = get_if_unique<PatternOptional>(&p);
    if(!opt) return typeError("optional node");
    return match(m.children_[0], opt->part);
  }else if(m.type_ == PatternMatcher::Type::repeat) {
    if(auto err = checkChildCount(m, 1)) return err;
    auto *rep = get_if_unique<PatternRepeat>(&p);
    if(!rep) return typeError("repeat node");
    return match(m.children_[0], rep->part);
  }else if(m.type_ == PatternMatcher::Type::fold) {
    if(auto err = checkChildCount(m, 2)) return err;
    auto *fold = get_if_unique<PatternFold>(&p);
    if(!fold) return typeError("fold node");
    if(auto err = match(m.children_[0], fold->part)) return err;
    else return match(m.children_[1], fold->glue);
  }else Unimplemented("Matching for matcher type '{}'", debugMatcherType(m));
  return nullopt;
}

void testPatternSimpleConcat() {
  // Setup
  char input[] = R"("foo + bar" "foo" "+" "bar")";
  auto [ctx, fquote] = setupMatchTest(__func__, input);
  GluedString s = fquote("foo + bar");
  vector<TokenOrPart> tops = tokenizePattern(*ctx, s, {}, lexopts);
  if(hasFusedPatternOpers(*ctx, tops)) BugMe("Input has fused metachars");

  // Test subject
  optional<Pattern> observed = parsePattern(*ctx, tops);

  // Expectations
  if(!observed.has_value()) {
    showDiags(ctx->diags);
    BugMe("Pattern-making failed");
  }
  PatternMatcher expected = concatMatcher("foo", "+", "bar");
  if(auto err = match(expected, *observed)) BugMe("{}", *err);
}

void testPatternSingleConcat() {
  // Setup
  char input[] = R"("foo")";
  auto [ctx, fquote] = setupMatchTest(__func__, input);
  GluedString s = fquote("foo");
  vector<TokenOrPart> tops = tokenizePattern(*ctx, s, {}, lexopts);

  // Test subject
  optional<Pattern> observed = parsePattern(*ctx, tops);

  // Expect no single-node concat list.
  if(!observed.has_value()) {
    showDiags(ctx->diags);
    BugMe("Pattern-making failed");
  }
  if(auto err = match("foo", *observed)) BugMe("{}", *err);
}

void testPatternOperators() {
  const string inputs[] = {"int [ [const | volatile] * ] x;",
                           "var x = value, ... , x = value;",
                           "stmt; ... stmt;",
                           "stmt; ... ; stmt;",
                           "a a ... a a"};
  PatternMatcher expectations[] = {
    concatMatcher(
      "int",
      optionalMatcher(concatMatcher(
        optionalMatcher(orListMatcher("const", "volatile")), "*"
      )),
      "x", ";"),
    concatMatcher(
      "var", foldMatcher(concatMatcher("x", "=", "value"), ","), ";"),
    repeatMatcher(concatMatcher("stmt", ";")),
    repeatMatcher(concatMatcher("stmt", ";")),
    repeatMatcher("a"),
  };
  for(size_t i=0; i<size(inputs); ++i) {
    // Setups
    auto [ctx, fquote] = setupMatchTest(__func__, '"' + inputs[i] + '"');
    GluedString s = fquote(inputs[i]);
    vector<TokenOrPart> tops = tokenizePattern(*ctx, s, {}, lexopts);
    if(hasFusedPatternOpers(*ctx, tops)) BugMe("Input has fused metachars");

    // Test subject
    optional<Pattern> observed = parsePattern(*ctx, tops);

    // Expectations
    if(!observed.has_value()) {
      showDiags(ctx->diags);
      BugMe("Pattern-making failed");
    }
    if(auto err = match(expectations[i], *observed)) BugMe("{}", *err);
  }
}

void testPatternErrorCases() {
  string inputs[] = {
    "int [ [const] * x;",
    "int [const] ] x;",
    "int [const [ ] ] x;",
    "int [const | volatile | ] x;",
    "| int [const | volatile ] x;",
    "int [const | volatile ] x; | ",
    "value + ... + value + ... + value",
    "[x] ... [x]",
  };
  string_view expectedDiags[] = {
    "Unmatched '['",
    "Unmatched ']'",
    "Empty '[]' not allowed",
    "group is already optional",
    "make this pattern optional in parent rules",
    "make this pattern optional in parent rules",
    "Multiple ellipsis",
    "Nothing to repeat around ellipsis",
  };
  static_assert(size(inputs) == size(expectedDiags));
  for(size_t i=0; i<size(inputs); ++i) {
    // Setup
    const string input = '"' + inputs[i] + '"';
    auto [ctx, fquote] = setupMatchTest(__func__, input);
    GluedString s = fquote(inputs[i]);
    vector<TokenOrPart> tops = tokenizePattern(*ctx, s, {}, lexopts);
    if(hasFusedPatternOpers(*ctx, tops)) BugMe("Input has fused metachars");

    // Test
    parsePattern(*ctx, tops);
    assertHasDiagWithSubstr(format("{}[{}]", __func__, i), ctx->diags,
                            expectedDiags[i]);
  }
}

void testRolloutEllipsis() {
  pair<string, RolloutEllipsisForTestResult> goodCases[] = {
    {"(a+...+a)", {"a+...+a", "a", "+", ""}},
    {"fname a ... a", {" a ... a", " a", "", ""}},
    {"(term + term + ... + term)", {"term + term + ... + term",
                                    "term", " + ", ""}},
    {"|arg...arg|", {"|arg...arg|", "|", "arg", ""}},
    {"|a|...|a|", {"|a|...|a|", "|", "a", ""}},
    {"|aa...|", {"aa...", "a", "", ""}},
    {"do {stmt; ...; stmt; }", {"stmt; ...; stmt; ", "stmt; ", "", ""}},
    {"do {stmt; ... stmt; }",  {"stmt; ... stmt; ",  "stmt; ", "", ""}},
    {"do {stmt; stmt; ...; stmt; }", {"stmt; stmt; ...; stmt; ",
                                      "stmt; ", "", ""}},
    {"abcabc...bca", {"abcabc...bca", "a", "bc", ""}},
    {"abcabc...", {"abcabc...", "abc", "", ""}},
    {"abcabc...cab", {"abcabc...cab", "ab", "c", ""}},
    {"abc...abcabcc", {"abc...abcabc", "abc", "", ""}},
    {"abc...abcabccc", {"abc...abcabc", "abc", "", ""}},
    {"abcabcc...cc", {"cc...cc", "c", "", ""}},
  };
  for(const auto& [input, expected] : goodCases) {
    auto observed = rolloutEllipsisForTest(input);
    assertEqual("err  for input "+input, ""s, observed.err);
    assertEqual("expr for input "+input, expected.expr, observed.expr);
    assertEqual("part for input "+input, expected.part, observed.part);
    assertEqual("glue for input "+input, expected.glue, observed.glue);
  }
  pair<string, string> badCases[] = {
    {"...abcabc", "Repeating parts need to appear before ellipsis as well"},
    {"abc...xyz", "Every repeating part must appear at least twice"},
    {"abcabc...a", "Repeating parts of an infix"},
    {"abcab...", "Every repeating part must appear at least twice"},
    {"abcabc...cc", "Ambiguous"},
    {"abcabc...abcc", "Ambiguous"},  // Either abc abc, or c abc c ... c abc c
    // Even though trailing repeats such as "...xyzxyz" would have been
    // illegal anyway, this is still confusing to the human reader.
    {"abcabc...xyzxyz", "Ambiguous"},
  };
  for(const auto& [input, errmsg] : badCases) {
    auto observed = rolloutEllipsisForTest(input);
    if(observed.err.empty())
      BugMe("Was expecting error for input '{}', got part '{}', glue '{}'",
            input, observed.part, observed.glue);
    if(!isSubstr(errmsg, observed.err))
      BugMe("On input '{}', observed error message '{}' "
            "doesn't have the expected '{}'", input, observed.err, errmsg);
  }
}

}  // namespace

int main() {
  testMatchAll();
  testEmptyPatternsFail();
  testConfusingPatterns();
  testMatchAllFailsOnOverlap();
  testUnfinishedMatch();
  testLabelParts();
  testCrossLabelOverlapFails();
  testEmptySuccess();
  testNoWordSplit();
  testTokenizeNoLabel();
  testTokenizeNoLabelRunoffComment();
  testTokenizeSuccess();
  testTokenizeLabelInComment();
  testTokenizeRunoffComment();
  testParaBreaks();
  testKeepAllNewlines();
  testTailcont();
  testNoEndingNewline();
  testUnmarkedPatternOpers();
  testPatternSimpleConcat();
  testPatternSingleConcat();
  testPatternOperators();
  testPatternErrorCases();
  testRolloutEllipsis();
}
