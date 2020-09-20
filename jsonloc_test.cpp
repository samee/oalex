#include "jsonloc.h"

#include <cctype>
#include <optional>
#include <map>
#include <string>
#include <vector>

#include "lexer.h"
#include "runtime/diags_test_util.h"
#include "runtime/input_view.h"
#include "runtime/test_util.h"
#include "runtime/util.h"
using std::get_if;
using std::isalnum;
using std::isdigit;
using std::map;
using std::nullopt;
using std::optional;
using std::string;
using std::string_view;
using std::vector;
using namespace std::string_literals;
using oalex::Bug;
using oalex::BugWarn;
using oalex::Input;
using oalex::InputDiags;
using oalex::JsonLoc;
using oalex::Resetter;
using oalex::uniqueKeys;
using oalex::lex::BracketGroup;
using oalex::lex::BracketType;
using oalex::lex::ExprToken;
using oalex::lex::QuotedString;
using oalex::lex::UnquotedToken;
using oalex::lex::lexBracketGroup;

namespace {

optional<JsonLoc> parseJsonLoc(InputDiags& ctx, size_t& i);
optional<JsonLoc> parseJsonLoc(InputDiags& ctx, const ExprToken& expr);
optional<JsonLoc> parseMap(InputDiags& ctx, const vector<ExprToken>& elts);
optional<JsonLoc> parseVector(InputDiags& ctx, const vector<ExprToken>& elts);

// This is meant for parsing lists. As such, it never returns empty elements.
// Errors out if it finds one, unless it's the last element. If the last
// element is empty, it is silently discarded to allow a trailing comma in a
// list. Note that this is different from Python's "".split(',') which returns
// a single empty string.
vector<vector<ExprToken>>
splitCommaNoEmpty(InputDiags& ctx,const vector<ExprToken>& elts) {
  vector<vector<ExprToken>> rv{ {} };
  for(const auto& elt : elts) {
    if(isToken(elt,",")) {
      if(rv.back().empty()) Error(ctx, stPos(elt),"Unexpected comma");
      else rv.emplace_back();
    }
    else rv.back().push_back(elt);
  }
  if(rv.back().empty()) rv.pop_back();
  return rv;
}

// Assumes the whole thing is surrouded by some kind of a bracket.
optional<JsonLoc> parseJsonLoc(InputDiags& ctx, size_t& i) {
  Resetter rst(ctx,i);
  optional<BracketGroup> bg = lexBracketGroup(ctx, i);
  if(!bg.has_value()) return nullopt;
  if(bg->type == BracketType::paren) return nullopt;
  rst.markUsed(i);
  if(bg->type == BracketType::square) return parseVector(ctx, bg->children);
  if(bg->type == BracketType::brace) return parseMap(ctx, bg->children);
  Bug("Unknown BracketType: {}", int(bg->type));
}

bool isIdent(string_view s) {
  if(s.empty()) return false;
  if(isdigit(s[0])) return false;
  for(char ch : s) if(!isalnum(ch) && ch!='_') return false;
  return true;
}

optional<UnquotedToken> parseIdent(InputDiags& ctx, const ExprToken& expr) {
  auto* token = get_if<UnquotedToken>(&expr);
  if(!token) return Error(ctx, stPos(expr),"Was expecting an identifier");
  if(!isIdent(token->token)) {
    Error(ctx, token->stPos,
          "'" + token->token + "' is not a valid identifier.");
    return UnquotedToken(token->stPos, token->enPos, "invalid_identifier");
  }
  return *token;
}

optional<JsonLoc> parseJsonLoc(InputDiags& ctx, const ExprToken& expr) {
  if(auto token = parseIdent(ctx,expr))
    return JsonLoc(JsonLoc::Placeholder{token->token});
  if(auto* qs = get_if<QuotedString>(&expr))
    return JsonLoc(*qs);
  if(auto* bg = get_if<BracketGroup>(&expr)) {
    if(bg->type == BracketType::brace) return parseMap(ctx, bg->children);
    if(bg->type == BracketType::square) return parseVector(ctx, bg->children);
    if(bg->type == BracketType::paren)
      return Error(ctx, bg->stPos, "Unexpected parenthesis");
    Bug("Unknown BracketType: {}", int(bg->type));
  }
  Bug("Unknown ExprType with index: {}", expr.index());
}

// TODO diags should throw after 3 or so errors.
// This is a reasonable example of what error-handling oalex could facilitate.
optional<JsonLoc> parseMap(InputDiags& ctx, const vector<ExprToken>& elts) {
  vector<vector<ExprToken>> splitres = splitCommaNoEmpty(ctx, elts);

  map<string,JsonLoc> rv;
  for(auto& elt : splitres) {
    if(elt.empty())
      BugMe("splitCommaNoEmpty() is returning empty elements.");
    optional<UnquotedToken> key = parseIdent(ctx, elt[0]);
    if(!key) {
      Error(ctx, stPos(elt[0]), "Was expecting a key.");
      continue;
    }
    optional<JsonLoc> parsedElt;
    if(elt.size() == 1) parsedElt = JsonLoc::Placeholder{key->token};
    else {
      if(!isToken(elt[1],":")) {
        Error(ctx, enPos(elt[0]), "Was expecting a colon sign after the key.");
        continue;
      }
      if(elt.size()<3) {
        Error(ctx, enPos(elt[1]), "Value missing after the colon.");
        continue;
      }
      parsedElt = parseJsonLoc(ctx, elt[2]);
      if(!parsedElt) continue;  // parseJsonLoc() has already logged an error.
      if(elt.size()>=4) {
        Error(ctx, stPos(elt[3]), "Was expecting a comma here");
        continue;
      }
    }

    if(rv.insert({key->token, std::move(*parsedElt)}).second == false)
      Error(ctx, key->stPos, "Duplicate key " + key->token);
  }
  return JsonLoc(rv);
}

optional<JsonLoc> parseVector(InputDiags& ctx, const vector<ExprToken>& elts) {
  vector<vector<ExprToken>> splitres = splitCommaNoEmpty(ctx, elts);

  vector<JsonLoc> rv;
  for(auto& elt : splitres) {
    if(elt.empty()) Bug("splitCommaNoEmpty() is returning empty elements.");
    if(elt.size()!=1) Error(ctx, stPos(elt[1]), "Was expecting a comma here");
    else {
      optional<JsonLoc> parsedElt = parseJsonLoc(ctx, elt[0]);
      if(parsedElt) rv.push_back(*parsedElt);
    }
  }
  return JsonLoc(rv);
}

// Testing convenience.
optional<JsonLoc> parseJsonLoc(string_view s) {
  size_t i = 0;
  InputDiags ctx = testInputDiags(s);
  return parseJsonLoc(ctx,i);
}

void testSimpleSuccess() {
  const char input[] = R"( {
    # We support comments, another divergence from json.org.
    # Includes a trailing comma.
    input: "hello world", output: ["hello", "world",], metadata: metadata } )";
  optional<JsonLoc> json = parseJsonLoc(input);
  string output = json->prettyPrint(2);
  const char expected[] = R"({
    input: "hello world",
    metadata: metadata,
    output: [
      "hello",
      "world"
    ]
  })";
  if(output != expected)
    BugMe("Unexpected output:\n{}", output);
}

void testSubstitution() {
  const char input[] = R"({
    input,
    list: ["item 1", input, "item 2"],   # Duplicate keyword nestled somewhere.
    input2,  # Lone keyword.
  })";
  optional<JsonLoc> json = parseJsonLoc(input);
  JsonLoc::PlaceholderMap blanks = json->allPlaceholders();
  const vector<string> expected_blanks{"input","input2"};
  if(uniqueKeys(blanks) != expected_blanks)
    BugMe("Unexpected blank set: [{}] != [{}]", uniqueKeys(blanks),
          expected_blanks);

  // Try one substitution.
  optional<JsonLoc> part1 = parseJsonLoc(R"({key: "value"})");
  json->substitute(blanks, "input", *part1);
  if(json->substitutionsOk())
    BugMe("Unexpectedly okay with substitution after only 1 of 2 subs");

  // Try second substitution.
  json->substitute(blanks, "input2", JsonLoc("hello"));
  if(!json->substitutionsOk())
    BugMe("Even after all substitutions, still not okay");

  string output = json->prettyPrint(2);
  const char expected[] = R"({
    input: {
      key: "value"
    },
    input2: "hello",
    list: [
      "item 1",
      {
        key: "value"
      },
      "item 2"
    ]
  })";
  if(output != expected)
    BugMe("Unexpected output:\n{}", output);
}

void testJsonLocFailure(const char input[], const char errmsg[]) {
  InputDiags ctx = testInputDiags(input);
  size_t i = 0;
  optional<JsonLoc> res = parseJsonLoc(ctx, i);
  if(res.has_value() && ctx.diags.empty())
    BugMe("Was expecting failure with input '{}'. Got this instead: {}",
          input, res->prettyPrint());
  assertHasDiagWithSubstr(__func__, ctx.diags, errmsg);
}

void testJsonLocPosition(const char input[], size_t endi) {
  InputDiags ctx = testInputDiags(input);
  size_t i = 0;
  optional<JsonLoc> res = parseJsonLoc(ctx, i);
  if(!ctx.diags.empty()) {
    for(const auto& d : ctx.diags) BugWarn("{}", string(d));
    Bug("Got unexpected diags.");
  }
  if(i != endi) Bug("For input '{}', expected end position was {} != {}",
                    input, endi, i);
}

}  // namespace

// Note: none of these check JsonLoc::stPos or enPos of parse results, since we
// don't quite know how they will actually be used in practice, or what methods
// are needed to support their use pattern. Avoiding overengineering for now.
int main() {
  testSimpleSuccess();
  testSubstitution();
  testJsonLocPosition("(a,b)", 0);
  testJsonLocPosition("foo", 0);
  testJsonLocPosition("[a, b] foo", "[a, b]"s.size());
  testJsonLocFailure("[a,,b]", "Unexpected comma");
  testJsonLocFailure("[a,b,,]", "Unexpected comma");
  testJsonLocFailure("[(a,b)]", "Unexpected parenthesis");
  testJsonLocFailure("{[]:[]}", "Was expecting a key");
  testJsonLocFailure("{a:}", "Value missing after the colon");
  testJsonLocFailure("{a:b:c}", "Was expecting a comma here");
  testJsonLocFailure("{a:b,a:c}", "Duplicate key a");
  testJsonLocFailure("[a b]", "Was expecting a comma");
  testJsonLocFailure("[123]", "'123' is not a valid identifier");
}
