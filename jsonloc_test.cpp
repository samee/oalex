#include "jsonloc.h"

#include <optional>
#include <map>
#include <string>
#include <vector>

#include "input_view_manual.h"
#include "lexer.h"
#include "test_util.h"
#include "util.h"
using std::get_if;
using std::map;
using std::nullopt;
using std::optional;
using std::string;
using std::string_view;
using std::vector;
using oalex::Bug;
using oalex::Input;
using oalex::GetFromString;
using oalex::JsonLoc;
using oalex::UserError;
using oalex::lex::BracketGroup;
using oalex::lex::BracketType;
using oalex::lex::ExprToken;
using oalex::lex::Lexer;
using oalex::lex::QuotedString;
using oalex::lex::UnquotedToken;

namespace {

optional<JsonLoc> parseJsonLoc(Lexer& lex, size_t& i);
optional<JsonLoc> parseJsonLoc(Lexer& lex, const ExprToken& expr);  // TODO
optional<JsonLoc> parseMap(Lexer& lex, const vector<ExprToken>& elts);
optional<JsonLoc> parseVector(Lexer& lex, const vector<ExprToken>& elts);

// This is meant for parsing lists. As such, it never returns empty elements.
// Errors out if it finds one, unless it's the last element. If the last
// element is empty, it is silently discarded to allow a trailing comma in a
// list. Note that this is different from Python's "".split(',') which returns
// a single empty string.
vector<vector<ExprToken>>
splitCommaNoEmpty(Lexer& lex,const vector<ExprToken>& elts) {
  vector<vector<ExprToken>> rv(1);
  for(const auto& elt : elts) {
    if(isToken(elt,",")) {
      if(rv.back().empty()) lex.Error(stPos(elt),"Unexpected comma");
      else rv.emplace_back();
    }
    else rv.back().push_back(elt);
  }
  if(rv.back().empty()) rv.pop_back();
  return rv;
}

// Assumes the whole thing is surrouded by some kind of a bracket.
optional<JsonLoc> parseJsonLoc(Lexer& lex, size_t& i) {
  size_t j=i;
  optional<BracketGroup> bg = lexBracketGroup(lex, j);
  if(!bg.has_value()) return nullopt;
  if(bg->type == BracketType::paren) return nullopt;
  if(bg->type == BracketType::square) return parseVector(lex, bg->children);
  if(bg->type == BracketType::brace) return parseMap(lex, bg->children);
  Bug()<<"Unknown BracketType: "<<int(bg->type);
}

optional<JsonLoc> parseJsonLoc(Lexer& lex, const ExprToken& expr) {
  if(auto* token = get_if<UnquotedToken>(&expr))
    return JsonLoc(JsonLoc::Placeholder{token->token});
  if(auto* qs = get_if<QuotedString>(&expr))
    return JsonLoc(qs->s);
  if(auto* bg = get_if<BracketGroup>(&expr)) {
    if(bg->type == BracketType::brace) return parseMap(lex, bg->children);
    if(bg->type == BracketType::square) return parseVector(lex, bg->children);
    if(bg->type == BracketType::paren)
      return lex.Error(bg->stPos, "Unexpected parenthesis");
    Bug()<<"Unknown BracketType: "<<int(bg->type);
  }
  Bug()<<"Unknown ExprType with index: "<<expr.index();
}

// TODO diags should throw after 3 or so errors.
// This is a reasonable example of what error-handling oalex could facilitate.
optional<JsonLoc> parseMap(Lexer& lex, const vector<ExprToken>& elts) {
  vector<vector<ExprToken>> splitres = splitCommaNoEmpty(lex, elts);

  map<string,JsonLoc> rv;
  for(auto& elt : splitres) {
    if(elt.empty()) Bug()<<"splitCommaNoEmpty() is returning empty elements.";
    const UnquotedToken* key = get_if<UnquotedToken>(&elt[0]);
    if(!key) {
      lex.Error(stPos(elt[0]), "Was expecting a key.");
      continue;
    }
    if(elt.size()<2 || !isToken(elt[1],":")) {
      lex.Error(stPos(elt[1]), "Was expecting a colon sign after the key.");
      continue;
    }
    if(elt.size()<3) {
      lex.Error(stPos(elt[2]), "Value missing after the colon.");
      continue;
    }
    optional<JsonLoc> parsedElt = parseJsonLoc(lex, elt[2]);
    if(parsedElt) {
      if(rv.insert({key->token, std::move(*parsedElt)}).second == false)
        lex.Error(key->stPos, "Duplicate key " + key->token);
    }else if(elt.size()>=4)
      lex.Error(stPos(elt[3]), "Was expecting a comma here");
  }
  return JsonLoc(rv);
}

optional<JsonLoc> parseVector(Lexer& lex, const vector<ExprToken>& elts) {
  vector<vector<ExprToken>> splitres = splitCommaNoEmpty(lex, elts);

  vector<JsonLoc> rv;
  for(auto& elt : splitres) {
    if(elt.empty()) Bug()<<"splitCommaNoEmpty() is returning empty elements.";
    if(elt.size()!=1) lex.Error(stPos(elt[1]), "Was expecting a comma here");
    else {
      optional<JsonLoc> parsedElt = parseJsonLoc(lex, elt[0]);
      if(parsedElt) rv.push_back(*parsedElt);
    }
  }
  return JsonLoc(rv);
}

// Testing convenience.
Lexer GetFromString(string_view s) {
  return Lexer{Input(oalex::GetFromString(s)),{}};
}

// Testing convenience.
optional<JsonLoc> parseJsonLoc(string_view s) {
  size_t i = 0;
  Lexer lex = GetFromString(s);
  return parseJsonLoc(lex,i);
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
    BugMe<<"Unexpected output:\n"<<output;
}

}  // namespace

// TODO failure tests. Empty braces and other corner cases.
// TODO substitution tests. Pretty-print tests.
int main() {
  testSimpleSuccess();
}
