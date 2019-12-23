#include "jsonloc.h"

#include <cctype>
#include <optional>
#include <map>
#include <string>
#include <vector>

#include "diags_test_util.h"
#include "lexer.h"
#include "test_util.h"
#include "runtime/input_view.h"
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
using oalex::uniqueKeys;
using oalex::UserError;
using oalex::operator<<;
using oalex::lex::BracketGroup;
using oalex::lex::BracketType;
using oalex::lex::ExprToken;
using oalex::lex::QuotedString;
using oalex::lex::UnquotedToken;
using oalex::lex::lexBracketGroup;

namespace {

optional<JsonLoc> parseJsonLoc(InputDiags& lex, size_t& i);
optional<JsonLoc> parseJsonLoc(InputDiags& lex, const ExprToken& expr);
optional<JsonLoc> parseMap(InputDiags& lex, const vector<ExprToken>& elts);
optional<JsonLoc> parseVector(InputDiags& lex, const vector<ExprToken>& elts);

// This is meant for parsing lists. As such, it never returns empty elements.
// Errors out if it finds one, unless it's the last element. If the last
// element is empty, it is silently discarded to allow a trailing comma in a
// list. Note that this is different from Python's "".split(',') which returns
// a single empty string.
vector<vector<ExprToken>>
splitCommaNoEmpty(InputDiags& lex,const vector<ExprToken>& elts) {
  vector<vector<ExprToken>> rv{ {} };
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
optional<JsonLoc> parseJsonLoc(InputDiags& lex, size_t& i) {
  size_t j = i;
  optional<BracketGroup> bg = lexBracketGroup(lex, j);
  if(!bg.has_value()) return nullopt;
  if(bg->type == BracketType::paren) return nullopt;
  i = j;
  if(bg->type == BracketType::square) return parseVector(lex, bg->children);
  if(bg->type == BracketType::brace) return parseMap(lex, bg->children);
  Bug()<<"Unknown BracketType: "<<int(bg->type);
}

bool isIdent(string_view s) {
  if(s.empty()) return false;
  if(isdigit(s[0])) return false;
  for(char ch : s) if(!isalnum(ch) && ch!='_') return false;
  return true;
}

optional<UnquotedToken> parseIdent(InputDiags& lex, const ExprToken& expr) {
  auto* token = get_if<UnquotedToken>(&expr);
  if(!token) return lex.Error(stPos(expr),"Was expecting an identifier");
  if(!isIdent(token->token)) {
    lex.Error(token->stPos,
      "'" + token->token + "' is not a valid identifier.");
    return UnquotedToken(token->stPos, token->enPos, "invalid_identifier");
  }
  return *token;
}

optional<JsonLoc> parseJsonLoc(InputDiags& lex, const ExprToken& expr) {
  if(auto token = parseIdent(lex,expr))
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
optional<JsonLoc> parseMap(InputDiags& lex, const vector<ExprToken>& elts) {
  vector<vector<ExprToken>> splitres = splitCommaNoEmpty(lex, elts);

  map<string,JsonLoc> rv;
  for(auto& elt : splitres) {
    if(elt.empty()) Bug()<<"splitCommaNoEmpty() is returning empty elements.";
    optional<UnquotedToken> key = parseIdent(lex, elt[0]);
    if(!key) {
      lex.Error(stPos(elt[0]), "Was expecting a key.");
      continue;
    }
    optional<JsonLoc> parsedElt;
    if(elt.size() == 1) parsedElt = JsonLoc::Placeholder{key->token};
    else {
      if(!isToken(elt[1],":")) {
        lex.Error(enPos(elt[0]), "Was expecting a colon sign after the key.");
        continue;
      }
      if(elt.size()<3) {
        lex.Error(enPos(elt[1]), "Value missing after the colon.");
        continue;
      }
      parsedElt = parseJsonLoc(lex, elt[2]);
      if(!parsedElt) continue;  // parseJsonLoc() has already logged an error.
      if(elt.size()>=4) {
        lex.Error(stPos(elt[3]), "Was expecting a comma here");
        continue;
      }
    }

    if(rv.insert({key->token, std::move(*parsedElt)}).second == false)
      lex.Error(key->stPos, "Duplicate key " + key->token);
  }
  return JsonLoc(rv);
}

optional<JsonLoc> parseVector(InputDiags& lex, const vector<ExprToken>& elts) {
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
InputDiags GetFromString(string_view s) {
  return InputDiags{Input(oalex::GetFromString(s)),{}};
}

// Testing convenience.
optional<JsonLoc> parseJsonLoc(string_view s) {
  size_t i = 0;
  InputDiags lex = GetFromString(s);
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
    BugMe<<"Unexpected blank set: ["<<uniqueKeys(blanks)
         <<"] != ["<<expected_blanks<<"]";

  // Try one substitution.
  optional<JsonLoc> part1 = parseJsonLoc(R"({key: "value"})");
  json->substitute(blanks, "input", *part1);
  if(json->substitutionsOk())
    BugMe<<"Unexpectedly okay with substitution after only 1 of 2 subs";

  // Try second substitution.
  json->substitute(blanks, "input2", JsonLoc("hello"));
  if(!json->substitutionsOk())
    BugMe<<"Even after all substitutions, still not okay";

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
    BugMe<<"Unexpected output:\n"<<output;
}

void testJsonLocFailure(const char input[], const char errmsg[]) {
  InputDiags lex = GetFromString(input);
  size_t i = 0;
  optional<JsonLoc> res = parseJsonLoc(lex, i);
  if(res.has_value() && lex.diags.empty())
    Bug()<<"Was expecting failure with input '"<<input
         <<"'. Got this instead: "<<res->prettyPrint();
  assertHasDiagWithSubstr(__func__, lex.diags, errmsg);
}

void testJsonLocPosition(const char input[], size_t endi) {
  InputDiags lex = GetFromString(input);
  size_t i = 0;
  optional<JsonLoc> res = parseJsonLoc(lex, i);
  if(!lex.diags.empty()) {
    for(const auto& d : lex.diags) BugWarn()<<string(d);
    Bug()<<"Got unexpected diags.";
  }
  if(i != endi) Bug()<<"For input '"<<input<<"', expected end position was "
                     <<endi<<" != "<<i;
}

}  // namespace

// Note: none of these check JsonLoc::stpos or enpos of parse results, since we
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
