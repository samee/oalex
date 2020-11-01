/*  Copyright 2019-2020 Google LLC

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "jsonloc_io.h"
#include <map>
#include <vector>
#include "lexer.h"
#include "runtime/util.h"
using oalex::Bug;
using oalex::InputDiags;
using oalex::JsonLoc;
using oalex::lex::BracketGroup;
using oalex::lex::BracketType;
using oalex::lex::ExprToken;
using oalex::lex::QuotedString;
using oalex::lex::UnquotedToken;
using oalex::lex::lexBracketGroup;
using std::get_if;
using std::map;
using std::nullopt;
using std::optional;
using std::string;
using std::string_view;
using std::vector;

namespace {

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

bool isErrorValue(const vector<ExprToken>& v) {
  if(v.size() != 1) return false;
  auto* token = get_if<UnquotedToken>(&v[0]);
  return token && token->token == "error_value";
}

optional<JsonLoc> parseJsonLoc(InputDiags& ctx, const ExprToken& expr) {
  if(auto token = parseIdent(ctx,expr))
    return JsonLoc(JsonLoc::Placeholder{token->token});
  if(auto* qs = get_if<QuotedString>(&expr))
    return JsonLoc(*qs);
  if(auto* bg = get_if<BracketGroup>(&expr)) {
    if(bg->type == BracketType::brace) return parseMap(ctx, bg->children);
    if(bg->type == BracketType::square) return parseVector(ctx, bg->children);
    if(bg->type == BracketType::paren) {
      if(isErrorValue(bg->children)) return JsonLoc::ErrorValue{};
      else return Error(ctx, bg->stPos, "Unexpected parenthesis");
    }
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
      Bug("splitCommaNoEmpty() is returning empty elements.");
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

}  // namespace

namespace oalex {

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

}  // namespace oalex