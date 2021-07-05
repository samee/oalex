/*  Copyright 2019-2021 The oalex authors.

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
using oalex::DiagsDest;
using oalex::InputDiags;
using oalex::JsonLoc;
using oalex::lex::BracketGroup;
using oalex::lex::BracketType;
using oalex::lex::ExprToken;
using oalex::lex::GluedString;
using oalex::lex::WholeSegment;
using oalex::lex::lexBracketGroup;
using std::holds_alternative;
using std::get_if;
using std::map;
using std::nullopt;
using std::optional;
using std::string;
using std::string_view;
using std::vector;

namespace {

optional<JsonLoc> parseJsonLoc(DiagsDest ctx, ExprToken expr);
optional<JsonLoc> parseMap(DiagsDest ctx, vector<ExprToken> elts);
optional<JsonLoc> parseVector(DiagsDest ctx, vector<ExprToken> elts);

bool isIdent(string_view s) {
  if(s.empty()) return false;
  if(isdigit(s[0])) return false;
  for(char ch : s) if(!isalnum(ch) && ch!='_') return false;
  return true;
}

optional<WholeSegment> parseIdent(DiagsDest ctx, const ExprToken& expr) {
  auto* seg = get_if<WholeSegment>(&expr);
  if(!seg) return Error(ctx, expr,"Was expecting an identifier");
  if(!isIdent(seg->data)) {
    Error(ctx, *seg, "'" + seg->data + "' is not a valid identifier.");
    return WholeSegment(seg->stPos, seg->enPos, "invalid_identifier");
  }
  return *seg;
}

bool isErrorValue(const vector<ExprToken>& v) {
  if(v.size() != 1) return false;
  auto* seg = get_if<WholeSegment>(&v[0]);
  return seg && seg->data == "error_value";
}

optional<JsonLoc> parseJsonLoc(DiagsDest ctx, ExprToken expr) {
  if(auto* seg = get_if<WholeSegment>(&expr)) {
    if(auto id = parseIdent(ctx, *seg))
      return JsonLoc(JsonLoc::Placeholder{id->data}, id->stPos, id->enPos);
    else return nullopt;
  }
  if(auto* qs = get_if<GluedString>(&expr))
    return JsonLoc(*qs);
  if(auto* bg = get_if<BracketGroup>(&expr)) {
    if(bg->type == BracketType::brace)
      return parseMap(ctx, std::move(bg->children));
    if(bg->type == BracketType::square)
      return parseVector(ctx, std::move(bg->children));
    if(bg->type == BracketType::paren) {
      if(isErrorValue(bg->children)) return JsonLoc::ErrorValue{};
      else return Error(ctx, bg->stPos, "Unexpected parenthesis");
    }
    Bug("Unknown BracketType: {}", int(bg->type));
  }
  Bug("Unknown ExprType: {}", exprTagName(expr));
}

// TODO diags should throw after 3 or so errors.
// This is a reasonable example of what error-handling oalex could facilitate.
optional<JsonLoc> parseMap(DiagsDest ctx, vector<ExprToken> elts) {
  vector<vector<ExprToken>> splitres = splitCommaNoEmpty(ctx, std::move(elts));

  JsonLoc::Map rv;
  for(auto& elt : splitres) {
    if(elt.empty())
      Bug("splitCommaNoEmpty() is returning empty elements.");
    optional<WholeSegment> key = parseIdent(ctx, elt[0]);
    if(!key) {
      Error(ctx, elt[0], "Was expecting a key.");
      continue;
    }
    optional<JsonLoc> parsedElt;
    if(elt.size() == 1)
      parsedElt = JsonLoc{JsonLoc::Placeholder{key->data},
                          key->stPos, key->enPos};
    else {
      if(!isToken(elt[1],":")) {
        Error(ctx, enPos(elt[0]), "Was expecting a colon sign after the key.");
        continue;
      }
      if(elt.size()<3) {
        Error(ctx, enPos(elt[1]), "Value missing after the colon.");
        continue;
      }
      parsedElt = parseJsonLoc(ctx, std::move(elt[2]));
      if(!parsedElt) continue;  // parseJsonLoc() has already logged an error.
      if(elt.size()>=4) {
        Error(ctx, stPos(elt[3]), "Was expecting a comma here");
        continue;
      }
    }

    if(rv.insert({key->data, std::move(*parsedElt)}).second == false)
      Error(ctx, *key, "Duplicate key " + key->data);
  }
  return JsonLoc(rv);
}

optional<JsonLoc>
parseVector(DiagsDest ctx, vector<ExprToken> elts) {
  vector<vector<ExprToken>> splitres = splitCommaNoEmpty(ctx, std::move(elts));

  vector<JsonLoc> rv;
  for(auto& elt : splitres) {
    if(elt.empty()) Bug("splitCommaNoEmpty() is returning empty elements.");
    if(elt.size()!=1) Error(ctx, stPos(elt[1]), "Was expecting a comma here");
    else {
      optional<JsonLoc> parsedElt = parseJsonLoc(ctx, std::move(elt[0]));
      if(parsedElt) rv.push_back(*parsedElt);
    }
  }
  return JsonLoc(rv);
}

bool allStringsSingleQuoted(DiagsDest ctx, const ExprToken& expr);

bool allStringsSingleQuoted(DiagsDest ctx, const BracketGroup& bg) {
  for(const auto& c : bg.children)
    if(!allStringsSingleQuoted(ctx, c)) return false;
  return true;
}

bool allStringsSingleQuoted(DiagsDest ctx, const ExprToken& expr) {
  if(holds_alternative<WholeSegment>(expr)) return true;
  if(auto* qs = get_if<GluedString>(&expr)) {
    if(qs->ctor() != GluedString::Ctor::squoted) {
      Error(ctx, *qs, "Output strings must be single-quoted");
      return false;
    }
    return true;
  }
  if(auto* bg = get_if<BracketGroup>(&expr))
    return allStringsSingleQuoted(ctx, *bg);

  Bug("Unknown ExprToken type {}", exprTagName(expr));
}

}  // namespace

namespace oalex {

optional<JsonLoc> parseJsonLocFromBracketGroup(DiagsDest ctx,
                                               BracketGroup bg) {
  if(bg.type == BracketType::paren) return nullopt;
  if(bg.type == BracketType::square)
    return parseVector(ctx, std::move(bg.children));
  if(bg.type == BracketType::brace)
    return parseMap(ctx, std::move(bg.children));
  Bug("Unknown BracketType: {}", int(bg.type));
}

// Assumes the whole thing is surrouded by some kind of a bracket.
optional<JsonLoc> parseJsonLoc(InputDiags& ctx, size_t& i) {
  Resetter rst(ctx,i);
  optional<BracketGroup> bg = lexBracketGroup(ctx, i);
  if(!bg.has_value()) return nullopt;
  bool allSingleQ = allStringsSingleQuoted(ctx, *bg);
  auto rv = parseJsonLocFromBracketGroup(ctx, std::move(*bg));
  if(rv.has_value() && allSingleQ) {
    rst.markUsed(i);
    return rv;
  }else return nullopt;
}

optional<JsonLoc> parseJsonLocFlexQuote(InputDiags& ctx, size_t& i) {
  Resetter rst(ctx,i);
  optional<BracketGroup> bg = lexBracketGroup(ctx, i);
  if(!bg.has_value()) return nullopt;
  auto rv = parseJsonLocFromBracketGroup(ctx, std::move(*bg));
  if(rv.has_value()) rst.markUsed(i);
  return rv;
}

}  // namespace oalex
