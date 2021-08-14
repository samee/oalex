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

#include "jsontmpl_parsers.h"
#include <vector>
#include "lexer.h"
#include "runtime/jsonloc.h"
#include "runtime/util.h"
using oalex::Bug;
using oalex::DiagsDest;
using oalex::InputDiags;
using oalex::JsonLoc;
using oalex::JsonTmpl;
using oalex::WholeSegment;
using oalex::lex::BracketGroup;
using oalex::lex::BracketType;
using oalex::lex::ExprToken;
using oalex::lex::GluedString;
using oalex::lex::lexBracketGroup;
using std::holds_alternative;
using std::get_if;
using std::nullopt;
using std::optional;
using std::string;
using std::string_view;
using std::vector;

namespace {

optional<JsonTmpl> parseJsonTmpl(DiagsDest ctx, ExprToken expr);
optional<JsonTmpl> parseMap(DiagsDest ctx, vector<ExprToken> elts);
optional<JsonTmpl> parseVector(DiagsDest ctx, vector<ExprToken> elts);

bool isIdent(string_view s) {
  if(s.empty()) return false;
  if(isdigit(s[0])) return false;
  for(char ch : s) if(!isalnum(ch) && ch!='_') return false;
  return true;
}

void sanitizeIdent(DiagsDest ctx, WholeSegment& seg) {
  if(!isIdent(seg.data)) {
    Error(ctx, seg, "'" + seg.data + "' is not a valid identifier.");
    seg = WholeSegment(seg.stPos, seg.enPos, "invalid_identifier");
  }
}

optional<JsonTmpl> locateAt(optional<JsonTmpl> jstmpl,
                            const oalex::Segment& seg) {
  if(jstmpl.has_value()) {
    jstmpl->stPos = seg.stPos;
    jstmpl->enPos = seg.enPos;
  }
  return jstmpl;
}

optional<JsonTmpl> parseJsonTmpl(DiagsDest ctx, ExprToken expr) {
  if(auto* seg = get_if<WholeSegment>(&expr)) {
    if(**seg == "...") return locateAt(JsonTmpl{JsonTmpl::Ellipsis{}}, *seg);
    sanitizeIdent(ctx, *seg);
    return locateAt(JsonTmpl(JsonTmpl::Placeholder{seg->data}), *seg);
  }
  if(auto* qs = get_if<GluedString>(&expr))
    return locateAt(JsonTmpl(*qs), *qs);
  if(auto* bg = get_if<BracketGroup>(&expr)) {
    if(bg->type == BracketType::brace)
      return locateAt(parseMap(ctx, std::move(bg->children)), *bg);
    if(bg->type == BracketType::square)
      return locateAt(parseVector(ctx, std::move(bg->children)), *bg);
    if(bg->type == BracketType::paren) {
      return Error(ctx, bg->stPos, "Unexpected parenthesis");
    }
    Bug("Unknown BracketType: {}", int(bg->type));
  }
  Bug("Unknown ExprType: {}", exprTagName(expr));
}

// TODO diags should throw after 3 or so errors.
// This is a reasonable example of what error-handling oalex could facilitate.
optional<JsonTmpl> parseMap(DiagsDest ctx, vector<ExprToken> elts) {
  vector<vector<ExprToken>> splitres = splitCommaNoEmpty(ctx, std::move(elts));

  JsonTmpl::Map rv;
  for(auto& elt : splitres) {
    if(elt.empty())
      Bug("splitCommaNoEmpty() is returning empty elements.");
    WholeSegment* key = get_if<WholeSegment>(&elt[0]);
    if(!key) {
      Error(ctx, elt[0], "Was expecting the key to be an identifier");
      continue;
    }
    sanitizeIdent(ctx, *key);
    optional<JsonTmpl> parsedElt;
    if(elt.size() == 1)
      parsedElt = locateAt(JsonTmpl{JsonTmpl::Placeholder{key->data}}, *key);
    else {
      if(!isToken(elt[1],":")) {
        Error(ctx, enPos(elt[0]), "Was expecting a colon sign after the key.");
        continue;
      }
      if(elt.size()<3) {
        Error(ctx, enPos(elt[1]), "Value missing after the colon.");
        continue;
      }
      parsedElt = parseJsonTmpl(ctx, std::move(elt[2]));
      if(!parsedElt) continue;  // parseJsonTmpl() has already logged an error.
      if(elt.size()>=4) {
        Error(ctx, stPos(elt[3]), "Was expecting a comma here");
        continue;
      }
    }

    if(JsonTmpl::mapScanForIndex(rv, key->data) != -1)
      Error(ctx, *key, "Duplicate key " + key->data);
    else rv.push_back({key->data, std::move(*parsedElt)});
  }
  JsonTmpl::mapSort(rv);
  return rv;
}

optional<JsonTmpl>
parseVector(DiagsDest ctx, vector<ExprToken> elts) {
  vector<vector<ExprToken>> splitres = splitCommaNoEmpty(ctx, std::move(elts));

  vector<JsonTmpl> rv;
  for(auto& elt : splitres) {
    if(elt.empty()) Bug("splitCommaNoEmpty() is returning empty elements.");
    if(elt.size()!=1) Error(ctx, stPos(elt[1]), "Was expecting a comma here");
    else {
      optional<JsonTmpl> parsedElt = parseJsonTmpl(ctx, std::move(elt[0]));
      if(parsedElt) rv.push_back(*parsedElt);
    }
  }
  return JsonTmpl(rv);
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

optional<JsonLoc> wrapOutput(optional<JsonTmpl> jstmpl) {
  if(jstmpl.has_value()) return jstmpl->outputIfFilled();
  else return nullopt;
}

}  // namespace

namespace oalex {

optional<JsonTmpl> parseJsonTmplFromBracketGroup(DiagsDest ctx,
                                                 BracketGroup bg) {
  if(bg.type == BracketType::paren) return nullopt;
  if(bg.type == BracketType::square)
    return locateAt(parseVector(ctx, std::move(bg.children)), bg);
  if(bg.type == BracketType::brace)
    return locateAt(parseMap(ctx, std::move(bg.children)), bg);
  Bug("Unknown BracketType: {}", int(bg.type));
}

// Assumes the whole thing is surrouded by some kind of a bracket.
optional<JsonTmpl> parseJsonTmpl(InputDiags& ctx, size_t& i) {
  Resetter rst(ctx,i);
  optional<BracketGroup> bg = lexBracketGroup(ctx, i);
  if(!bg.has_value()) return nullopt;
  bool allSingleQ = allStringsSingleQuoted(ctx, *bg);
  auto rv = parseJsonTmplFromBracketGroup(ctx, std::move(*bg));
  if(rv.has_value() && allSingleQ) {
    rst.markUsed(i);
    return rv;
  }else return nullopt;
}

optional<JsonTmpl> parseJsonTmpl(string_view s) {
  size_t i = 0;
  InputDiags ctx{Input(string(s))};
  return parseJsonTmpl(ctx,i);
}

optional<JsonLoc> parseJsonLoc(string_view s) {
  return wrapOutput(parseJsonTmpl(s));
}

optional<JsonTmpl> parseJsonTmplFlexQuote(InputDiags& ctx, size_t& i) {
  Resetter rst(ctx,i);
  optional<BracketGroup> bg = lexBracketGroup(ctx, i);
  if(!bg.has_value()) return nullopt;
  auto rv = parseJsonTmplFromBracketGroup(ctx, std::move(*bg));
  if(rv.has_value()) rst.markUsed(i);
  return rv;
}

}  // namespace oalex
