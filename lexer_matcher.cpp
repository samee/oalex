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

#include "lexer_matcher.h"
#include <format>
#include "runtime/util.h"
using std::format;
using std::get_if;
using std::holds_alternative;
using std::nullopt;
using std::optional;
using std::string;
using std::string_view;

namespace oalex::lex::matcher {

namespace {

string stringMismatch(string_view s, string_view t) {
  return format("\"{}\" != \"{}\"", s, t);
}

string debugType(const ExprToken& x) {
  if(holds_alternative<GluedString>(x)) return "GluedString";
  if(holds_alternative<WholeSegment>(x)) return "WholeSegment";
  if(holds_alternative<RegexPattern>(x)) return "RegexPattern";
  if(holds_alternative<BracketGroup>(x)) return "BracketGroup";
  Bug("ExprToken has unknown type: {}", x.index());
}

string debugType(BracketType t) {
  switch(t) {
    case BracketType::square: return "BracketType::square";
    case BracketType::brace:  return "BracketType::brace";
    case BracketType::paren:  return "BracketType::paren";
    default: Bug("Unknown BracketType: {}", int(t));
  }
}

string typeMismatch(string_view s, const ExprToken& x) {
  return format("Type {} != {}", s, debugType(x));
}

string typeMismatch(BracketType bt1,BracketType bt2) {
  return format("{} != {}", debugType(bt1), debugType(bt2));
}

string sizeMismatch(size_t a, size_t b) {
  return format("Size {} != {}", a, b);
}

}  // namespace

optional<string> match(ExprMatcher pattern, const ExprToken& expr) {
  if(const auto* m = get_if<GluedMatcher>(&pattern)) {
    const auto* x = get_if<GluedString>(&expr);
    if(!x) return typeMismatch("GluedString", expr);
    else if(m->s != string_view(*x)) return stringMismatch(m->s, *x);
    else return nullopt;
  }
  if(const auto* m = get_if<WholeSegmentMatcher>(&pattern)) {
    const auto* x = get_if<WholeSegment>(&expr);
    if(!x) return typeMismatch("WholeSegment", expr);
    else if(m->data != x->data) return stringMismatch(m->data, x->data);
    else return nullopt;
  }
  if(holds_alternative<RegexPatternMatcher>(pattern)) {
    const auto* x = get_if<RegexPattern>(&expr);
    if(!x) return typeMismatch("RegexPattern", expr);
    // Just check that it's *some* regex, don't bother with regex contents.
    return nullopt;
  }
  if(const auto* m = get_if<BracketGroupMatcher>(&pattern)) {
    const auto* x = get_if<BracketGroup>(&expr);
    if(!x) return typeMismatch("BracketMatcher", expr);
    else if(m->type != x->type) return typeMismatch(m->type, x->type);
    else if(m->children.size() != x->children.size())
      return sizeMismatch(m->children.size(), x->children.size());

    for(size_t i=0; i<m->children.size(); ++i)
      if(auto err = match(m->children[i], x->children[i])) return err;
    return nullopt;
  }
  Bug("ExprMatcher has unknown type: {}", pattern.index());
}

}  // namespace oalex::lex::matcher
