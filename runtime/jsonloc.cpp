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

#include "jsonloc.h"
#include "jsonloc_fmt.h"

#include "fmt/format.h"
#include "util.h"
using fmt::format_to;
using fmt::memory_buffer;
using oalex::Bug;
using std::get;
using std::get_if;
using std::holds_alternative;
using std::make_pair;
using std::map;
using std::string;
using std::string_view;

namespace oalex {

using ErrorValue = JsonLoc::ErrorValue;
using Placeholder = JsonLoc::Placeholder;
using String = JsonLoc::String;
using Map = JsonLoc::Map;
using Vector = JsonLoc::Vector;

[[noreturn]] static void BugUnknownJsonType(const JsonLoc& json) {
  Bug("Strange JsonLoc type with index = {}", json.value.index());
}

// Template parameters parameterize over const-qualifiers.
template <class PlaceholderMap, class JsonLocInput>
static void allPlaceholdersImpl(PlaceholderMap& rv, JsonLocInput& json) {
  if(auto* p = get_if<Placeholder>(&json)) rv.emplace_back(p->key, &json);
  else if(holds_alternative<String>(json.value)) return;
  else if(auto* v = get_if<Vector>(&json))
    for(auto& elt : *v) allPlaceholdersImpl(rv,elt);
  else if(auto* m = get_if<Map>(&json))
    for(auto& [k,v] : *m) allPlaceholdersImpl(rv,v);
  else BugUnknownJsonType(json);
}

auto JsonLoc::allPlaceholders() -> PlaceholderMap {
  auto byfirst = [](auto a, auto b) { return a.first < b.first; };
  PlaceholderMap rv;
  allPlaceholdersImpl(rv,*this);
  std::sort(rv.begin(), rv.end(), byfirst);
  return {rv.begin(), rv.end()};
}

auto JsonLoc::allPlaceholders() const -> ConstPlaceholderMap {
  auto byfirst = [](auto a, auto b) { return a.first < b.first; };
  ConstPlaceholderMap rv;
  allPlaceholdersImpl(rv,*this);
  std::sort(rv.begin(), rv.end(), byfirst);
  return {rv.begin(), rv.end()};
}

size_t JsonLoc::substitute(const PlaceholderMap& pmap, string_view key,
                           const JsonLoc& json) {
  auto locmp = +[](const PlaceholderMap::value_type& kv, string_view key)
    { return kv.first < key; };
  auto hicmp = +[](string_view key, const  PlaceholderMap::value_type& kv)
    { return key < kv.first; };
  auto lo = std::lower_bound(pmap.begin(), pmap.end(), key, locmp);
  auto hi = std::upper_bound(pmap.begin(), pmap.end(), key, hicmp);
  size_t rv=0;
  for(auto it=lo; it!=hi; ++it) {
    *it->second = json;
    ++rv;
  }
  return rv;
}

// assumes (stPos == npos) == (enPos == npos)
static bool childIsGood(const JsonLoc& parent, const JsonLoc& child) {
  if(!child.substitutionsOk()) return false;
  else if(child.stPos == JsonLoc::npos) return true;
  else if(parent.stPos == JsonLoc::npos) return false;
  else if(child.stPos < parent.stPos) return false;
  else if(child.enPos > parent.enPos) return false;
  else return true;
}

bool JsonLoc::substitutionsOk() const {
  if(holds_alternative<Placeholder>(this->value)) return false;
  if((this->stPos==JsonLoc::npos) != (this->enPos==JsonLoc::npos)) return false;

  if(auto* v = get_if<Vector>(&this->value)) {
    for(auto& elt : *v) if(!childIsGood(*this,elt)) return false;
  } else if(auto* m = get_if<Map>(&this->value)) {
    for(auto& [k,v] : *m) if(!childIsGood(*this,v)) return false;
  } else if(!holds_alternative<String>(this->value)) {
    BugUnknownJsonType(*this);
  }
  return true;
}

static void printString(fmt::memory_buffer& buf, string_view s) {
  format_to(buf, "\"");
  // If this changes, please change lexQuotedEscape as well.
  // TODO write test.
  for(char ch : s) {
    if(ch=='"') format_to(buf, "\"");
    else if(ch=='\\') format_to(buf, "\\\\");
    else if(ch=='\n') format_to(buf, "\\n");
    else if(ch=='\t') format_to(buf, "\\t");
    else if(isprint(ch))
      format_to(buf, "{}", ch);  // check this after '"' and '\\'.
    else format_to(buf, "\\x{:02x}", ch);
  }
  format_to(buf, "\"");
}

static string_view assertIdent(string_view ctx, string_view s) {
  if(s.empty()) Bug("{}: Identifier can't be null.", ctx);
  if(isdigit(s[0])) Bug("{}: Identifier can't start with a digit.", ctx);
  for(size_t i=0; i<s.size(); ++i) if(s[i]!='_' && !isalnum(s[i]))
    Bug("{}: Invalid identifier character at position {}.", ctx, i);
  return s;
}

static void prettyPrint(fmt::memory_buffer& buf,
                        size_t indent, const JsonLoc& json,
                        bool quoteMapKeys) {
  if(auto* p = get_if<Placeholder>(&json))
    format_to(buf, "{}", assertIdent(__func__, p->key));
  else if(auto* s = get_if<String>(&json)) printString(buf, *s);
  else if(auto* v = get_if<Vector>(&json)) {
    format_to(buf, "[\n");
    bool first = true;
    for(const JsonLoc& elt : *v) {
      if(!first) format_to(buf, ",\n");
      first = false;
      format_to(buf, "{:{}}", "", indent+2);
      prettyPrint(buf, indent+2, elt, quoteMapKeys);
    }
    format_to(buf, "\n{:{}}]", "", indent);
  }else if(auto* m = get_if<Map>(&json)) {
    format_to(buf, "{{\n");
    bool first = true;
    for(auto& [k,v] : *m) {
      if(!first) format_to(buf, ",\n");
      first = false;
      if(quoteMapKeys) {
        format_to(buf, "{:{}}\"{}\": ", "", indent+2, assertIdent(__func__,k));
      }else {
        format_to(buf, "{:{}}{}: ", "", indent+2, assertIdent(__func__,k));
      }
      prettyPrint(buf, indent+2, v, quoteMapKeys);
    }
    format_to(buf, "\n{:{}}}}", "", indent);
  }else if(holds_alternative<ErrorValue>(json.value)) {
    format_to(buf, "(error_value)");
  }else BugUnknownJsonType(json);
}

string JsonLoc::prettyPrint(size_t indent) const {
  fmt::memory_buffer buf;
  oalex::prettyPrint(buf, indent, *this, false);
  return fmt::to_string(buf);
}

string JsonLoc::prettyPrintJson(size_t indent) const {
  fmt::memory_buffer buf;
  oalex::prettyPrint(buf, indent, *this, true);
  return fmt::to_string(buf);
}

bool JsonLoc::supportsEquality() const {
  if(holds_alternative<Placeholder>(value) ||
     holds_alternative<ErrorValue>(value)) return false;
  if(holds_alternative<String>(value)) return true;
  if(auto* vec = get_if<Vector>(&value)) {
    for(auto& v : *vec) if(!v.supportsEquality()) return false;
    return true;
  }
  if(auto* m = get_if<Map>(&value)) {
    for(auto& [k,v] : *m) if(!v.supportsEquality()) return false;
    return true;
  }
  BugUnknownJsonType(*this);
}

bool JsonLoc::operator==(const JsonLoc& that) const {
  if(this->value.index() != that.value.index()) return false;

  if(holds_alternative<Placeholder>(value) ||
     holds_alternative<ErrorValue>(value)) Bug("supportsEquality() is false");
  if(auto* s = get_if<String>(&value)) return *s == get<String>(that.value);
  if(auto* v = get_if<Vector>(&value)) return *v == get<Vector>(that.value);
  if(auto* m = get_if<Map>(&value)) return *m == get<Map>(that.value);
  BugUnknownJsonType(*this);
}

}  // namespace oalex

auto fmt::formatter<oalex::JsonLoc>::parse(format_parse_context& ctx)
  -> decltype(ctx.begin()) {
  constexpr int max_indent = 100;
  auto it = ctx.begin();
  while(it != ctx.end() && *it != '}') {
    if(!isdigit(*it)) Bug("invalid JsonLoc indent");
    indent_ = 10*indent_ + *it - '0';
    if(indent_ > max_indent) Bug("Indent can't be more than {}", max_indent);
    ++it;
  }
  return it;
}
