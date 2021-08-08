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

#include "jsontmpl.h"
#include "fmt/format.h"
#include "runtime/jsonloc.h"
#include "runtime/util.h"
using fmt::format_to;
using fmt::memory_buffer;
using oalex::Bug;
using std::get;
using std::make_pair;
using std::pair;
using std::string;
using std::string_view;
using std::vector;

namespace oalex {

using Placeholder = JsonTmpl::Placeholder;
using String = JsonTmpl::String;
using Map = JsonTmpl::Map;
using Vector = JsonTmpl::Vector;

[[noreturn]] static void BugUnknownJsonType(const JsonTmpl::Tag& tag) {
  Bug("Strange JsonTmpl type with enum = {}", int(tag));
}

void JsonTmpl::copyValue(const JsonTmpl& that) {
  switch(tag_) {
    case Tag::String:
      new (&stringValue_) String{that.stringValue_}; return;
    case Tag::Vector:
      new (&vectorValue_) Vector(that.vectorValue_); return;
    case Tag::Map:
      new (&mapValue_) Map(that.mapValue_); return;
    case Tag::Placeholder:
      new (&placeholderValue_) Placeholder{that.placeholderValue_};
  }
}

void JsonTmpl::moveValue(JsonTmpl&& that) {
  switch(tag_) {
    case Tag::String:
      new (&stringValue_) String{std::move(that.stringValue_)}; return;
    case Tag::Vector:
      new (&vectorValue_) Vector(std::move(that.vectorValue_)); return;
    case Tag::Map:
      new (&mapValue_) Map(std::move(that.mapValue_)); return;
    case Tag::Placeholder:
      new (&placeholderValue_) Placeholder{std::move(that.placeholderValue_)};
  }
}

void JsonTmpl::destroyValue() {
  switch(tag_) {
    case Tag::String: stringValue_.~string(); return;
    case Tag::Vector: vectorValue_.~vector(); return;
    case Tag::Map: mapValue_.~vector(); return;
    case Tag::Placeholder: placeholderValue_.~Placeholder(); return;
  }
}

JsonTmpl::JsonTmpl(JsonTmpl&& that)
  : stPos{that.stPos}, enPos{that.enPos},
    tag_{that.tag_} {
  moveValue(std::move(that));
}
JsonTmpl::JsonTmpl(const JsonTmpl& that)
  : stPos{that.stPos}, enPos{that.enPos}, tag_{that.tag_} {
  copyValue(that);
}
JsonTmpl& JsonTmpl::operator=(JsonTmpl&& that) {
  destroyValue();
  this->stPos = that.stPos;
  this->enPos = that.enPos;
  this->tag_ = that.tag_;
  moveValue(std::move(that));
  return *this;
}
JsonTmpl& JsonTmpl::operator=(const JsonTmpl& that) {
  destroyValue();
  this->stPos = that.stPos;
  this->enPos = that.enPos;
  this->tag_ = that.tag_;
  copyValue(that);
  return *this;
}
JsonTmpl::~JsonTmpl() { destroyValue(); }

string_view JsonTmpl::tagName() const {
  switch(tag_) {
    case Tag::String: return "String";
    case Tag::Vector: return "Vector";
    case Tag::Map: return "Map";
    case Tag::Placeholder: return "Placeholder";
    default: BugUnknownJsonType(tag_);
  }
}

ssize_t JsonTmpl::mapLinearFind(const Map& m, std::string_view k) {
  for(ssize_t i=0; i<ssize(m); ++i) if(k == m[i].first) return i;
  return -1;
}

void JsonTmpl::mapSort(Map& m) {
  auto byfirst = +[](const Map::value_type& a, const Map::value_type& b)
    { return a.first < b.first; };
  sort(m.begin(), m.end(), byfirst);

  for(size_t i=1; i<m.size(); ++i) if(m[i-1].first == m[i].first)
    Bug("maps are supposed to be key-disjoint");
}

// Template parameters parameterize over const-qualifiers.
template <class PlaceholderMap, class JsonTmplInput>
static void allPlaceholdersImpl(PlaceholderMap& rv, JsonTmplInput& json) {
  if(auto* p = json.getIfPlaceholder()) rv.emplace_back(p->key, &json);
  else if(json.holdsString()) return;
  else if(auto* v = json.getIfVector())
    for(auto& elt : *v) allPlaceholdersImpl(rv,elt);
  else if(auto* m = json.getIfMap())
    for(auto& [k,v] : *m) allPlaceholdersImpl(rv,v);
  else BugUnknownJsonType(json.tag());
}

auto JsonTmpl::allPlaceholders() const -> ConstPlaceholderMap {
  auto byfirst = [](auto a, auto b) { return a.first < b.first; };
  ConstPlaceholderMap rv;
  allPlaceholdersImpl(rv,*this);
  std::sort(rv.begin(), rv.end(), byfirst);
  return {rv.begin(), rv.end()};
}

static JsonLoc
findSub(const vector<pair<string, JsonLoc>>& subs,
        const JsonTmpl::Placeholder& p) {
  for(auto& [k, v] : subs) if (k == p.key) return v;
  Bug("No substitutions found for '{}'", p.key);
}

JsonLoc
JsonTmpl::substituteAll(const vector<pair<string, JsonLoc>>& subs) const {
  if(auto* s = getIfString()) return *s;
  else if(auto* p = getIfPlaceholder()) return findSub(subs, *p);
  else if(auto* v = getIfVector()) {
    JsonLoc::Vector rv;
    for(auto& elt : *v) rv.push_back(elt.substituteAll(subs));
    return rv;
  }else if(auto* m = getIfMap()) {
    JsonLoc::Map rv;
    for(auto& [k,v] : *m) rv.emplace_back(k, v.substituteAll(subs));
    return rv;
  }
  return JsonLoc::ErrorValue{};
}

JsonLoc JsonTmpl::outputIfFilled() const {
  return substituteAll({});
}

bool JsonTmpl::substitutionsNeeded() const {
  if(holdsPlaceholder()) return true;
  if(auto* v = getIfVector()) {
    for(auto& elt : *v) if(elt.substitutionsNeeded()) return true;
  } else if(auto* m = getIfMap()) {
    for(auto& [k,v] : *m) if(v.substitutionsNeeded()) return true;
  }
  return false;
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
                        size_t indent, const JsonTmpl& json,
                        bool quoteMapKeys) {
  if(auto* p = json.getIfPlaceholder())
    format_to(buf, "{}", assertIdent(__func__, p->key));
  else if(auto* s = json.getIfString()) printString(buf, *s);
  else if(auto* v = json.getIfVector()) {
    format_to(buf, "[\n");
    bool first = true;
    for(const JsonTmpl& elt : *v) {
      if(!first) format_to(buf, ",\n");
      first = false;
      format_to(buf, "{:{}}", "", indent+2);
      prettyPrint(buf, indent+2, elt, quoteMapKeys);
    }
    format_to(buf, "\n{:{}}]", "", indent);
  }else if(auto* m = json.getIfMap()) {
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
  }else BugUnknownJsonType(json.tag());
}

string JsonTmpl::prettyPrint(size_t indent) const {
  fmt::memory_buffer buf;
  oalex::prettyPrint(buf, indent, *this, false);
  return fmt::to_string(buf);
}

string JsonTmpl::prettyPrintJson(size_t indent) const {
  fmt::memory_buffer buf;
  oalex::prettyPrint(buf, indent, *this, true);
  return fmt::to_string(buf);
}

}  // namespace oalex
