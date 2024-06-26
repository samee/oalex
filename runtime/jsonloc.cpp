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

#include "jsonloc.h"
#include "jsonloc_fmt.h"

#include <algorithm>
#include <format>
#include <iterator>
#include "util.h"
using oalex::Bug;
using std::back_insert_iterator;
using std::format_to;
using std::get;
using std::make_pair;
using std::pair;
using std::sort;
using std::string;
using std::string_view;
using std::vector;

namespace oalex {

using ErrorValue = JsonLoc::ErrorValue;
using String = JsonLoc::String;
using Map = JsonLoc::Map;
using Vector = JsonLoc::Vector;

[[noreturn]] static void BugUnknownJsonType(const JsonLoc::Tag& tag) {
  Bug("Strange JsonLoc type with enum = {}", int(tag));
}

void JsonLoc::copyValue(const JsonLoc& that) {
  switch(tag_) {
    case Tag::ErrorValue: return;
    case Tag::String:
      new (&stringValue_) String{that.stringValue_}; return;
    case Tag::Vector:
      new (&vectorValue_) Vector(that.vectorValue_); return;
    case Tag::Map:
      new (&mapValue_) Map(that.mapValue_); return;
  }
}

void JsonLoc::moveValue(JsonLoc&& that) {
  switch(tag_) {
    case Tag::ErrorValue: return;
    case Tag::String:
      new (&stringValue_) String{std::move(that.stringValue_)}; return;
    case Tag::Vector:
      new (&vectorValue_) Vector(std::move(that.vectorValue_)); return;
    case Tag::Map:
      new (&mapValue_) Map(std::move(that.mapValue_)); return;
  }
}

void JsonLoc::destroyValue() {
  switch(tag_) {
    case Tag::ErrorValue: return;
    case Tag::String: stringValue_.~string(); return;
    case Tag::Vector: vectorValue_.~vector(); return;
    case Tag::Map: mapValue_.~vector(); return;
  }
}

JsonLoc::JsonLoc(JsonLoc&& that)
  : stPos{that.stPos}, enPos{that.enPos},
    tag_{that.tag_} {
  moveValue(std::move(that));
}
JsonLoc::JsonLoc(const JsonLoc& that)
  : stPos{that.stPos}, enPos{that.enPos}, tag_{that.tag_} {
  copyValue(that);
}
JsonLoc& JsonLoc::operator=(JsonLoc&& that) {
  JsonLoc tmp{std::move(that)};  // that may be part of this. Save that first.
  destroyValue();
  this->stPos = tmp.stPos;
  this->enPos = tmp.enPos;
  this->tag_ = tmp.tag_;
  moveValue(std::move(tmp));
  return *this;
}
JsonLoc& JsonLoc::operator=(const JsonLoc& that) {
  JsonLoc tmp{that};  // that may be part of this. Save that first.
  destroyValue();
  this->stPos = tmp.stPos;
  this->enPos = tmp.enPos;
  this->tag_ = tmp.tag_;
  moveValue(std::move(tmp));
  return *this;
}
JsonLoc::~JsonLoc() { destroyValue(); }

string_view JsonLoc::tagName() const {
  switch(tag_) {
    case Tag::ErrorValue: return "ErrorValue";
    case Tag::String: return "String";
    case Tag::Vector: return "Vector";
    case Tag::Map: return "Map";
    default: BugUnknownJsonType(tag_);
  }
}

ssize_t JsonLoc::mapScanForIndex(const Map& m, string_view k) {
  for(ssize_t i=0; i<ssize(m); ++i) if(k == m[i].first) return i;
  return -1;
}
const JsonLoc* JsonLoc::mapScanForValue(const Map& m, string_view k) {
  ssize_t i = mapScanForIndex(m, k);
  return i != -1 ? &m[i].second : nullptr;
}
JsonLoc* JsonLoc::mapScanForValue(Map& m, std::string_view k) {
  ssize_t i = mapScanForIndex(m, k);
  return i != -1 ? &m[i].second : nullptr;
}

void JsonLoc::mapSort(Map& m) {
  auto byfirst = +[](const Map::value_type& a, const Map::value_type& b)
    { return a.first < b.first; };
  sort(m.begin(), m.end(), byfirst);

  for(size_t i=1; i<m.size(); ++i) if(m[i-1].first == m[i].first)
    Bug("maps are supposed to be key-disjoint");
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
  if((this->stPos==JsonLoc::npos) != (this->enPos==JsonLoc::npos)) return false;

  if(auto* v = getIfVector()) {
    for(auto& elt : *v) if(!childIsGood(*this,elt)) return false;
  } else if(auto* m = getIfMap()) {
    for(auto& [k,v] : *m) if(!childIsGood(*this,v)) return false;
  } else if(!holdsString()) {
    BugUnknownJsonType(tag_);
  }
  return true;
}

static void prettyPrint(string& buf, size_t indent, const JsonLoc& json,
                        bool quoteMapKeys) {
  back_insert_iterator buf_app{buf};
  if(auto* s = json.getIfString()) printJsonLocString(buf, *s);
  else if(auto* v = json.getIfVector()) {
    format_to(buf_app, "[\n");
    bool first = true;
    for(const JsonLoc& elt : *v) {
      if(!first) format_to(buf_app, ",\n");
      first = false;
      format_to(buf_app, "{:{}}", "", indent+2);
      prettyPrint(buf, indent+2, elt, quoteMapKeys);
    }
    format_to(buf_app, "\n{:{}}]", "", indent);
  }else if(auto* m = json.getIfMap()) {
    format_to(buf_app, "{{\n");
    bool first = true;
    for(auto& [k,v] : *m) {
      if(!first) format_to(buf_app, ",\n");
      first = false;
      if(quoteMapKeys) {
        format_to(buf_app, "{:{}}\"{}\": ", "", indent+2,
                  assertJsonLocKey(__func__,k));
      }else {
        format_to(buf_app, "{:{}}{}: ", "", indent+2,
                  assertJsonLocKey(__func__, k));
      }
      prettyPrint(buf, indent+2, v, quoteMapKeys);
    }
    format_to(buf_app, "\n{:{}}}}", "", indent);
  }else if(json.holdsErrorValue()) {
    format_to(buf_app, "(error_value)");
  }else BugUnknownJsonType(json.tag());
}

string JsonLoc::prettyPrint(size_t indent) const {
  string buf;
  oalex::prettyPrint(buf, indent, *this, false);
  return buf;
}

string JsonLoc::prettyPrintJson(size_t indent) const {
  string buf;
  oalex::prettyPrint(buf, indent, *this, true);
  return buf;
}

bool JsonLoc::operator==(const JsonLoc& that) const {
  if(this->tag_ != that.tag_) return false;

  if(holdsErrorValue()) return true;
  if(auto* s = getIfString()) return *s == *that.getIfString();
  if(auto* v = getIfVector()) return *v == *that.getIfVector();
  if(auto* m = getIfMap()) return *m == *that.getIfMap();
  BugUnknownJsonType(tag_);
}

void mapNestedAppend(JsonLoc& jsloc, const vector<JsonPathComp>& path_to_map,
                     string new_key, JsonLoc new_value) {
  JsonLoc* cur = &jsloc;
  for(auto& comp : path_to_map) {
    if(comp.key.empty()) cur = &cur->getIfVector()->at(comp.pos);
    else cur = JsonLoc::mapScanForValue(*cur->getIfMap(), comp.key);
  }
  // TODO: Print out JsonPathComp in dot-notation.
  JsonLoc::Map* m = cur->getIfMap();
  if(m == nullptr) Bug("Can only append to map values");
  m->push_back({std::move(new_key), std::move(new_value)});
  JsonLoc::mapSort(*m);
}

JsonPathComp::JsonPathComp(std::string s)
  : key{std::move(s)}, pos{-1} {
  if(key.empty()) Bug("JsonPath component cannot have an empty key");
}
JsonPathComp::JsonPathComp(const char* s) : JsonPathComp{string{s}} {}
JsonPathComp::JsonPathComp(ssize_t pos)
  : key{}, pos{pos} {
  if(pos < 0) Bug("JsonPath position cannot be negative");
}

}  // namespace oalex

auto std::formatter<oalex::JsonLoc>::parse(format_parse_context& ctx)
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
