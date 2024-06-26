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

#include "jsontmpl.h"

#include <algorithm>
#include <format>
#include <iterator>
#include "runtime/jsonloc.h"
#include "runtime/jsonloc_fmt.h"
#include "runtime/util.h"
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
      new (&placeholderValue_) Placeholder{that.placeholderValue_}; return;
    case Tag::Ellipsis: return;
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
      return;
    case Tag::Ellipsis: return;
  }
}

void JsonTmpl::destroyValue() {
  switch(tag_) {
    case Tag::String: stringValue_.~string(); return;
    case Tag::Vector: vectorValue_.~vector(); return;
    case Tag::Map: mapValue_.~vector(); return;
    case Tag::Placeholder: placeholderValue_.~Placeholder(); return;
    case Tag::Ellipsis: return;
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
  JsonTmpl tmp{std::move(that)};  // that may be part of this. Save that first.
  destroyValue();
  this->stPos = tmp.stPos;
  this->enPos = tmp.enPos;
  this->tag_ = tmp.tag_;
  moveValue(std::move(tmp));
  return *this;
}
JsonTmpl& JsonTmpl::operator=(const JsonTmpl& that) {
  JsonTmpl tmp{that};  // that may be part of this. Save that first.
  destroyValue();
  this->stPos = tmp.stPos;
  this->enPos = tmp.enPos;
  this->tag_ = tmp.tag_;
  moveValue(std::move(tmp));
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

ssize_t JsonTmpl::mapScanForIndex(const Map& m, string_view k) {
  for(ssize_t i=0; i<ssize(m); ++i) if(k == m[i].first) return i;
  return -1;
}

const JsonTmpl* JsonTmpl::mapScanForValue(const Map& m, string_view k) {
  ssize_t i = mapScanForIndex(m, k);
  return i != -1 ? &m[i].second : nullptr;
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
  else if(json.holdsString() || json.holdsEllipsis()) return;
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

static const JsonLoc*
findSub(const vector<pair<string, JsonLoc>>& subs,
        const JsonTmpl::Placeholder& p) {
  for(auto& [k, v] : subs) if (k == p.key) return &v;
  return nullptr;
}

JsonLoc
JsonTmpl::substituteAll(const vector<pair<string, JsonLoc>>& subs) const {
  if(auto* s = getIfString()) return *s;
  else if(holdsEllipsis()) Bug("Tried to produce JsonLoc out of '...'");
  else if(auto* p = getIfPlaceholder()) {
    if(auto* jsloc = findSub(subs, *p)) return *jsloc;
    Bug("No substitutions foudn for '{}'", p->key);
  }else if(auto* v = getIfVector()) {
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

// substituteAll() requires that all placeholders in *this have valid
// substitutions present in `subs`. On the other hand, substituteOrDrop() tries
// to do something "reasonable" for missing placeholders.
//
//   * If the placeholder appears directly as a json dictionary entry, both the
//     key and the value will be dropped from the output (so that a missing key
//     can be used to detect a missing value).
//   * For all other cases (e.g. if the placeholder is a list element), it is
//     replaced with an empty dictionary if no substitutions are provided.
//
// This behavior defines how eval() handles missing optional fields.
JsonLoc
JsonTmpl::substituteOrDrop(const vector<pair<string, JsonLoc>>& subs) const {
  if(auto* s = getIfString()) return *s;
  else if(holdsEllipsis()) Bug("Tried to produce JsonLoc out of '...'");
  else if(auto* p = getIfPlaceholder()) {
    if(const JsonLoc* j = findSub(subs, *p)) return *j;
    else return JsonLoc::Map{{}};
  }else if(auto* v = getIfVector()) {
    JsonLoc::Vector rv;
    for(auto& elt : *v) rv.push_back(elt.substituteOrDrop(subs));
    return rv;
  }else if(auto* m = getIfMap()) {
    JsonLoc::Map rv;
    for(auto& [k,v] : *m) {
      if(auto* p = v.getIfPlaceholder()) {
        if(!findSub(subs, *p)) continue;
      }
      rv.emplace_back(k, v.substituteOrDrop(subs));
    }
    return rv;
  }
  return JsonLoc::ErrorValue{};
}

JsonLoc JsonTmpl::outputIfFilled() const {
  return substituteAll({});
}

bool JsonTmpl::substitutionsNeeded() const {
  if(holdsPlaceholder()) return true;
  if(holdsEllipsis()) return false;
  if(auto* v = getIfVector()) {
    for(auto& elt : *v) if(elt.substitutionsNeeded()) return true;
  } else if(auto* m = getIfMap()) {
    for(auto& [k,v] : *m) if(v.substitutionsNeeded()) return true;
  }
  return false;
}

static void prettyPrint(string& buf,
                        size_t indent, const JsonTmpl& json,
                        bool quoteMapKeys) {
  back_insert_iterator buf_app{buf};
  if(auto* p = json.getIfPlaceholder())
    format_to(buf_app, "{}", assertJsonLocKey(__func__, p->key));
  else if(json.holdsEllipsis()) format_to(buf_app, "...");
  else if(auto* s = json.getIfString()) printJsonLocString(buf, *s);
  else if(auto* v = json.getIfVector()) {
    format_to(buf_app, "[\n");
    bool first = true;
    for(const JsonTmpl& elt : *v) {
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
                  assertJsonLocKey(__func__,k));
      }
      prettyPrint(buf, indent+2, v, quoteMapKeys);
    }
    format_to(buf_app, "\n{:{}}}}", "", indent);
  }else BugUnknownJsonType(json.tag());
}

string JsonTmpl::prettyPrint(size_t indent) const {
  string buf;
  oalex::prettyPrint(buf, indent, *this, false);
  return buf;
}

string JsonTmpl::prettyPrintJson(size_t indent) const {
  string buf;
  oalex::prettyPrint(buf, indent, *this, true);
  return buf;
}

}  // namespace oalex
