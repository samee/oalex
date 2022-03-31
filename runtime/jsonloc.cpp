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
using std::make_pair;
using std::pair;
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
  destroyValue();
  this->stPos = that.stPos;
  this->enPos = that.enPos;
  this->tag_ = that.tag_;
  moveValue(std::move(that));
  return *this;
}
JsonLoc& JsonLoc::operator=(const JsonLoc& that) {
  destroyValue();
  this->stPos = that.stPos;
  this->enPos = that.enPos;
  this->tag_ = that.tag_;
  copyValue(that);
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

// TODO: refactor this with its duplicate in jsontmpl.cpp.
// We've already had to fix a bug here twice.
static void printString(fmt::memory_buffer& buf, string_view s) {
  format_to(buf, "\"");
  // If this changes, please change lexQuotedEscape as well.
  // TODO write test.
  for(char ch : s) {
    if(ch=='"') format_to(buf, "\\\"");
    else if(ch=='\\') format_to(buf, "\\\\");
    else if(ch=='\n') format_to(buf, "\\n");
    else if(ch=='\t') format_to(buf, "\\t");
    else if(isprint(ch))
      format_to(buf, "{}", ch);  // check this after '"' and '\\'.
    else format_to(buf, "\\x{:02x}", ch);
  }
  format_to(buf, "\"");
}

// TODO: refactor this with its duplicate in jsontmpl.cpp.
// We've already had to fix a bug here twice.
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
  if(auto* s = json.getIfString()) printString(buf, *s);
  else if(auto* v = json.getIfVector()) {
    format_to(buf, "[\n");
    bool first = true;
    for(const JsonLoc& elt : *v) {
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
  }else if(json.holdsErrorValue()) {
    format_to(buf, "(error_value)");
  }else BugUnknownJsonType(json.tag());
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

bool JsonLoc::operator==(const JsonLoc& that) const {
  if(this->tag_ != that.tag_) return false;

  if(holdsErrorValue()) return true;
  if(auto* s = getIfString()) return *s == *that.getIfString();
  if(auto* v = getIfVector()) return *v == *that.getIfVector();
  if(auto* m = getIfMap()) return *m == *that.getIfMap();
  BugUnknownJsonType(tag_);
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
