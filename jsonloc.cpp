/*  Copyright 2019 Google LLC

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

#include "fmt/format.h"
#include "runtime/util.h"
using fmt::format_to;
using fmt::memory_buffer;
using std::get_if;
using std::holds_alternative;
using std::make_pair;
using std::map;
using std::string;
using std::string_view;

namespace oalex {

using Placeholder = JsonLoc::Placeholder;
using String = JsonLoc::String;
using Map = JsonLoc::Map;
using Vector = JsonLoc::Vector;

[[noreturn]] static void BugUnknownJsonType(const JsonLoc& json) {
  BugFmt("Strange JsonLoc type with index = {}", json.value.index());
}

static void allPlaceholdersImpl(JsonLoc::PlaceholderMap& rv,
                                JsonLoc& json) {
  if(auto* p = get_if<Placeholder>(&json))
    rv.insert(make_pair(p->key, &json));
  else if(holds_alternative<String>(json.value)) return;
  else if(auto* v = get_if<Vector>(&json))
    for(auto& elt : *v) allPlaceholdersImpl(rv,elt);
  else if(auto* m = get_if<Map>(&json))
    for(auto& [k,v] : *m) allPlaceholdersImpl(rv,v);
  else BugUnknownJsonType(json);
}

auto JsonLoc::allPlaceholders() -> PlaceholderMap {
  PlaceholderMap rv;
  allPlaceholdersImpl(rv,*this);
  return rv;
}

size_t JsonLoc::substitute(const PlaceholderMap& pmap, string_view key,
                           const JsonLoc& json) {
  auto [lo,hi] = pmap.equal_range(key);
  size_t rv=0;
  for(auto it=lo; it!=hi; ++it) {
    *it->second = json;
    ++rv;
  }
  return rv;
}

// assumes (stpos == npos) == (enpos == npos)
static bool childIsGood(const JsonLoc& parent, const JsonLoc& child) {
  if(!child.substitutionsOk()) return false;
  else if(child.stpos == JsonLoc::npos) return true;
  else if(parent.stpos == JsonLoc::npos) return false;
  else if(child.stpos < parent.stpos) return false;
  else if(child.enpos > parent.enpos) return false;
  else return true;
}

bool JsonLoc::substitutionsOk() const {
  if(holds_alternative<Placeholder>(this->value)) return false;
  if((this->stpos==JsonLoc::npos) != (this->enpos==JsonLoc::npos)) return false;

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
  if(s.empty()) BugFmt("{}: Identifier can't be null.", ctx);
  if(isdigit(s[0])) BugFmt("{}: Identifier can't start with a digit.", ctx);
  for(size_t i=0; i<s.size(); ++i) if(s[i]!='_' && !isalnum(s[i]))
    BugFmt("{}: Invalid identifier character at position {}.", ctx, i);
  return s;
}

static void prettyPrint(fmt::memory_buffer& buf,
                        size_t indent, const JsonLoc& json) {
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
      prettyPrint(buf, indent+2, elt);
    }
    format_to(buf, "\n{:{}}]", "", indent);
  }else if(auto* m = get_if<Map>(&json)) {
    format_to(buf, "{{\n");
    bool first = true;
    for(auto& [k,v] : *m) {
      if(!first) format_to(buf, ",\n");
      first = false;
      format_to(buf, "{:{}}{}: ", "", indent+2, assertIdent(__func__,k));
      prettyPrint(buf, indent+2, v);
    }
    format_to(buf, "\n{:{}}}}", "", indent);
  }else BugUnknownJsonType(json);
}

string JsonLoc::prettyPrint(size_t indent) const {
  fmt::memory_buffer buf;
  oalex::prettyPrint(buf, indent, *this);
  return fmt::to_string(buf);
}

}  // namespace oalex
