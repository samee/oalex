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

#include <iomanip>
#include <sstream>
#include "runtime/util.h"
using std::get_if;
using std::holds_alternative;
using std::make_pair;
using std::map;
using std::ostringstream;
using std::string;
using std::string_view;
using oalex::Bug;

namespace oalex {

using Placeholder = JsonLoc::Placeholder;
using String = JsonLoc::String;
using Map = JsonLoc::Map;
using Vector = JsonLoc::Vector;

[[noreturn]] static void BugUnknownJsonType(const JsonLoc& json) {
  Bug()<<"Strange JsonLoc type with index = "<<json.value.index();
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

static void printString(ostringstream& os, string_view s) {
  os<<'"';
  // If this changes, please change lexQuotedEscape as well.
  // TODO write test.
  for(char ch : s) {
    if(ch=='"') os<<"\\\"";
    else if(ch=='\\') os<<"\\\\";
    else if(ch=='\n') os<<"\\n";
    else if(ch=='\t') os<<"\\t";
    else if(isprint(ch)) os<<ch;  // check this after '"' and '\\'.
    else os<<"\\x"<<std::setfill('0')<<std::setw(2)<<std::hex<<int(ch);
  }
  os<<'"';
}

static string_view assertIdent(string_view ctx, string_view s) {
  if(s.empty()) Bug()<<ctx<<": Identifier can't be null.";
  if(isdigit(s[0])) Bug()<<ctx<<": Identifier can't start with a digit.";
  for(size_t i=0; i<s.size(); ++i) if(s[i]!='_' && !isalnum(s[i]))
    Bug()<<ctx<<": Invalid identifier character at position "<<i;
  return s;
}

static void prettyPrint(ostringstream& os, size_t indent, const JsonLoc& json) {
  if(auto* p = get_if<Placeholder>(&json)) os<<assertIdent(__func__,p->key);
  else if(auto* s = get_if<String>(&json)) printString(os,*s);
  else if(auto* v = get_if<Vector>(&json)) {
    os<<"[\n";
    bool first = true;
    for(const JsonLoc& elt : *v) {
      if(!first) os<<",\n";
      first = false;
      os<<string(indent+2,' ');
      prettyPrint(os,indent+2,elt);
    }
    os<<'\n'<<string(indent,' ')<<']';
  }else if(auto* m = get_if<Map>(&json)) {
    os<<"{\n";
    bool first = true;
    for(auto& [k,v] : *m) {
      if(!first) os<<",\n";
      first = false;
      os<<string(indent+2,' ')<<assertIdent(__func__,k)<<": ";
      prettyPrint(os,indent+2,v);
    }
    os<<"\n"<<string(indent,' ')<<'}';
  }else BugUnknownJsonType(json);
}

string JsonLoc::prettyPrint(size_t indent) const {
  ostringstream os;
  oalex::prettyPrint(os,indent,*this);
  return os.str();
}

}  // namespace oalex
