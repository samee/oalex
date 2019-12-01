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
#include "util.h"
using std::get_if;
using std::holds_alternative;
using std::make_pair;
using std::map;
using std::string;
using std::string_view;
using oalex::Bug;

namespace oalex {

auto JsonLoc::allPlaceholders() -> PlaceholderMap {
  PlaceholderMap rv;
  allPlaceholdersImpl(rv);
  return rv;
}

void JsonLoc::allPlaceholdersImpl(PlaceholderMap& rv) {
  if(auto* p = get_if<Placeholder>(&this->value))
    rv.insert(make_pair(p->key, this));
  else if(holds_alternative<String>(this->value)) return;
  else if(auto* v = get_if<Vector>(&this->value))
    for(auto& elt : *v) elt.allPlaceholdersImpl(rv);
  else if(auto* m = get_if<Map>(&this->value))
    for(auto& [k,v] : *m) v.allPlaceholdersImpl(rv);
  else Bug()<<"Strange JsonLoc type with index = "<<this->value.index();
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
    Bug()<<"Strange JsonLoc type with index = "<<this->value.index();
  }
  return true;
}

}  // namespace oalex
