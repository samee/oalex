/*  Copyright 2020 Google LLC

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "template.h"
#include <runtime/util.h>
using std::make_pair;
using std::nullopt;
using std::optional;
using std::pair;
using std::string;
using std::string_view;
using std::vector;
using oalex::lex::QuotedString;

namespace oalex {

static auto matchAllParts(const QuotedString& spatt, const QuotedString& s)
  -> optional<vector<pair<size_t, size_t>>> {
  if(spatt.empty()) return nullopt;
  vector<pair<size_t, size_t>> rv;
  for(size_t i=0; i+spatt.size() <= s.size(); ++i)
    if(s.substr(i, spatt.size()) == spatt) {
      if(!rv.empty() && rv.back().second<i) return nullopt; // Disallow overlaps
      else rv.push_back(make_pair(i, i+spatt.size()));
    }
  return rv;
}

static auto matchAllParts(const DelimPair& dpatt, const QuotedString& s)
  -> optional<vector<pair<size_t,size_t>>> {
  if(dpatt.st.empty() || dpatt.en.empty()) return nullopt;
  if(dpatt.st.find(dpatt.en, 1) != string::npos) return nullopt;
  vector<pair<size_t, size_t>> rv;
  for(size_t i=0; i+dpatt.st.size() <= s.size(); ++i)
    if(s.substr(i, dpatt.st.size()) == dpatt.st) {
      if(!rv.empty() && i<rv.back().second) return nullopt; // Disallow overlaps
      size_t j = s.find(dpatt.en, i+1);
      if(j == string::npos) return nullopt;  // TODO recover
      rv.push_back(make_pair(i, j+dpatt.en.size()));
    }
  return rv;
}

// TODO: Produce error message for every `return nullopt` here.
auto matchAllParts(const PartPattern& patt, const QuotedString& s)
  -> optional<vector<pair<size_t, size_t>>> {
  if(auto* spatt = get_if<QuotedString>(&patt)) return matchAllParts(*spatt, s);
  if(auto* dpatt = get_if<DelimPair>(&patt)) return matchAllParts(*dpatt, s);
  Bug()<<"matchAllParts() called with unknown variant: index "<<patt.index();
}

}  // namespace oalex
