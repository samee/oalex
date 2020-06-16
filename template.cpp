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
#include <algorithm>
#include <map>
#include "fmt/format.h"
#include "runtime/util.h"
using fmt::format;
using std::get_if;
using std::make_pair;
using std::map;
using std::max;
using std::nullopt;
using std::optional;
using std::pair;
using std::string;
using std::string_view;
using std::vector;
using oalex::lex::QuotedString;
using oalex::lex::UnquotedToken;

namespace oalex {

static string debug(const QuotedString& qs) { return qs; }
static string debug(const DelimPair& dp) {
  return format("{} ... {}", string(dp.st), string(dp.en));
}
static string debug(const PartPattern& pp) {
  if(auto* p = get_if<QuotedString>(&pp)) return debug(*p);
  if(auto* p = get_if<DelimPair>(&pp)) return debug(*p);
  Bug("Unknown PartPattern with index {}", pp.index());
}

static auto matchAllParts(const QuotedString& spatt, const QuotedString& s)
  -> optional<vector<pair<size_t, size_t>>> {
  if(spatt.empty())
    return Error(spatt, 0, spatt.size(), "Placeholder pattern cannot be empty");
  vector<pair<size_t, size_t>> rv;
  for(size_t i=0; i+spatt.size() <= s.size(); ++i)
    if(s.substr(i, spatt.size()) == spatt) {
      if(!rv.empty() && i<rv.back().second)
        return Error(s, rv.back().first, i + spatt.size(),
                     format("Pattern '{}' matches overlapping segments",
                            debug(spatt)));
      else rv.push_back(make_pair(i, i+spatt.size()));
    }
  return rv;
}

static auto matchAllParts(const DelimPair& dpatt, const QuotedString& s)
  -> optional<vector<pair<size_t,size_t>>> {
  if(dpatt.st.empty() || dpatt.en.empty()) {
    const QuotedString& qs = (dpatt.st.empty() ? dpatt.st : dpatt.en);
    return Error(qs, 0, qs.size(), "Placeholder pattern cannot be empty");
  }
  if(dpatt.st.find(dpatt.en, 1) != string::npos)
    return Error(dpatt.en, 0, dpatt.en.size(),
                 "End pattern is a substring of the start pattern");
  vector<pair<size_t, size_t>> rv;
  for(size_t i=0; i+dpatt.st.size() <= s.size(); ++i)
    if(s.substr(i, dpatt.st.size()) == dpatt.st) {
      if(!rv.empty() && i<rv.back().second) {
        return Error(s, rv.back().first,
                     max(rv.back().second, i + dpatt.st.size()),
                     format("Pattern '{}' matches overlapping segments",
                            debug(dpatt)));
      }
      size_t j = s.find(dpatt.en, i+1);
      if(j == string::npos) {
        Error(s, i, i+dpatt.st.size(), "Unterminated segment");
        continue;  // recover
      }
      rv.push_back(make_pair(i, j+dpatt.en.size()));
    }
  return rv;
}

// TODO: Produce error message for every `return nullopt` here.
auto matchAllParts(const PartPattern& patt, const QuotedString& s)
  -> optional<vector<pair<size_t, size_t>>> {
  if(auto* spatt = get_if<QuotedString>(&patt)) return matchAllParts(*spatt, s);
  if(auto* dpatt = get_if<DelimPair>(&patt)) return matchAllParts(*dpatt, s);
  Bug("matchAllParts() called with unknown variant: index {}", patt.index());
}

// A map of non-empty, non-overlapping intervals. Key is the start position,
// while mapped_type.first is end position (end position being exclusive).
using IntervalMap = map<size_t, pair<size_t, Ident>>;

static bool
overlaps(const IntervalMap::value_type& a, const IntervalMap::value_type& b) {
  return b.first < a.second.first && a.first < b.second.first;
}

// Inserts x into the m. If the insert is successful, it returns nullptr.
// If the insert is unsuccessful, it returns a pointer to an overlapping
// interval already in the array.
static auto
insert(IntervalMap& m, const IntervalMap::value_type& x)
  -> const IntervalMap::value_type* {
  if(x.first >= x.second.first)
    Bug("matchAllParts() produced invalid interval: {}, {}",
        x.first, x.second.first);

  IntervalMap::iterator next = m.lower_bound(x.first);
  if(next != m.end() && overlaps(x, *next)) return &*next;
  if(next != m.begin()) {
    auto prev = next; --prev;
    if(overlaps(x, *prev)) return &*prev;
  }
  m.insert(next, x);
  return nullptr;
}

static size_t stPattPos(const PartPattern& patt) {
  if(auto* q = get_if<QuotedString>(&patt)) return stPos(*q);
  if(auto* dp = get_if<DelimPair>(&patt)) return stPos(dp->st);
  Bug("stPattPos() called with unknown index {}", patt.index());
}

static size_t enPattPos(const PartPattern& patt) {
  if(auto* q = get_if<QuotedString>(&patt)) return enPos(*q);
  if(auto* dp = get_if<DelimPair>(&patt)) return enPos(dp->st);
  Bug("enPattPos() called with unknown index {}", patt.index());
}

static const QuotedString& pattStart(const PartPattern& patt) {
  if(auto* q = get_if<QuotedString>(&patt)) return *q;
  if(auto* dp = get_if<DelimPair>(&patt)) return dp->st;
  Bug("pattStart() called with unknown index {}", patt.index());
}

auto labelParts(const QuotedString& s,
                const map<Ident,PartPattern>& partPatterns)
    -> vector<LabelOrPart> {
  bool matchError = false;
  // Find patterns, make IntervalMap.
  IntervalMap m;
  for(auto& [id,patt] : partPatterns) {
    auto matches = matchAllParts(patt, s);
    if(!matches) { matchError = true; continue; }
    if(matches->empty())
      Warning(pattStart(patt), stPattPos(patt), enPattPos(patt),
              format("No match found for pattern '{}'", debug(patt)));
    for(const auto& [st,en] : *matches) {
      const IntervalMap::value_type interval{st, {en, id}};
      const IntervalMap::value_type* ovlap = insert(m, interval);
      if(ovlap) {
        Error(s, st, en,
              format("Part '{}' overlaps with '{}' at {}", debug(patt),
                     debug(partPatterns.at(ovlap->second.second)),
                     locationString(s, ovlap->first, ovlap->second.first)));
        matchError = true;
      }
    }
  }
  if(matchError) return {};

  // Split up string.
  vector<LabelOrPart> rv;
  size_t lastEn = 0;
  for(const auto& [st, enid] : m) {
    auto& [en, id] = enid;
    if(st < lastEn)
      Bug("Overlapping intervals should have been caught earlier: {}"
          " starts before ending another interval at {}",
          locationString(s, st, en),
          locationString(s, lastEn, lastEn+1));
    if(st > lastEn) rv.push_back(s.subqstr(lastEn, st-lastEn));
    rv.push_back(id);
    lastEn = en;
  }
  if(lastEn < s.size() || rv.empty())
    rv.push_back(s.subqstr(lastEn, s.size()-lastEn));
  return rv;
}

// This function assumes we are starting with ctx.input.sizeGt(i).
static auto lexTemplateToken(const QuotedString& s, size_t& i,
                             const LexDirective& opts)
                             -> pair<UnquotedToken,bool> {
  const size_t st = i;
  const bool isword = matchesCharSet(s[i], opts.wordChars);
  while(s.sizeGt(i) && !opts.skip.canStart(s, i)) {
    if(isword != matchesCharSet(s[i], opts.wordChars)) break;
    ++i;
  }
  return pair(UnquotedToken(s.subqstr(st, i-st)), isword);
}

auto tokenizeTemplateWithoutLabels(const QuotedString& s,
                                   const LexDirective& opts)
                                   -> vector<pair<UnquotedToken,bool>> {
  size_t i=0;
  vector<pair<UnquotedToken,bool>> rv;
  while(true) {
    i = opts.skip.acrossLines(s, i);
    if(!s.sizeGt(i)) break;
    rv.push_back(lexTemplateToken(s, i, opts));
  }
  return rv;
}

}  // namespace oalex
