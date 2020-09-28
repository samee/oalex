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

// Unit tests for the code here are outside the runtime/ folder, in
// lookahead_regex_io_test.cc, since they use regex parsing functions.
#include "lookahead_regex.h"
#include "util.h"
using std::get;
using std::make_unique;
using std::string;
using std::unique_ptr;
using std::variant;
using std::vector;

namespace oalex::regex {

namespace {

using MatchState = variant<
  unique_ptr<vector<bool>>,            // Used for CharSet and string
  unique_ptr<struct OptionalState>,    // Used only for Optional
  unique_ptr<struct MatchStateVector>  // Used for OrList and Concat
                                       // Repeat doesn't keep any internal state
>;

struct OptionalState {
  bool justStarted;
  MatchState part;
};

struct MatchStateVector {
  vector<MatchState> parts;
};

MatchState init(const Regex& regex);
void start(const Regex& regex, MatchState& state);
void advance(const Regex& regex, unsigned char ch, MatchState& state);
bool matched(const Regex& regex, const MatchState& state);
bool might_match(const Regex& regex, const MatchState& state);

auto partInit(const vector<Regex>& parts) {
  vector<MatchState> rv;
  for(auto& part : parts) rv.push_back(init(part));
  return rv;
}

auto assertVectorBool(MatchState& state, size_t sz) -> vector<bool>& {
  auto& v = get_unique<vector<bool>>(state);
  if(v.size() != sz)
    Bug("MatchState expected to have {} bools, not {}", sz, v.size());
  return v;
}

auto assertMatchStateVector(MatchState& state, size_t sz)
  -> vector<MatchState>& {
  auto& partstates = get_unique<MatchStateVector>(state).parts;
  if(sz != partstates.size())
    Bug("init() produced states with the wrong size: {} != {}",
        sz, partstates.size());
  return partstates;
}

MatchState init(const Regex& regex) {
  if(holds_one_of_unique<CharSet, Anchor>(regex))
    return make_unique<vector<bool>>(2, false);
  else if(auto* s = get_if_unique<string>(&regex))
    return make_unique<vector<bool>>(s->size()+1, false);
  else if(auto* opt = get_if_unique<Optional>(&regex))
    return move_to_unique(OptionalState{false, init(opt->part)});
  else if(auto* ors = get_if_unique<OrList>(&regex))
    return move_to_unique(MatchStateVector{partInit(ors->parts)});
  else if(auto* seq = get_if_unique<Concat>(&regex))
    return move_to_unique(MatchStateVector{partInit(seq->parts)});
  else if(auto* rep = get_if_unique<Repeat>(&regex))
    return init(rep->part);
  else Unimplemented("init() for index {}", regex.index());
}

void start(const Regex& regex, MatchState& state) {
  if(holds_one_of_unique<CharSet, string, Anchor>(regex))
    get_unique<vector<bool>>(state).at(0) = true;
  else if(auto* opt = get_if_unique<Optional>(&regex)) {
    auto& optstate = get_unique<OptionalState>(state);
    optstate.justStarted = true;
    start(opt->part, optstate.part);
  }else if(auto* ors = get_if_unique<OrList>(&regex)) {
    size_t i = 0;
    for(auto& part : get_unique<MatchStateVector>(state).parts)
      start(ors->parts.at(i++), part);
  }else if(auto* rep = get_if_unique<Repeat>(&regex)) start(rep->part, state);
  else if(auto* seq = get_if_unique<Concat>(&regex)) {
    if(seq->parts.empty())
      Bug("Cannot Concat over an empty vector. "
          "Use an empty string instead.");
    auto& partstates = assertMatchStateVector(state, seq->parts.size());
    start(seq->parts[0], partstates[0]);
    for(size_t i=1; i<seq->parts.size(); ++i) {
      if(!matched(seq->parts[i-1], partstates[i-1])) break;
      start(seq->parts[i], partstates[i]);
    }
  }else Bug("Unknown index in start() {}", regex.index());
}

bool any(const vector<bool>& v) {
  for(bool b : v) if(b) return true;
  return false;
}

bool matches_any_part(const vector<Regex>& regexParts,
                      const MatchState& stateVector,
                      bool (*recurse)(const Regex&, const MatchState&)) {
  size_t i = 0;
  for(auto& part : get_unique<MatchStateVector>(stateVector).parts)
    if(recurse(regexParts.at(i++), part)) return true;
  return false;
}

bool matched(const Regex& regex, const MatchState& state) {
  if(holds_one_of_unique<CharSet, string, Anchor>(regex))
    return get_unique<vector<bool>>(state).back();
  else if(auto* opt = get_if_unique<Optional>(&regex)) {
    auto& optstate = get_unique<OptionalState>(state);
    return optstate.justStarted || matched(opt->part, optstate.part);
  }else if(auto* ors = get_if_unique<OrList>(&regex)) {
    return matches_any_part(ors->parts, state, matched);
  }else if(auto* rep = get_if_unique<Repeat>(&regex))
    return matched(rep->part, state);
  else if(auto* seq = get_if_unique<Concat>(&regex))
    return matched(seq->parts.back(),
        get_unique<MatchStateVector>(state).parts.back());
  else Bug("Unknown index in matched() {}", regex.index());
}

bool might_match(const Regex& regex, const MatchState& state) {
  if(holds_one_of_unique<CharSet, string, Anchor>(regex))
    return any(get_unique<vector<bool>>(state));
  else if(auto* opt = get_if_unique<Optional>(&regex)) {
    auto& optstate = get_unique<OptionalState>(state);
    return optstate.justStarted || might_match(opt->part, optstate.part);
  }else if(auto* ors = get_if_unique<OrList>(&regex))
    return matches_any_part(ors->parts, state, might_match);
  else if(auto* rep = get_if_unique<Repeat>(&regex))
    return might_match(rep->part, state);
  else if(auto* seq = get_if_unique<Concat>(&regex))
    return matches_any_part(seq->parts, state, might_match);
  else Bug("Unknown index in might_match() {}", regex.index());
}

void shiftRight(vector<bool>& v) {
  v.pop_back();
  v.insert(v.begin(), false);
}

void advance(const Regex& regex, unsigned char ch, MatchState& state) {
  if(auto* cset = get_if_unique<CharSet>(&regex)) {
    auto& v = assertVectorBool(state, 2);
    if(!matchesCharSet(ch, *cset)) v[0] = false;
    shiftRight(v);
  }else if(auto* s = get_if_unique<string>(&regex)) {
    auto& v = assertVectorBool(state, s->size()+1);
    for(size_t i=0; i<s->size(); ++i) if(ch != (*s)[i]) v[i] = false;
    shiftRight(v);
  }else if(get_if_unique<Anchor>(&regex)) {
    auto& v = assertVectorBool(state, 2);
    v[0] = v[1] = false;
  }else if(auto* opt = get_if_unique<Optional>(&regex)) {
    auto& optstate = get_unique<OptionalState>(state);
    optstate.justStarted = false;
    advance(opt->part, ch, optstate.part);
  }else if(auto* ors = get_if_unique<OrList>(&regex)) {
    auto& partstates = assertMatchStateVector(state, ors->parts.size());
    for(size_t i=0; i<partstates.size(); ++i)
      advance(ors->parts[i], ch, partstates[i]);
  }else if(auto* rep = get_if_unique<Repeat>(&regex)) {
    advance(rep->part, ch, state);
    if(matched(rep->part, state)) start(rep->part, state);
  }else if(auto* seq = get_if_unique<Concat>(&regex)) {
    auto& partstates = assertMatchStateVector(state, seq->parts.size());
    for(size_t i=0; i<partstates.size(); ++i)
      advance(seq->parts[i], ch, partstates[i]);
    for(size_t i=0; i+1<partstates.size(); ++i)
      if(matched(seq->parts[i], partstates[i]))
        start(seq->parts[i+1], partstates[i+1]);
  }
  else Bug("Unknown index in advance() {}", regex.index());
}

enum AnchorMatches { matchesWordEdge = 1, matchesBol = 2, matchesEol = 4 };

AnchorMatches anchorBetweenChars(char from, char to, const RegexOptions& opts) {
  bool w1 = matchesCharSet(from, opts.word);
  bool w2 = matchesCharSet(to, opts.word);
  int rv = 0;
  if(w1 != w2) rv |= matchesWordEdge;
  if(from == '\n') rv |= matchesBol;
  if(to == '\n') rv |= matchesEol;
  return static_cast<AnchorMatches>(rv);
}

// Only ever adds more true values to MatchState, never takes them away.
void advanceAnchor(const Regex& regex, MatchState& state, AnchorMatches anch) {
  if(holds_one_of_unique<CharSet, string>(regex)) return;
  else if(auto* a = get_if_unique<Anchor>(&regex)) {
    auto& v = assertVectorBool(state, 2);
    if(!v[0]) return;
    if(((anch & matchesWordEdge) && *a == Anchor::wordEdge) ||
       ((anch & matchesBol) && *a == Anchor::bol) ||
       ((anch & matchesEol) && *a == Anchor::eol)) v[1] = true;
  }else if(auto* opt = get_if_unique<Optional>(&regex)) {
    advanceAnchor(opt->part, get_unique<OptionalState>(state).part, anch);
  }else if(auto* ors = get_if_unique<OrList>(&regex)) {
    size_t i = 0;
    for(auto& p : assertMatchStateVector(state, ors->parts.size()))
      advanceAnchor(ors->parts[i++], p, anch);
  }else if(auto* rep = get_if_unique<Repeat>(&regex)) {
    bool startedMatched = matched(rep->part, state);
    advanceAnchor(rep->part, state, anch);
    // Fixpoint guaranteed in a single additional iteration.
    if(!startedMatched && matched(rep->part, state)) {
      start(rep->part, state);
      advanceAnchor(rep->part, state, anch);
    }
  }else if(auto* seq = get_if_unique<Concat>(&regex)) {
    size_t n = seq->parts.size();
    if(n == 0) return;
    auto& v = assertMatchStateVector(state, n);
    for(size_t i=0; i+1<n; i++) {
      const Regex& p = seq->parts[i];
      bool startedMatched = matched(p, v[i]);
      advanceAnchor(p, v[i], anch);
      if(!startedMatched && matched(p, v[i])) start(seq->parts[i+1], v[i+1]);
    }
    advanceAnchor(seq->parts[n-1], v[n-1], anch);
  }
  else Bug("Unknown index in advance() {}", regex.index());
}

}  // namespace

bool matchesCharSet(char ch, const CharSet& cset) {
  for(auto& range : cset.ranges)
    if(range.from <= ch && ch <= range.to) return !cset.negated;
  return cset.negated;
}

bool startsWith(const Input& input, size_t i, const Regex& regex,
                const RegexOptions& opts) {
  MatchState state = init(regex);
  char prev = '\n';
  start(regex, state);
  while(!matched(regex, state) && input.sizeGt(i)) {
    advanceAnchor(regex, state, anchorBetweenChars(prev, input[i], opts));
    if(matched(regex, state)) return true;
    advance(regex, input[i], state);
    prev = input[i++];
  }
  advanceAnchor(regex, state, anchorBetweenChars(prev, '\n', opts));
  return matched(regex, state);
}

bool consumeGreedily(const Input& input, size_t& i, const Regex& regex,
                     const RegexOptions& opts) {
  MatchState state = init(regex);
  char prev = '\n';
  start(regex, state);
  size_t j = i;
  size_t last_matched = string::npos;

  while(might_match(regex, state) && input.sizeGt(j)) {
    advanceAnchor(regex, state, anchorBetweenChars(prev, input[j], opts));
    if(matched(regex, state)) last_matched = j;
    advance(regex, input[j], state);
    prev = input[j++];
  }
  advanceAnchor(regex, state, anchorBetweenChars(prev, '\n', opts));
  if(matched(regex, state)) last_matched = j;
  if(last_matched != string::npos) { i = last_matched; return true; }
  else return false;
}

}  // namespace oalex::regex
