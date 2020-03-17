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

auto partInit(const vector<Regex>& parts) {
  vector<MatchState> rv;
  for(auto& part : parts) rv.push_back(init(part));
  return rv;
}

MatchState init(const Regex& regex) {
  if(get_if_unique<CharSet>(&regex)) return make_unique<vector<bool>>(2, false);
  else if(auto* s = get_if_unique<string>(&regex))
    return make_unique<vector<bool>>(s->size()+1, false);
  else if(auto* opt = get_if_unique<Optional>(&regex))
    return unique_braces<OptionalState>(false, init(opt->part));
  else if(auto* ors = get_if_unique<OrList>(&regex))
    return unique_braces<MatchStateVector>(partInit(ors->parts));
  else if(auto* seq = get_if_unique<Concat>(&regex))
    return unique_braces<MatchStateVector>(partInit(seq->parts));
  else if(auto* rep = get_if_unique<Repeat>(&regex))
    return init(rep->part);
  else Unimplemented()<<"init() for index "<<regex.index();
}

bool matchesCharSet(char ch, const CharSet& cset) {
  for(auto& range : cset.ranges)
    if(range.from <= ch && ch <= range.to) return !cset.negated;
  return cset.negated;
}

void start(const Regex& regex, MatchState& state) {
  if(get_if_unique<CharSet>(&regex) || get_if_unique<string>(&regex))
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
    auto& partstates = get_unique<MatchStateVector>(state).parts;
    if(seq->parts.empty())
      Bug()<<"Cannot Concat over an empty vector. Use an empty string instead.";
    if(seq->parts.size() != partstates.size())
      Bug()<<"init() produced states with the wrong size: "<<seq->parts.size()
           <<" != "<<partstates.size();
    start(seq->parts[0], partstates[0]);
    for(size_t i=1; i<seq->parts.size(); ++i) {
      if(!matched(seq->parts[i-1], partstates[i-1])) break;
      start(seq->parts[i], partstates[i]);
    }
  }else Bug()<<"Unknown index in start() "<<regex.index();
}

bool matched(const Regex& regex, const MatchState& state) {
  if(get_if_unique<CharSet>(&regex) || get_if_unique<string>(&regex))
    return get_unique<vector<bool>>(state).back();
  else if(auto* opt = get_if_unique<Optional>(&regex)) {
    auto& optstate = get_unique<OptionalState>(state);
    return optstate.justStarted || matched(opt->part, optstate.part);
  }else if(auto* ors = get_if_unique<OrList>(&regex)) {
    size_t i = 0;
    for(auto& part : get_unique<MatchStateVector>(state).parts)
      if(matched(ors->parts.at(i++), part)) return true;
    return false;
  }else if(auto* rep = get_if_unique<Repeat>(&regex))
    return matched(rep->part, state);
  else if(auto* seq = get_if_unique<Concat>(&regex))
    return matched(seq->parts.back(),
        get_unique<MatchStateVector>(state).parts.back());
  else Bug()<<"Unknown index in matched() "<<regex.index();
}

void shiftRight(vector<bool>& v) {
  v.pop_back();
  v.insert(v.begin(), false);
}

void advance(const Regex& regex, unsigned char ch, MatchState& state) {
  if(auto* cset = get_if_unique<CharSet>(&regex)) {
    auto& v = get_unique<vector<bool>>(state);
    if(v.size() != 2)
      Bug()<<"CharSet's MatchState should have 2 bools, not "<<v.size();
    if(!matchesCharSet(ch, *cset)) v[0] = false;
    shiftRight(v);
  }else if(auto* s = get_if_unique<string>(&regex)) {
    auto& v = get_unique<vector<bool>>(state);
    if(v.size() != s->size()+1)
      Bug()<<"string's MatchState should have "<<s->size()+1
           <<" bools, not "<<v.size();
    for(size_t i=0; i<s->size(); ++i) if(ch != (*s)[i]) v[i] = false;
    shiftRight(v);
  }else if(auto* opt = get_if_unique<Optional>(&regex)) {
    auto& optstate = get_unique<OptionalState>(state);
    optstate.justStarted = false;
    advance(opt->part, ch, optstate.part);
  }else if(auto* ors = get_if_unique<OrList>(&regex)) {
    auto& partstates = get_unique<MatchStateVector>(state).parts;
    if(ors->parts.size() != partstates.size())
      Bug()<<"State vector size mismatch on OrList: "<<ors->parts.size()
           <<" != "<<partstates.size();
    for(size_t i=0; i<partstates.size(); ++i)
      advance(ors->parts[i], ch, partstates[i]);
  }else if(auto* rep = get_if_unique<Repeat>(&regex)) {
    advance(rep->part, ch, state);
    if(matched(rep->part, state)) start(rep->part, state);
  }else if(auto* seq = get_if_unique<Concat>(&regex)) {
    auto& partstates = get_unique<MatchStateVector>(state).parts;
    if(seq->parts.size() != partstates.size())
      Bug()<<"State vector size mismatch on Concat: "<<seq->parts.size()
           <<" != "<<partstates.size();
    for(size_t i=0; i<partstates.size(); ++i)
      advance(seq->parts[i], ch, partstates[i]);
    for(size_t i=0; i+1<partstates.size(); ++i)
      if(matched(seq->parts[i], partstates[i]))
        start(seq->parts[i+1], partstates[i+1]);
  }
  else Bug()<<"Unknown index in advance() "<<regex.index();
}

}  // namespace

bool startsWith(const Input& input, size_t i, const Regex& regex) {
  MatchState state = init(regex);
  start(regex, state);
  while(!matched(regex, state) && input.sizeGt(i))
    advance(regex, input[i++], state);
  return matched(regex, state);
}

}  // namespace oalex::regex
