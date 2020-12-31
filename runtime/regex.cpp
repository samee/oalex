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
// regex_io_test.cc, since they use regex parsing functions.
#include "regex.h"
#include "util.h"
using std::get;
using std::make_unique;
using std::string;
using std::unique_ptr;
using std::vector;

namespace oalex {

namespace {

enum class MatchStateType { boolvec, optional, statevec };

class MatchState {
 public:
  const MatchStateType type;
  explicit MatchState(MatchStateType t) : type(t) {}
  virtual ~MatchState() {}
};

class BoolVectorState final : public MatchState {
 public:
  explicit BoolVectorState(vector<bool> v)
    : MatchState(MatchStateType::boolvec), valid(std::move(v)) {}
  vector<bool> valid;
};
class OptionalState final : public MatchState {
 public:
  explicit OptionalState(unique_ptr<MatchState> p)
    : MatchState(MatchStateType::optional), part(std::move(p)) {}
  bool justStarted = false;
  unique_ptr<MatchState> part;
};
class MatchVectorState final : public MatchState {
 public:
  explicit MatchVectorState(vector<unique_ptr<MatchState>> v)
    : MatchState(MatchStateType::statevec), parts(std::move(v)) {}
  vector<unique_ptr<MatchState>> parts;
};

unique_ptr<MatchState> init(const Regex& regex);
void start(const Regex& regex, MatchState& state);
bool matched(const Regex& regex, const MatchState& state);
bool might_match(const Regex& regex, const MatchState& state);
void advance(const Regex& regex, unsigned char ch, MatchState& state);

auto partInit(const vector<unique_ptr<const Regex>>& parts) {
  vector<unique_ptr<MatchState>> rv;
  for(auto& part : parts) rv.push_back(init(*part));
  return rv;
}

string debug(MatchStateType t) {
  switch(t) {
    case MatchStateType::boolvec: return "boolvec";
    case MatchStateType::optional: return "optional";
    case MatchStateType::statevec: return "statevec";
    default: return std::to_string(int(t));
  }
}

auto assertVectorBool(MatchState& state, size_t sz) -> vector<bool>& {
  if(state.type != MatchStateType::boolvec)
    Bug("MatchState was expected to have bools, not {}", debug(state.type));
  auto& v = static_cast<BoolVectorState&>(state).valid;
  if(v.size() != sz)
    Bug("MatchState expected to have {} bools, not {}", sz, v.size());
  return v;
}

auto assertMatchVectorState(MatchState& state, size_t sz)
  -> vector<unique_ptr<MatchState>>& {
  if(state.type != MatchStateType::statevec)
    Bug("MatchState was expected to have children state, not {}",
        debug(state.type));
  auto& partstates = static_cast<MatchVectorState&>(state).parts;
  if(sz != partstates.size())
    Bug("init() produced states with the wrong size: {} != {}",
        sz, partstates.size());
  return partstates;
}

unique_ptr<MatchState> init(const Regex& regex) {
  switch(regex.nodeType) {
    case RegexNodeType::charSet:
    case RegexNodeType::anchor:
      return make_unique<BoolVectorState>(vector<bool>(2, false));
    case RegexNodeType::string: {
      auto& s = static_cast<const RegexString&>(regex);
      return make_unique<BoolVectorState>(
          vector<bool>(s.value.size()+1, false) );
    }
    case RegexNodeType::optional: {
      auto& opt = static_cast<const RegexOptional&>(regex);
      return move_to_unique(OptionalState(init(*opt.part)));
    }
    case RegexNodeType::orList: {
      auto& ors = static_cast<const RegexOrList&>(regex);
      return move_to_unique(MatchVectorState(partInit(ors.parts)));
    }
    case RegexNodeType::concat: {
      auto& seq = static_cast<const RegexConcat&>(regex);
      return move_to_unique(MatchVectorState(partInit(seq.parts)));
    }
    case RegexNodeType::repeat: {
      auto& rep = static_cast<const RegexRepeat&>(regex);
      return init(*rep.part);
    }
    default:
      Unimplemented("init() for RegexType {}", int(regex.nodeType));
  }
}

void start(const Regex& regex, MatchState& state) {
  switch(regex.nodeType) {
    case RegexNodeType::charSet:
    case RegexNodeType::string:
    case RegexNodeType::anchor:
      static_cast<BoolVectorState&>(state).valid.at(0) = true;
      break;
    case RegexNodeType::optional: {
      auto& opt = static_cast<const RegexOptional&>(regex);
      auto& optstate = static_cast<OptionalState&>(state);
      optstate.justStarted = true;
      start(*opt.part, *optstate.part);
      break;
    }
    case RegexNodeType::orList: {
      auto& ors = static_cast<const RegexOrList&>(regex);
      size_t i = 0;
      for(auto& part : static_cast<MatchVectorState&>(state).parts)
        start(*ors.parts.at(i++), *part);
      break;
    }
    case RegexNodeType::repeat: {
      auto& rep = static_cast<const RegexRepeat&>(regex);
      start(*rep.part, state);
      break;
    }
    case RegexNodeType::concat: {
      auto& seq = static_cast<const RegexConcat&>(regex);
      if(seq.parts.empty())
        Bug("Cannot Concat over an empty vector. "
            "Use an empty string instead.");
      auto& partstates = assertMatchVectorState(state, seq.parts.size());
      start(*seq.parts[0], *partstates[0]);
      for(size_t i=1; i<seq.parts.size(); ++i) {
        if(!matched(*seq.parts[i-1], *partstates[i-1])) break;
        start(*seq.parts[i], *partstates[i]);
      }
      break;
    }
    default: Bug("Unknown index in start() {}", int(regex.nodeType));
  }
}

bool any(const vector<bool>& v) {
  for(bool b : v) if(b) return true;
  return false;
}

bool matches_any_part(const vector<unique_ptr<const Regex>>& regexParts,
                      const MatchState& stateVector,
                      bool (*recurse)(const Regex&, const MatchState&)) {
  size_t i = 0;
  for(auto& part : static_cast<const MatchVectorState&>(stateVector).parts)
    if(recurse(*regexParts.at(i++), *part)) return true;
  return false;
}

bool matched(const Regex& regex, const MatchState& state) {
  switch(regex.nodeType) {
    case RegexNodeType::charSet:
    case RegexNodeType::string:
    case RegexNodeType::anchor:
      return static_cast<const BoolVectorState&>(state).valid.back();
    case RegexNodeType::optional: {
      auto& opt = static_cast<const RegexOptional&>(regex);
      auto& optstate = static_cast<const OptionalState&>(state);
      return optstate.justStarted || matched(*opt.part, *optstate.part);
    }
    case RegexNodeType::orList: {
      auto& ors = static_cast<const RegexOrList&>(regex);
      return matches_any_part(ors.parts, state, matched);
    }
    case RegexNodeType::repeat: {
      auto& rep = static_cast<const RegexRepeat&>(regex);
      return matched(*rep.part, state);
    }
    case RegexNodeType::concat: {
      auto& seq = static_cast<const RegexConcat&>(regex);
      return matched(*seq.parts.back(),
                     *static_cast<const MatchVectorState&>(state).parts.back());
    }
    default: Bug("Unknown index in matched() {}", int(regex.nodeType));
  }
}

bool might_match(const Regex& regex, const MatchState& state) {
  switch(regex.nodeType) {
    case RegexNodeType::charSet:
    case RegexNodeType::string:
    case RegexNodeType::anchor:
      return any(static_cast<const BoolVectorState&>(state).valid);
    case RegexNodeType::optional: {
      auto& opt = static_cast<const RegexOptional&>(regex);
      auto& optstate = static_cast<const OptionalState&>(state);
      return optstate.justStarted || might_match(*opt.part, *optstate.part);
    }
    case RegexNodeType::orList: {
      auto& ors = static_cast<const RegexOrList&>(regex);
      return matches_any_part(ors.parts, state, might_match);
    }
    case RegexNodeType::repeat: {
      auto& rep = static_cast<const RegexRepeat&>(regex);
      return might_match(*rep.part, state);
    }
    case RegexNodeType::concat: {
      auto& seq = static_cast<const RegexConcat&>(regex);
      return matches_any_part(seq.parts, state, might_match);
    }
    default: Bug("Unknown index in might_match() {}", int(regex.nodeType));
  }
}

void shiftRight(vector<bool>& v) {
  v.pop_back();
  v.insert(v.begin(), false);
}

void advance(const Regex& regex, unsigned char ch, MatchState& state) {
  switch(regex.nodeType) {
    case RegexNodeType::charSet: {
      auto& cset = static_cast<const RegexCharSet&>(regex);
      auto& v = assertVectorBool(state, 2);
      if(!matchesRegexCharSet(ch, cset)) v[0] = false;
      shiftRight(v);
      break;
    }
    case RegexNodeType::string: {
      const string& s = static_cast<const RegexString&>(regex).value;
      auto& v = assertVectorBool(state, s.size()+1);
      for(size_t i=0; i<s.size(); ++i) if(ch != s[i]) v[i] = false;
      shiftRight(v);
      break;
    }
    case RegexNodeType::anchor: {
      auto& v = assertVectorBool(state, 2);
      v[0] = v[1] = false;
      break;
    }
    case RegexNodeType::optional: {
      auto& opt = static_cast<const RegexOptional&>(regex);
      auto& optstate = static_cast<OptionalState&>(state);
      optstate.justStarted = false;
      advance(*opt.part, ch, *optstate.part);
      break;
    }
    case RegexNodeType::orList: {
      auto& ors = static_cast<const RegexOrList&>(regex);
      auto& partstates = assertMatchVectorState(state, ors.parts.size());
      for(size_t i=0; i<partstates.size(); ++i)
        advance(*ors.parts[i], ch, *partstates[i]);
      break;
    }
    case RegexNodeType::repeat: {
      auto& rep = static_cast<const RegexRepeat&>(regex);
      advance(*rep.part, ch, state);
      if(matched(*rep.part, state)) start(*rep.part, state);
      break;
    }
    case RegexNodeType::concat: {
      auto& seq = static_cast<const RegexConcat&>(regex);
      auto& partstates = assertMatchVectorState(state, seq.parts.size());
      for(size_t i=0; i<partstates.size(); ++i)
        advance(*seq.parts[i], ch, *partstates[i]);
      for(size_t i=0; i+1<partstates.size(); ++i)
        if(matched(*seq.parts[i], *partstates[i]))
          start(*seq.parts[i+1], *partstates[i+1]);
      break;
    }
    default: Bug("Unknown index in advance() {}", int(regex.nodeType));
  }
}

enum AnchorMatches { matchesWordEdge = 1, matchesBol = 2, matchesEol = 4 };

AnchorMatches anchorBetweenChars(char from, char to, const RegexOptions& opts) {
  bool w1 = matchesRegexCharSet(from, opts.word);
  bool w2 = matchesRegexCharSet(to, opts.word);
  int rv = 0;
  if(w1 != w2) rv |= matchesWordEdge;
  if(from == '\n') rv |= matchesBol;
  if(to == '\n') rv |= matchesEol;
  return static_cast<AnchorMatches>(rv);
}

// Only ever adds more true values to MatchState, never takes them away.
void advanceAnchor(const Regex& regex, MatchState& state, AnchorMatches anch) {
  switch(regex.nodeType) {
    case RegexNodeType::charSet:
    case RegexNodeType::string:
      break;
    case RegexNodeType::anchor: {
      auto a = static_cast<const RegexAnchor&>(regex).anchorType;
      auto& v = assertVectorBool(state, 2);
      if(!v[0]) return;
      if(((anch & matchesWordEdge) && a == RegexAnchor::wordEdge) ||
         ((anch & matchesBol) && a == RegexAnchor::bol) ||
         ((anch & matchesEol) && a == RegexAnchor::eol)) v[1] = true;
      break;
    }
    case RegexNodeType::optional: {
      auto& opt = static_cast<const RegexOptional&>(regex);
      auto& optstate = static_cast<OptionalState&>(state);
      advanceAnchor(*opt.part, *optstate.part, anch);
      break;
    }
    case RegexNodeType::orList: {
      auto& ors = static_cast<const RegexOrList&>(regex);
      auto& partstates = assertMatchVectorState(state, ors.parts.size());
      for(size_t i=0; i<partstates.size(); ++i)
        advanceAnchor(*ors.parts[i], *partstates[i], anch);
      break;
    }
    case RegexNodeType::repeat: {
      auto& rep = static_cast<const RegexRepeat&>(regex);
      bool startedMatched = matched(*rep.part, state);
      advanceAnchor(*rep.part, state, anch);
      // Fixpoint guaranteed in a single additional iteration.
      if(!startedMatched && matched(*rep.part, state)) {
        start(*rep.part, state);
        advanceAnchor(*rep.part, state, anch);
      }
      break;
    }
    case RegexNodeType::concat: {
      auto& seq = static_cast<const RegexConcat&>(regex);
      size_t n = seq.parts.size();
      if(n == 0) return;
      auto& v = assertMatchVectorState(state, n);
      for(size_t i=0; i+1<n; i++) {
        const Regex& p = *seq.parts[i];
        bool startedMatched = matched(p, *v[i]);
        advanceAnchor(p, *v[i], anch);
        if(!startedMatched && matched(p, *v[i]))
          start(*seq.parts[i+1], *v[i+1]);
      }
      advanceAnchor(*seq.parts[n-1], *v[n-1], anch);
      break;
    }
    default: Bug("Unknown index in advance() {}", int(regex.nodeType));
  }
}

}  // namespace

bool matchesRegexCharSet(char ch, const RegexCharSet& cset) {
  for(auto& range : cset.ranges)
    if(range.from <= ch && ch <= range.to) return !cset.negated;
  return cset.negated;
}

bool startsWithRegex(const Input& input, size_t i, const Regex& regex,
                     const RegexOptions& opts) {
  unique_ptr<MatchState> state = init(regex);
  char prev = '\n';
  start(regex, *state);
  while(input.sizeGt(i)) {
    advanceAnchor(regex, *state, anchorBetweenChars(prev, input[i], opts));
    if(matched(regex, *state)) return true;
    advance(regex, input[i], *state);
    if(matched(regex, *state)) return true;
    prev = input[i++];
  }
  advanceAnchor(regex, *state, anchorBetweenChars(prev, '\n', opts));
  return matched(regex, *state);
}

bool consumeGreedily(const Input& input, size_t& i, const Regex& regex,
                     const RegexOptions& opts) {
  unique_ptr<MatchState> state = init(regex);
  char prev = '\n';
  start(regex, *state);
  size_t j = i;
  size_t last_matched = string::npos;

  // Important invariants:
  //   matches() implies might_match()
  //   advanceAnchor() never makes match() or might_match() become false.
  //   advance() can potentially make might_match() turn from true to false,
  //     never the other way around.
  while(might_match(regex, *state) && input.sizeGt(j)) {
    advanceAnchor(regex, *state, anchorBetweenChars(prev, input[j], opts));
    if(matched(regex, *state)) last_matched = j;
    advance(regex, input[j], *state);
    prev = input[j++];
  }
  if(!input.sizeGt(j)) {
    advanceAnchor(regex, *state, anchorBetweenChars(prev, '\n', opts));
    if(matched(regex, *state)) last_matched = j;
  }
  if(last_matched != string::npos) { i = last_matched; return true; }
  else return false;
}

}  // namespace oalex
