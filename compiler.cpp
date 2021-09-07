/*  Copyright 2020-2021 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "compiler.h"
#include "fmt/core.h"
#include "runtime/util.h"
using fmt::format;
using oalex::lex::oalexSkip;
using oalex::lex::NewlineChar;
using std::make_unique;
using std::string;
using std::string_view;
using std::unique_ptr;
using std::vector;

namespace oalex {

ssize_t
PatternToRulesCompiler::processConcat(const PatternConcat& concatPatt) {
  ConcatFlatRule concatRule{ {} };
  for(ssize_t i = 0; i < (ssize_t)concatPatt.parts.size(); ++i) {
    if(i > 0) {
      // Intersperse concat components with SkipPoint components.
      concatRule.comps.push_back({rl_->ssize(), {}});
      rl_->appendAnonRule(SkipPoint{/* stayWithinLine */ false, &oalexSkip});
    }
    const Pattern& child = concatPatt.parts[i];
    ssize_t j = this->process(child);
    concatRule.comps.push_back({j, {}});
  }
  rl_->appendAnonRule(std::move(concatRule));
  return rl_->ssize()-1;
}

// TODO failed tests should output location. Or tests should have names.
ssize_t
PatternToRulesCompiler::processOrList(const PatternOrList& orPatt) {
  OrRule orRule{{}, /* flattenOnDemand */ true};
  for(ssize_t i = 0; i < ssize(orPatt.parts); ++i) {
    const Pattern& child = orPatt.parts[i];
    ssize_t j = this->process(child);
    if(i+1 != ssize(orPatt.parts)) j = rl_->appendAnonRule(QuietMatch{j});
    orRule.comps.push_back({-1, j, passthroughTmpl});
  }
  rl_->appendAnonRule(std::move(orRule));
  return rl_->ssize()-1;
}

ssize_t
PatternToRulesCompiler::processOptional(const PatternOptional& optPatt) {
  ssize_t i;
  OrRule orRule{{}, /* flattenOnDemand */ true};

  i = this->process(optPatt.part);
  i = rl_->appendAnonRule(QuietMatch{i});
  orRule.comps.push_back({-1, i, passthroughTmpl});

  // This branch always produces a Map on success.
  i = rl_->appendAnonRule(StringRule{{}});
  orRule.comps.push_back({-1, i, JsonTmpl::Map{}});

  return rl_->appendAnonRule(std::move(orRule));
}

ssize_t
PatternToRulesCompiler::processIdent(const Ident& ident) {
  size_t i;
  for(i=0; i<p2rule_->size(); ++i) if(ident == p2rule_->at(i).first) break;
  if(i == p2rule_->size())
    Bug("Ident map should already have an entry for '{}'",
        ident.preserveCase());
  return rl_->appendAnonRule(ConcatFlatRule{{
      {p2rule_->at(i).second, ident.preserveCase()},
  }});
}

ssize_t
PatternToRulesCompiler::processRepeat(const PatternRepeat& repPatt) {
  ssize_t i = this->process(repPatt.part);
  ssize_t ski = rl_->appendAnonRule(SkipPoint{/* stayWithinLine */ false,
                                              &oalexSkip});
  return rl_->appendAnonRule(LoopRule{{
      .partidx = i, .partname = "", .glueidx = -1, .gluename = "",
      .lookidx = -1, .skipidx = ski}});
}

ssize_t
PatternToRulesCompiler::processFold(const PatternFold& foldPatt) {
  ssize_t pi = this->process(foldPatt.part);
  ssize_t gi = this->process(foldPatt.glue);
  ssize_t ski = rl_->appendAnonRule(SkipPoint{/* stayWithinLine */ false,
                                              &oalexSkip});
  return rl_->appendAnonRule(LoopRule{{
      .partidx = pi, .partname = "", .glueidx = gi, .gluename = "",
      .lookidx = -1, .skipidx = ski}});
}

ssize_t
PatternToRulesCompiler::process(const Pattern& patt) {
  if(auto* word = get_if_unique<WordToken>(&patt)) {
    return appendWordOrError(*rl_, **word);
  }else if(auto* oper = get_if_unique<OperToken>(&patt)) {
    return appendLiteralOrError(*rl_, **oper);
  }else if(get_if_unique<NewlineChar>(&patt)) {
    return appendLiteralOrError(*rl_, "\n");
  }else if(auto* ident = get_if_unique<Ident>(&patt)) {
    return processIdent(*ident);
  }else if(auto* concatPatt = get_if_unique<PatternConcat>(&patt)) {
    return processConcat(*concatPatt);
  }else if(auto* orPatt = get_if_unique<PatternOrList>(&patt)) {
    return processOrList(*orPatt);
  }else if(auto* optPatt = get_if_unique<PatternOptional>(&patt)) {
    return processOptional(*optPatt);
  }else if(auto* repPatt = get_if_unique<PatternRepeat>(&patt)) {
    return processRepeat(*repPatt);
  }else if(auto* foldPatt = get_if_unique<PatternFold>(&patt)) {
    return processFold(*foldPatt);
  }else {
    Unimplemented("Pattern compilation of index {}", patt.index());
  }
}

void
logLocalNamesakeError(DiagsDest ctx, const Ident& ident) {
  Error(ctx, ident.stPos(), ident.enPos(),
        format("Local variable name '{}' conflicts with a global name",
               ident.preserveCase()));
}

Rule& RulesWithLocs::operator[](ssize_t i) {
  if(rules_[i]) return *rules_[i];
  else Bug("Dereferencing null Rule at index {}", i);
}

ssize_t
RulesWithLocs::findOrAppendIdent(const Ident& id) {
  LocPair thisPos{id.stPos(), id.enPos()};
  for(ssize_t i=0; i<this->ssize(); ++i) {
    const Ident* name = rules_[i]->nameOrNull();
    if(name == nullptr || id != *name) continue;
    if(firstUseLocs_[i] == nrange) firstUseLocs_[i] = thisPos;
    return i;
  }
  rules_.push_back(make_unique<UnassignedRule>(id));
  firstUseLocs_.push_back(thisPos);
  return this->ssize()-1;
}

ssize_t
RulesWithLocs::defineIdent(DiagsDest ctx, const Ident& ident) {
  for(ssize_t i=0; i<this->ssize(); ++i) {
    const Ident* name = rules_[i]->nameOrNull();
    if(name == nullptr || ident != *name) continue;
    if(dynamic_cast<const ReservedRule*>(rules_[i].get())) {
      logLocalNamesakeError(ctx, *name);
      rules_[i] = make_unique<UnassignedRule>(ident);
      return i;
    }else if(!dynamic_cast<const UnassignedRule*>(rules_[i].get())) {
      Error(ctx, ident.stPos(), ident.enPos(),
            format("'{}' has multiple definitions", ident.preserveCase()));
      return -1;
    }else return i;
  }
  rules_.push_back(make_unique<UnassignedRule>(ident));
  firstUseLocs_.push_back(nrange);
  return this->ssize()-1;
}

void
RulesWithLocs::reserveLocalName(DiagsDest ctx, const Ident& ident) {
  for(ssize_t i=0; i<this->ssize(); ++i) {
    const Ident* name = rules_[i]->nameOrNull();
    if(name == nullptr || ident != *name) continue;
    if(dynamic_cast<const UnassignedRule*>(rules_[i].get())) {
      rules_[i] = make_unique<ReservedRule>(ident);
    }else if(!dynamic_cast<const ReservedRule*>(rules_[i].get())) {
      logLocalNamesakeError(ctx, ident);
      return;
    }
  }
  rules_.push_back(make_unique<ReservedRule>(ident));
  firstUseLocs_.push_back(nrange);
}

template <class X> ssize_t
RulesWithLocs::appendAnonRule(X x) {
  rules_.push_back(move_to_unique(x));
  firstUseLocs_.emplace_back(-1, -1);
  return rules_.size()-1;
}

template ssize_t RulesWithLocs::appendAnonRule(UnassignedRule);
template ssize_t RulesWithLocs::appendAnonRule(ReservedRule);
template ssize_t RulesWithLocs::appendAnonRule(StringRule);
template ssize_t RulesWithLocs::appendAnonRule(WordPreserving);
template ssize_t RulesWithLocs::appendAnonRule(RegexRule);
template ssize_t RulesWithLocs::appendAnonRule(SkipPoint);
template ssize_t RulesWithLocs::appendAnonRule(ConcatRule);
template ssize_t RulesWithLocs::appendAnonRule(ConcatFlatRule);
template ssize_t RulesWithLocs::appendAnonRule(OutputTmpl);
template ssize_t RulesWithLocs::appendAnonRule(LoopRule);
template ssize_t RulesWithLocs::appendAnonRule(OrRule);
template ssize_t RulesWithLocs::appendAnonRule(ErrorRule);
template ssize_t RulesWithLocs::appendAnonRule(QuietMatch);
template ssize_t RulesWithLocs::appendAnonRule(MatchOrError);

bool
RulesWithLocs::hasUndefinedRules(DiagsDest ctx) const {
  for(ssize_t i=0; i<this->ssize(); ++i)
    if(dynamic_cast<const UnassignedRule*>(rules_[i].get())) {
      const Ident* name = rules_[i]->nameOrNull();
      if(!name) Bug("Anonymous rules should always be initialized");
      const auto [st, en] = firstUseLocs_[i];
      Error(ctx, st, en, format("Rule '{}' was used but never defined",
                                name->preserveCase()));
      return true;
    }
  return false;
}

void
RulesWithLocs::resize_down(ssize_t n) noexcept {
  if(n >= this->ssize()) return;
  // Don't use erase() because it needs an assignment operator.
  // Don't use resize() because it needs default constructor.
  // Repeated pop_back() it is.
  while(n < this->ssize()) rules_.pop_back();
  firstUseLocs_.resize(n);
}

vector<unique_ptr<Rule>>
RulesWithLocs::releaseRules() {
  firstUseLocs_.clear();
  return std::move(rules_);  // This is guaranteed to clear rules_.
}

template <class X> void
RulesWithLocs::deferred_assign(ssize_t idx, X x) {
  if(rules_[idx] != nullptr &&
     !dynamic_cast<const UnassignedRule*>(rules_[idx].get()))
    oalex::Bug("deferred_assign() cannot be used a rule already assigned");
  Ident name;
  if(rules_[idx] != nullptr) {
    if(auto name2 = rules_[idx]->nameOrNull()) name = std::move(*name2);
  }
  x.deferred_name(std::move(name));
  rules_[idx] = move_to_unique(x);
}

template void RulesWithLocs::deferred_assign(ssize_t idx, SkipPoint);
template void RulesWithLocs::deferred_assign(ssize_t idx, ConcatRule);
template void RulesWithLocs::deferred_assign(ssize_t idx, OutputTmpl);
template void RulesWithLocs::deferred_assign(ssize_t idx, OrRule);
template void RulesWithLocs::deferred_assign(ssize_t idx, MatchOrError);

void
assignLiteralOrError(RulesWithLocs& rl, size_t ruleIndex, string_view literal) {
  rl.deferred_assign(ruleIndex, MatchOrError{
      rl.ssize(), format("Expected '{}'", literal)
  });
  rl.appendAnonRule(StringRule(string(literal)));
}
ssize_t
appendLiteralOrError(RulesWithLocs& rl, string_view literal) {
  ssize_t newIndex = rl.ssize();
  rl.appendAnonRule(UnassignedRule{});
  assignLiteralOrError(rl, newIndex, literal);
  return newIndex;
}

ssize_t
appendWordOrError(RulesWithLocs& rl, string_view word) {
  ssize_t newIndex = rl.ssize();
  rl.appendAnonRule(WordPreserving{word});
  rl.appendAnonRule(MatchOrError{newIndex, format("Expected '{}'", word)});
  return newIndex + 1;
}

void
assignRegexOrError(RulesWithLocs& rl, size_t ruleIndex,
                   string errmsg, unique_ptr<const Regex> regex) {
  rl.deferred_assign(ruleIndex, MatchOrError{rl.ssize(), std::move(errmsg)});
  rl.appendAnonRule(RegexRule{std::move(regex)});
}

ssize_t
appendRegexOrError(RulesWithLocs& rl, unique_ptr<const Regex> regex) {
  ssize_t newIndex = rl.ssize();
  rl.appendAnonRule(RegexRule{std::move(regex)});
  rl.appendAnonRule(MatchOrError{newIndex, "Does not match expected pattern"});
  return newIndex + 1;
}

}  // namespace oalex
