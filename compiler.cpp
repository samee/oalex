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
#include <map>
#include <utility>
#include "fmt/core.h"
#include "runtime/util.h"
using fmt::format;
using oalex::lex::GluedString;
using oalex::lex::oalexSkip;
using oalex::lex::NewlineChar;
using std::make_unique;
using std::map;
using std::optional;
using std::pair;
using std::string;
using std::string_view;
using std::unique_ptr;
using std::vector;

namespace oalex {

namespace {

/*
Pattern to Rule compilation
---------------------------

Objects of class PatternToRulesCompiler are not usually long-lived. They are
used only for the compilation of a single input rule. Conventions followed by
methods of this class:

  * They append newly compiled rules into RulesWithLocs rl_.
  * Each function may append one or more Rules.
  * They return the location of the entry point index for the newly created
    block of Rules.

The result of pattern compilation is some rule that produces one of two things:

  * The entire Pattern is just one Ident and nothing else.
  * All the identifiers in the pattern is to be gathered up into a single
    flat JsonLoc::Map object (e.g. by using ConcatFlatRule).

These identifiers are then assembled by an OutputTmpl object specified outside
the Pattern object. The syntax for that varies. The impliciation is that if
we want a particular rule output to be available to the outermost OutputTmpl,
we need to keep propagating it up.

Every pattern produces a map, except for the literal string ones (e.g.
appendWordOrError, appendLiteralOrError, appendRegexOrError). The Word and
Literal variants produce a string but that result is ignored when part of a
Pattern object. The Regex variant also returns a string, but the result is
generally propagated up to the user. They could produce maps too, or nothing at
all, but right now they produce strings just so the functions can be used
elsewhere in the frontend.

How to follow this convention for new rules types with subcomponents:
If you want something to be preserved until an OutputTmpl catches it, make it a
named field in a map. You can safely use passthroughTmpl: map fields will be
propagated, while strings will be dropped either by OutputTmpl or
ConcatFlatRule.
*/

class PatternToRulesCompiler {
  DiagsDest ctx_;      // These is where all errors and warnings get logged.
  RulesWithLocs* rl_;  // This is where new rules get appended.
  // Rule index for placeholders
  const vector<pair<Ident,ssize_t>>* p2rule_;
  ssize_t processConcat(const PatternConcat& concatPatt);
  ssize_t processOrList(const PatternOrList& orPatt);
  ssize_t processOptional(const PatternOptional& optPatt);
  ssize_t processIdent(const Ident& ident);
  ssize_t processRepeat(const PatternRepeat& repPatt);
  ssize_t processFold(const PatternFold& foldPatt);
 public:
  PatternToRulesCompiler(DiagsDest ctx, RulesWithLocs& rl,
                         const vector<pair<Ident,ssize_t>>& p2rule) :
    ctx_(ctx), rl_(&rl), p2rule_(&p2rule) {}
  // Just to prevent accidental copying.
  PatternToRulesCompiler(const PatternToRulesCompiler&) = delete;
  ssize_t process(const Pattern& patt);
};

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

}  // namespace

Rule& RulesWithLocs::operator[](ssize_t i) {
  if(rules_[i]) return *rules_[i];
  else Bug("Dereferencing null Rule at index {}", i);
}

ssize_t
RulesWithLocs::findOrAppendIdent(const Ident& id) {
  if(!id) Bug("findOrAppendIdent() invoked with empty Ident");
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

Ident
RulesWithLocs::findReservedLocalIdent(const Ident& ident) const {
  for(size_t i=0; i<reservedLocalNames_.size(); ++i)
    if(reservedLocalNames_[i] == ident) return reservedLocalNames_[i];
  return {};
}

ssize_t
RulesWithLocs::defineIdent(DiagsDest ctx, const Ident& ident) {
  if(!ident) Bug("defineIdent() invoked with empty Ident");
  Ident res_instance = findReservedLocalIdent(ident);
  if(res_instance) logLocalNamesakeError(ctx, res_instance);
  for(ssize_t i=0; i<this->ssize(); ++i) {
    const Ident* name = rules_[i]->nameOrNull();
    if(name == nullptr || ident != *name) continue;
    if(dynamic_cast<const UnassignedRule*>(rules_[i].get())) {
      rules_[i] = make_unique<DefinitionInProgress>(ident);
      return i;
    }else {
      Error(ctx, ident.stPos(), ident.enPos(),
            format("'{}' has multiple definitions", ident.preserveCase()));
      return -1;
    }
  }
  rules_.push_back(make_unique<DefinitionInProgress>(ident));
  firstUseLocs_.push_back(nrange);
  return this->ssize()-1;
}

static void appendIfNew(vector<Ident>& v, const Ident& ident) {
  for(auto& elt : v) if(elt == ident) return;
  v.push_back(ident);
}

void
RulesWithLocs::reserveLocalName(DiagsDest ctx, const Ident& ident) {
  if(!ident) Bug("reserveLocalName() invoked with empty Ident");
  appendIfNew(reservedLocalNames_, ident);
  for(const auto& rule : rules_) {
    const Ident* name = rule->nameOrNull();
    if(name == nullptr || ident != *name) continue;
    if(!dynamic_cast<const UnassignedRule*>(rule.get()))
      logLocalNamesakeError(ctx, ident);
    return;
  }
}

template <class X> ssize_t
RulesWithLocs::appendAnonRule(X x) {
  rules_.push_back(move_to_unique(x));
  firstUseLocs_.emplace_back(-1, -1);
  return rules_.size()-1;
}

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
     !dynamic_cast<const DefinitionInProgress*>(rules_[idx].get()) &&
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
  rl.appendAnonRule(DefinitionInProgress{});
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

// ---------------------- Start appendPatternRules() --------------------------

// Looks for [p, ...] for some `JsonTmpl::Placeholder p`. If found, returns
// &p, which is guaranteed to point to a JsonTmpl::Placeholder. If not, returns
// nullptr. Produces error only for [p, ..., "extra items"]
static const JsonTmpl*
simpleListPlaceholder(DiagsDest ctx, const JsonTmpl& jstmpl) {
  const JsonTmpl::Vector* vec = jstmpl.getIfVector();
  if(!vec || vec->size() < 2 || !vec->at(1).holdsEllipsis()) return nullptr;
  if(vec->at(0).holdsPlaceholder()) {
    if(vec->size() > 2)
      Error(ctx, vec->at(2).stPos, "This list should have ended here");
    // Return even if size() > 2, just ignore the other elements.
    return &(*vec)[0];
  }
  return nullptr;
}

// Return false means there were uncleaned ellipsis left in.
// TODO generalize this to accept interlaced outputs.
static bool
desugarEllipsisPlaceholdersRecur(DiagsDest ctx, JsonTmpl& jstmpl,
                                 vector<WholeSegment>& listNames) {
  if(const JsonTmpl* p = simpleListPlaceholder(ctx, jstmpl)) {
    listNames.emplace_back(p->stPos, p->enPos, p->getIfPlaceholder()->key);
    jstmpl = *p;
    return true;
  }
  if(jstmpl.holdsEllipsis()) {
    Error(ctx, jstmpl.stPos, jstmpl.enPos, "Ellipsis out of place");
    return false;
  }
  if(jstmpl.holdsString() || jstmpl.holdsPlaceholder()) return true;
  bool rv = true;
  if(auto* v = jstmpl.getIfVector()) {
    for(auto& elt : *v)
      if(!desugarEllipsisPlaceholdersRecur(ctx, elt, listNames))
        rv = false;
  }else if(auto* m = jstmpl.getIfMap()) {
    for(auto& [k,v] : *m)
      if(!desugarEllipsisPlaceholdersRecur(ctx, v, listNames))
        rv = false;
  }else Bug("Unknown JsonTmpl variant in desugarEllipsis {}", jstmpl.tagName());
  return rv;
}

// If any ellipsis is found out of place, the whole tmpl is cleared empty
// and an error is logged. The idea is that we should still be able to
// continue compilation. Modifies jstmpl in-place to convert any
// `[p, ...]` to just `p`. Returns all the converted placeholders
// in an array, so caller can figure out which placeholders were converted.
static vector<WholeSegment>
desugarEllipsisPlaceholders(DiagsDest ctx, JsonTmpl& jstmpl) {
  vector<WholeSegment> rv;
  if(!desugarEllipsisPlaceholdersRecur(ctx, jstmpl, rv)) {
    jstmpl = JsonTmpl::Map{};
    rv.clear();
  }else {
    sort(rv.begin(), rv.end(), [](auto& a, auto& b) { return *a < *b; });
    rv.erase(unique(rv.begin(), rv.end(),
                    [](auto& a, auto& b) { return *a == *b; }),
             rv.end());
  }
  return rv;
}

// We can later add where-stanza arguments for extracting partPatterns
static map<Ident,PartPattern>
makePartPatterns(DiagsDest ctx, const JsonTmpl& jstmpl) {
  if(jstmpl.holdsVector())
    Unimplemented("Directly outputting list not encased in a map");
  const JsonTmpl::Map* jstmplmap = jstmpl.getIfMap();
  if(jstmplmap == nullptr)
    Bug("parseJsonTmplFromBracketGroup() returned something strange");

  map<Ident, PartPattern> rv;
  for(const auto& [p, j] : jstmpl.allPlaceholders()) {
    WholeSegment seg(j->stPos, j->enPos, p);
    Ident id = Ident::parse(ctx, seg);
    if(id) rv.insert({id, GluedString(std::move(seg))});
  }
  return rv;
}

// Checks if all Ident Patterns nested in PatternRepeat or PatternFold
// are in present listNames and, conversely, if Ident Patterns not in those
// constructs are absent in listNames. `repeat` indicates if we are currently
// inside a fold or repeat pattern.
static bool
checkPlaceholderTypes(DiagsDest ctx, const vector<WholeSegment>& listNames,
                      const Pattern& patt, bool repeat) {
  if(holds_one_of_unique<WordToken, OperToken, NewlineChar>(patt)) return true;
  else if(auto* id = get_if_unique<Ident>(&patt)) {
    bool rv = !repeat;
    for(auto& n : listNames) if(*n == id->preserveCase()) {
      rv = repeat;
      break;
    }
    if(!rv) {
      string msg = format(repeat ? "Should be list-expanded: [{}, ...]"
                                 : "`{}` is a single element",
                          id->preserveCase());
      Error(ctx, id->stPos(), id->enPos(), msg);
    }
    return rv;
  }else if(auto* seq = get_if_unique<PatternConcat>(&patt)) {
    for(auto& elt : seq->parts)
      if(!checkPlaceholderTypes(ctx, listNames, elt, repeat))
        return false;
    return true;
  }else if(auto* ors = get_if_unique<PatternOrList>(&patt)) {
    for(auto& elt : ors->parts)
      if(!checkPlaceholderTypes(ctx, listNames, elt, repeat))
        return false;
    return true;
  }else if(auto* opt = get_if_unique<PatternOptional>(&patt)) {
    return checkPlaceholderTypes(ctx, listNames, opt->part, repeat);
  }else if(auto* rep = get_if_unique<PatternRepeat>(&patt)) {
    return checkPlaceholderTypes(ctx, listNames, rep->part, true);
  }else if(auto* fold = get_if_unique<PatternFold>(&patt)) {
    return checkPlaceholderTypes(ctx, listNames, fold->part, true) &&
           checkPlaceholderTypes(ctx, listNames, fold->glue, true);
  }else
    Bug("Unknown pattern index in checkPlaceholderTypes() {}", patt.index());
}

static bool
checkMultipleTmplPartsRecur(DiagsDest ctx,
                            vector<pair<string, int>>& counts,
                            const Pattern& patt) {
  if(holds_one_of_unique<WordToken, OperToken, NewlineChar>(patt)) return true;
  else if(auto* id = get_if_unique<Ident>(&patt)) {
    for(auto& [k, count] : counts) if(k == id->preserveCase() && ++count > 1) {
      Error(ctx, id->stPos(), id->enPos(),
        format("Output part '{}' appears multiple times in the pattern", k));
      return false;
    }
    return true;
  }else if(auto* seq = get_if_unique<PatternConcat>(&patt)) {
    for(auto& elt : seq->parts)
      if(!checkMultipleTmplPartsRecur(ctx, counts, elt))
        return false;
    return true;
  }else if(auto* ors = get_if_unique<PatternOrList>(&patt)) {
    for(auto& elt : ors->parts)
      if(!checkMultipleTmplPartsRecur(ctx, counts, elt))
        return false;
    return true;
  }else if(auto* opt = get_if_unique<PatternOptional>(&patt)) {
    return checkMultipleTmplPartsRecur(ctx, counts, opt->part);
  }else if(auto* rep = get_if_unique<PatternRepeat>(&patt)) {
    return checkMultipleTmplPartsRecur(ctx, counts, rep->part);
  }else if(auto* fold = get_if_unique<PatternFold>(&patt)) {
    return checkMultipleTmplPartsRecur(ctx, counts, fold->part) &&
           checkMultipleTmplPartsRecur(ctx, counts, fold->glue);
  }else
    Bug("Unknown pattern index in checkMultipleTmplParts() {}", patt.index());
}

// Dev-note: we do intend to allow Pattern Idents that do *not* appear in
// the output template. While such Idents cannot be represented in the input
// syntax today, it will become possible later.
static bool
checkMultipleTmplParts(DiagsDest ctx, const JsonTmpl::ConstPlaceholderMap& m,
                       const Pattern& patt) {
  vector<pair<string, int>> counts;
  for(auto& [k,v] : m) counts.emplace_back(k, 0);
  return checkMultipleTmplPartsRecur(ctx, counts, patt);
}

static Ident
identOf(DiagsDest ctx, const JsonTmpl& jstmpl) {
  auto* p = jstmpl.getIfPlaceholder();
  if(!p) Bug("Expected a Placeholder, got {}", jstmpl.prettyPrint());
  return Ident::parse(ctx, WholeSegment{jstmpl.stPos, jstmpl.enPos, p->key});
}

// Caller must already ensure no duplicate bindings for the same outTmplKey.
static const PatternToRuleBinding*
findRuleLocalBinding(string_view outk,
                     const vector<PatternToRuleBinding>& pattToRule) {
  for(auto& binding : pattToRule) if(binding.outTmplKey.preserveCase() == outk)
    return &binding;
  return nullptr;
}

static vector<pair<Ident, ssize_t>>
mapToRule(DiagsDest ctx, RulesWithLocs& rl,
          const vector<PatternToRuleBinding>& pattToRule,
          const JsonTmpl::ConstPlaceholderMap& outputKeys) {
  vector<pair<Ident, ssize_t>> rv;
  for(auto& [k, kcontainer] : outputKeys) {
    Ident outputIdent = identOf(ctx, *kcontainer);
    Ident ruleIdent;

    const PatternToRuleBinding* local = findRuleLocalBinding(k, pattToRule);
    if(local != nullptr) {
      rl.reserveLocalName(ctx, local->outTmplKey);
      ruleIdent = local->ruleName;
    }else ruleIdent = outputIdent;
    ssize_t ruleIndex = rl.findOrAppendIdent(ruleIdent);
    rv.emplace_back(std::move(outputIdent), ruleIndex);
  }
  return rv;
}

// Once we have extracted everything we need from InputDiags,
// this is where we compile the extracted string fragments into a rule.
// InputDiags is still used as a destination for error messages.
// Dev-note: we assume no duplicate binding for same jstmpl Placeholder.
void
appendPatternRules(DiagsDest ctx, const Ident& ident,
                   GluedString patt_string, const LexDirective& lexOpts,
                   vector<PatternToRuleBinding> pattToRule, JsonTmpl jstmpl,
                   RulesWithLocs& rl) {
  vector<WholeSegment> listNames = desugarEllipsisPlaceholders(ctx, jstmpl);
  map<Ident,PartPattern> partPatterns = makePartPatterns(ctx, jstmpl);

  auto toks = tokenizePattern(ctx, patt_string, partPatterns, lexOpts);
  if(!patt_string.empty() && toks.empty()) return;
  optional<Pattern> patt = parsePattern(ctx, std::move(toks));
  if(!patt.has_value()) return;
  if(!checkPlaceholderTypes(ctx, listNames, *patt, false)) return;
  if(!checkMultipleTmplParts(ctx, jstmpl.allPlaceholders(), *patt)) return;
  vector<pair<Ident, ssize_t>> pl2ruleMap
    = mapToRule(ctx, rl, pattToRule, jstmpl.allPlaceholders());

  PatternToRulesCompiler comp{ctx, rl, pl2ruleMap};
  ssize_t newIndex = comp.process(*patt);
  ssize_t newIndex2 = rl.defineIdent(ctx, ident);
  if(newIndex2 == -1) return;
  rl.deferred_assign(newIndex2, OutputTmpl{
      /* childidx */ newIndex,
      /* childName */ "",
      /* outputTmpl */ std::move(jstmpl)
  });
}

}  // namespace oalex
