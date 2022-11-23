/*  Copyright 2020-2022 The oalex authors.

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
#include <algorithm>
#include <map>
#include <utility>
#include "fmt/core.h"
#include "runtime/jsonloc.h"
#include "runtime/util.h"
#include "lexer.h"
using fmt::format;
using oalex::lex::GluedString;
using oalex::lex::NewlineChar;
using oalex::lex::unquote;
using std::make_unique;
using std::map;
using std::optional;
using std::pair;
using std::sort;
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
  * All the identifiers in the pattern are to be gathered up into a single
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
  // Optional error message for any failures. This can later be extended
  // with recovery rules and alternate branches. The Idents here are assumed
  // to be a subset of those in p2rule_ above.
  const vector<pair<Ident,string>>* errmsg_;
  ssize_t skipIndex_;
  ssize_t processConcat(const PatternConcat& concatPatt);
  ssize_t processOrList(const PatternOrList& orPatt);
  ssize_t processOptional(const PatternOptional& optPatt);
  ssize_t processIdent(const Ident& ident);
  ssize_t processRepeat(const PatternRepeat& repPatt);
  ssize_t processFold(const PatternFold& foldPatt);
 public:
  PatternToRulesCompiler(DiagsDest ctx, RulesWithLocs& rl,
                         const vector<pair<Ident,ssize_t>>& p2rule,
                         const vector<pair<Ident,string>>& errmsg,
                         ssize_t skipIndex) :
    ctx_(ctx), rl_(&rl), p2rule_(&p2rule),
    errmsg_(&errmsg), skipIndex_(skipIndex) {}
  // Just to prevent accidental copying.
  PatternToRulesCompiler(const PatternToRulesCompiler&) = delete;
  ssize_t process(const Pattern& patt);
};

ssize_t
PatternToRulesCompiler::processConcat(const PatternConcat& concatPatt) {
  ConcatFlatRule concatRule{ {} };
  for(ssize_t i = 0; i < ssize(concatPatt.parts); ++i) {
    if(i > 0) {
      // Intersperse concat components with SkipPoint components.
      concatRule.comps.push_back({rl_->ssize(), {}});
      rl_->appendAnonRule(SkipPoint{skipIndex_});
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
  return appendOptionalRule(*rl_, this->process(optPatt.part));
}

// TODO: optimize QuietMatch(OrRule(X, ErrorRule)) to just X.
ssize_t
PatternToRulesCompiler::processIdent(const Ident& ident) {
  size_t i;
  for(i=0; i<p2rule_->size(); ++i) if(ident == p2rule_->at(i).first) break;
  if(i == p2rule_->size())
    Bug("Ident map should already have an entry for '{}'",
        ident.preserveCase());
  ssize_t identRule = rl_->appendAnonRule(ConcatFlatRule{{
      {p2rule_->at(i).second, ident.preserveCase()},
  }});

  for(i=0; i<errmsg_->size(); ++i) if(ident == errmsg_->at(i).first) break;
  if(i == errmsg_->size()) return identRule;
  identRule = rl_->appendAnonRule(QuietMatch{identRule});
  ssize_t errRule = rl_->appendAnonRule(ErrorRule{errmsg_->at(i).second});

  return rl_->appendAnonRule(OrRule{ {
      {.lookidx{-1}, .parseidx{identRule}, .tmpl{passthroughTmpl}},
      {.lookidx{-1}, .parseidx{errRule}, .tmpl{passthroughTmpl}},
  }, true});
}

ssize_t
PatternToRulesCompiler::processRepeat(const PatternRepeat& repPatt) {
  ssize_t i = this->process(repPatt.part);
  ssize_t ski = rl_->appendAnonRule(SkipPoint{skipIndex_});
  return rl_->appendAnonRule(LoopRule{{
      .partidx = i, .partname = "", .glueidx = -1, .gluename = "",
      .lookidx = -1, .skipidx = ski}});
}

ssize_t
PatternToRulesCompiler::processFold(const PatternFold& foldPatt) {
  ssize_t pi = this->process(foldPatt.part);
  ssize_t gi = this->process(foldPatt.glue);
  ssize_t ski = rl_->appendAnonRule(SkipPoint{skipIndex_});
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

// Used for catching inconsistent-cased identifiers.
bool
exactMatch(const Ident& a, const Ident& b) {
  return a.preserveCase() == b.preserveCase();
}
bool
requireExactMatch(DiagsDest ctx, const Ident& a, const Ident& b) {
  bool rv = exactMatch(a, b);
  if(!rv)
    Error(ctx, a.stPos(), a.enPos(),
          format("'{}' case-conflicts with '{}'",
                 a.preserveCase(), b.preserveCase()));
  return rv;
}

void
logLocalNamesakeError(DiagsDest ctx, const Ident& local, const Ident& global) {
  if(!exactMatch(local, global))
    Error(ctx, local.stPos(), local.enPos(),
          format("Local variable name '{}' conflicts with the global name '{}'",
                 local.preserveCase(), global.preserveCase()));
  else
    Error(ctx, local.stPos(), local.enPos(),
          format("Local variable name '{}' conflicts with a global name",
                 local.preserveCase()));
}

bool
isEmptyMap(const JsonLoc& jsloc) {
  const JsonLoc::Map* m = jsloc.getIfMap();
  return m && m->empty();
}

// MapFields takes a JsonLoc::Map from some bootstrapped rule, and
// extracts named fields. Most errors here result in Bug() rather than a
// user-reported Error().
class MapFields {
 public:
  MapFields(const JsonLoc::Map* jmap, string_view rule_name)
    : jmap_{jmap}, rule_name_{rule_name} {
    if(!jmap) Bug("Parser output for {} should have been a map", rule_name);
  }

  template <class T> using get_t = std::conditional_t<
    std::is_pointer_v<T>,
    const std::remove_pointer_t<T>*,
    const T&>;
  template <class T> get_t<T> get(string_view field_name) const = delete;

  // This exists just to support T = StringLoc. It's generic just to keep
  // interface consistency with get<T>() above.
  // Dev note: fold this into get<T>() if
  //   we ever make getIfStringLoc() return a reference.
  template <class T, class =
    std::enable_if_t<!std::is_reference_v<T> && !std::is_pointer_v<T>>>
    T get_copy(string_view field_name) const { return get<T>(field_name); }
 private:
  const JsonLoc::Map* jmap_;
  string_view rule_name_;
};

template <> const JsonLoc&
MapFields::get<JsonLoc>(string_view field_name) const {
  const JsonLoc* rv = JsonLoc::mapScanForValue(*jmap_, field_name);
  if(!rv) Bug("Expected {} in {}", field_name, rule_name_);
  return *rv;
}

template <> StringLoc
MapFields::get_copy<StringLoc>(string_view field_name) const {
  const JsonLoc& jsloc = this->get<JsonLoc>(field_name);
  StringLoc s = jsloc.getIfStringLoc();
  if(!s) Bug("Expected {} in {} to be a string", field_name, rule_name_);
  return s;
}

template <> const JsonLoc::Vector*
MapFields::get<JsonLoc::Vector*>(string_view field_name) const {
  const JsonLoc* jsloc = JsonLoc::mapScanForValue(*jmap_, field_name);
  if(!jsloc || isEmptyMap(*jsloc)) return nullptr;
  const JsonLoc::Vector* vec = jsloc->getIfVector();
  if(!vec) Bug("Expected {} in {} to be a vector", field_name, rule_name_);
  return vec;
}

}  // namespace

const Rule& RulesWithLocs::operator[](ssize_t i) const {
  if(rules_[i]) return *rules_[i];
  else Bug("Dereferencing null Rule at index {}", i);
}

ssize_t
RulesWithLocs::findIdent(DiagsDest ctx, const Ident& id) const {
  if(!id) Bug("findIdent() invoked with empty Ident");
  for(ssize_t i=0; i<this->ssize(); ++i) {
    const Ident* name = rules_[i]->nameOrNull();
    if(name == nullptr || id != *name) continue;
    requireExactMatch(ctx, id, *name);
    return i;
  }
  return -1;
}

ssize_t
RulesWithLocs::findOrAppendIdent(DiagsDest ctx, const Ident& id) {
  LocPair thisPos{id.stPos(), id.enPos()};
  ssize_t i = findIdent(ctx, id);
  if(i != -1) {
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
RulesWithLocs::defineIdent(DiagsDest ctx, const Ident& ident,
                           ssize_t context_skipper) {
  if(!ident) Bug("defineIdent() invoked with empty Ident");
  Ident res_instance = findReservedLocalIdent(ident);
  if(res_instance) logLocalNamesakeError(ctx, res_instance, ident);
  for(ssize_t i=0; i<this->ssize(); ++i) {
    const Ident* name = rules_[i]->nameOrNull();
    if(name == nullptr || ident != *name) continue;
    requireExactMatch(ctx, ident, *name);

    if(dynamic_cast<const UnassignedRule*>(rules_[i].get())) {
      rules_[i] = make_unique<DefinitionInProgress>(ident, context_skipper);
      return i;
    }else {
      Error(ctx, ident.stPos(), ident.enPos(),
            format("'{}' has multiple definitions", ident.preserveCase()));
      return -1;
    }
  }
  rules_.push_back(make_unique<DefinitionInProgress>(ident, context_skipper));
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
      logLocalNamesakeError(ctx, ident, *name);
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

RuleSet
RulesWithLocs::releaseRulesWith(RegexOptions regexOpts) {
  firstUseLocs_.clear();
  reservedLocalNames_.clear();
  // std::move() is guaranteed to clear vectors.
  return RuleSet{std::move(rules_), std::move(skips_), std::move(regexOpts)};
}

// Bypasses Rule::nameOrNull() protections. Use with care!
static Ident
ruleNameOrEmpty(const Rule* r) {
  if(r == nullptr) return {};
  else if(const Ident* id = r->nameOrNull()) return *id;
  else return {};
}

template <class X> void
RulesWithLocs::deferred_assign(ssize_t idx, X x) {
  if(rules_[idx] != nullptr &&
     !dynamic_cast<const DefinitionInProgress*>(rules_[idx].get()) &&
     !dynamic_cast<const UnassignedRule*>(rules_[idx].get()))
    oalex::Bug("deferred_assign() cannot be used a rule already assigned");

  x.deferred_name(ruleNameOrEmpty(rules_[idx].get()));  // preserve old name
  x.context_skipper(rules_[idx]->context_skipper());    // and skipper
  rules_[idx] = move_to_unique(x);
}

template void RulesWithLocs::deferred_assign(ssize_t idx, SkipPoint);
template void RulesWithLocs::deferred_assign(ssize_t idx, ConcatRule);
template void RulesWithLocs::deferred_assign(ssize_t idx, OutputTmpl);
template void RulesWithLocs::deferred_assign(ssize_t idx, OrRule);
template void RulesWithLocs::deferred_assign(ssize_t idx, MatchOrError);

// Does very simple deduplication.
ssize_t
RulesWithLocs::addSkipper(Skipper skip) {
  if(!skips_.empty()) {
    // In case it's the default skip.
    if(skips_[0] == skip) return 0;
    // In case it's the most recent skip.
    if(skips_.back() == skip) return oalex::ssize(skips_)-1;
  }
  skips_.push_back(std::move(skip));
  return oalex::ssize(skips_)-1;
}

// For now, we can always return 0. If necessary, we can later switch into
// saving the default index in a private member variable.
ssize_t
RulesWithLocs::defaultSkipper(Skipper skip) {
  if(!skips_.empty()) Bug("Setting default skipper too late");
  skips_.push_back(std::move(skip));
  return 0;
}
ssize_t
RulesWithLocs::defaultSkipper() const {
  if(skips_.empty()) Bug("defaultSkipper was never set");
  return 0;
}

void
assignLiteralOrError(RulesWithLocs& rl, size_t ruleIndex, string_view literal) {
  string expectation = format("Expected '{}'", literal);
  if(literal == "\n") expectation = "Expected a newline";
  rl.deferred_assign(ruleIndex, MatchOrError{rl.ssize(), expectation});
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

static map<Ident,PartPattern>
makePartPatterns(DiagsDest ctx, const JsonTmpl& jstmpl,
                 const vector<PatternToRuleBinding>& pattToRule) {
  map<Ident, PartPattern> rv;
  for(const auto& [p, j] : jstmpl.allPlaceholders()) {
    WholeSegment seg(j->stPos, j->enPos, p);
    Ident id = Ident::parse(ctx, seg);
    if(id) rv.insert({id, GluedString(ctx, std::move(seg))});
  }
  for(const auto& binding : pattToRule)
    rv.insert({binding.localName, binding.pp}).second;
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

static void
patternCollectIdentRecur(const Pattern& patt, vector<Ident>& output) {
  if(holds_one_of_unique<WordToken, OperToken, NewlineChar>(patt)) return;
  else if(auto* id = get_if_unique<Ident>(&patt)) {
    output.push_back(*id);
  }else if(auto* seq = get_if_unique<PatternConcat>(&patt)) {
    for(auto& elt : seq->parts) patternCollectIdentRecur(elt, output);
  }else if(auto* ors = get_if_unique<PatternOrList>(&patt)) {
    for(auto& elt : ors->parts) patternCollectIdentRecur(elt, output);
  }else if(auto* opt = get_if_unique<PatternOptional>(&patt)) {
    patternCollectIdentRecur(opt->part, output);
  }else if(auto* rep = get_if_unique<PatternRepeat>(&patt)) {
    patternCollectIdentRecur(rep->part, output);
  }else if(auto* fold = get_if_unique<PatternFold>(&patt)) {
    patternCollectIdentRecur(fold->part, output);
    patternCollectIdentRecur(fold->glue, output);
  }else
    Bug("Unknown pattern index in checkMultipleUsage() {}", patt.index());
}

static vector<Ident>
patternCollectIdent(const Pattern& patt) {
  vector<Ident> rv;
  patternCollectIdentRecur(patt, rv);
  sort(rv.begin(), rv.end());
  return rv;
}

static void
ruleExprCollectInputIdents(const RuleExpr& rxpr, vector<Ident>& output);

static bool
checkMultipleUsage(DiagsDest ctx, const vector<Ident>& idents) {
  bool rv = true;
  for(ssize_t i=1; i<ssize(idents); ++i) if(idents[i] == idents[i-1]) {
      Error(ctx, idents[i].stPos(), idents[i].enPos(),
            format("Duplicate part '{}' will be impossible "
                   "to distinguish in the output.", idents[i].preserveCase()));
      // TODO: change pattern.cpp to use locations of where the idents appear
      // in patterns, not where they are defined. This will allow us to output
      // a "Previously used in..." note here. Right now, we don't have that
      // information at all. Then change patternCollectIdent() to use
      // stable_sort().
      rv = false;
  }
  return rv;
}

static Ident
identOf(DiagsDest ctx, const JsonTmpl& jstmpl) {
  auto* p = jstmpl.getIfPlaceholder();
  if(!p) Bug("Expected a Placeholder, got {}", jstmpl.prettyPrint());
  return Ident::parse(ctx, WholeSegment{jstmpl.stPos, jstmpl.enPos, p->key});
}

static void
checkUnusedParts(DiagsDest ctx, vector<Ident> patternIdents,
                 const vector<PatternToRuleBinding>& pattToRule,
                 const JsonTmpl& jstmpl) {
  for(auto& [k,v]: jstmpl.allPlaceholders()) {
    const Ident outid = identOf(ctx, *v);
    if(!binary_search(patternIdents.begin(), patternIdents.end(), outid))
      Error(ctx, outid.stPos(), outid.enPos(),
            format("Output field '{}' was not found in the rule pattern",
                   outid.preserveCase()));
  }
  vector<Ident> allIdents = std::move(patternIdents);
  for(auto& b: pattToRule) ruleExprCollectInputIdents(*b.ruleExpr, allIdents);
  sort(allIdents.begin(), allIdents.end());
  for(auto& b: pattToRule) {
    const Ident outid = b.localName;
    if(!binary_search(allIdents.begin(),allIdents.end(), outid))
      Error(ctx, outid.stPos(), outid.enPos(),
            format("Local rule '{}' is not used in this rule",
                   outid.preserveCase()));
  }
}

// Caller must already ensure no duplicate bindings for the same localName.
static const PatternToRuleBinding*
findRuleLocalBinding(DiagsDest ctx, const Ident& outputIdent,
                     const vector<PatternToRuleBinding>& pattToRule) {
  for(auto& binding : pattToRule) if(binding.localName == outputIdent) {
    requireExactMatch(ctx, binding.localName, outputIdent);
    return &binding;
  }
  return nullptr;
}

static const Ident*
getIfIdent(const RuleExpr& rxpr) {
  if(auto* idxpr = dynamic_cast<const RuleExprIdent*>(&rxpr))
    return &idxpr->ident;
  else return nullptr;
}

// This is only used in preparation for reserveLocalNameInRule().
static vector<Ident>
filterUniqueRuleNames(const vector<PatternToRuleBinding>& pattToRule) {
  vector<Ident> rv;
  for(auto& b : pattToRule) if(auto* ruleName = getIfIdent(*b.ruleExpr))
    rv.push_back(*ruleName);
  sort(rv.begin(), rv.end());
  size_t i=0, j=0;
  for(i=j=0; j<rv.size(); ++j) {
    if((j>0 && rv[j]==rv[j-1]) || (j+1<rv.size() && rv[j]==rv[j+1])) continue;
    rv[i] = rv[j];
    ++i;
  }
  rv.erase(rv.begin()+i, rv.end());
  return rv;
}

// Special case for allowing `"+" as binop ~ binop`, while we usually
// don't allow local-global name collisions.
static bool
reusesGlobalName(const PatternToRuleBinding& b) {
  auto* id = getIfIdent(*b.ruleExpr);
  return id && *id == b.localName;
}

static void
reserveLocalNameInRule(DiagsDest ctx, RulesWithLocs& rl,
                       const PatternToRuleBinding& binding,
                       const vector<Ident>& unq_lhs) {
  if(!reusesGlobalName(binding))
    rl.reserveLocalName(ctx, binding.localName);
  else if(!std::binary_search(unq_lhs.begin(), unq_lhs.end(),
                              binding.localName)) {
    Error(ctx, binding.localName.stPos(), binding.localName.enPos(),
          "Reusing rule name is allowed only if "
          "its use is unique in this definition");
  }
}

// This function looks up rule names, and resolves them to rule indices.
// It combines variables used in an `output:` template and in a `where` stanza.
//
// It proceeds in two stages: first, it resolves all names to either local or
// global identifiers. This produces the return value rv. Then, in a second
// phase, it uses that name-to-ruleIndex mapping to compile the RuleExpr
// objects. The second phase is separate so that we can use all the names
// resolved in the first phase there.
static vector<pair<Ident, ssize_t>>
mapToRule(DiagsDest ctx, RulesWithLocs& rl,
          const vector<PatternToRuleBinding>& pattToRule,
          const JsonTmpl::ConstPlaceholderMap& outputKeys) {
  vector<pair<Ident, ssize_t>> rv;
  vector<const RuleExpr*> lhs_exprs;
  vector<Ident> unq_lhs = filterUniqueRuleNames(pattToRule);
  for(auto& [k, kcontainer] : outputKeys) {
    Ident outputIdent = identOf(ctx, *kcontainer);

    const PatternToRuleBinding* local
      = findRuleLocalBinding(ctx, outputIdent, pattToRule);
    if(local) continue;
    ssize_t ruleIndex = rl.findOrAppendIdent(ctx, outputIdent);
    rv.emplace_back(std::move(outputIdent), ruleIndex);
    lhs_exprs.push_back(nullptr);
  }
  for(auto& binding : pattToRule) {
    reserveLocalNameInRule(ctx, rl, binding, unq_lhs);
    size_t ruleIndex;
    if(reusesGlobalName(binding))
      ruleIndex = rl.findOrAppendIdent(ctx, binding.localName);
    else ruleIndex = rl.appendAnonRule(DefinitionInProgress{});
    rv.emplace_back(binding.localName, ruleIndex);
    lhs_exprs.push_back(binding.ruleExpr.get());
  }
  // rv is now ready to be returned, and to be used for lookups.
  for(ssize_t i=0; i<ssize(lhs_exprs); ++i) {
    ssize_t j = rv[i].second;
    if(lhs_exprs[i] && dynamic_cast<const DefinitionInProgress*>(&rl[j]))
      assignRuleExpr(ctx, *lhs_exprs[i], rv, rl, j);
  }
  return rv;
}

/* Input: the JsonLoc from the output of error_stanza in frontend_pieces.oalex.
   Argument errors.items should be a JsonLoc::Vector that looks like this: [
     { ident: "id1", error_msg: "msg1" },
     { ident: "id2", error_msg: "msg2" },
     ...
   ]
   Output: { {"id1", "msg1"}, {"id2", "msg2"}, ... },
     converted from JsonLoc to C++ standard structures. */
vector<pair<Ident, string>>
destructureErrors(DiagsDest ctx, JsonLoc errors) {
  MapFields fields{errors.getIfMap(), "errors of a rule"};
  const auto* rname = fields.get<JsonLoc::Vector*>("items");
  if(!rname) return {};
  vector<pair<Ident, string>> rv;
  for(auto& pair_jsloc : *rname) {
    MapFields pair{pair_jsloc.getIfMap(), "custom errors"};
    auto part_name = pair.get_copy<StringLoc>("ident");
    auto msgq = pair.get_copy<StringLoc>("error_msg");
    optional<GluedString> msg = unquote(msgq, ctx);
    if(!msg) continue;
    rv.push_back({Ident::parse(ctx, part_name), string{*msg}});
  }
  return rv;
}

static bool
requireValidIdents(DiagsDest ctx, const vector<pair<Ident,string>>& errmsg,
                   const vector<pair<Ident,ssize_t>>& p2rule) {
  bool rv = true;
  for(auto& [id, _] : errmsg) {
    bool found = false;
    for(auto& [id2, _] : p2rule) if(id == id2) { found = true; break; }
    if(!found) {
      Error(ctx, id.stPos(), id.enPos(),
            format("Undefined part pattern '{}'", id.preserveCase()));
      rv = false;
    }
  }
  for(size_t i=0; i<errmsg.size(); ++i)
    for(size_t j=i+1; j<errmsg.size(); ++j)
      if(errmsg[i].first == errmsg[j].first) {
        const Ident& id = errmsg[j].first;
        Error(ctx, id.stPos(), id.enPos(),
            format("'{}' has custom errors specified more than once",
              id.preserveCase()));
        rv = false;
      }
  return rv;
}

static JsonTmpl
deduceOutputTmpl(const vector<Ident>& idents) {
  JsonTmpl::Map rv;
  for(auto& id : idents) {
    string s = id.preserveCase();
    rv.push_back({s, JsonTmpl::Placeholder{s}});
  }
  return rv;
}

// parses a pattern, but deduces placeholders and tokenization from
// various local definitions.
static optional<Pattern>
parsePatternForLocalEnv(DiagsDest ctx, GluedString patt_string,
    const LexDirective& lexOpts, const map<Ident,PartPattern>& partPatterns) {
  auto toks = tokenizePattern(ctx, patt_string, partPatterns, lexOpts);
  if(!patt_string.empty() && toks.empty()) return std::nullopt;
  return parsePattern(ctx, std::move(toks));
}

static void
compilePattern(DiagsDest ctx, const Ident& ruleName, const Pattern& patt,
               const vector<PatternToRuleBinding>& pattToRule,
               const LexDirective& lexopts, JsonTmpl jstmpl, JsonLoc errors,
               RulesWithLocs& rl) {
  vector<pair<Ident, ssize_t>> pl2ruleMap
    = mapToRule(ctx, rl, pattToRule, jstmpl.allPlaceholders());
  vector<pair<Ident, string>> errmsg
    = destructureErrors(ctx, std::move(errors));
  if(!requireValidIdents(ctx, errmsg, pl2ruleMap)) return;

  ssize_t skipIndex = rl.addSkipper(lexopts.skip);
  PatternToRulesCompiler comp{ctx, rl, pl2ruleMap, errmsg, skipIndex};
  ssize_t newIndex = comp.process(patt);
  ssize_t newIndex2 = rl.defineIdent(ctx, ruleName, skipIndex);
  if(newIndex2 == -1) return;
  rl.deferred_assign(newIndex2, OutputTmpl{
      /* childidx */ newIndex,
      /* childName */ "",
      /* outputTmpl */ std::move(jstmpl)
  });
}

// Once we have extracted everything we need from InputDiags,
// this is where we compile the extracted string fragments into a rule.
// InputDiags is still used as a destination for error messages.
// This function utilizes a hack of using JsonTmpl::Ellipsis to mean the
// absense of an `outputs` stanza, where the template needs to be automatically
// deduced.
// Dev-note: we assume no duplicate binding for same jstmpl Placeholder.
void
appendPatternRules(DiagsDest ctx, const Ident& ruleName,
                   GluedString patt_string, const LexDirective& lexOpts,
                   vector<PatternToRuleBinding> pattToRule, JsonTmpl jstmpl,
                   JsonLoc errors, RulesWithLocs& rl) {
  map<Ident,PartPattern> partPatterns
    = makePartPatterns(ctx, jstmpl, pattToRule);

  optional<Pattern> patt = parsePatternForLocalEnv(ctx, std::move(patt_string),
                                                   lexOpts, partPatterns);
  if(!patt.has_value()) return;
  if(!jstmpl.holdsEllipsis()) {
    vector<WholeSegment> listNames = desugarEllipsisPlaceholders(ctx, jstmpl);
    if(!checkPlaceholderTypes(ctx, listNames, *patt, false)) return;
  }
  vector<Ident> patternIdents = patternCollectIdent(*patt);
  if(!checkMultipleUsage(ctx, patternIdents)) return;
  // ruleExprMakeOutputTmpl() checks for multiple usage in each RuleExpr.

  if(jstmpl.holdsEllipsis()) jstmpl = deduceOutputTmpl(patternIdents);

  // Errors are not fatal. Unused fields stay unused.
  checkUnusedParts(ctx, patternIdents, pattToRule, jstmpl);

  compilePattern(ctx, ruleName, *patt, pattToRule, lexOpts, std::move(jstmpl),
                 errors, rl);
}

ssize_t
appendOptionalRule(RulesWithLocs& rl, ssize_t ruleIndex) {
  OrRule orRule{{}, /* flattenOnDemand */ true};

  ssize_t i = rl.appendAnonRule(QuietMatch{ruleIndex});
  orRule.comps.push_back({-1, i, passthroughTmpl});

  // This branch always produces a Map on success.
  i = rl.appendAnonRule(StringRule{{}});
  orRule.comps.push_back({-1, i, JsonTmpl::Map{}});

  return rl.appendAnonRule(std::move(orRule));
}

static Ident
identFrom(const JsonLoc& jsloc, string_view desc, DiagsDest ctx) {
  const string* s = jsloc.getIfString();
  if(!s) Bug("{} should have been a string, found {}", desc, jsloc.tagName());
  const WholeSegment wseg{jsloc.stPos, jsloc.enPos, *s};
  return Ident::parse(ctx, wseg);
}

// TODO revisit error-handling here.
// TODO revisit all error-handling from Ident::parse() in the repo.
//   It may return empty. findOrAppendIdent() and friends don't expect it.
void
appendExternRule(const JsonLoc ruletoks, DiagsDest ctx, RulesWithLocs& rl) {
  if(ruletoks.holdsErrorValue()) return;  // Diags already populated in parser
  MapFields fields{ruletoks.getIfMap(), "extern_rule"};
  auto& rname    = fields.get<JsonLoc>("rule_name");
  auto ext_name  = fields.get_copy<StringLoc>("external_name");
  auto* params   = fields.get<JsonLoc::Vector*>("param");

  vector<ssize_t> ruleIndices;
  if(params) for(auto& p: *params) {
    auto param_ident = identFrom(p, "param", ctx);
    if(param_ident)
      ruleIndices.push_back(rl.findOrAppendIdent(ctx, param_ident));
  }
  ssize_t newIndex = rl.defineIdent(ctx, identFrom(rname, "rule name", ctx),
                                    rl.defaultSkipper());
  if(newIndex == -1) return;

  if(ExternParser::requireValidNameAndParamCount(ext_name, ruleIndices.size(),
                                                  ctx))
    rl.deferred_assign(newIndex,
                       ExternParser{ *ext_name, std::move(ruleIndices) });
  else {
    rl.deferred_assign(newIndex, StringRule{"suppress-undefined-error"});
  }
}

class RuleExprCompiler {
 public:
  RuleExprCompiler(RulesWithLocs& rl, DiagsDest ctx, const SymbolTable& symtab)
    : rl_{&rl}, ctx_{ctx}, symtab_{&symtab} {}
  RuleExprCompiler(const RuleExprCompiler&) = delete;
  RuleExprCompiler(RuleExprCompiler&&) = default;
  ssize_t process(const RuleExpr& rxpr);
  ssize_t lookupIdent(const Ident& id) const;
 private:
  RulesWithLocs* rl_;
  DiagsDest ctx_;
  const SymbolTable* symtab_;  // Assumed to not have duplicates.
  ssize_t appendFlatIdent(const Ident& ident, ssize_t ruleIndex);
  ssize_t processMappedIdent(const RuleExprMappedIdent& midxpr);
  ssize_t processConcat(const RuleExprConcat& catxpr);
  ssize_t processRepeat(const RuleExprRepeat& repxpr);
};
ssize_t
RuleExprCompiler::appendFlatIdent(const Ident& ident, ssize_t ruleIndex) {
  return rl_->appendAnonRule(ConcatFlatRule{{
       {ruleIndex, ident.preserveCase()}
  }});
}
ssize_t
RuleExprCompiler::lookupIdent(const Ident& id) const {
  for(auto& [entry_id,target]: *symtab_) if(id == entry_id) return target;
  // Fall back to globals. Our symbol table only contain local symbols from
  // `where` and `outputs`. `RuleExpr` can refer to globals not in either.
  return rl_->findOrAppendIdent(ctx_, id);
}
ssize_t
RuleExprCompiler::process(const RuleExpr& rxpr) {
  if(auto* id = dynamic_cast<const RuleExprIdent*>(&rxpr)) {
    return appendFlatIdent(id->ident, lookupIdent(id->ident));
  }else if(auto* s = dynamic_cast<const RuleExprSquoted*>(&rxpr)) {
    return appendLiteralOrError(*rl_, s->s);
  }else if(auto* regex = dynamic_cast<const RuleExprRegex*>(&rxpr)) {
    return appendRegexOrError(*rl_, regex->regex->clone());
  }else if(auto* mid = dynamic_cast<const RuleExprMappedIdent*>(&rxpr)) {
    return this->processMappedIdent(*mid);
  }else if(auto* cat = dynamic_cast<const RuleExprConcat*>(&rxpr)) {
    return this->processConcat(*cat);
  }else if(auto* opt = dynamic_cast<const RuleExprOptional*>(&rxpr)) {
    return appendOptionalRule(*rl_, this->process(*opt->part));
  }else if(auto* rep = dynamic_cast<const RuleExprRepeat*>(&rxpr)) {
    return this->processRepeat(*rep);
  }else {
    Bug("{} cannot handle RuleExpr of type {}", __func__, typeid(rxpr).name());
  }
}
ssize_t
RuleExprCompiler::processMappedIdent(const RuleExprMappedIdent& midxpr) {
  if(auto* rhsid = dynamic_cast<const RuleExprIdent*>(midxpr.rhs.get())) {
    ssize_t targetIndex = lookupIdent(rhsid->ident);
    return this->appendFlatIdent(midxpr.lhs, targetIndex);
  }else if(dynamic_cast<const RuleExprRegex*>(midxpr.rhs.get()) ||
           dynamic_cast<const RuleExprSquoted*>(midxpr.rhs.get())) {
    ssize_t newIndex = this->process(*midxpr.rhs);
    return this->appendFlatIdent(midxpr.lhs, newIndex);
  }else
    Bug("Mapped ident cannot have {} on the rhs", typeid(*midxpr.rhs).name());
}
ssize_t
RuleExprCompiler::processConcat(const RuleExprConcat& catxpr) {
  vector<ConcatFlatRule::Component> comps;
  for(const unique_ptr<const RuleExpr>& c : catxpr.parts) {
    comps.push_back({process(*c), {}});
  }
  return rl_->appendAnonRule(ConcatFlatRule{{std::move(comps)}});
}
ssize_t
RuleExprCompiler::processRepeat(const RuleExprRepeat& repxpr) {
  ssize_t i = this->process(*repxpr.part);
  ssize_t j = repxpr.glue ? this->process(*repxpr.glue) : -1;
  return rl_->appendAnonRule(LoopRule{{
      .partidx = i, .partname = "", .glueidx = j, .gluename = "",
      .lookidx = -1, .skipidx = -1}});
}


// Dev-note: This "code reuse" feels like a kludge, that will eventually
// accrue all sorts of "options" parameters for slight variations. Be on the
// lookout cleanup opportunities.
enum class RuleExprIdentCollection {
  inputsUsed,
  outputsProduced,
};

static void
ruleExprCollectIdents(const RuleExpr& rxpr, RuleExprIdentCollection coll,
                     vector<Ident>& output) {
  if(auto* id = dynamic_cast<const RuleExprIdent*>(&rxpr))
    output.push_back(id->ident);
  else if(dynamic_cast<const RuleExprSquoted*>(&rxpr) ||
          dynamic_cast<const RuleExprRegex*>(&rxpr)) return;
  else if(auto* mid = dynamic_cast<const RuleExprMappedIdent*>(&rxpr)) {
    if(coll == RuleExprIdentCollection::inputsUsed)
      ruleExprCollectIdents(*mid->rhs, coll, output);
    else if(coll == RuleExprIdentCollection::outputsProduced)
      output.push_back(mid->lhs);
    else Bug("Bad identifier collection config");
    if(!dynamic_cast<const RuleExprRegex*>(mid->rhs.get()) &&
       !dynamic_cast<const RuleExprSquoted*>(mid->rhs.get()) &&
       !dynamic_cast<const RuleExprIdent*>(mid->rhs.get()))
      Bug("Mapped idents must have simple rhs. Found {}",
          typeid(*mid->rhs).name());
  }
  else if(auto* cat = dynamic_cast<const RuleExprConcat*>(&rxpr)) {
    for(const unique_ptr<const RuleExpr>& part : cat->parts)
      ruleExprCollectIdents(*part, coll, output);
  }
  else if(auto* opt = dynamic_cast<const RuleExprOptional*>(&rxpr))
    ruleExprCollectIdents(*opt->part, coll, output);
  else if(auto* rep = dynamic_cast<const RuleExprRepeat*>(&rxpr)) {
    ruleExprCollectIdents(*rep->part, coll, output);
    if(rep->glue) ruleExprCollectIdents(*rep->glue, coll, output);
  }
  else
    Bug("{} cannot handle RuleExpr of type {}", __func__, typeid(rxpr).name());
}

static void
ruleExprCollectOutputIdents(const RuleExpr& rxpr, vector<Ident>& output) {
  ruleExprCollectIdents(rxpr, RuleExprIdentCollection::outputsProduced,
                        output);
}

static void
ruleExprCollectInputIdents(const RuleExpr& rxpr, vector<Ident>& output) {
  ruleExprCollectIdents(rxpr, RuleExprIdentCollection::inputsUsed,
                        output);
}

static JsonTmpl
ruleExprMakeOutputTmpl(DiagsDest ctx, const RuleExpr& rxpr) {
  vector<Ident> ids;
  ruleExprCollectOutputIdents(rxpr, ids);
  sort(ids.begin(), ids.end());  // to skip over duplicates later

  JsonTmpl::Map rv;
  for(ssize_t i=0; i<ssize(ids); ++i) {
    if(i>0 && ids[i] == ids[i-1]) {
      Error(ctx, ids[i].stPos(), ids[i].enPos(),
            format("Duplicate identifier '{}' will be impossible "
                   "to distinguish in the output.", ids[i].preserveCase()));
      Note(ctx, ids[i-1].stPos(), ids[i-1].enPos(), "Previously found here");
      continue;
    }
    string s = ids[i].preserveCase();
    rv.push_back({s, JsonTmpl::Placeholder{s}});
  }
  return rv;
}

void
assignRuleExpr(DiagsDest ctx, const RuleExpr& rxpr,
               const SymbolTable& symtab,
               RulesWithLocs& rl, ssize_t ruleIndex) {
  // TODO: Add more special-cases:
  //  - If rxpr has no idents anywhere, ie. if ruleExprCollectInputIdents()
  //    produces an empty vector. But that requires a new codegen Rule that
  //    collects and concatenates strings. Or it requires some strange
  //    regex surgery.
  if(auto* regxpr = dynamic_cast<const RuleExprRegex*>(&rxpr))
    return assignRegexOrError(
        rl, ruleIndex, "Does not match expected pattern",
        regxpr->regex->clone());
  else if(auto* sq = dynamic_cast<const RuleExprSquoted*>(&rxpr))
    return assignLiteralOrError(rl, ruleIndex, sq->s);

  RuleExprCompiler comp{rl, ctx, symtab};

  if(auto* id = getIfIdent(rxpr))
    return rl.deferred_assign(ruleIndex, AliasRule{comp.lookupIdent(*id)});
  ssize_t flatRule = comp.process(rxpr);
  rl.deferred_assign(ruleIndex, OutputTmpl{
      flatRule,  // childidx
      {},        // childName, ignored for map-returning childidx
      ruleExprMakeOutputTmpl(ctx, rxpr)  // outputTmpl
  });
}

// TODO: Refactor common parts between this and appendPatternRule().
// Dev-note: Right now, this supports ruleName being empty. But this should
// only be used for tests.
ssize_t
appendExprRule(DiagsDest ctx, const Ident& ruleName, const RuleExpr& rxpr,
               vector<PatternToRuleBinding> pattToRule, RulesWithLocs& rl) {
  vector<pair<Ident, ssize_t>> pl2ruleMap = mapToRule(ctx, rl, pattToRule, {});
  ssize_t newIndex = ruleName
    ? rl.defineIdent(ctx, ruleName, Rule::removedContext)
    : rl.appendAnonRule(DefinitionInProgress{});
  if(newIndex != -1) assignRuleExpr(ctx, rxpr, pl2ruleMap, rl, newIndex);
  return newIndex;
}

}  // namespace oalex
