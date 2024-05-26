/*  Copyright 2019-2024 The oalex authors.

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
#include <format>
#include <map>
#include <utility>
#include "runtime/jsonloc.h"
#include "runtime/util.h"
#include "lexer.h"
using oalex::lex::GluedString;
using oalex::lex::NewlineChar;
using oalex::lex::unquote;
using std::format;
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

static unique_ptr<Rule>
createOptionalRule(RulesWithLocs& rl, ssize_t ruleIndex);
static unique_ptr<Rule>
createWordOrError(RulesWithLocs& rl, string_view word, ssize_t regexOptsIdx);
static unique_ptr<Rule>
createLiteralOrError(RulesWithLocs& rl, string_view literal);

namespace {

// TODO: optimize QuietMatch(OrRule(X, ErrorRule)) to just X.
unique_ptr<Rule>
wrapErrorIfCustomized(unique_ptr<Rule> targetRule, const Ident& targetIdent,
                      const vector<pair<Ident,string>>& errmsg,
                      RulesWithLocs& rl) {
  ssize_t i;
  for(i=0; i<ssize(errmsg); ++i) if(targetIdent == errmsg.at(i).first) break;
  if(i == ssize(errmsg)) return targetRule;
  ssize_t targetidx = rl.appendAnonRulePtr(std::move(targetRule));
  targetRule = move_to_unique(QuietMatch{targetidx});
  ssize_t erridx = rl.appendAnonRule(ErrorRule{errmsg.at(i).second});

  return move_to_unique(OrRule{ {
      {.lookidx{-1}, .parseidx{targetidx}, .tmpl{passthroughTmpl}},
      {.lookidx{-1}, .parseidx{erridx}, .tmpl{passthroughTmpl}},
  }, true});
}

// This is used to look up rules in RulesWithLocs by name.
// This usually represents all visible symbols in a certain context. When this
// object is constructed, we have usually already resolved symbols to either
// globally defined rules or ones locally defined in a rule.
using SymbolTable = std::vector<std::pair<Ident, ssize_t>>;

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
createWordOrError, createLiteralOrError, createRegexOrError). The Word and
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
  SymbolTable symtab_;
  // Optional error message for any failures. This can later be extended
  // with recovery rules and alternate branches. The Idents here are assumed
  // to be a subset of those in symtab_ above.
  const vector<pair<Ident,string>>* errmsg_;
  ssize_t skipIndex_, regexOptsIdx_;
  unique_ptr<Rule> processConcat(const PatternConcat& concatPatt);
  unique_ptr<Rule> processOrList(const PatternOrList& orPatt);
  unique_ptr<Rule> processOptional(const PatternOptional& optPatt);
  unique_ptr<Rule> processIdent(const Ident& ident);
  unique_ptr<Rule> processRepeat(const PatternRepeat& repPatt);
  unique_ptr<Rule> processFold(const PatternFold& foldPatt);
 public:
  PatternToRulesCompiler(DiagsDest ctx, RulesWithLocs& rl,
                         SymbolTable symtab,
                         const vector<pair<Ident,string>>& errmsg,
                         ssize_t skipIndex, ssize_t regexOptsIdx) :
    ctx_(ctx), rl_(&rl), symtab_(std::move(symtab)),
    errmsg_(&errmsg), skipIndex_(skipIndex), regexOptsIdx_(regexOptsIdx) {}
  // Just to prevent accidental copying.
  PatternToRulesCompiler(const PatternToRulesCompiler&) = delete;
  unique_ptr<Rule> process(const Pattern& patt);
};

unique_ptr<Rule>
PatternToRulesCompiler::processConcat(const PatternConcat& concatPatt) {
  ConcatFlatRule concatRule{ {} };
  for(ssize_t i = 0; i < ssize(concatPatt.parts); ++i) {
    if(i > 0) {
      // Intersperse concat components with SkipPoint components.
      concatRule.comps.push_back({rl_->ssize(), {}});
      rl_->appendAnonRule(SkipPoint{skipIndex_});
    }
    const Pattern& child = concatPatt.parts[i];
    ssize_t j = rl_->appendAnonRulePtr(this->process(child));
    concatRule.comps.push_back({j, {}});
  }
  return move_to_unique(concatRule);
}

// TODO failed tests should output location. Or tests should have names.
unique_ptr<Rule>
PatternToRulesCompiler::processOrList(const PatternOrList& orPatt) {
  OrRule orRule{{}, /* flattenOnDemand */ true};
  for(ssize_t i = 0; i < ssize(orPatt.parts); ++i) {
    const Pattern& child = orPatt.parts[i];
    ssize_t j = rl_->appendAnonRulePtr(this->process(child));
    if(i+1 != ssize(orPatt.parts)) j = rl_->appendAnonRule(QuietMatch{j});
    orRule.comps.push_back({-1, j, passthroughTmpl});
  }
  return move_to_unique(orRule);
}

unique_ptr<Rule>
PatternToRulesCompiler::processOptional(const PatternOptional& optPatt) {
  return createOptionalRule(*rl_,
      rl_->appendAnonRulePtr(this->process(optPatt.part)) );
}

unique_ptr<Rule>
PatternToRulesCompiler::processIdent(const Ident& ident) {
  size_t i;
  for(i=0; i<symtab_.size(); ++i) if(ident == symtab_.at(i).first) break;
  if(i == symtab_.size())
    Bug("Ident map should already have an entry for '{}'",
        ident.preserveCase());
  unique_ptr<Rule> identRule = move_to_unique(ConcatFlatRule{{
      {symtab_.at(i).second, ident.preserveCase()},
  }});
  return wrapErrorIfCustomized(std::move(identRule), ident, *errmsg_, *rl_);
}

unique_ptr<Rule>
PatternToRulesCompiler::processRepeat(const PatternRepeat& repPatt) {
  ssize_t i = rl_->appendAnonRulePtr(this->process(repPatt.part));
  ssize_t ski = rl_->appendAnonRule(SkipPoint{skipIndex_});
  return move_to_unique(LoopRule{{
      .partidx = i, .glueidx = -1, .lookidx = -1, .skipidx = ski}});
}

unique_ptr<Rule>
PatternToRulesCompiler::processFold(const PatternFold& foldPatt) {
  ssize_t pi = rl_->appendAnonRulePtr(this->process(foldPatt.part));
  ssize_t gi = rl_->appendAnonRulePtr(this->process(foldPatt.glue));
  ssize_t ski = rl_->appendAnonRule(SkipPoint{skipIndex_});
  return move_to_unique(LoopRule{{
      .partidx = pi, .glueidx = gi, .lookidx = -1, .skipidx = ski}});
}

unique_ptr<Rule>
PatternToRulesCompiler::process(const Pattern& patt) {
  if(auto* word = get_if_unique<WordToken>(&patt)) {
    return createWordOrError(*rl_, **word, regexOptsIdx_);
  }else if(auto* oper = get_if_unique<OperToken>(&patt)) {
    return createLiteralOrError(*rl_, **oper);
  }else if(get_if_unique<NewlineChar>(&patt)) {
    return createLiteralOrError(*rl_, "\n");
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

ssize_t
RulesWithLocs::appendAnonRulePtr(unique_ptr<Rule> rule) {
  rules_.push_back(std::move(rule));
  firstUseLocs_.emplace_back(-1, -1);
  return rules_.size()-1;
}

template <class X> ssize_t
RulesWithLocs::appendAnonRule(X x) {
  return appendAnonRulePtr(move_to_unique(x));
}

template ssize_t RulesWithLocs::appendAnonRule(StringRule);
template ssize_t RulesWithLocs::appendAnonRule(WordPreserving);
template ssize_t RulesWithLocs::appendAnonRule(RegexRule);
template ssize_t RulesWithLocs::appendAnonRule(SkipPoint);
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

RuleSet
RulesWithLocs::releaseRules() {
  firstUseLocs_.clear();
  reservedLocalNames_.clear();
  // std::move() is guaranteed to clear vectors.
  return RuleSet{.rules = std::move(rules_),
                 .skips = std::move(skips_),
                 .regexOpts = std::move(regexOpts_)};
}

// Bypasses Rule::nameOrNull() protections. Use with care!
static Ident
ruleNameOrEmpty(const Rule* r) {
  if(r == nullptr) return {};
  else if(const Ident* id = r->nameOrNull()) return *id;
  else return {};
}

void
RulesWithLocs::deferred_assign_ptr(ssize_t idx, unique_ptr<Rule> rule) {
  if(rules_[idx] != nullptr &&
     !dynamic_cast<const DefinitionInProgress*>(rules_[idx].get()) &&
     !dynamic_cast<const UnassignedRule*>(rules_[idx].get()))
    oalex::Bug("deferred_assign() cannot be used a rule already assigned");

  rule->deferred_name(ruleNameOrEmpty(rules_[idx].get()));  // preserve old name
  rule->context_skipper(rules_[idx]->context_skipper());    // and skipper
  rules_[idx] = std::move(rule);
}

template <class X> void
RulesWithLocs::deferred_assign(ssize_t idx, X x) {
  deferred_assign_ptr(idx, move_to_unique(x));
}
template void RulesWithLocs::deferred_assign(ssize_t idx, SkipPoint);
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
    if(skips_.back() == skip) return std::ssize(skips_)-1;
  }
  skips_.push_back(std::move(skip));
  return std::ssize(skips_)-1;
}

// Two charsets can be equal even if they have different representation.
// This equality function will return true iff they have the exact same
// representation.
static bool
isIdentical(const RegexOptions& r1, const RegexOptions& r2) {
  if(r1.word.negated != r2.word.negated) return false;
  if(r1.word.ranges.size() != r2.word.ranges.size()) return false;
  ssize_t n = r1.word.ranges.size();
  for(ssize_t i=0; i<n; ++i) {
    if(r1.word.ranges[i].from != r2.word.ranges[i].from) return false;
    if(r1.word.ranges[i].to != r2.word.ranges[i].to) return false;
  }
  return true;
}

// Does very simple deduplication.
ssize_t
RulesWithLocs::addRegexOpts(RegexOptions regexOpts) {
  if(!regexOpts_.empty()) {
    if(isIdentical(regexOpts_[0], regexOpts))
      return 0;
    if(isIdentical(regexOpts_.back(), regexOpts))
      return std::ssize(regexOpts_)-1;
  }
  regexOpts_.push_back(std::move(regexOpts));
  return std::ssize(regexOpts_)-1;
}

bool
RulesWithLocs::defaultLexopts(LexDirective lexopts) {
  if(skips_.size() >= 2 || regexOpts_.size() >= 2 || this->ssize() > 0)
    return false;
  if(skips_.empty()) skips_.resize(1);
  if(regexOpts_.empty()) regexOpts_.resize(1);
  skips_[0] = std::move(lexopts.skip);
  regexOpts_[0] = RegexOptions{.word = std::move(lexopts.wordChars)};
  return true;
}

// Dev-note: This is copying a bit too much. Keep an internal cache if
// performance becomes a problem.
LexDirective
RulesWithLocs::defaultLexopts() const {
  return LexDirective{
    .wordChars = regexOpts_.at(0).word,
    .skip = skips_.at(0),
    .tailcont = false,
  };
}

ssize_t
RulesWithLocs::defaultSkipper() const {
  if(skips_.empty()) Bug("defaultSkipper was never set");
  return 0;
}

static unique_ptr<Rule>
createLiteralOrError(RulesWithLocs& rl, string_view literal) {
  string expectation = format("Expected '{}'", literal);
  if(literal == "\n") expectation = "Expected a newline";
  ssize_t newIndex = rl.appendAnonRule(StringRule(string(literal)));
  return move_to_unique(MatchOrError{newIndex, expectation});
}

// Used if we encounter errors after a slot has already been allocated
// as DefinitionInProgress.
static unique_ptr<Rule>
dummyRule() { return move_to_unique(StringRule{""}); }

static unique_ptr<Rule>
createWordOrError(RulesWithLocs& rl, string_view word, ssize_t regexOptsIdx) {
  ssize_t newIndex = rl.ssize();
  rl.appendAnonRule(WordPreserving{word, regexOptsIdx});
  return move_to_unique(MatchOrError{newIndex, format("Expected '{}'", word)});
}

static unique_ptr<Rule>
createRegexOrError(RulesWithLocs& rl, unique_ptr<const Regex> regex,
                   ssize_t regexOptsIdx) {
  ssize_t newIndex = rl.ssize();
  rl.appendAnonRule(RegexRule{std::move(regex), regexOptsIdx});
  return move_to_unique(
      MatchOrError{newIndex, "Does not match expected pattern"} );
}

// ---------------------- Start appendExprRule() ------------------------------

// TODO: Rename this. It is used for exported idents too, which is different
// from idents used.
struct IdentUsage {
  Ident id;
  bool inList;
};

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
                                 vector<Ident>& listNames) {
  if(const JsonTmpl* p = simpleListPlaceholder(ctx, jstmpl)) {
    Ident pid
      = Ident::parse(ctx, StringLoc{p->getIfPlaceholder()->key, p->stPos});
    // TODO: this needs to be fuzz-tested.
    if(!pid) Bug("JsonTmpl placeholders should all be valid identifiers");
    listNames.push_back(std::move(pid));
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
static vector<Ident>
desugarEllipsisPlaceholders(DiagsDest ctx, JsonTmpl& jstmpl) {
  vector<Ident> rv;
  if(!desugarEllipsisPlaceholdersRecur(ctx, jstmpl, rv)) {
    jstmpl = JsonTmpl::Map{};
    rv.clear();
  }else {
    sort(rv.begin(), rv.end());
    rv.erase(unique(rv.begin(), rv.end()), rv.end());
  }
  return rv;
}

static map<Ident,PartPattern>
makePartPatterns(DiagsDest ctx, const JsonTmpl& jstmpl,
                 const vector<LocalBinding>& locals) {
  map<Ident, PartPattern> rv;
  for(const auto& [p, j] : jstmpl.allPlaceholders()) {
    WholeSegment seg(j->stPos, j->enPos, p);
    Ident id = Ident::parse(ctx, seg);
    if(id) rv.insert({id, GluedString(ctx, std::move(seg))});
  }
  for(const auto& local : locals)
    rv.insert({local.localName, local.pp}).second;
  return rv;
}

// Checks if all patternIdents with inList set also appear in tmplLists and,
// conversely, if Ident Patterns with inList unset are absent in tmplLists.
static bool
checkPlaceholderTypes(DiagsDest ctx, const vector<Ident>& tmplLists,
                      const vector<IdentUsage>& patternIdents) {
  bool rv = true;
  for(auto& usage : patternIdents) {
    bool found = false;
    for(auto& tl : tmplLists) if(usage.id == tl) { found = true; break; }
    if(usage.inList == found) continue;
    rv = false;
    string msg = usage.inList
      ? format("Should be list-expanded: [{}, ...]", usage.id.preserveCase())
      : format("`{}` is a single element", usage.id.preserveCase());
    Error(ctx, usage.id.stPos(), usage.id.enPos(), msg);
  }
  return rv;
}

bool idlt(const IdentUsage& a, const IdentUsage& b) { return a.id < b.id; }
void setInList(vector<IdentUsage>::iterator a, vector<IdentUsage>::iterator b) {
  for(auto it=a; it!=b; ++it) it->inList=true;
}

// I kept forgetting which vector<IdentUsage> needs to be sorted and which
// isn't. So this is a type to keep track of it. As long as the vector is
// wrapped in a SortedIdents object, it can be assumed to be sorted.
class SortedIdents {
  vector<IdentUsage> data_;
 public:
  SortedIdents() = default;
  explicit SortedIdents(vector<IdentUsage> v)
    : data_(std::move(v)) { sort(data_.begin(), data_.end(), idlt); }
  vector<IdentUsage> release() { return std::move(data_); }
  const vector<IdentUsage>& get() const { return data_; }
  ssize_t ssize() const { return std::ssize(data_); }
  const IdentUsage& operator[] (ssize_t idx) const { return data_[idx]; }
  bool contains(const Ident& id) const {
    return binary_search(data_.begin(), data_.end(), IdentUsage{id, false},
                         idlt);
  }
};

// Dev-note: This setInList() pattern can sometimes be error-prone. Switch to
// using a dedicated `Config` struct if we need to add more such state. See
// ruleExprCollectIdents() as an example.
static void
patternCollectIdentRecur(const Pattern& patt, vector<IdentUsage>& output) {
  if(holds_one_of_unique<WordToken, OperToken, NewlineChar>(patt)) return;
  else if(auto* id = get_if_unique<Ident>(&patt)) {
    output.push_back({*id, false});
  }else if(auto* seq = get_if_unique<PatternConcat>(&patt)) {
    for(auto& elt : seq->parts) patternCollectIdentRecur(elt, output);
  }else if(auto* ors = get_if_unique<PatternOrList>(&patt)) {
    for(auto& elt : ors->parts) patternCollectIdentRecur(elt, output);
  }else if(auto* opt = get_if_unique<PatternOptional>(&patt)) {
    patternCollectIdentRecur(opt->part, output);
  }else if(auto* rep = get_if_unique<PatternRepeat>(&patt)) {
    ssize_t oldsize = output.size();
    patternCollectIdentRecur(rep->part, output);
    setInList(output.begin()+oldsize, output.end());
  }else if(auto* fold = get_if_unique<PatternFold>(&patt)) {
    ssize_t oldsize = output.size();
    patternCollectIdentRecur(fold->part, output);
    patternCollectIdentRecur(fold->glue, output);
    setInList(output.begin()+oldsize, output.end());
  }else
    Bug("Unknown pattern index in checkMultipleUsage() {}", patt.index());
}

static SortedIdents
patternCollectIdent(const Pattern& patt) {
  vector<IdentUsage> rv;
  patternCollectIdentRecur(patt, rv);
  return SortedIdents{std::move(rv)};
}

static void
ruleExprCollectInputIdents(
    const RuleExpr& rxpr, const map<string,SortedIdents>& patternIdents,
    vector<IdentUsage>& output);

static bool
checkMultipleUsage(DiagsDest ctx, const SortedIdents& idents) {
  bool rv = true;
  for(ssize_t i=1; i<idents.ssize(); ++i) if(idents[i].id == idents[i-1].id) {
      Error(ctx, idents[i].id.stPos(), idents[i].id.enPos(),
            format("Duplicate output '{}' will be impossible "
                   "to distinguish in the output.",
                   idents[i].id.preserveCase()));
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

static SortedIdents
appendLocals(SortedIdents ids,
             const map<string,SortedIdents>& localRulePatternIdents,
             const vector<LocalBinding>& locals) {
  vector<IdentUsage> v = ids.release();
  for(auto& l: locals)
    ruleExprCollectInputIdents(*l.ruleExpr, localRulePatternIdents, v);
  return SortedIdents{std::move(v)};
}

static bool
checkUndefinedOutfields(DiagsDest ctx, const SortedIdents& exportedIdents,
                        const JsonTmpl& jstmpl) {
  bool error_found = false;
  for(auto& [k,v]: jstmpl.allPlaceholders()) {
    const Ident outid = identOf(ctx, *v);
    if(!exportedIdents.contains(outid)) {
      error_found = true;
      Error(ctx, outid.stPos(), outid.enPos(),
            format("Output field '{}' was not found in the rule pattern",
                   outid.preserveCase()));
    }
  }
  return !error_found;
}

static void
checkUnusedParts(DiagsDest ctx, SortedIdents identsUsed,
                 const map<string,SortedIdents>& localRulePatternIdents,
                 const vector<LocalBinding>& locals) {
  SortedIdents allIdents = appendLocals(std::move(identsUsed),
                                        localRulePatternIdents, locals);
  for(auto& l: locals) {
    const Ident outid = l.localName;
    if(!allIdents.contains(outid))
      Error(ctx, outid.stPos(), outid.enPos(),
            format("Local rule '{}' is not used in this rule",
                   outid.preserveCase()));
  }
}

// Caller must already ensure no duplicate bindings for the same localName.
static const LocalBinding*
findRuleLocalBinding(DiagsDest ctx, const Ident& outputIdent,
                     const vector<LocalBinding>& locals) {
  for(auto& local : locals) if(local.localName == outputIdent) {
    requireExactMatch(ctx, local.localName, outputIdent);
    return &local;
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
filterUniqueRuleNames(const vector<LocalBinding>& locals) {
  vector<Ident> rv;
  for(auto& l : locals) if(auto* ruleName = getIfIdent(*l.ruleExpr))
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
reusesGlobalName(const LocalBinding& b) {
  auto* id = getIfIdent(*b.ruleExpr);
  return id && *id == b.localName;
}

static void
reserveLocalNameInRule(DiagsDest ctx, RulesWithLocs& rl,
                       const LocalBinding& binding,
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
// The output is sorted by name, so that lookups can be binary searched.
// RuleExprCompiler::lookupLocalIdent() uses this property.
static SymbolTable
mapToRule(DiagsDest ctx, RulesWithLocs& rl, const vector<LocalBinding>& locals,
          const vector<Ident>& mappedIdents,
          const JsonTmpl::ConstPlaceholderMap& outputKeys) {
  SymbolTable rv;
  vector<Ident> unq_lhs = filterUniqueRuleNames(locals);
  for(auto& [k, kcontainer] : outputKeys) {
    Ident outputIdent = identOf(ctx, *kcontainer);

    const LocalBinding* local
      = findRuleLocalBinding(ctx, outputIdent, locals);
    if(local) continue;
    if(find(mappedIdents.begin(), mappedIdents.end(), outputIdent)
        != mappedIdents.end()) continue;
    ssize_t ruleIndex = rl.findOrAppendIdent(ctx, outputIdent);
    rv.emplace_back(std::move(outputIdent), ruleIndex);
  }
  for(auto& local : locals) {
    reserveLocalNameInRule(ctx, rl, local, unq_lhs);
    size_t ruleIndex;
    if(reusesGlobalName(local))
      ruleIndex = rl.findOrAppendIdent(ctx, local.localName);
    else ruleIndex = rl.appendAnonRule(DefinitionInProgress{});
    rv.emplace_back(local.localName, ruleIndex);
  }
  sort(rv.begin(), rv.end());
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
destructureErrors(DiagsDest ctx, const ParsedIndentedList& errors) {
  // TODO that reference version of the cast.
  vector<pair<Ident, string>> rv;
  for(auto& item : errors.items) {
    const auto* line = item.try_cast<ParsedErrorStanzaLine>();
    if(!line) Bug("Error line has the wrong type: {}", item.type().name());
    StringLoc part_name = line->fields.ident;
    StringLoc msgq = line->fields.error_msg;
    optional<GluedString> msg = unquote(msgq, ctx);
    if(!msg) continue;
    rv.push_back({Ident::parse(ctx, part_name), string{*msg}});
  }
  return rv;
}

static bool
requireValidIdents(DiagsDest ctx, const vector<pair<Ident,string>>& errmsg,
                   const SymbolTable& symtab) {
  bool rv = true;
  for(auto& [id, _] : errmsg) {
    bool found = false;
    for(auto& [id2, _] : symtab) if(id == id2) { found = true; break; }
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
deduceOutputTmpl(const SortedIdents& outputIdents) {
  JsonTmpl::Map rv;
  for(ssize_t i=0; i<outputIdents.ssize(); ++i) {
    if(i>0 && outputIdents[i].id == outputIdents[i-1].id) continue;
    string s = outputIdents[i].id.preserveCase();
    rv.push_back({s, JsonTmpl::Placeholder{s}});
  }
  return rv;
}

// parses a pattern, but deduces placeholders and tokenization from
// various local definitions.
static optional<Pattern>
parsePatternForLocalEnv(DiagsDest ctx, GluedString patt_string,
    const LexDirective& lexopts, const map<Ident,PartPattern>& partPatterns) {
  auto toks = tokenizePattern(ctx, patt_string, partPatterns, lexopts);
  if(!patt_string.empty() && toks.empty()) return std::nullopt;
  return parsePattern(ctx, std::move(toks));
}

static unique_ptr<Rule>
createOptionalRule(RulesWithLocs& rl, ssize_t ruleIndex) {
  OrRule orRule{{}, /* flattenOnDemand */ true};

  ssize_t i = rl.appendAnonRule(QuietMatch{ruleIndex});
  orRule.comps.push_back({-1, i, passthroughTmpl});

  // This branch always produces a Map on success.
  i = rl.appendAnonRule(StringRule{{}});
  orRule.comps.push_back({-1, i, JsonTmpl::Map{}});

  return move_to_unique(orRule);
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
appendExternRule(const ParsedExternRule& ext, DiagsDest ctx,
                 RulesWithLocs& rl) {
  vector<ssize_t> ruleIndices;
  for(const StringLoc& p: ext.fields.param) {
    auto param_ident = identFrom(p, "param", ctx);
    if(param_ident)
      ruleIndices.push_back(rl.findOrAppendIdent(ctx, param_ident));
  }
  ssize_t newIndex
    = rl.defineIdent(ctx, identFrom(ext.fields.rule_name, "rule name", ctx),
                     rl.defaultSkipper());
  if(newIndex == -1) return;

  const StringLoc& ext_name = ext.fields.external_name;
  if(ExternParser::requireValidNameAndParamCount(ext_name, ruleIndices.size(),
                                                  ctx))
    rl.deferred_assign(newIndex,
                       ExternParser{ *ext_name, std::move(ruleIndices) });
  else {
    rl.deferred_assign(newIndex, StringRule{"suppress-undefined-error"});
  }
}

struct CompiledSingleExpr {
  unique_ptr<Rule> rule;  // never nullptr. unique_ptr for type erasure only.
  SortedIdents identsUsed;
};

struct CompiledRuleBranch {
  OrRule::Component or_comp;
  SortedIdents identsUsed, exportedIdents;
};

class RuleExprCompiler {
 public:
  /* Params:

    * lexopts: The `lexical:` stanza, or defaults.
    * symtab: Local rules from the `where:` stanza, plus any defined in
        the output template (if any). RuleExprCompuler falls back to global
        rules during RuleExpr compilation, but not in a pattern compilation.
        Produced from mapToRule(), used by either lookupLocalIdent() or by
        lookupIdent().
    * partPatterns: Used by tokenizePattern() of pattern.cpp. They should
        already be in symtab.
    * errmsg: The `errors after failing:` stanza.

  Effects: New rules are appended to RulesWithLocs as a result of compilation.
  */
  RuleExprCompiler(RulesWithLocs& rl, DiagsDest ctx,
                   const LexDirective& lexopts, const SymbolTable& symtab,
                   const map<Ident,PartPattern>& partPatterns,
                   const vector<pair<Ident,string>>& errmsg)
    : rl_{&rl}, ctx_{ctx}, lexOpts_{&lexopts}, symtab_{&symtab},
      partPatterns_{&partPatterns}, errmsg_{&errmsg},
      regexOptsIdx_{rl_->addRegexOpts(RegexOptions{lexOpts_->wordChars})},
      pattComp_{ctx_, rl, *symtab_, errmsg, rl_->addSkipper(lexOpts_->skip),
                regexOptsIdx_}
      {}
  RuleExprCompiler(const RuleExprCompiler&) = delete;
  RuleExprCompiler(RuleExprCompiler&&) = default;

  // Returns the identifiers referenced in a given RuleExprDquoted string.
  // Only works once the given string has been compiled by compileRuleExpr().
  //
  // Dev-note: Only processDquoted() ever adds to this.
  const map<string,SortedIdents>& patternIdents() const {
    return patternIdents_;
  }

  // The main methods for compiling RuleExpr.
  void compileLocalRules(const vector<LocalBinding>& locals);
  optional<CompiledSingleExpr> compileSingleExpr(const RuleExpr& rxpr);
  optional<CompiledSingleExpr> compileSingleExprWithTmpl(const RuleExpr& rxpr,
                                                         JsonTmpl jstmpl);
  optional<CompiledRuleBranch> compileRuleBranch(const RuleBranch& branch,
                                                 bool exposeFields);

 private:
  RulesWithLocs* rl_;
  DiagsDest ctx_;
  const LexDirective* lexOpts_;
  const SymbolTable* symtab_;  // Assumed to not have duplicates.
  const map<Ident,PartPattern>* partPatterns_;
  const vector<pair<Ident,string>>* errmsg_;
  ssize_t regexOptsIdx_;
  PatternToRulesCompiler pattComp_;

  // The map values are sorted for deduceOutputTmpl() and
  // ruleExprOutputIdentsCheckUnique.
  map<string,SortedIdents> patternIdents_;

  bool somePatternFailed_ = false;
  unique_ptr<Rule> process(const RuleExpr& rxpr);
  unique_ptr<Rule> createIfStringExpr(const RuleExpr& rxpr);
  unique_ptr<Rule> createFlatIdent(const Ident& ident, ssize_t ruleIndex);
  unique_ptr<Rule> identComponent(const Ident& id);  // Uses `error:` overrides.
  unique_ptr<Rule> processMappedIdent(const RuleExprMappedIdent& midxpr);
  unique_ptr<Rule> processConcat(const RuleExprConcat& catxpr);
  unique_ptr<Rule> processRepeat(const RuleExprRepeat& repxpr);
  unique_ptr<Rule> processDquoted(const RuleExprDquoted& dq);
  static unique_ptr<Rule> unflattenableWrapper(ssize_t targetRule,
                                               const SortedIdents& usage);
  unique_ptr<Rule> unflattenableWrapper(ssize_t targetPattRule,
                                        const string& patt);

  // Look up an ident for rule index in the context of a RuleExpr.
  // Performs local name lookups, and falls back to global names if
  // that fails.
  ssize_t lookupIdent(const Ident& id) const;

  // Does not fall back to global names. Raises Bug if name not found.
  ssize_t lookupLocalIdent(const Ident& name) const;

  // The recursion driver for compiling RuleExpr
  unique_ptr<Rule> compileRuleExpr(const RuleExpr& rxpr,
                                   vector<IdentUsage>& exportedOutput);

  unique_ptr<Rule> compileToFlatStruct(const RuleExpr& rxpr,
                                       vector<IdentUsage>& exportedOutput);
};
unique_ptr<Rule>
RuleExprCompiler::createFlatIdent(const Ident& ident, ssize_t ruleIndex) {
  return move_to_unique(ConcatFlatRule{{
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
unique_ptr<Rule>
RuleExprCompiler::process(const RuleExpr& rxpr) {
  if(auto* id = dynamic_cast<const RuleExprIdent*>(&rxpr)) {
    return this->identComponent(id->ident);
  }else if(auto* s = dynamic_cast<const RuleExprSquoted*>(&rxpr)) {
    return createLiteralOrError(*rl_, s->s);
  }else if(auto* dq = dynamic_cast<const RuleExprDquoted*>(&rxpr)) {
    return this->processDquoted(*dq);
  }else if(auto* regex = dynamic_cast<const RuleExprRegex*>(&rxpr)) {
    return createRegexOrError(*rl_, regex->regex->clone(), regexOptsIdx_);
  }else if(auto* mid = dynamic_cast<const RuleExprMappedIdent*>(&rxpr)) {
    return this->processMappedIdent(*mid);
  }else if(auto* cat = dynamic_cast<const RuleExprConcat*>(&rxpr)) {
    return this->processConcat(*cat);
  }else if(auto* opt = dynamic_cast<const RuleExprOptional*>(&rxpr)) {
    return createOptionalRule(*rl_,
        rl_->appendAnonRulePtr(this->process(*opt->part)) );
  }else if(auto* rep = dynamic_cast<const RuleExprRepeat*>(&rxpr)) {
    return this->processRepeat(*rep);
  }else {
    Bug("{} cannot handle RuleExpr of type {}", __func__, typeid(rxpr).name());
  }
}
unique_ptr<Rule>
RuleExprCompiler::identComponent(const Ident& id) {
  return wrapErrorIfCustomized( createFlatIdent(id, lookupIdent(id)),
                                id, *errmsg_, *rl_ );
}
unique_ptr<Rule>
RuleExprCompiler::processMappedIdent(const RuleExprMappedIdent& midxpr) {
  unique_ptr<Rule> result;
  if(auto* rhsid = dynamic_cast<const RuleExprIdent*>(midxpr.rhs.get())) {
    ssize_t targetIndex = lookupIdent(rhsid->ident);
    result = this->createFlatIdent(midxpr.lhs, targetIndex);
  }else if(dynamic_cast<const RuleExprRegex*>(midxpr.rhs.get()) ||
           dynamic_cast<const RuleExprSquoted*>(midxpr.rhs.get())) {
    ssize_t newIndex = rl_->appendAnonRulePtr(this->process(*midxpr.rhs));
    result = this->createFlatIdent(midxpr.lhs, newIndex);
  }else if(auto* dq = dynamic_cast<const RuleExprDquoted*>(midxpr.rhs.get())) {
    ssize_t newIndex = rl_->appendAnonRulePtr(this->processDquoted(*dq));
    newIndex = rl_->appendAnonRulePtr(
        this->unflattenableWrapper(newIndex, dq->gs) );
    result = this->createFlatIdent(midxpr.lhs, newIndex);
  }else
    Bug("Mapped ident cannot have {} on the rhs", typeid(*midxpr.rhs).name());
  return wrapErrorIfCustomized(std::move(result), midxpr.lhs, *errmsg_, *rl_);
}
unique_ptr<Rule>
RuleExprCompiler::processConcat(const RuleExprConcat& catxpr) {
  vector<ConcatFlatRule::Component> comps;
  for(const unique_ptr<const RuleExpr>& c : catxpr.parts) {
    comps.push_back({rl_->appendAnonRulePtr(process(*c)), {}});
  }
  return move_to_unique(ConcatFlatRule{{std::move(comps)}});
}
unique_ptr<Rule>
RuleExprCompiler::processRepeat(const RuleExprRepeat& repxpr) {
  ssize_t i = rl_->appendAnonRulePtr(this->process(*repxpr.part));
  ssize_t j = -1;
  if(repxpr.glue) j = rl_->appendAnonRulePtr(this->process(*repxpr.glue));
  return move_to_unique(LoopRule{{
      .partidx = i, .glueidx = j, .lookidx = -1, .skipidx = -1}});
}
// TODO change this to use string_view.
static const SortedIdents&
getPrecomputedOrDie(const map<string,SortedIdents>& precomp,
                    const string& patt) {
  auto it = precomp.find(patt);
  if(it == precomp.end())
    Bug("processDquoted() hasn't yet been called on '{}'", patt);
  return it->second;
}

// Wraps targetRule in an OutputTmpl{} so that it's not flattenable anymore.
// This is usually the last step for any rule, local or toplovel. This function
// automatically deduces the template to use based on the list of identifiers
// that appear in the rule.
//
// The `exported` param is expected to be already sorted here, so duplicate
// identifiers can be silently removed.
unique_ptr<Rule>
RuleExprCompiler::unflattenableWrapper(ssize_t targetRule,
                                       const SortedIdents& exported) {
  JsonTmpl jstmpl = deduceOutputTmpl(exported);
  return move_to_unique(OutputTmpl{
      targetRule,  // childidx
      {},          // childName, ignored for map-returning childidx
      std::move(jstmpl), // outputTmpl
  });
}

unique_ptr<Rule>
RuleExprCompiler::unflattenableWrapper(ssize_t targetPattRule,
                                       const string& patt) {
  return unflattenableWrapper( targetPattRule,
                               getPrecomputedOrDie(patternIdents_, patt) );
}

unique_ptr<Rule>
RuleExprCompiler::processDquoted(const RuleExprDquoted& dq) {
  optional<Pattern> patt = parsePatternForLocalEnv(ctx_, dq.gs, *lexOpts_,
                                                   *partPatterns_);
  // Dev-note: I'm slightly surprised that this is the only error case in
  // all of RuleExprCompiler::process();
  if(!patt.has_value()) {
    somePatternFailed_ = true;
    return dummyRule();
  }

  // It's okay if the pattern already exists in patternIdents_.
  // See the comment for compileLocalRules() for how to optimize this.
  SortedIdents patternIdents = patternCollectIdent(*patt);
  patternIdents_.insert({dq.gs, patternIdents});
  return pattComp_.process(*patt);
}

static SortedIdents
ruleExprOutputIdentsCheckUnique(
      DiagsDest ctx, const RuleExpr& rxpr,
      const map<string,SortedIdents>& patternIdents);

// This method is the "main" entry-point for recursively compiling RuleExpr.
// This means the output type and flattenability depends on the concrete type
// of rxpr.
//
//   * RuleExprSquoted and RuleExprRegex will produce string outputs, and
//     thus do not satisfy resultFlattenableOrError().
//   * Everything else other than RuleExprDquoted produces a flattenable struct.
//   * RuleExprDquoted output varies likewise, and can be either a flattenable
//     struct or a string depending on whether it has child components.
//
// Sometimes, the caller special-cases RuleExpr{Squoted,Regex,Ident} so that
// they are not handled by this function.
//
// All identifiers in rxpr are appended to exportedOutput. They are used to
// generate an output template if needed. Moreover, they are also used to
// verify that any `outputs:` template provided are actually defined, and has
// matching listyness.
//
// If an error is encountered, e.g. because a double-quoted string had an
// illegal pattern, this function returns nullptr.
//
//   CompiledSingleExpr::identsUsed needs to be sorted by the caller.
unique_ptr<Rule>
RuleExprCompiler::compileRuleExpr(const RuleExpr& rxpr,
                                  vector<IdentUsage>& exportedOutput) {
  // This processing also collects identifiers in rxpr. This must be called
  // before we can produce a vector<IdentUsage>.
  unique_ptr<Rule> rv = this->process(rxpr);
  if(somePatternFailed_) return nullptr;
  vector<IdentUsage> ids
    = ruleExprOutputIdentsCheckUnique(ctx_, rxpr, patternIdents_).release();
  for(auto& id: ids) exportedOutput.push_back(std::move(id));
  return rv;
}

struct RuleExprCollectConfig {
  enum class Type {
    inputsUsed,
    outputsProduced,
  } type;
  const map<string,SortedIdents>* patternIdents;
  bool inList;
};

static void
ruleExprCollectIdents(const RuleExpr& rxpr, RuleExprCollectConfig& conf,
                      vector<IdentUsage>& output) {
  if(auto* id = dynamic_cast<const RuleExprIdent*>(&rxpr))
    output.push_back({id->ident, conf.inList});
  else if(dynamic_cast<const RuleExprSquoted*>(&rxpr) ||
          dynamic_cast<const RuleExprRegex*>(&rxpr)) return;
  else if(auto* dq = dynamic_cast<const RuleExprDquoted*>(&rxpr)) {
    const SortedIdents* patternIdents =
      &getPrecomputedOrDie(*conf.patternIdents, string(dq->gs));
    for(const IdentUsage& iu : patternIdents->get()) output.push_back(iu);
  }
  else if(auto* mid = dynamic_cast<const RuleExprMappedIdent*>(&rxpr)) {
    if(conf.type == RuleExprCollectConfig::Type::inputsUsed)
      ruleExprCollectIdents(*mid->rhs, conf, output);
    else if(conf.type == RuleExprCollectConfig::Type::outputsProduced)
      output.push_back({mid->lhs, conf.inList});
    else Bug("Bad identifier collection config");
    if(!dynamic_cast<const RuleExprRegex*>(mid->rhs.get()) &&
       !dynamic_cast<const RuleExprSquoted*>(mid->rhs.get()) &&
       !dynamic_cast<const RuleExprDquoted*>(mid->rhs.get()) &&
       !dynamic_cast<const RuleExprIdent*>(mid->rhs.get()))
      Bug("Mapped idents must have simple rhs. Found {}",
          typeid(*mid->rhs).name());
  }
  else if(auto* cat = dynamic_cast<const RuleExprConcat*>(&rxpr)) {
    for(const unique_ptr<const RuleExpr>& part : cat->parts)
      ruleExprCollectIdents(*part, conf, output);
  }
  else if(auto* opt = dynamic_cast<const RuleExprOptional*>(&rxpr))
    ruleExprCollectIdents(*opt->part, conf, output);
  else if(auto* rep = dynamic_cast<const RuleExprRepeat*>(&rxpr)) {
    conf.inList = true;
    ruleExprCollectIdents(*rep->part, conf, output);
    if(rep->glue) ruleExprCollectIdents(*rep->glue, conf, output);
    conf.inList = false;
  }
  else
    Bug("{} cannot handle RuleExpr of type {}", __func__, typeid(rxpr).name());
}

static SortedIdents
ruleExprOutputIdentsCheckUnique(
      DiagsDest ctx, const RuleExpr& rxpr,
      const map<string,SortedIdents>& patternIdents) {
  vector<IdentUsage> usage;
  RuleExprCollectConfig conf{ RuleExprCollectConfig::Type::outputsProduced,
                              &patternIdents, /* inList */ false };
  ruleExprCollectIdents(rxpr, conf, usage);
  SortedIdents rv{std::move(usage)};
  checkMultipleUsage(ctx, rv);
  return rv;
}

static void
ruleExprCollectInputIdents(
      const RuleExpr& rxpr, const map<string,SortedIdents>& patternIdents,
      vector<IdentUsage>& output) {
  RuleExprCollectConfig conf{ RuleExprCollectConfig::Type::inputsUsed,
                              &patternIdents, /* inList */ false };
  ruleExprCollectIdents(rxpr, conf, output);
}

// This function is used for special-casing string-producing rxpr values. In
// this case, the rule expression's outputs are _not_ encased in a new
// layer of json maps. Most other RuleExpr currently output through
// OutputTmpl{}.
//
// If this function returns nullptr, it means rxpr doesn't produce a string.
unique_ptr<Rule>
RuleExprCompiler::createIfStringExpr(const RuleExpr& rxpr) {
  // TODO: Add more special-cases:
  //  - If rxpr has no idents anywhere, ie. if ruleExprCollectInputIdents()
  //    produces an empty vector. But that requires a new codegen Rule that
  //    collects and concatenates strings. Or it requires some strange
  //    regex surgery.
  if(auto* regxpr = dynamic_cast<const RuleExprRegex*>(&rxpr))
    return createRegexOrError(*rl_, regxpr->regex->clone(), regexOptsIdx_);
  else if(auto* sq = dynamic_cast<const RuleExprSquoted*>(&rxpr))
    return createLiteralOrError(*rl_, sq->s);
  else return nullptr;
}

// This function is used for special-casing RuleExprIdent values. In
// this case, the rule expression's outputs are _not_ encased in a new
// layer of json maps. Most other RuleExpr currently output through
// OutputTmpl{}.
//
// If this function returns nullptr, it means rxpr isn't a RuleExprIdent.
static const Ident*
markUsedIfIdent(const RuleExpr& rxpr, vector<IdentUsage>& usageOutput) {
  const Ident* id = getIfIdent(rxpr);
  if(id) usageOutput.push_back({.id = *id, .inList = false});
  return id;
}

// This function is used to compile:
//
//   * Local rules (in `where:`)
//   * The main rule of a single-choice rule without a template
//   * A single branch of a multi-choice rule without a template
//
// Per the usual convention, the compiled rules are all appeneded to *this->rl_,
// except for the last rule which is returned. This allows the caller to
// preallocate indices to which the last rule can be assigned, e.g., to break
// compilation cycles in recursive rules.
//
// Semantics:
//
//   RuleExpr{Ident,Squoted,Regex}: pass the match through,
//                                  unchanged and unrestricted.
//   Others: the result is wrapped in unflattenableWrapper().
optional<CompiledSingleExpr>
RuleExprCompiler::compileSingleExpr(const RuleExpr& rxpr) {
  vector<IdentUsage> idsUsed;
  if(unique_ptr<Rule> s = this->createIfStringExpr(rxpr))
    return CompiledSingleExpr{std::move(s), SortedIdents{std::move(idsUsed)}};
  // TODO: errmsg_ customization.
  else if(const Ident* id = markUsedIfIdent(rxpr, idsUsed)) {
    ssize_t idx = this->lookupIdent(*id);
    if(idx == -1) return std::nullopt;
    return CompiledSingleExpr{move_to_unique(AliasRule{idx}),
                              SortedIdents{std::move(idsUsed)}};
  }

  vector<IdentUsage> idsExported;
  if(unique_ptr<Rule> flatRule = this->compileRuleExpr(rxpr, idsExported)) {
    unique_ptr<Rule> outrule = unflattenableWrapper(
        rl_->appendAnonRulePtr(std::move(flatRule)),
        SortedIdents{std::move(idsExported)});
    ruleExprCollectInputIdents(rxpr, patternIdents_, idsUsed);
    return CompiledSingleExpr{std::move(outrule),
                              SortedIdents{std::move(idsUsed)}};
  }

  return std::nullopt;
}

// This function compiles a RuleExpr, such that the output satisfies
// resultFlattenableOrError(). It is used in two different situations:
//
//   * To compile the main body of a single-choice rule.
//   * To compile a branch of a multi-choice rule.
//
// In both cases, it is used only if an `outputs:` stanza is explicitly
// provided, where such a flattenable struct output is important.
//
// The exportedOutput is an output parameter that aggregates all the fields
// we see across the branches.
//
// Semantics:
//
//   RuleExprIdent: produces a ConcatFlatRule with a single field.
//   RuleExpr{Squoted,Regex}: produces an empty map.
//   Others: passes through the map from compileRuleExpr().
unique_ptr<Rule>
RuleExprCompiler::compileToFlatStruct(
    const RuleExpr& rxpr, vector<IdentUsage>& exportedOutput) {

  if(unique_ptr<Rule> s = this->createIfStringExpr(rxpr)) {
    // There are no fields to expose, but we still need an empty struct.
    ssize_t ridx = rl_->appendAnonRulePtr(std::move(s));
    return move_to_unique(ConcatFlatRule {{{ .idx = ridx,
                                             .outputPlaceholder{} }}} );
  }else if(const Ident* id = markUsedIfIdent(rxpr, exportedOutput))
    return this->identComponent(*id);
  else return this->compileRuleExpr(rxpr, exportedOutput);
}

// This function is used only to compile the main rule expression of a
// single-choice rule. It accepts a jstmpl, unlike compileSingleExpr().
optional<CompiledSingleExpr>
RuleExprCompiler::compileSingleExprWithTmpl(const RuleExpr& rxpr,
                                            JsonTmpl jstmpl) {
  unique_ptr<Rule> flatRule;
  vector<IdentUsage> unsorted;
  flatRule = this->compileToFlatStruct(rxpr, unsorted);
  if(!flatRule) return std::nullopt;

  SortedIdents idsExported{std::move(unsorted)};
  if(!checkUndefinedOutfields(ctx_, idsExported, jstmpl)) return std::nullopt;

  vector<Ident> listNames = desugarEllipsisPlaceholders(ctx_, jstmpl);
  checkPlaceholderTypes(ctx_, listNames, idsExported.get());
  vector<IdentUsage> idsUsed;
  ruleExprCollectInputIdents(rxpr, patternIdents_, idsUsed);
  return CompiledSingleExpr{
    move_to_unique(OutputTmpl{
      rl_->appendAnonRulePtr(std::move(flatRule)),  // childidx
      {},        // childName, ignored for map-returning childidx
      std::move(jstmpl), // outputTmpl
    }),
    SortedIdents{std::move(idsUsed)},
  };
}

ssize_t
RuleExprCompiler::lookupLocalIdent(const Ident& name) const {
  size_t i = std::lower_bound(symtab_->begin(), symtab_->end(),
                              pair{name, ssize_t{0}})
             - symtab_->begin();
  if(i >= symtab_->size()) Bug("mapToRule() missed allocating for {}",
                               name.preserveCase());
  return symtab_->at(i).second;
}

// TODO: Reuse the result of identical patterns. I.e. For a local decl like
// `a, b ~ ( some complex expr )`, don't instantiate that left-hand
// expression multiple times.
void
RuleExprCompiler::compileLocalRules(const vector<LocalBinding>& locals) {
  for(auto& local : locals) {
    ssize_t j = this->lookupLocalIdent(local.localName);
    if(dynamic_cast<const DefinitionInProgress*>(&(*rl_)[j])) {
      if(optional<CompiledSingleExpr> res
          = this->compileSingleExpr(*local.ruleExpr))
        rl_->deferred_assign_ptr(j, std::move(res->rule));
      else rl_->deferred_assign_ptr(j, dummyRule());
    }
  }
}

void
collectMappedIdents(const RuleExpr& rxpr, vector<Ident>& output) {
  if(const auto* mid = dynamic_cast<const RuleExprMappedIdent*>(&rxpr)) {
    output.push_back(mid->lhs);
  }else if(const auto* cat = dynamic_cast<const RuleExprConcat*>(&rxpr)) {
    for(const auto& p : cat->parts) collectMappedIdents(*p, output);
  }else if(const auto* opt = dynamic_cast<const RuleExprOptional*>(&rxpr)) {
    collectMappedIdents(*opt->part, output);
  }
}

vector<Ident>
mappedIdents(const RuleExpr& rxpr) {
  vector<Ident> rv;
  collectMappedIdents(rxpr, rv);
  return rv;
}

// Dev-note: Right now, this supports ruleName being empty. But empty ruleName
// should only be used for tests.
void
appendExprRule(DiagsDest ctx, const Ident& ruleName, const RuleExpr& rxpr,
               const RuleStanzas& stz, RulesWithLocs& rl) {
  // TODO: A named part in the main rule is available for output template, and
  // is not defined as a global. But we should not allow them in any dquoted
  // patterns. I should disallow the name from being defined as a local or
  // global. reserveLocalName() only does the latter.
  //
  // TODO: named keywords like this need better names for their types in
  // generated code.
  SymbolTable symtab
    = mapToRule(ctx, rl, stz.local_decls, mappedIdents(rxpr),
                stz.jstmpl.allPlaceholders());
  map<Ident,PartPattern> partPatterns
    = makePartPatterns(ctx, stz.jstmpl, stz.local_decls);

  // In case of failure, keep going with default error messages.
  vector<pair<Ident,string>> errmsg
    = destructureErrors(ctx, stz.errors);
  if(!requireValidIdents(ctx, errmsg, symtab)) errmsg.clear();

  RuleExprCompiler comp{rl, ctx, stz.lexopts, symtab, partPatterns, errmsg};
  comp.compileLocalRules(stz.local_decls);
  ssize_t skipIndex = rl.addSkipper(stz.lexopts.skip);
  ssize_t newIndex = ruleName
    ? rl.defineIdent(ctx, ruleName, skipIndex)
    : rl.appendAnonRule(DefinitionInProgress{});
  if(newIndex == -1) return;

  SortedIdents exprIdents;
  optional<CompiledSingleExpr> res = stz.jstmpl.holdsEllipsis()
    ? comp.compileSingleExpr(rxpr)
    : comp.compileSingleExprWithTmpl(rxpr, stz.jstmpl);

  if(res) {
    rl.deferred_assign_ptr(newIndex, std::move(res->rule));
    exprIdents = std::move(res->identsUsed);
  }else {
    rl.deferred_assign_ptr(newIndex, dummyRule());
    return;
  }

  // This error is not fatal. Unused fields stay unused.
  checkUnusedParts(ctx, exprIdents, comp.patternIdents(), stz.local_decls);
}

// Dev-note: maybe move to pattern.h
bool
isUserWord(const LexDirective& lexopts, string_view s) {
  for(char ch : s) if(!matchesRegexCharSet(ch, lexopts.wordChars))
    return false;
  return true;
}

// TODO: Replace this with RuleExprCompiler.
// It should also look up local rules.
static ssize_t
appendLookahead(DiagsDest ctx, const RuleExpr& lookahead,
                const LexDirective& lexopts, RulesWithLocs& rl) {
  if(auto* id = dynamic_cast<const RuleExprIdent*>(&lookahead))
    return rl.findOrAppendIdent(ctx, id->ident);
  else if(auto* dq = dynamic_cast<const RuleExprDquoted*>(&lookahead)) {
    if(!isUserWord(lexopts, dq->gs)) {
      Error(ctx, dq->gs, "Non-word inline lookahead");
      return -1;
    }else {
      ssize_t roi = rl.addRegexOpts(RegexOptions{lexopts.wordChars});
      return rl.appendAnonRule(WordPreserving{dq->gs, roi});
    }
  }else Unimplemented("RuleExpr {} cannot yet be used as a lookahead",
                      typeid(lookahead).name());
}

optional<CompiledRuleBranch>
RuleExprCompiler::compileRuleBranch(const RuleBranch& branch,
                                    bool exposeFields) {
  unique_ptr<Rule> target;
  SortedIdents identsUsed;
  vector<IdentUsage> exportedIdents;
  if(branch.target != nullptr) {
    if(branch.diagMsg.empty() && branch.diagType == RuleBranch::DiagType::none){
      if(exposeFields) {
        target = this->compileToFlatStruct(*branch.target, exportedIdents);
        vector<IdentUsage> unsorted;
        ruleExprCollectInputIdents(*branch.target, patternIdents_, unsorted);
        identsUsed = SortedIdents{std::move(unsorted)};
      }
      else if(optional<CompiledSingleExpr> res
          = this->compileSingleExpr(*branch.target)) {
        target = std::move(res->rule);
        identsUsed = std::move(res->identsUsed);
      }
    }
    else Unimplemented("Good actions with warnings");
  }else if(branch.diagType != RuleBranch::DiagType::error) {
    // Consider promoting this to an Error() later.
    Unimplemented("Actions without a target should produce an error");
  }else target = move_to_unique(ErrorRule{string{branch.diagMsg}});

  if(!target) return std::nullopt;
  ssize_t targetidx = rl_->appendAnonRulePtr(std::move(target));

  // If lookahead-compilation fails, we still continue without it.
  ssize_t lookidx = branch.lookahead
    ? appendLookahead(ctx_, *branch.lookahead, rl_->defaultLexopts(), *rl_)
    : -1;
  return CompiledRuleBranch{
    .or_comp { .lookidx = lookidx, .parseidx = targetidx,
               .tmpl{passthroughTmpl} },
    .identsUsed = std::move(identsUsed),
    .exportedIdents = SortedIdents{std::move(exportedIdents)},
  };
}

void
appendMultiExprRule(DiagsDest ctx, const Ident& ruleName,
                    vector<RuleBranch> branches, const RuleStanzas& stz,
                    RulesWithLocs& rl) {
  if(stz.sawErrorsKw) Unimplemented("errors for multi-match rules");

  // TODO: Dedup with appendExprRule()
  vector<Ident> mappedOutputs;
  for(const auto& branch : branches) if(branch.target)
    collectMappedIdents(*branch.target, mappedOutputs);

  SymbolTable symtab = mapToRule(ctx, rl, stz.local_decls, mappedOutputs,
                                 stz.jstmpl.allPlaceholders());
  map<Ident,PartPattern> partPatterns
    = makePartPatterns(ctx, stz.jstmpl, stz.local_decls);
  vector<pair<Ident,string>> errmsg
    = destructureErrors(ctx, stz.errors);
  RuleExprCompiler comp{rl, ctx, stz.lexopts, symtab, partPatterns, errmsg};
  comp.compileLocalRules(stz.local_decls);

  bool exposeFields = !stz.jstmpl.holdsEllipsis();
  OrRule orRule{{}, /* flattenOnDemand */ exposeFields};
  vector<IdentUsage> exportedIds, identsUsed;
  for(const auto& branch : branches)
    if(auto opt = comp.compileRuleBranch(branch, exposeFields)) {
      orRule.comps.push_back(std::move(opt->or_comp));
      for(auto& id : opt->identsUsed.release())
        identsUsed.push_back(std::move(id));
      for(auto& id : opt->exportedIdents.release())
        exportedIds.push_back(std::move(id));
    }
  if(orRule.comps.empty()) return;  // Too many errors.
  if(!checkUndefinedOutfields(ctx, SortedIdents{exportedIds}, stz.jstmpl))
    return;

  ssize_t skipIndex = rl.addSkipper(stz.lexopts.skip);
  ssize_t newIndex = rl.defineIdent(ctx, ruleName, skipIndex);
  if(newIndex == -1) return;

  if(stz.jstmpl.holdsEllipsis())
    rl.deferred_assign(newIndex, std::move(orRule));
  else {
    JsonTmpl outputTmpl = stz.jstmpl;
    vector<Ident> listNames = desugarEllipsisPlaceholders(ctx, outputTmpl);
    checkPlaceholderTypes(ctx, listNames, exportedIds);
    ssize_t orIndex = rl.appendAnonRule(std::move(orRule));
    rl.deferred_assign(newIndex, OutputTmpl{
        orIndex,   // childidx
        {},        // childName, ignored for map-returning childidx
        outputTmpl,
    });
  }
  // This error is not fatal. Unused fields stay unused.
  // We just need identsUsed.
  checkUnusedParts(ctx, SortedIdents{std::move(identsUsed)},
                   comp.patternIdents(), stz.local_decls);
}

}  // namespace oalex
