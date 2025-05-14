#include "analysis.h"

#include <format>
#include <string>
#include <vector>

#include "codegen.h"  // TODO: move RuleSet to a separate header.
#include "ident.h"
#include "jsontmpl.h"
#include "runtime/util.h"
using oalex::Bug;
using oalex::ConcatFlatRule;
using oalex::ErrorRule;
using oalex::ExternParser;
using oalex::flatWrapperTarget;
using oalex::Ident;
using oalex::JsonTmpl;
using oalex::LoopRule;
using oalex::OrRule;
using oalex::OutputTmpl;
using oalex::RegexRule;
using oalex::resolveIfWrapper;
using oalex::resultFlattenableOrError;
using oalex::Rule;
using oalex::RuleField;
using oalex::RuleSet;
using oalex::SkipPoint;
using oalex::StringRule;
using oalex::WordPreserving;
using std::format;
using std::string;
using std::vector;

// --------------------------- populateFlatFields ----------------------------

namespace {
namespace popnflat {
// A separate namespace helps me keep track of dependencies. Otherwise it gets
// confusing for me to figure out which functions I should pay attention to at
// any given time.

// Used only for Bug(). Not formatted for users.
static string
listCycle(const RuleSet& ruleset, const vector<ssize_t>& edges_remaining) {
  string rv;
  for(ssize_t i=0; i<ssize(edges_remaining); ++i) {
    if(edges_remaining[i] == 0) continue;
    if(!rv.empty()) rv += ", ";
    if(const Ident* name = ruleset.rules.at(i)->nameOrNull())
      rv += name->preserveCase();
    else rv += format("rules[{}]", i);
  }
  return rv;
}

// See if it's of the type { key: "child" } with a single key. Return that key.
static string
orBranchSimpleTmpl(const RuleSet& rs, ssize_t partidx, const JsonTmpl& tmpl) {
  if(resultFlattenableOrError(rs, partidx)) return "";
  const JsonTmpl::Map* m = tmpl.getIfMap();
  if(!m) return "";
  if(m->size() == 0) return "";
  if(m->size() == 1) return m->at(0).first;
  return "";
}

// Semantics: This function abuses the RuleField type to represent a
// single-layer of relationship, before we've managed to resolve a component to
// its ultimate source. So it only lists direct components, like
// ConcatFlatRule. The semantics are also analogous to ConcatFlatRule.
//
//   .field_name is empty for flattened components or flatWrapperTarget(),
//     but must be provided for other components. Non-flattenable components
//     with no names will be discarded.
//   .schema_source is abused to represent a direct component.
//   .container represents information we gleaned at this level. I.e.
//     LoopRule will introduce vector, OrRule introduce optional.
static vector<RuleField>
flatDirectComps(const RuleSet& rs, ssize_t ruleidx) {
  const Rule& r = *rs.rules.at(ruleidx);
  if(dynamic_cast<const StringRule*>(&r) ||
     dynamic_cast<const WordPreserving*>(&r) ||
     dynamic_cast<const ExternParser*>(&r) ||
     dynamic_cast<const SkipPoint*>(&r) ||
     dynamic_cast<const RegexRule*>(&r) ||
     dynamic_cast<const OutputTmpl*>(&r) ||
     dynamic_cast<const ErrorRule*>(&r)) return {};
  else if(auto* seq = dynamic_cast<const ConcatFlatRule*>(&r)) {
    vector<RuleField> rv;
    // Note: comp.outputPlaceholder may be empty. That's okay.
    for(auto& comp : seq->comps)
      rv.push_back({.field_name = comp.outputPlaceholder,
                    .schema_source = comp.idx,
                    .container = RuleField::single});
    return rv;
  }
  else if(auto* loop = dynamic_cast<const LoopRule*>(&r)) {
    vector<RuleField> rv;
    bool initInLoop = false;
    for(ssize_t c: loop->loopbody) {
      rv.push_back({"", c, RuleField::vector});
      if(c == loop->initidx) initInLoop = true;
    }
    if(!initInLoop)
      Bug("LoopRule::initidx should appear again as part of loopbody");
    return rv;
  }
  else if(ssize_t target = flatWrapperTarget(r); target != -1) {
    return {{
      .field_name{}, .schema_source = target, .container = RuleField::single
    }};
  }
  else if(auto* ors = dynamic_cast<const OrRule*>(&r)) {
    if(!ors->flattenOnDemand) return {};
    vector<RuleField> rv;
    for(auto& comp : ors->comps)
      rv.push_back({.field_name{
                       orBranchSimpleTmpl(rs, comp.parseidx, comp.tmpl)
                    },
                    .schema_source = comp.parseidx,
                    .container = RuleField::optional});
    return rv;
  }
  Bug("Unknown rule type {} in flatDirectComps", r.specifics_typename());
}

static RuleField::Container
flexContainer(RuleField::Container a, RuleField::Container b) {
  if(a == RuleField::vector || b == RuleField::vector) return RuleField::vector;
  if(a == RuleField::optional || b == RuleField::optional)
    return RuleField::optional;
  return RuleField::single;
}

// This function is only used in the context of the toposort in
// populateFlatFields(). We assume flatFields[i] is already populated
// for all i in flatDirectComps(ruleset,ruleIndex)[*].schema_source.
// The rest are ignored.
//
// Remember that the semantics of "directs" is a bit different, and is described
// in the comment for flatDirectComps().
static vector<RuleField>
gatherFlatComps(const RuleSet& ruleset, ssize_t ruleIndex,
                const vector<vector<RuleField>>& flatFields) {

  const vector<RuleField> directs = flatDirectComps(ruleset, ruleIndex);
  vector<RuleField> rv;
  for(RuleField field: directs) {  // copied element
    field.schema_source = resolveIfWrapper(ruleset, field.schema_source);
    if(!resultFlattenableOrError(ruleset, field.schema_source)) {
      if(field.field_name.empty()) continue;  // discard unnamed field
      rv.push_back(std::move(field));
    }else {
      if(!field.field_name.empty())
        Bug("Compiler should not provide a field name for flattenable fields. "
            "Got `{}`", field.field_name);
      for(const RuleField& child: flatFields[field.schema_source]) {
        rv.push_back(child);
        if(field.container == RuleField::vector &&
           child.container == RuleField::optional)
          Bug("The frontend shouldn't allow optional field {} inside "
              "LoopRule. Makes it impossible to line up indices of a "
              "result.", child.field_name);
        rv.back().container = flexContainer(field.container, child.container);
      }
    }
  }
  for(auto& f: rv)
    if(f.schema_source < 0 || f.schema_source >= ssize(ruleset.rules)) {
      Bug("Rule {} pointing to source out of bound at {}",
          ruleIndex, f.schema_source);
    }
  // TODO: check for duplicate names.
  return rv;
}

[[maybe_unused]] static string
debugFlatDirectComps(const RuleSet& rs, ssize_t ruleidx) {
  string rv;
  vector comps = flatDirectComps(rs, ruleidx);
  for(ssize_t i=0; i<ssize(comps); ++i) {
    if(i != 0) rv += ", ";
    rv += std::to_string(comps[i].schema_source);
  }
  return rv;
}

}  // namespace popnflat
}  // namespace

namespace oalex {

void
populateFlatFields(RuleSet& ruleset) {
  using namespace popnflat;
  ssize_t n = ssize(ruleset.rules);
  vector<vector<ssize_t>> revedge(n);
  vector<ssize_t> pending;
  vector<ssize_t> edges_remaining(n);
  for(ssize_t i=0; i<n; ++i) {
    vector<RuleField> fwd = flatDirectComps(ruleset, i);
    edges_remaining[i] = ssize(fwd);
    if(edges_remaining[i] == 0) pending.push_back(i);
    for(auto& field: fwd) revedge[field.schema_source].push_back(i);
  }

  vector<vector<RuleField>> flatFields(n);
  while(!pending.empty()) {
    ssize_t ruleIndex = pending.back();
    pending.pop_back();

    for(ssize_t parentIndex : revedge[ruleIndex])
      if(--edges_remaining[parentIndex] == 0) pending.push_back(parentIndex);

    flatFields[ruleIndex] = gatherFlatComps(ruleset, ruleIndex, flatFields);
  }

  for(ssize_t i=0; i<n; ++i) if(edges_remaining[i] != 0)
    Bug("The compiler produced a cycle in flattenable rules. {}",
        listCycle(ruleset, edges_remaining));

  for(ssize_t i=0; i<n; ++i)
    ruleset.rules[i]->flatFields(std::move(flatFields[i]));
}

}  // namespace oalex


