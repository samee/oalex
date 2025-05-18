#include "analysis.h"

#include <format>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "codegen.h"  // TODO remove after helpers have been moved out.
#include "ident.h"
#include "jsontmpl.h"
#include "ruleset.h"
#include "runtime/util.h"
using oalex::Bug;
using oalex::ConcatFlatRule;
using oalex::ErrorRule;
using oalex::ExternParser;
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
using oalex::WrapperRule;
using oalex::WordPreserving;
using std::format;
using std::optional;
using std::string;
using std::unique_ptr;
using std::vector;

namespace oalex {

// ------------------- computeUserExposureForTypes ----------------------------

// Assumes:
// * all topLevel rules have names.
// * resolveWrapperTypes is already complete, so we can use outType().
void
computeUserExposureForTypes(RuleSet& ruleset) {
  for(ssize_t ri = 0; ri < ssize(ruleset.rules); ++ri) {
    Rule* r = ruleset.rules.at(ri).get();
    OutputType ot = r->outType(ruleset).type();
    if(ot == OutputType::string || ot == OutputType::jsonLike)
      r->exposure().state(UserExposure::builtin);
    // If the rule has a global name assigned by the user,
    // it's topLevel by definition.
    else if(r->nameOrNull()) r->exposure().state(UserExposure::topLevel);
    else if(dynamic_cast<WrapperRule*>(r))
      r->exposure().state(UserExposure::notGenerated);
  }
  for(ssize_t ri = 0; ri < ssize(ruleset.rules); ++ri) {
    const Rule& r = *ruleset.rules.at(ri);
    if(r.exposure().state() != UserExposure::topLevel) continue;
    for(const RuleField& field: r.flatFields()) {
      ssize_t fi = field.schema_source;
      UserExposure& fe = ruleset.rules[fi]->exposure();
      if(fe.state() == UserExposure::topLevel
         || fe.state() == UserExposure::builtin) continue;
      else if(optional<ssize_t> oldopt = fe.nestedIn(); oldopt) {
        Bug("Rule {} is already nested in {}, it cannot also be"
            " a child of {}", fi,
            ruleset.rules.at(*oldopt)->nameOrNull()->preserveCase(),
            r.nameOrNull()->preserveCase() );
      }
      else if(fe.state() == UserExposure::unknown) {
        if(!fe.nestedIn(ri)) {
          Bug("Couldn't set field exposure a field of {}",
              r.nameOrNull()->preserveCase() );
        }
      }else Bug("Unexpected type exposure on rule {}: {}",
                fi, ssize_t{fe.state()});
    }
  }

  for(const unique_ptr<Rule>& r : ruleset.rules)
    if(r->exposure().state() == UserExposure::unknown)
      r->exposure().state(UserExposure::notExposed);
}

// --------------------- dependencyOrderForCodegen ----------------------------

// Dev notes:
// * Abusing RuleSlot as a convenient return struct.
// * The if-else structure is borrowed from genTypeDefinition() below.
//   We should refactor out this pattern if it becomes common.
static vector<RuleSlot>
dependencies(const RuleSet& rs, ssize_t idx) {
  const Rule& r = *rs.rules.at(idx);
  if(auto* out = dynamic_cast<const OutputTmpl*>(&r)) {
    if(resultFlattenableOrError(rs, out->childidx)) {
      // Technically, we don't need to depend on fields we don't use in our
      // template. But this is a simpler overapproximation for now.
      return dependencies(rs, out->childidx);
    } else if(!out->childName.empty()) {
      return {RuleSlot{.ruleidx = out->childidx,
                       .slotType = RuleSlot::Type::definition
             }};
    }
  }else if(resultFlattenableOrError(rs, idx)) {
    vector<RuleSlot> rv;
    for(const RuleField& f: r.flatFields()) {
      RuleSlot::Type t = f.container == RuleField::single
        ? RuleSlot::Type::definition
        : RuleSlot::Type::forwardDecl;
      rv.push_back(RuleSlot{.ruleidx = f.schema_source,
                            .slotType = t});
    }
    return rv;
  }
  else if(auto* wr = dynamic_cast<const WrapperRule*>(&r);
          wr && returnsGeneratedStruct(rs, idx)) {
      return {RuleSlot{.ruleidx = wr->target(),
                       .slotType = RuleSlot::Type::definition
             }};
  }

  return {};  // Strings and errors.
}

// If the field is wrapped in indopt or vector, it's a soft dependency, i.e.
// we only need a forwardDecl. Everything else is a hard ordering dependency.
// Cycles in hard dependencies are bugs in compilation.
vector<RuleSlot>
dependencyOrderForCodegen(const RuleSet& rs) {
  ssize_t n = ssize(rs.rules);
  vector<ssize_t> inorder(n, 0);

  // Count dependencies.
  for(ssize_t i=0; i<n; ++i) {
    vector<RuleSlot> deps = dependencies(rs, i);
    for(const RuleSlot& d: deps) if(d.slotType == RuleSlot::Type::definition)
      inorder[d.ruleidx]++;
  }

  // Because of the direction of our dependency edge where we already store
  // for other purposes, we generate the reverse order, and fix it just before
  // returning. So definitions always need to come _after_ use while we sort.
  vector<RuleSlot> rv;
  vector<ssize_t> stk;
  vector<bool> defined(n, false), needsForw(n, false);
  for(ssize_t i=0; i<n; ++i) if(inorder[i] == 0) stk.push_back(i);
  while(!stk.empty()) {
    ssize_t ri = stk.back();
    stk.pop_back();
    rv.push_back(RuleSlot{.ruleidx = ri,
                          .slotType = RuleSlot::Type::definition});
    defined[ri]=true;
    for(const RuleSlot& d: dependencies(rs, ri)) {
      ssize_t di = d.ruleidx;
      if(d.slotType == RuleSlot::Type::definition) {
        ssize_t inc = --inorder[di];
        if(inc == 0) stk.push_back(di);
      }else if(d.slotType == RuleSlot::Type::forwardDecl) {
        if(defined[di] && returnsGeneratedStruct(rs, di) && di != ri)
          needsForw[di] = true;
      }else Bug("Unknown slot type {}", int(d.slotType));
    }
  }
  for(ssize_t i=0; i<n; ++i) if(needsForw[i])
    rv.push_back(RuleSlot{.ruleidx = i,
                          .slotType = RuleSlot::Type::forwardDecl});
  std::reverse(rv.begin(), rv.end());
  return rv;
}

}  // namespace oalex

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
//   .field_name is empty for flattened components or WrapperRules,
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
  else if(auto* wr = dynamic_cast<const WrapperRule*>(&r)) {
    return {{
      .field_name{}, .schema_source = wr->target(),
      .container = RuleField::single
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

void
resolveWrapperTypes(RuleSet& ruleset) {
  ssize_t n = ssize(ruleset.rules);
  ssize_t wrapperRemaining = 0;
  vector<vector<ssize_t>> revedge(n);
  vector<ssize_t> pending;
  for(ssize_t i=0; i<n; ++i) {
    if(auto* wr = dynamic_cast<const WrapperRule*>(ruleset.rules.at(i).get())) {
      revedge[wr->target()].push_back(i);
      ++wrapperRemaining;
    } else pending.push_back(i);
  }
  while(!pending.empty()) {
    ssize_t i = pending.back();
    pending.pop_back();
    ssize_t ts = i;
    if(auto* w = dynamic_cast<const WrapperRule*>(ruleset.rules.at(i).get()))
      ts = w->typeSource();
    for(ssize_t j : revedge[i]) {
      auto* w = dynamic_cast<WrapperRule*>(ruleset.rules.at(j).get());
      if(!w) Bug("revedge member should be WrapperRule");
      w->typeSource(ts);
      --wrapperRemaining;
      pending.push_back(j);
    }
  }
  if(wrapperRemaining != 0) Bug("We have cycles among wrappers");
}

}  // namespace oalex


