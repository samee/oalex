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

#include "codegen.h"
#include <map>
#include <memory>
#include <string_view>
#include <utility>
#include <vector>
#include "runtime/jsonloc_fmt.h"
#include "runtime/oalex.h"
#include "runtime/util.h"
#include "fmt/core.h"
using fmt::format;
using oalex::Regex;
using oalex::RegexOptions;
using std::exchange;
using std::map;
using std::pair;
using std::string;
using std::string_view;
using std::unique_ptr;
using std::vector;

namespace oalex {

static bool validExtName(string_view name) {
  return name.find("oalexPlugin") == 0 || name.find("oalexBuiltin") == 0;
}
static ssize_t
expectedParamCount(string_view builtinSuff) {
  const vector<pair<string, ssize_t>> builtin_param_counts {
    {"Hello", 0}, {"IndentedList", 2}
  };
  for(auto& [name,count] : builtin_param_counts) if(name == builtinSuff) {
    return count;
  }
  return -1;
}

bool ExternParser::requireValidNameAndParamCount(
    const StringLoc& extName, ssize_t providedParamCount, DiagsDest ctx) {
  if(extName->find("oalexPlugin") == 0) return true;
  else if(extName->find("oalexBuiltin") != 0) {
    Error(ctx, extName.stPos(), extName.enPos(),
          "External parser names need to start either with 'oalexPlugin...'"
          " or 'oalexBuiltin...'");
    return false;
  }
  string_view suff = *extName;
  suff.remove_prefix(sizeof("oalexBuiltin")-1);
  ssize_t expected = expectedParamCount(suff);
  if(expected == providedParamCount) return true;
  else if(expected == -1) {
    Error(ctx, extName.stPos(), extName.enPos(),
          format("{} is not a known builtin parser", *extName));
  }else {
    Error(ctx, extName.stPos(), extName.enPos(), format(
            "oalexBuiltin{}() expects {} parameters, but {} was provided",
            suff, expected, providedParamCount));
  }
  return false;
}

ExternParser::ExternParser(string_view extName, vector<ssize_t> params)
  : externalName_{extName}, params_{std::move(params)} {
  if(!validExtName(extName))
    Bug("External names need to start either with oalexPlugin or oalexBuiltin");
}

const string&
ExternParser::externalName() const {
  if(!externalName_.empty()) return externalName_;
  else Bug("External parsers must have an external name");
}

// eval()
// ------

static JsonLoc::ErrorValue
skipError(InputDiags& ctx, ssize_t preskip_i, const Skipper& skip) {
  ssize_t com = skip.whitespace(ctx.input(), preskip_i);
  if(!ctx.input().sizeGt(com)) Bug("skipper returned npos without a comment");
  Error(ctx, com, "Unfinished comment");
  return {};
}

static JsonLoc
skip(InputDiags& ctx, ssize_t& i, const SkipPoint& sp, const RuleSet& rs) {
  const InputPiece& input = ctx.input();
  const ssize_t oldi = i;
  const Skipper* skip = &rs.skips[sp.skipperIndex];
  i = skip->next(input, i);
  if(i == ssize_t(string::npos)) return skipError(ctx, oldi, *skip);
  else return JsonLoc::Map();  // Just something non-error and flattenable.
}

// TODO use std::source_locatio when moving to C++20.
static const Rule&
ruleAtImpl(const RuleSet& rs, ssize_t ruleidx, const char* context) {
  const unique_ptr<Rule>& r = rs.rules.at(ruleidx);
  if(r) return *r;
  else Bug("{}: Dereferencing null rule {}", context, ruleidx);
}
#define ruleAt(rs, idx) ruleAtImpl(rs, idx, __func__)

static ssize_t
flatWrapperTarget(const Rule& rule) {
  if(auto* mor = dynamic_cast<const MatchOrError*>(&rule)) return mor->compidx;
  if(auto* qm = dynamic_cast<const QuietMatch*>(&rule)) return qm->compidx;
  if(auto* alias = dynamic_cast<const AliasRule*>(&rule))
    return alias->targetidx;
  return -1;
}

static bool
resultFlattenableOrError(const RuleSet& rs, ssize_t ruleidx);

static bool
resultFlattenableOrError(const RuleSet& rs, const Rule& rule) {
  if(auto* orRule = dynamic_cast<const OrRule*>(&rule))
    return orRule->flattenOnDemand;
  if(ssize_t target = flatWrapperTarget(rule); target != -1)
    return resultFlattenableOrError(rs, target);
  else {
    auto& t = typeid(rule);
    // ConcatFlatRule: pass all fields through.
    // LoopRule: wrap all fields in vector.
    // ErrorRule: has no output field, but can be used
    //   in a flattenable context.
    return t == typeid(ConcatFlatRule) || t == typeid(LoopRule)
        || t == typeid(ErrorRule);
  }
}

static bool
resultFlattenableOrError(const RuleSet& rs, ssize_t ruleidx) {
  const Rule& rule = ruleAt(rs, ruleidx);
  return resultFlattenableOrError(rs, rule);
}

static ssize_t
resolveIfWrapper(const RuleSet& ruleset, ssize_t target) {
  ssize_t nxt = flatWrapperTarget(ruleAt(ruleset, target));
  if(nxt == -1) return target;
  else return resolveIfWrapper(ruleset, nxt);
}

static string
ruleDebugId(const RuleSet& rs, ssize_t i) {
  if(const Ident* opt = ruleAt(rs, i).nameOrNull())
    return format("rule '{}'", opt->preserveCase());
  else return format("rule {}", i);
}

static JsonLoc evalQuiet(const InputPiece& input, ssize_t& i,
                         const RuleSet& rs, ssize_t ruleIndex);

static JsonLoc
eval(InputDiags& ctx, ssize_t& i,
     const ConcatFlatRule& seq, const RuleSet& rs) {
  JsonLoc::Map rv;
  ssize_t j = i;
  for(auto& [idx, outname] : seq.comps) {
    JsonLoc out = eval(ctx, j, rs, idx);
    if(out.holdsErrorValue()) return out;
    else if(resultFlattenableOrError(rs,idx)) {
      auto* m = out.getIfMap();
      if(!m) Bug("Child {} was expected to return a map, got: {}",
                 ruleDebugId(rs, idx), out);
      if(!outname.empty())
        Bug("Flattenable children are not supposed to have names. Got {}",
            outname);
      mapAppend(rv, std::move(*m));
    }
    else if(!outname.empty())
      rv.push_back({std::move(outname), std::move(out)});
  }
  return JsonLoc::withPos(std::move(rv), std::exchange(i, j), j);
}

static JsonLoc
eval(InputDiags& ctx, ssize_t& i, const OutputTmpl& out, const RuleSet& rs) {
  JsonLoc outfields = eval(ctx, i, rs, out.childidx);
  if(outfields.holdsErrorValue()) return outfields;
  if(!out.outputTmpl.substitutionsNeeded())
    return out.outputTmpl.outputIfFilled();
  JsonLoc::Map container;
  auto* m = outfields.getIfMap();
  if(!m) {
    if(out.childName.empty())
      Bug("OutputTmpl child must be named, or they need to produce a Map");
    container.push_back({out.childName, std::move(outfields)});
    m = &container;
  }

  return JsonLoc::withPos(out.outputTmpl.substituteOrDrop(*m),
                          outfields.stPos, outfields.enPos);
}

static JsonLoc
eval(InputDiags& ctx, ssize_t& i, const LoopRule& loop, const RuleSet& rs) {
  if(loop.lookidx != -1) Unimplemented("LoopRule lookahead");

  const SkipPoint* sp = nullptr;
  if(loop.skipidx != -1) {
    sp = dynamic_cast<const SkipPoint*>(&ruleAt(rs, loop.skipidx));
    if(!sp) Bug("LoopRule skipidx {} is not a SkipPoint rule",
                ruleDebugId(rs, loop.skipidx));
  }

  JsonLoc::Map rv;
  ssize_t maxsize = 0;
  auto addChild = [&rv, &maxsize](const string& name, JsonLoc val) {
    if(name.empty()) return;
    ssize_t i = JsonLoc::mapScanForIndex(rv, name);
    if(i == -1) {
      if(maxsize > 1) Unimplemented("Late addition needs static key collation");
      i = ssize(rv);
      rv.push_back({name, JsonLoc::Vector{}});
    }
    auto* v = rv[i].second.getIfVector();
    if(!v) Bug("All elements should be vectors");
    v->push_back(std::move(val));
    maxsize = std::max(maxsize, ssize(*v));
  };
  auto recordComponent =
    [&addChild, &rs](string_view desc, JsonLoc comp, ssize_t idx) {
    if(resultFlattenableOrError(rs, idx)) {
      // TODO refactor out this validation between here and ConcatFlatRule.
      auto* m = comp.getIfMap();
      if(!m) Bug("LoopRule {} {} was expected to return a map, got {}",
                 desc, ruleDebugId(rs, idx), comp);
      for(auto& [k,v] : *m) addChild(k, std::move(v));
    }
    // else ignore component.
  };

  // Unlike other rules, our fallback_point is different j because we need may
  // need to discard even successful SkipPoints if the next component fails.
  ssize_t j = i, fallback_point = i;
  bool first = true;
  while(true) {
    JsonLoc out = (first || loop.glueidx != -1)
                    ? eval(ctx, j, rs, loop.partidx)
                    : evalQuiet(ctx.input(), j, rs, loop.partidx);
    if(out.holdsErrorValue()) {
      if(loop.glueidx == -1 && !first) break;
      else return out;
    }else {
      recordComponent("child", std::move(out), loop.partidx);
      fallback_point = j;
    }
    if(sp && evalQuiet(ctx.input(), j, rs, loop.skipidx).holdsErrorValue())
      break;
    if(loop.glueidx != -1) {
      JsonLoc out = evalQuiet(ctx.input(), j, rs, loop.glueidx);
      if(out.holdsErrorValue()) break;
      else recordComponent("glue", std::move(out), loop.glueidx);
      if(sp) {
        out = skip(ctx, j, *sp, rs);
        if(out.holdsErrorValue()) return out;
      }
    }
    first = false;
  }
  // discard any failures and SkipPoints.
  return JsonLoc::withPos(std::move(rv), std::exchange(i, fallback_point),
                                         fallback_point);
}

static JsonLoc evalQuiet(const InputPiece& input, ssize_t& i,
                         const RuleSet& rs, ssize_t ruleIndex) {
  InputDiags proxy{unowned(input)};
  return eval(proxy, i, rs, ruleIndex);
}

static bool
evalPeek(const InputPiece& input, ssize_t i,
         const RuleSet& rs, ssize_t ruleIndex) {
  return !evalQuiet(input, i, rs, ruleIndex).holdsErrorValue();
}

static bool
orBranchFlattenableOrError(const RuleSet& rs, ssize_t partidx,
                           const JsonTmpl& tmpl) {
  if(tmpl.holdsMap()) return true;
  return isPassthroughTmpl(tmpl) && resultFlattenableOrError(rs, partidx);
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

// TODO we need an `eval test -v` that produces verbose debug logs.
static JsonLoc
eval(InputDiags& ctx, ssize_t& i, const OrRule& ors, const RuleSet& rs) {
  if(ors.comps.empty()) Bug("Found an empty OrList RuleSet");
  JsonLoc out{JsonLoc::ErrorValue{}};
  for(auto& [lidx, pidx, tmpl]: ors.comps) {
    if(ors.flattenOnDemand && !orBranchFlattenableOrError(rs,pidx,tmpl))
      Bug("OrRule branch {} does not produce a map", ruleDebugId(rs, pidx));
    if(lidx != -1 && !evalPeek(ctx.input(), i, rs, lidx)) continue;
    out = eval(ctx, i, rs, pidx);
    if(!out.holdsErrorValue())
      return JsonLoc::withPos(tmpl.substituteAll({{"child", out}}),
                              out.stPos, out.enPos);

    // If we passed evalPeek(), don't try anything else.
    if(lidx != -1) return out;
  }
  return out;  // Return the last error.
}

static JsonLoc
eval(InputDiags& ctx, ssize_t& i, const MatchOrError& me, const RuleSet& rs) {
  ssize_t oldi = i;
  JsonLoc out = eval(ctx, i, rs, me.compidx);
  if(out.holdsErrorValue()) Error(ctx, oldi, me.errmsg);
  return out;
}

static JsonLoc
eval(InputDiags& ctx, ssize_t& i, const AliasRule& alias, const RuleSet& rs) {
  return eval(ctx, i, rs, alias.targetidx);
}

// Dev-note: move this to runtime/ directory if we want to use this in
// `oalex build`. Or link the generated source files with oalex-bin-lib.
static JsonLike
oalexBuiltinHello(InputDiags& ctx, ssize_t& i) {
  if(ctx.input().substr(i, 5) == "hello") {
    i += 5;
    return JsonLoc::String{"hello"};
  } else {
    Error(ctx, i, "Expected 'hello'");
    return JsonLoc::ErrorValue{};
  }
}

class ExternParserParam : public Parser {
 public:
  ExternParserParam(const RuleSet& rs, ssize_t ruleIndex)
    : rs_{rs}, ruleIndex_{ruleIndex} {}
  JsonLike operator()(InputDiags& ctx, ssize_t& i) const override {
    return eval(ctx, i, rs_, ruleIndex_);
  }
 private:
  const RuleSet& rs_;
  ssize_t ruleIndex_;
};

static vector<unique_ptr<Parser>>
externParams(const vector<ssize_t>& params, const RuleSet& rs) {
  vector<unique_ptr<Parser>> rv;
  for(ssize_t ri : params)
    rv.push_back(move_to_unique(ExternParserParam{rs, ri}));
  return rv;
}

// Dev-note: Generalize this to requireEqual() if needed.
static void
requireParamCount(size_t expected_count, size_t observed_count,
                  string_view externalName) {
  if(expected_count != observed_count)
    Bug("{} requires {} parameters, found {}", externalName,
        expected_count, observed_count);
}

static JsonLoc
eval(InputDiags& ctx, ssize_t& i, const ExternParser& ext, const RuleSet& rs) {
  string_view extname = ext.externalName();
  if(extname.find("oalexBuiltin") == 0) {
    string_view builtin = extname.substr(sizeof("oalexBuiltin")-1);
    vector<unique_ptr<Parser>> params = externParams(ext.params(), rs);
    requireParamCount(expectedParamCount(builtin), params.size(), extname);
    if(builtin == "Hello") return oalexBuiltinHello(ctx, i);
    else if(builtin == "IndentedList")
      return oalexBuiltinIndentedList(ctx, i, *params[0], *params[1]);
    else Bug("Unknown builtin. Should have been caught during compilation");
  }else {
    // Use dlopen() someday
    Error(ctx, i, "eval() doesn't support 'extern' parsers");
    return JsonLoc::ErrorValue{};
  }
}

// TODO recheck if we are properly initializing
// rv.stPos and rv.enPos in all cases.
JsonLoc
eval(InputDiags& ctx, ssize_t& i, const RuleSet& ruleset, ssize_t ruleIndex) {
  const Rule& r = ruleAt(ruleset, ruleIndex);
  if(auto* s = dynamic_cast<const StringRule*>(&r))
    return toJsonLoc(match(ctx, i, s->val));
  else if(auto* wp = dynamic_cast<const WordPreserving*>(&r))
    return toJsonLoc(match(ctx, i, ruleset.regexOpts.at(wp->regexOptsIdx).word,
                           **wp));
  else if(auto* ext = dynamic_cast<const ExternParser*>(&r))
    return eval(ctx, i, *ext, ruleset);
  else if(auto* sp = dynamic_cast<const SkipPoint*>(&r))
    return skip(ctx, i, *sp, ruleset);
  else if(auto* regex = dynamic_cast<const RegexRule*>(&r))
    return toJsonLoc(match(ctx, i, *regex->patt,
                           ruleset.regexOpts.at(regex->regexOptsIdx)));
  else if(auto* seq = dynamic_cast<const ConcatFlatRule*>(&r))
    return eval(ctx, i, *seq, ruleset);
  else if(auto* out = dynamic_cast<const OutputTmpl*>(&r))
    return eval(ctx, i, *out, ruleset);
  else if(auto* loop = dynamic_cast<const LoopRule*>(&r))
    return eval(ctx, i, *loop, ruleset);
  else if(auto* err = dynamic_cast<const ErrorRule*>(&r)) {
    if(!err->msg.empty()) errorValue(ctx, i, err->msg);
    return JsonLoc::ErrorValue{};
  }else if(auto* qm = dynamic_cast<const QuietMatch*>(&r))
    return evalQuiet(ctx.input(), i, ruleset, qm->compidx);
  else if(auto* ors = dynamic_cast<const OrRule*>(&r))
    return eval(ctx, i, *ors, ruleset);
  else if(auto* me = dynamic_cast<const MatchOrError*>(&r))
    return eval(ctx, i, *me, ruleset);
  else if(auto* alias = dynamic_cast<const AliasRule*>(&r))
    return eval(ctx, i, *alias, ruleset);
  Bug("Unknown rule type {} in eval", r.specifics_typename());
}

JsonLoc
trimAndEval(InputDiags& ctx, ssize_t& i,
            const RuleSet& ruleset, ssize_t ruleIndex) {
  ssize_t ctxskip = ruleset.rules[ruleIndex]->context_skipper();
  if(ctxskip == Rule::helperRuleNoContext)
    Bug("Internal helper rules do not have a specified context");
  if(ctxskip == Rule::removedContext)
    return eval(ctx, i, ruleset, ruleIndex);
  if(ctxskip < 0)
    Bug("Unknown ContextSkipper value: {}", ctxskip);
  if(ctxskip >= ssize(ruleset.rules))
    Bug("trimAndEval(): Out of bounds: {}", ctxskip);
  const Skipper& skip = ruleset.skips[ctxskip];
  ssize_t oldi = i;
  i = skip.next(ctx.input(), i);
  if(i == ssize_t(string::npos)) return skipError(ctx, oldi, skip);
  JsonLoc rv = eval(ctx, i, ruleset, ruleIndex);
  if(rv.holdsErrorValue()) return rv;
  oldi = i;
  i = skip.next(ctx.input(), i);  // Don't discard rv even on error.
  if(i == ssize_t(string::npos)) skipError(ctx, oldi, skip);
  return rv;
}

// codegen()
// ---------

// Forward declaration.
static void codegenParserCall(const Rule& rule, string_view posVar,
                              const OutputStream& cppos);

static string cEscaped(char c) {
  switch(c) {
    // Technically, we should only need \", \n, and \\, but this should help
    // readability.
    case '"' : return "\\\"";
    case '\\': return "\\\\";
    case '\a': return "\\a";
    case '\b': return "\\b";
    case '\f': return "\\f";
    case '\n': return "\\n";
    case '\r': return "\\r";
    case '\t': return "\\t";
    case '\v': return "\\v";
    case '\0': return "\\0";
    // TODO hex code for bytes > 0x7f
    default: return string(1, c);
  }
}

static string cEscaped(string_view s) {
  string rv;
  for(char c : s) rv += cEscaped(c);
  return rv;
}

static string squoted(char ch) { return format("'{}'", cEscaped(ch)); }
static string dquoted(string_view s) { return format("\"{}\"", cEscaped(s)); }

static string anchorName(const RegexAnchor& a) {
  switch(a.anchorType) {
    case RegexAnchor::wordEdge: return "wordEdge";
    case RegexAnchor::bol: return "bol";
    case RegexAnchor::eol: return "eol";
    default: Bug("Unknown RegexAnchor of type {}", int(a.anchorType));
  }
}

static void linebreak(const OutputStream& os, ssize_t indent) {
  os("\n" + string(indent, ' '));
}

static const char* alphabool(bool b) { return b ? "true" : "false"; }

static void
genRegexCharSet(const RegexCharSet& cset,
                const OutputStream& cppos, ssize_t indent) {
  auto br = [&]() { linebreak(cppos, indent); };
  cppos("RegexCharSet({"); br();
  for(auto& range : cset.ranges) {
    cppos(format("  {{ {}, {} }},", squoted(range.from), squoted(range.to)));
    br();
  }
  cppos(format("}}, {})", alphabool(cset.negated)));
}

template <class T, class Cb, class BrCb> static void
genMakeVector(const string& eltType, const vector<T>& vec,
              Cb genElt, BrCb br, const OutputStream& cppos) {
  cppos(format("makeVector<{}>(", eltType)); br();
  for(const T& elt : vec) {
    cppos("  "); genElt(elt);
    if(&elt != &vec.back()) cppos(",");
    br();
  }
  cppos(")");
}

static void
genRegexComponents(const Regex& regex, const OutputStream& cppos,
                   ssize_t indent) {
  auto br = [&]() { linebreak(cppos, indent); };
  switch(regex.nodeType) {
    case RegexNodeType::charSet: {
      auto& cset = static_cast<const RegexCharSet&>(regex);
      genRegexCharSet(cset, cppos, indent);
      break;
    }
    case RegexNodeType::string: {
      const string& s = static_cast<const RegexString&>(regex).value;
      cppos(format("RegexString({})", dquoted(s)));
      break;
    }
    case RegexNodeType::anchor: {
      auto& a = static_cast<const RegexAnchor&>(regex);
      cppos(format("RegexAnchor(RegexAnchor::{})", anchorName(a)));
      break;
    }
    case RegexNodeType::concat: {
      auto& seq = static_cast<const RegexConcat&>(regex);
      cppos("RegexConcat({");
      genMakeVector("unique_ptr<const Regex>", seq.parts, [&](auto& part) {
                      cppos("move_to_unique(");
                      genRegexComponents(*part, cppos, indent+2);
                      cppos(")");
                    }, br, cppos);
      cppos("})");
      break;
    }
    case RegexNodeType::optional: {
      auto& opt = static_cast<const RegexOptional&>(regex);
      cppos("RegexOptional(move_to_unique("); br(); cppos("  ");
      genRegexComponents(*opt.part, cppos, indent+2);
      br(); cppos("))");
      break;
    }
    case RegexNodeType::repeat: {
      auto& rep = static_cast<const RegexRepeat&>(regex);
      cppos("RegexRepeat(move_to_unique("); br(); cppos("  ");
      genRegexComponents(*rep.part, cppos, indent+2);
      br(); cppos("))");
      break;
    }
    case RegexNodeType::orList: {
      auto& ors = static_cast<const RegexOrList&>(regex);
      cppos("RegexOrList({");
      genMakeVector("unique_ptr<const Regex>", ors.parts, [&](auto& part) {
                      cppos("move_to_unique(");
                      genRegexComponents(*part, cppos, indent+2);
                      cppos(")");
                    }, br, cppos);
      cppos("})");
      break;
    }
    default:
      Bug("Unknown regex type index {} in codegen", int(regex.nodeType));
  }
}

// Requires using oalex::{RegexOptions, RegexCharSet}.
static void
codegenRegexOpts(const RegexOptions& regexOpts, const OutputStream& cppos,
                 ssize_t indent) {
  auto br = [&]() { linebreak(cppos, indent); };
  cppos("RegexOptions{"); br();
    cppos("  .word = ");
    genRegexCharSet(regexOpts.word, cppos, indent+10); br();
  cppos("}");
}

void
codegenDefaultRegexOptions(const RuleSet& ruleset, const OutputStream& cppos) {
  cppos("const oalex::RegexOptions& defaultRegexOpts() {\n");
  cppos("  using oalex::RegexCharSet;\n");
  cppos("  using oalex::RegexOptions;\n");
  cppos("  static const RegexOptions *opts = new ");
    codegenRegexOpts(ruleset.regexOpts.at(0), cppos, 2); cppos(";\n");
  cppos("  return *opts;\n");
  cppos("}\n");
}

// Assumes resolveIfWrapper() is already invoked.
static bool
producesGeneratedStruct(const Rule& r, bool flattenable) {
  if(flattenable) return !dynamic_cast<const ErrorRule*>(&r);
  auto* out = dynamic_cast<const OutputTmpl*>(&r);
  return out && out->nameOrNull();
}

// Assumes resolveIfWrapper() is already invoked.
static bool
producesString(const Rule& r) {
  return dynamic_cast<const StringRule*>(&r) ||
         dynamic_cast<const RegexRule*>(&r) ||
         dynamic_cast<const WordPreserving*>(&r);
  // TODO: add passthrough wrappers
}

// Dev-note: This is only meant for parserResultTraits(). But sometimes other
// code will use it as an unnecessary optimization to avoid a dynamic_cast.
// TODO: replace all uses of this outside of parserResultTraits() with
// parserResultTraits().type.
static string
parserResultName(const Rule& rule, bool flattenable) {
  if(producesString(rule)) return "oalex::StringLoc";
  else if(producesGeneratedStruct(rule, flattenable))
   return "Parsed" + rule.nameOrNull()->toUCamelCase();
  else if(dynamic_cast<const ExternParser*>(&rule) ||
          dynamic_cast<const OrRule*>(&rule)) return "oalex::JsonLike";
  else if(dynamic_cast<const SkipPoint*>(&rule)) return "bool";
  else if(dynamic_cast<const ErrorRule*>(&rule)) return "std::nullopt_t";
  else return "oalex::JsonLoc";
}
static string
parserResultName(const RuleSet& ruleset, const Rule& rule) {
  return parserResultName(rule, resultFlattenableOrError(ruleset, rule));
}

struct ParserResultTraits {
  string type;
  string optional;

  string get_value_tmpl;
  string value(string_view xpr) const { return format(get_value_tmpl, xpr); }
};

static ParserResultTraits
parserResultTraits(const RuleSet& ruleset, ssize_t ruleidx) {
  const Rule& rule = ruleAt(ruleset, resolveIfWrapper(ruleset, ruleidx));
  bool isflat = resultFlattenableOrError(ruleset, ruleidx);
  if(producesGeneratedStruct(rule, isflat) || producesString(rule)) {
    string restype = parserResultName(ruleset, rule);
    return { .type = restype,
             .optional = format("std::optional<{}>", restype),
             .get_value_tmpl = "{}.value()",
           };
  }
  else if(dynamic_cast<const ExternParser*>(&rule) ||
          dynamic_cast<const OrRule*>(&rule))
    return { .type = "oalex::JsonLike",
             .optional = "oalex::JsonLike",
             .get_value_tmpl = "{}",
           };
  // TODO: make .type "void" rather than "bool". Or find a way to never use it.
  else if(dynamic_cast<const SkipPoint*>(&rule))
    return { .type = "bool",
             .optional = "bool",
             .get_value_tmpl = "{}",
           };
  else if(dynamic_cast<const ErrorRule*>(&rule))
    return { .type = "std::nullopt_t",
             .optional = "std::nullopt_t",
             .get_value_tmpl = "{}",
           };
  else return { .type = "oalex::JsonLoc",
                .optional = "oalex::JsonLoc",
                .get_value_tmpl = "{}",
              };
}

static string
parserResultOptional(const RuleSet& ruleset, ssize_t ruleidx) {
  return parserResultTraits(ruleset, ruleidx).optional;
}

static string
parserName(const Ident& rname) {
  return "parse" + rname.toUCamelCase();
}

// It's a version of producesGeneratedStruct() that can be called outside
// of parserResultTraits().
static bool
returnsGeneratedStruct(const RuleSet& ruleset, ssize_t ruleidx) {
  return producesGeneratedStruct(
      ruleAt(ruleset, resolveIfWrapper(ruleset, ruleidx)),
      false);
}

static void
parserHeaders(const RuleSet& ruleset, ssize_t ruleidx,
              const OutputStream& cppos, const OutputStream& hos) {
  const Rule& rule = ruleAt(ruleset, ruleidx);
  const Ident& rname = *rule.nameOrNull();
  string header = format("{} {}(oalex::InputDiags& ctx, ssize_t& i)",
                         parserResultOptional(ruleset, ruleidx),
                         parserName(rname));
  hos(header + ";\n");

  // TODO complex parsers should have comments with the source line.
  cppos(header + " ");
}

static void
codegen(const RuleSet& ruleset, const RegexRule& regex,
        const OutputStream& cppos) {
  // TODO trim this down whenever possible.
  cppos("  using oalex::makeVector;\n");
  cppos("  using oalex::move_to_unique;\n");
  cppos("  using oalex::Regex;\n");
  cppos("  using oalex::RegexAnchor;\n");
  cppos("  using oalex::RegexCharSet;\n");
  cppos("  using oalex::RegexConcat;\n");
  cppos("  using oalex::RegexOptional;\n");
  cppos("  using oalex::RegexOrList;\n");
  cppos("  using oalex::RegexRepeat;\n");
  cppos("  using oalex::RegexString;\n");
  cppos("  using std::literals::string_literals::operator\"\"s;\n");
  cppos("  using std::unique_ptr;\n");
  cppos("  static const Regex *r = new ");
  genRegexComponents(*regex.patt, cppos, 4);
  cppos(";\n");

  if(regex.regexOptsIdx == 0) {
    cppos("  return oalex::match(ctx, i, *r, defaultRegexOpts());\n");
    return;
  }
  cppos("  using oalex::RegexOptions;\n");
  cppos("  static const RegexOptions* regexOpts = new ");
    codegenRegexOpts(ruleset.regexOpts.at(regex.regexOptsIdx), cppos, 2);
    cppos(";\n");
  cppos("  return oalex::match(ctx, i, *r, *regexOpts);\n");
}

// TODO: use sorted vector instead of map here too.
static void
codegen(const JsonTmpl& jstmpl, const OutputStream& cppos,
        const map<string,string>& placeholders, ssize_t indent) {
  auto br = [&]() { linebreak(cppos, indent); };
  if(auto* p = jstmpl.getIfPlaceholder()) {
    auto v = placeholders.find(p->key);
    if(v == placeholders.end())
      Bug("Undefined placeholder in codegen: {}", p->key);
    cppos(v->second);
  }else if(jstmpl.holdsEllipsis()) {
    Bug("Frontend should have desugared all ellipsis");
  }else if(auto* s = jstmpl.getIfString()) {
    cppos(format("{}s", dquoted(*s)));
  }else if(auto* v = jstmpl.getIfVector()) {
    cppos("JsonLoc::Vector{");
    genMakeVector("JsonLoc", *v, [&](auto& child) {
                   codegen(child, cppos, placeholders, indent+2);
                 }, br, cppos);
    cppos("}");
  }else if(auto* m = jstmpl.getIfMap()) {
    cppos("JsonLoc::Map{"); br();
    for(const auto& [k, v] : *m) {
      cppos(format("  {{{}, ", dquoted(k)));
      codegen(v, cppos, placeholders, indent+4);
      cppos("},"); br();
    }
    cppos("}");
  }else cppos("JsonLoc::ErrorValue{}");
}

static void
codegen(const RuleSet& ruleset, const ConcatFlatRule& cfrule,
        const OutputStream& cppos) {
  ssize_t comp_serial = 0;
  string outType = parserResultName(ruleset, cfrule);
  cppos("  using oalex::holdsErrorValue;\n");
  cppos("  ssize_t j = i;\n\n");
  cppos(format("  {} rv;\n", outType));
  for(auto& [childid, key] : cfrule.comps) {
    ParserResultTraits resdeets = parserResultTraits(ruleset, childid);
    // TODO Check for duplicate keys at compile-time.
    string rescomp = format("res{}", comp_serial);
    cppos(format("\n  {} {} = ", resdeets.optional, rescomp));
      codegenParserCall(ruleAt(ruleset, childid), "j", cppos);
      cppos(";\n");
    cppos(format("  if(holdsErrorValue({})) return std::nullopt;\n", rescomp));
    cppos(format("  mergePart{}Into{}(std::move({}), rv);\n",
                 comp_serial, outType, resdeets.value(rescomp)));
    comp_serial++;
  }
  if(cfrule.comps.empty())
    cppos("  std::ignore = ctx;\n");
  cppos("  rv.loc.first = i; rv.loc.second = j;\n");
  cppos("  i = j;\n");
  cppos("  return rv;\n");
}

// TODO: use sorted vector instead of map here too.
static void
genStructValues(const JsonTmpl& outputTmpl,
                const map<string,string>& placeholders, ssize_t indent,
                const OutputStream& cppos) {
  if(auto* s = outputTmpl.getIfString()) {
    cppos(" = "); cppos(dquoted(*s));
  }else if(auto* p = outputTmpl.getIfPlaceholder()) {
    auto v = placeholders.find(p->key);
    if(v == placeholders.end())
      Bug("Undefined placeholder in codegen: {}", p->key);
    cppos(" = ");
    cppos(v->second);
  }else if(auto* v = outputTmpl.getIfVector()) {
    cppos(" = ");
    genMakeVector("JsonLoc", *v, [&](auto& child) {
                   genStructValues(child, placeholders, indent+2, cppos);
                 }, [&]{ linebreak(cppos, indent); }, cppos);
  }else if(auto* m = outputTmpl.getIfMap()) {
    cppos("{"); linebreak(cppos, indent);
    for(auto& [k,v] : *m) {
      cppos("  ."); cppos(Ident::parseGenerated(k).toSnakeCase());
      genStructValues(v, placeholders, indent+2, cppos);
      cppos(","); linebreak(cppos, indent);
    }
    cppos("}");
  }else if(outputTmpl.holdsEllipsis())
    Bug("{}: compiler should have removed ellipsis", __func__);
  else Bug("{}: Unknown type {}", __func__, outputTmpl.tagName());
}

// TODO Make it possible to figure out at compile-time whether a rule produces
// a JsonLoc::Map. E.g. by having OrRule also autobox its non-map branches.
// TODO Once we can figure out at compile-time whether something produces a
// Map, and reflect that in the generated code. E.g. by returning the concrete
// type.
static void
codegen(const RuleSet& ruleset, const OutputTmpl& out,
        const OutputStream& cppos) {
  cppos("  ssize_t oldi = i;\n");
  cppos(format("  {} outfields = ",
               parserResultOptional(ruleset, out.childidx)));
    codegenParserCall(ruleAt(ruleset, out.childidx), "i", cppos);
    cppos(";\n");
  cppos("  if(oalex::holdsErrorValue(outfields)) return std::nullopt;\n");
  map<string,string> placeholders;
  if(resultFlattenableOrError(ruleset, out.childidx)) {
    for(auto& [key, jsloc] : out.outputTmpl.allPlaceholders())
      placeholders.insert({key,
          format("std::move(outfields->fields.{})",
                 Ident::parseGenerated(key).toSnakeCase()) });
  }
  else if(!out.childName.empty())
    placeholders.insert({{ out.childName, "std::move(*outfields)" }});
  cppos(format("  {} rv{{\n", parserResultName(ruleset, out)));
  cppos("    .loc{oldi, i},\n");
  cppos("    .fields");
    genStructValues(out.outputTmpl, placeholders, 4, cppos);
    cppos(",\n");
  cppos("  };\n");
  cppos("  return rv;\n");
}

static void
codegen(const RuleSet& ruleset, const LoopRule& loop,
        const OutputStream& cppos) {
  if(loop.lookidx != -1) Unimplemented("LoopRule lookahead");

  Ident skipname;
  if(loop.skipidx != -1) {
    if(!dynamic_cast<const SkipPoint*>(&ruleAt(ruleset, loop.skipidx)))
      Bug("LoopRule skipidx {} is not a SkipPoint rule",
          ruleDebugId(ruleset, loop.skipidx));
    if(auto name = ruleAt(ruleset, loop.skipidx).nameOrNull()) skipname = *name;
    else Bug("LoopRule skipidx {} rule needs a name",
             ruleDebugId(ruleset, loop.skipidx));
  }
  string outType = parserResultName(ruleset, loop);
  ParserResultTraits partdeets = parserResultTraits(ruleset, loop.partidx),
                     gluedeets;
  if(loop.glueidx != -1) gluedeets = parserResultTraits(ruleset, loop.glueidx);

  cppos("  using oalex::quietMatch;\n");
  cppos("  ssize_t j = i, fallback_point = i;\n\n");
  cppos(format("  {} rv;\n", outType));
  if(loop.glueidx == -1)
    cppos("  bool first = true;\n");
  cppos("  while(true) {\n");
  cppos(format("    {} part;\n\n", partdeets.optional));

  if(loop.glueidx == -1) {
    // TODO resolve this `first` case at compile-time.
    cppos("    if(first) part = ");
      codegenParserCall(ruleAt(ruleset, loop.partidx), "j", cppos);
      cppos(";\n");
    cppos(format("    else part = quietMatch(ctx.input(), j, {});\n",
                 parserName(*ruleAt(ruleset, loop.partidx).nameOrNull())));
    cppos("    if(oalex::holdsErrorValue(part)) {\n");
    cppos("      if(first) return std::nullopt;\n");
    cppos("      else break;\n");
    cppos("    }\n");
  }else {
    cppos("    part = ");
      codegenParserCall(ruleAt(ruleset, loop.partidx), "j", cppos);
      cppos(";\n");
    cppos("    if(oalex::holdsErrorValue(part)) return std::nullopt;\n");
  }
  cppos(format("    mergePartInto{}(std::move({}), rv);\n", outType,
               partdeets.value("part")));

  cppos("    fallback_point = j;\n");
  cppos("\n");
  if(loop.skipidx != -1) {
    // TODO: Replace this `auto` with `bool` when the return-type migration
    // is done.
    cppos(format("    auto skipres = quietMatch(ctx.input(), j, {});\n",
                 parserName(skipname)));
    cppos("    if(oalex::holdsErrorValue(skipres)) break;\n");
  }
  if(loop.glueidx != -1) {
    if(auto gluename = ruleAt(ruleset, loop.glueidx).nameOrNull())
      cppos(format("    {} glue = quietMatch(ctx.input(), j, {});\n",
                   gluedeets.optional, parserName(*gluename)));
    else Bug("Glue rules need a name for codegen(LoopRule)");
    cppos("    if(oalex::holdsErrorValue(glue)) break;\n");
    cppos(format("    mergeGlueInto{}(std::move({}), rv);\n", outType,
                 gluedeets.value("glue")));
    if(loop.skipidx != -1) {
      cppos("    skipres = ");
        codegenParserCall(ruleAt(ruleset, loop.skipidx),
                                      "j", cppos);
        cppos(";\n");
      cppos("    if(oalex::holdsErrorValue(skipres)) {\n");
      cppos("      oalex::Error(ctx, j, \"Unfinished comment\");\n");
      cppos("      return std::nullopt;\n");
      cppos("    }\n");
    }
  }
  if(loop.glueidx == -1)
    cppos("    first = false;\n");
  cppos("  }\n");
  cppos("  rv.loc.first = i; rv.loc.second = fallback_point;\n");
  cppos("  i = fallback_point;\n");
  cppos("  return rv;\n");
}

static const Ident&
externParamName(const RuleSet& ruleset, ssize_t ruleIndex) {
  const Ident* name = ruleAt(ruleset, ruleIndex).nameOrNull();
  if(!name) Bug("Extern params must have names");
  return *name;
}

static string
parserNamesJoinTail(const RuleSet& ruleset, const vector<ssize_t>& rules) {
  string rv;
  for(auto& ruleIndex: rules) {
    rv += format(", *{}Wrapper",
                 externParamName(ruleset, ruleIndex).toLCamelCase());
  }
  return rv;
}

// TODO: Add names from rules we point to.
// TODO: Emit oalex:: qualifier only for headers, not cpp file.
static string
parserCallbacksTail(size_t paramCount) {
  string rv;
  for(size_t i=0; i<paramCount; ++i)
    rv += ", const oalex::Parser&";
  return rv;
}

static void
codegen(const RuleSet& ruleset, const ExternParser& extRule,
        const OutputStream& cppos) {
  cppos("  using oalex::InputDiags;\n");
  cppos("  using oalex::JsonLike;\n");
  cppos("  using oalex::Parser;\n");
  cppos("  using oalex::ParserPtr;\n");
  cppos(format(
        "  extern JsonLike {}(InputDiags& ctx, ssize_t& i{});\n",
        extRule.externalName(), parserCallbacksTail(extRule.params().size())));

  for(const auto& param : extRule.params()) {
    const Ident& name = externParamName(ruleset, param);
    cppos(format("  const static Parser* {}Wrapper = new ParserPtr(\n"
                 "    +[](InputDiags& ctx, ssize_t& i) {{\n"
                 "      return JsonLike({}(ctx, i));\n"
                 "    }});\n",
          name.toLCamelCase(), parserName(name)));
  }
  cppos(format("  return {}(ctx, i{});\n", extRule.externalName(),
               parserNamesJoinTail(ruleset, extRule.params())));
}

static void
codegen(const ErrorRule& errRule, const OutputStream& cppos) {
  if(!errRule.msg.empty()) cppos(format("  oalex::errorValue(ctx, i, {});\n",
                                        dquoted(errRule.msg)));
  cppos("  return std::nullopt;\n");
}

static void
codegen(const RuleSet& ruleset, const QuietMatch& qm,
        const OutputStream& cppos) {
  if(const Ident* name = ruleAt(ruleset, qm.compidx).nameOrNull())
    cppos(format("  return oalex::quietMatch(ctx.input(), i, {});\n",
                 parserName(*name)));
  else Bug("QuietMatch::compidx targets need to have names");
}

static void
codegenLookahead(const RuleSet& ruleset, ssize_t lidx,
                 const OutputStream& cppos) {
  const Rule& rule = ruleAt(ruleset, lidx);
  if(auto* s = dynamic_cast<const StringRule*>(&rule))
    cppos(format("ctx.input().hasPrefix(i, {})", dquoted(s->val)));
  else if(auto* wp = dynamic_cast<const WordPreserving*>(&rule);
          wp && wp->regexOptsIdx == 0) {
    cppos(format("oalex::peekMatch(ctx, i, defaultRegexOpts().word, {})",
                 dquoted(**wp)));
  }
  // When adding a new branch here, remember to change needsName().
  else {
    if(const Ident* name = rule.nameOrNull())
      cppos(format("oalex::peekMatch(ctx.input(), i, {})", parserName(*name)));
    else Bug("The frontend must always name lookidx for {} rules",
             rule.specifics_typename());
  }
}

static bool isEmptyMap(const JsonTmpl& jstmpl) {
  const JsonTmpl::Map* m = jstmpl.getIfMap();
  return m && m->empty();
}

static string_view getIfSingleKeyBranch(const JsonTmpl& jstmpl) {
  const JsonTmpl::Map* m = jstmpl.getIfMap();
  if(!m || m->size() != 1 || !isPassthroughTmpl(m->at(0).second)) return {};
  else return m->at(0).first;
}

static void
genMergeHelpers(const RuleSet& ruleset, const OrRule& orRule,
                const OutputStream& cppos) {
  if(!orRule.flattenOnDemand)
    Bug("{}: caller should directly return components without merger helpers",
        __func__);

  ssize_t branchNumber = 0;
  string outType = parserResultName(orRule, orRule.flattenOnDemand);
  for(auto& comp: orRule.comps) {
    string funName = format("convertBranch{}Into{}", branchNumber++, outType);
    const Rule& compRule = ruleAt(ruleset, comp.parseidx);
    string compType = parserResultTraits(ruleset, comp.parseidx).type;
    bool flatBranch = resultFlattenableOrError(ruleset, comp.parseidx);
    if(isEmptyMap(comp.tmpl)
       || (isPassthroughTmpl(comp.tmpl) && compRule.flatFields().empty())) {
      cppos(format("static {} {}(const {}&) {{ return {{}}; }}\n",
                   outType, funName, compType));
      continue;
    }

    cppos(format("static {} {}({} src) {{\n", outType, funName, compType));
    cppos(format("  {} dest = {{}};\n", outType));
    if(string_view field = getIfSingleKeyBranch(comp.tmpl); !field.empty())
      cppos(format("  dest.fields.{} = std::move(src);\n", field));
    else if(isPassthroughTmpl(comp.tmpl)) {
      if(!flatBranch) Bug("Conflicting branch flattening requirements");
      for(const auto& field: compRule.flatFields()) {
        cppos(format("  dest.fields.{0} = std::move(src.fields.{0});\n",
                     field.field_name));
      }
    }
    cppos("  return dest;\n");
    cppos("}\n\n");
  }
}

// TODO: resolveIfWrapper() feels inconsistently used. Make a rule about when
// it's resolved on the caller side vs. callee side.
static void
genMergeHelperCatPart(const RuleSet& ruleset, ssize_t compidx,
    string_view funName, string_view outType, string_view outField,
    string_view nonFlatMergeTmpl, string_view flatMergeTmpl,
    const OutputStream& cppos) {
  const Rule& compRule = ruleAt(ruleset, resolveIfWrapper(ruleset, compidx));
  const bool flat = resultFlattenableOrError(ruleset, compidx);
  string compType = parserResultName(compRule, flat);
  const bool emptyFun = flat ? compRule.flatFields().empty() : outField.empty();
  if(emptyFun) {
    cppos(format("static void {}({} /*src*/, {}& /*dest*/) {{}}\n",
                 funName, compType, outType));
    return;
  }
  string funHeader = format("static void {}({} src, {}& dest)", funName,
                            compType, outType);

  cppos(funHeader + " {\n");
  if(!flat) cppos(format("  {};\n", format(nonFlatMergeTmpl, outField)));
  else for(auto& field: compRule.flatFields())
    cppos(format("  {};\n", format(flatMergeTmpl, field.field_name)));
  cppos("}\n\n");
}

static void
genMergeHelpers(const RuleSet& ruleset, const ConcatFlatRule& seq,
                const OutputStream& cppos) {
  ssize_t partNumber = 0;
  string outType = parserResultName(ruleset, seq);
  for(auto& comp: seq.comps) {
    string funName = format("mergePart{}Into{}", partNumber++, outType);
    genMergeHelperCatPart(
        ruleset, comp.idx, funName, outType, comp.outputPlaceholder,
        "dest.fields.{} = std::move(src)",
        "dest.fields.{0} = std::move(src.fields.{0})", cppos);
  }
}

static void
genMergeHelpers(const RuleSet& ruleset, const LoopRule& rep,
                const OutputStream& cppos) {
  string outType = parserResultName(ruleset, rep);
  string funName = "mergePartInto" + outType;
  genMergeHelperCatPart(
      ruleset, rep.partidx,
      funName,  outType, "",
      "dest.fields.{}.push_back(std::move(src))",
      "dest.fields.{0}.push_back(std::move(src.fields.{0}))", cppos);

  if(rep.glueidx == -1) return;
  funName = "mergeGlueInto" + outType;
  genMergeHelperCatPart(
      ruleset, rep.glueidx,
      funName,  outType, "",
      "dest.fields.{}.push_back(std::move(src))",
      "dest.fields.{0}.push_back(std::move(src.fields.{0}))", cppos);
}

static void
genMergeHelpers(const RuleSet& ruleset, ssize_t ruleidx,
                const OutputStream& cppos) {
  const Rule& rule = ruleAt(ruleset, ruleidx);
  if(auto* ors = dynamic_cast<const OrRule*>(&rule);
     ors && ors->flattenOnDemand)
    genMergeHelpers(ruleset, *ors, cppos);
  else if(auto* seq = dynamic_cast<const ConcatFlatRule*>(&rule))
    genMergeHelpers(ruleset, *seq, cppos);
  else if(auto* rep = dynamic_cast<const LoopRule*>(&rule))
    genMergeHelpers(ruleset, *rep, cppos);
}

static void
codegen(const RuleSet& ruleset, const OrRule& orRule,
        const OutputStream& cppos) {
  cppos("  using std::literals::string_literals::operator\"\"s;\n");
  cppos("  using oalex::holdsErrorValue;\n");
  if(orRule.flattenOnDemand) {
    ssize_t branchNum = 0;
    string outType = parserResultName(ruleset, orRule);
    for(auto& [lidx, pidx, tmpl] : orRule.comps) {
      // Frontend should make sure even string-producing rules are
      // wrapped in empty maps.
      if(!orBranchFlattenableOrError(ruleset,pidx,tmpl))
        Bug("OrRule branch {} does not produce a map",
            ruleDebugId(ruleset, pidx));
      ParserResultTraits brDeets = parserResultTraits(ruleset, pidx);
      string resvar = format("res{}", branchNum);
      if(lidx == -1) {
        cppos(format("  {} {} = ", brDeets.optional, resvar));
          codegenParserCall(ruleAt(ruleset, pidx), "i", cppos);
          cppos(";\n");
        cppos(format("  if(!holdsErrorValue({}))\n"
                     "    return convertBranch{}Into{}({});\n", resvar,
                     branchNum, outType, brDeets.value(resvar)));
      }else {
        cppos("  if(");
          codegenLookahead(ruleset, lidx, cppos);
          cppos(") {\n");
        cppos(format("    {} {} = ", brDeets.optional, resvar));
          codegenParserCall(ruleAt(ruleset, pidx), "i", cppos);
          cppos(";\n");
        cppos(format("    if(holdsErrorValue({})) return std::nullopt;\n",
                     resvar));
        cppos(format("    return convertBranch{}Into{}({});\n",
                     branchNum, outType, brDeets.value(resvar)));
        cppos("  }\n");
      }
      branchNum++;
    }
    cppos("  return std::nullopt;\n");
  }
  else {
    cppos("  oalex::JsonLike res;\n");
    for(auto& [lidx, pidx, tmpl] : orRule.comps) {
      if(!isPassthroughTmpl(tmpl))
        Bug("Non-flattenable or-branch {} must pass values along unmodified",
            ruleDebugId(ruleset, pidx));
      if(lidx == -1) {
        cppos("  res = ");
          codegenParserCall(ruleAt(ruleset, pidx), "i", cppos);
          cppos(";\n");
        cppos("  if(!holdsErrorValue(res)) return res;\n");
      }else {
        cppos("  if(");
          codegenLookahead(ruleset, lidx, cppos);
          cppos(") {\n");
        cppos("    return ");
          codegenParserCall(ruleAt(ruleset, pidx), "i", cppos);
          cppos(";\n");
        cppos("  }\n");
      }
    }
    cppos("  return res;\n");
  }
}

static void
codegen(const RuleSet& ruleset, const MatchOrError& me,
        const OutputStream& cppos) {
  cppos("  using oalex::Error;\n");
  cppos("  using oalex::holdsErrorValue;\n");
  cppos(format("  {} res = ", parserResultOptional(ruleset, me.compidx)));
     codegenParserCall(ruleAt(ruleset, me.compidx), "i", cppos);
     cppos(";\n");
  cppos("  if(holdsErrorValue(res))\n");
  cppos(format("    Error(ctx, i, {});\n", dquoted(me.errmsg)));
  cppos("  return res;\n");
}

static void
codegen(const RuleSet& ruleset, const AliasRule& alias,
        const OutputStream& cppos) {
  cppos("  return ");
    codegenParserCall(ruleAt(ruleset, alias.targetidx), "i", cppos);
    cppos(";\n");
}

static void
codegen(const RuleSet& ruleset, const SkipPoint& sp,
        const OutputStream& cppos) {
  const Skipper* skip = &ruleset.skips[sp.skipperIndex];

  cppos("  using oalex::Skipper;\n");
  cppos("  static Skipper* skip = new Skipper{\n");
  cppos("    .unnestedComments{\n");
  for(auto& [st,en] : skip->unnestedComments)
    cppos(format("     {{ {}, {} }},\n", dquoted(st), dquoted(en)));
  cppos("    },\n");
  if(skip->nestedComment.has_value()) {
    auto& [st,en] = *skip->nestedComment;
    cppos(format("    .nestedComment{{ {{ {}, {} }} }},\n",
                 dquoted(st), dquoted(en)));
  }else {
    cppos("    .nestedComment{},\n");
  }
  cppos(format("    .newlines = Skipper::Newlines::{},\n",
               to_string(skip->newlines)));
  cppos("  };\n");
  cppos("  ssize_t j = skip->next(ctx.input(), i);\n");
  cppos("  if (static_cast<size_t>(j) != oalex::Input::npos) {\n");
  cppos("    i = j;\n");
  cppos("    return true;\n");
  cppos("  } else return false;\n");
}

static void
codegen(const RuleSet& ruleset, const WordPreserving& wp,
        const OutputStream& cppos) {
  if(wp.regexOptsIdx == 0) {
    cppos(format("  return oalex::match(ctx, i, "
                     "defaultRegexOpts().word, {});\n", dquoted(*wp)));
    return;
  }
  cppos("  using oalex::match;\n");
  cppos("  using oalex::RegexCharSet;\n");
  cppos("  using oalex::toJsonLoc;\n");
  cppos("  static const RegexCharSet* wordChars = new ");
    genRegexCharSet(ruleset.regexOpts.at(wp.regexOptsIdx).word, cppos, 2);
    cppos(";\n");
  cppos(format("  return match(ctx, i, *wordChars, {});\n",
               dquoted(*wp)));
}

void
Rule::deferred_name(Ident name) {
  if(name_) Bug("Cannot rename rule {} to {}",
                name_.preserveCase(), name.preserveCase());
  name_ = name;
}
void
Rule::context_skipper(ssize_t skipper_index) {
  if(contextSkipper_ != Rule::helperRuleNoContext)
    Bug("We shouldn't have to assign context_skipper() to the same rule "
        "multiple times.");
  contextSkipper_ = skipper_index;
}

bool needsName(const Rule& rule, bool isTentativeTarget) {
  if(dynamic_cast<const StringRule*>(&rule)) return false;
  auto* wp = dynamic_cast<const WordPreserving*>(&rule);
  if(wp && wp->regexOptsIdx == 0) return false;
  if(dynamic_cast<const ErrorRule*>(&rule)) return isTentativeTarget;
  return true;
}

static void
codegenParserCall(const Rule& rule, string_view posVar,
                  const OutputStream& cppos) {
  if(auto* s = dynamic_cast<const StringRule*>(&rule))
    cppos(format("oalex::match(ctx, {}, {})", posVar, dquoted(s->val)));
  else if(auto* wp = dynamic_cast<const WordPreserving*>(&rule);
          wp && wp->regexOptsIdx == 0) {
    cppos(format("oalex::match(ctx, {}, defaultRegexOpts().word, {})",
                 posVar, dquoted(**wp)));
  }
  else if(auto* err = dynamic_cast<const ErrorRule*>(&rule)) {
    if(err->msg.empty()) cppos("std::nullopt");
    else cppos(format("oalex::errorValue(ctx, {}, {})",
                      posVar, dquoted(err->msg)));
  }
  else if(auto* ext = dynamic_cast<const ExternParser*>(&rule);
          ext && ext->params().empty())
    cppos(format("{}(ctx, {})", ext->externalName(), posVar));
  else if(const Ident* rname = rule.nameOrNull())
    cppos(format("{}(ctx, {})", parserName(*rname), posVar));

  // When adding a new branch here, remember to change needsName().
  else Unimplemented("nameless component of type {}",
                     rule.specifics_typename());
}

static void
genExternDeclaration(const OutputStream& hos, string_view extName,
                     ssize_t paramCount) {
  hos(format("extern oalex::JsonLike {}(oalex::InputDiags& ctx, "
             "ssize_t& j{});\n", extName, parserCallbacksTail(paramCount)));
}

static string
flatFieldType(const RuleSet& ruleset, const RuleField& field) {
  const Rule& source = ruleAt(ruleset, field.schema_source);
  string vtype = parserResultName(ruleset, source);

  if(field.container == RuleField::single) return vtype;
  else if(field.container == RuleField::optional)
    return format("std::optional<{}>", vtype);
  else if(field.container == RuleField::vector)
    return format("std::vector<{}>", vtype);
  else Bug("Bad container type {}", int(field.container));
}

// TODO: Eliminate parseGenerated() by keeping them as idents.
static void
genOutputFields(const JsonTmpl& t, const Ident& fieldName,
  const RuleSet& ruleset, ssize_t childidx, ssize_t indent,
  const OutputStream& hos) {
  auto findField = [&](string_view qname) -> const RuleField& {
    for(const RuleField& f: ruleset.rules[childidx]->flatFields())
      if(f.field_name == qname) return f;
    Bug("Cannot field for placeholder {} among {} fields", qname,
        ruleset.rules[childidx]->flatFields().size());
  };
  if(const JsonTmpl::String* s = t.getIfString())
    hos(format("std::string {} = {};", fieldName.toSnakeCase(),
               dquoted(*s)));
  else if(const JsonTmpl::Placeholder* p = t.getIfPlaceholder()) {
    string vtype;
    if(!resultFlattenableOrError(ruleset, childidx))
      vtype = parserResultName(ruleset, ruleAt(ruleset, childidx));
    else vtype = flatFieldType(ruleset, findField(p->key));
    hos(format("{} {};", vtype, fieldName.toSnakeCase()));
  }
  else if(t.holdsEllipsis())
    Bug("The compiler was supposed to have removed ellipsis");
  else if(t.holdsVector())
    // This one is for vectors that stay vectors after desugaring.
    // I'm not sure if this should become JsonLike, actually.
    hos(format("std::vector<oalex::JsonLoc> {};", fieldName.toSnakeCase()));
  else if(const JsonTmpl::Map* m = t.getIfMap()) {
    hos(format("struct {} {{", fieldName.toUCamelCase()));
      linebreak(hos, indent);  // not indent+2, in case the next line is '}'
    for(auto& [k,child]: *m) {
      hos("  ");
      genOutputFields(child, Ident::parseGenerated(k), ruleset, childidx,
                           indent+2, hos);
      linebreak(hos, indent);
    }
    hos(format("}} {};", fieldName.toSnakeCase()));
  }else Bug("Don't know how to generate field for type {}", t.tagName());
}

static bool
excludedField(const vector<string>& excluded_fields, const JsonTmpl& tmpl) {
  for(const string& ex : excluded_fields)
    if(isPlaceholder(tmpl, ex)) return true;
  return false;
}

// excluded_fields is for suppressing optional fields that appear as part of
// a map output, e.g. { key: some_optional_value, ... }. In this case, the
// placeholder `some_optional_value` will appear here. It has no impact on
// placeholders in other contexts, if it's not directly a member of a map.
// Missing optional values will appear as {} in those cases.
//
// TODO: see if we can refactor parts of this with codegen(JsonTmpl) above.
// TODO: make move conversion.
// TODO: Eliminate parseGenerated() by keeping them as idents.
// TODO: Disable list template for non-repeating fields. I don't really want
// to support lists in mapNestedAppend("a.b[3].c")
static void
genFieldConversion(const JsonTmpl& t, string field_prefix,
                   const vector<string>& excluded_fields,
                   const OutputStream& cppos, ssize_t indent) {
  if(t.holdsString()) cppos(field_prefix);
  else if(t.holdsPlaceholder()) cppos(format("toJsonLoc({})", field_prefix));
  else if(t.holdsEllipsis())
    Bug("{}: ellipsis should have been desugared in the compiler", __func__);
  else if(const JsonTmpl::Vector* v = t.getIfVector()) {
    cppos("JsonLoc::Vector{"); linebreak(cppos, indent);
    for(ssize_t i=0; i<ssize(*v); ++i) {
      cppos(format("  toJsonLoc({}[{}]),", field_prefix, i));
        linebreak(cppos, indent);
    }
    cppos("}");
  }else if(const JsonTmpl::Map* m = t.getIfMap()) {
    cppos("JsonLoc::Map{"); linebreak(cppos, indent);
    for(const auto& [k, v] : *m) {
      if(excludedField(excluded_fields, v)) continue;
      cppos(format("  {{{}, ", dquoted(k)));
      genFieldConversion(
          v, format("{}.{}", field_prefix,
                    Ident::parseGenerated(k).toSnakeCase()),
          excluded_fields, cppos, indent+2);
      cppos("},"); linebreak(cppos, indent);
    }
    cppos("}");
  }
}

// TODO: Make an std::move version
// TODO: indent is always 2. Remove it.
static void
genRequiredFieldsConversion(
    const vector<RuleField>& fields,
    const OutputStream& cppos, ssize_t indent) {
  cppos("JsonLoc::Map{"); linebreak(cppos, indent);
  for(auto& field: fields) if(field.container != RuleField::optional) {
    cppos(format("  {{{}, ", dquoted(field.field_name)));
    if(field.container == RuleField::single)
      cppos(format("JsonLoc{{fields.{}}}", field.field_name));
    else
      cppos(format("oalex::toJsonLoc(fields.{})", field.field_name));
    cppos("},"); linebreak(cppos, indent);
  }
  cppos("}");
}
static void
genOptionalFieldAppends(
    const vector<RuleField>& fields,
    string_view resvar,
    const OutputStream& cppos) {
  cppos("  using oalex::holdsErrorValue;\n");
  for(auto& field: fields) if(field.container == RuleField::optional) {
    cppos(format("  if(!holdsErrorValue(fields.{}))\n", field.field_name));
    cppos(format("    {}.emplace_back({}, oalex::toJsonLoc(fields.{}));\n",
                 resvar, dquoted(field.field_name), field.field_name));
  }
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
  const Rule& r = ruleAt(rs, ruleidx);
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
    vector<RuleField> rv{ {"", loop->partidx, RuleField::vector},
                          {"", loop->glueidx, RuleField::vector} };
    if(loop->glueidx == -1) rv.pop_back();
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

// Used only for Bug(). Not formatted for users.
static string
listCycle(const RuleSet& ruleset, const vector<ssize_t>& edges_remaining) {
  string rv;
  for(ssize_t i=0; i<ssize(edges_remaining); ++i) {
    if(edges_remaining[i] == 0) continue;
    if(!rv.empty()) rv += ", ";
    if(const Ident* name = ruleAt(ruleset, i).nameOrNull())
      rv += name->preserveCase();
    else rv += format("rules[{}]", i);
  }
  return rv;
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

void
populateFlatFields(RuleSet& ruleset) {
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

// TODO: refactor out repetitions between this and genOutputTmplTypeDefinition.
static void
genFlatTypeDefinition(const RuleSet& ruleset, ssize_t ruleIndex,
                      const OutputStream& cppos, const OutputStream& hos) {
  const Rule& r = ruleAt(ruleset, ruleIndex);
  if(r.nameOrNull() == nullptr) return;
  string className = parserResultName(ruleset, r);
  hos("struct " + className + " {\n");
  hos("  oalex::LocPair loc;\n");
  hos("  struct Fields {\n");
  for(const RuleField& field : r.flatFields())
    hos(format("    {} {};\n", flatFieldType(ruleset, field),
               field.field_name));
  hos("  } fields;\n");
  hos("  explicit operator oalex::JsonLoc() const;\n");
  hos("};\n\n");

  cppos(format("{}::operator JsonLoc() const {{\n", className));
  cppos("  auto rv = ");
    genRequiredFieldsConversion(r.flatFields(), cppos, 2);
    cppos(";\n");
  genOptionalFieldAppends(r.flatFields(), "rv", cppos);
  cppos("  return JsonLoc::withPos(rv, loc.first, loc.second);\n");
  cppos("}\n\n");
}

static vector<string>
getOptionalFields(const RuleSet& ruleset, ssize_t ruleIndex) {
  const Rule& r = ruleAt(ruleset, ruleIndex);
  vector<string> rv;
  for(const RuleField& field : r.flatFields())
    if(field.container == RuleField::Container::optional)
      rv.push_back(field.field_name);
  return rv;
}

static bool
contains(const vector<string>& v, const string& s) {
  for(auto& e : v) if(s == e) return true;
  return false;
}
// This function searches jstmpl for placeholders, and returns their path.
// Since a placeholder can appear multiple times in jstmpl, or even zero times,
// the output vector size might not match the input vector size.
// TODO: See if we still allow that.
static vector<vector<JsonPathComp>>
getPlaceholderPaths(const vector<string>& placeholders,
                    const JsonTmpl& jstmpl) {
  vector<vector<JsonPathComp>> rv;
  vector<JsonPathComp> curpath;
  auto recur = [&](const JsonTmpl& curtmpl, auto&& recur) {
    if(curtmpl.holdsString()) return;
    else if(curtmpl.holdsEllipsis())
      Bug("Ellipsis should have been desugared away"
          " before generating structs.");
    else if(auto* p = curtmpl.getIfPlaceholder()) {
      if(contains(placeholders, p->key)) rv.push_back(curpath);
    }
    else if(auto* v = curtmpl.getIfVector()) {
      for(ssize_t i=0; i<ssize(*v); ++i) {
        curpath.push_back(i);
        recur(v->at(i), recur);
        curpath.pop_back();
      }
    }
    else if(auto* m = curtmpl.getIfMap()) {
      for(auto& [k,v] : *m) {
        curpath.push_back(k);
        recur(v, recur);
        curpath.pop_back();
      }
    }
  };
  recur(jstmpl, recur);
  return rv;
}

static void
genOptionalTemplateField(string_view dest, string_view first_comp,
                         vector<JsonPathComp> path,
                         const OutputStream& cppos) {
  // Convert path to a C++ field.<...> accessor.
  string struct_field{first_comp};
  for(auto& c : path) {
    if(c.key.empty()) struct_field += format("[{}]", c.pos);
    else struct_field += "." + c.key;
  }

  // The new_value parameter for mapAppendNestedMap().
  string_view inner_key = path.back().key;
  if(inner_key.empty()) Bug("Cannot suppress field not directly in a map");

  // The new_key parameter for mapAppendNestedMap().
  // The last key is already part of inner_key above.
  string output_path = "{";
  for(ssize_t i=0; i+1 < ssize(path); ++i) {
    if(path[i].key.empty()) output_path += std::to_string(path[i].pos);
    else output_path += dquoted(path[i].key);
    if(i+2 < ssize(path)) output_path += ", ";
  }
  output_path += "}";

  // TODO: Instead of an asterisk, use ParserResultTraits::get_value_tmpl.
  // Requires us to know schema_source, though.
  cppos(format("  if(!holdsErrorValue({}))\n", struct_field));
  cppos(format("    mapNestedAppend({}, {}, {}, JsonLoc(*{}));\n",
               dest, output_path, dquoted(inner_key), struct_field));
}

static void
genOutputTmplTypeDefinition(const RuleSet& ruleset, const OutputTmpl& out,
    const OutputStream& cppos, const OutputStream& hos) {
  if(out.nameOrNull() == nullptr) return;
  string className = parserResultName(ruleset, out);
  hos("struct " + className + " {\n");
  hos("  oalex::LocPair loc;\n");
  hos("  ");
    genOutputFields(out.outputTmpl, Ident::parseGenerated("fields"),
                    ruleset, out.childidx, 2, hos);
    hos("\n");
  hos("  explicit operator oalex::JsonLoc() const;\n");
  hos("};\n\n");

  vector<string> optIdents = getOptionalFields(ruleset, out.childidx);
  vector<vector<JsonPathComp>> optPaths
    = getPlaceholderPaths(optIdents, out.outputTmpl);
  cppos(format("{}::operator JsonLoc() const {{\n", className));
  if(!optPaths.empty())
    cppos("  using oalex::holdsErrorValue;\n");
  cppos("  using oalex::toJsonLoc;\n");
  cppos("  auto rv = JsonLoc::withPos(");
    genFieldConversion(out.outputTmpl, "fields", optIdents, cppos, 2);
    cppos(", loc.first, loc.second);\n");
  cppos("\n");
  for(auto& optPath : optPaths)
    genOptionalTemplateField("rv", "fields", optPath, cppos);
  if(!optPaths.empty()) cppos("\n");
  cppos("  return rv;\n");
  cppos("}\n\n");
}

static void
genTypeDefinition(const RuleSet& ruleset, ssize_t ruleIndex,
                  const OutputStream& cppos, const OutputStream& hos) {
  const Rule& r = ruleAt(ruleset, ruleIndex);
  if(auto* out = dynamic_cast<const OutputTmpl*>(&r))
    genOutputTmplTypeDefinition(ruleset, *out, cppos, hos);
  else if(resultFlattenableOrError(ruleset, ruleIndex))
    genFlatTypeDefinition(ruleset, ruleIndex, cppos, hos);
  else if(flatWrapperTarget(r) != -1
          && returnsGeneratedStruct(ruleset, ruleIndex)) {
    // Forward declaration, possibly redundant.
    hos(format("struct {};\n", parserResultTraits(ruleset, ruleIndex).type));
  }
}

// TODO make OutputStream directly accept format() strings. Perhaps with
// an OutputStream::unfmt() for brace-heavy output. Additionally, figure out
// nested formatters.  I.e.
//   format("Hello {}", anotherFormattedStringProducer());
void
codegen(const RuleSet& ruleset, ssize_t ruleIndex,
        const OutputStream& cppos, const OutputStream& hos) {
  const Rule& r = ruleAt(ruleset, ruleIndex);
  if(r.nameOrNull() == nullptr) Bug("Cannot codegen for unnamed rules");
  Ident rname = *r.nameOrNull();

  if(auto* ext = dynamic_cast<const ExternParser*>(&r)) {
    genExternDeclaration(hos, ext->externalName(),  ext->params().size());
  }
  genTypeDefinition(ruleset, ruleIndex, cppos, hos);
  genMergeHelpers(ruleset, ruleIndex, cppos);
  parserHeaders(ruleset, ruleIndex, cppos, hos); cppos("{\n");
  if(auto* s = dynamic_cast<const StringRule*>(&r)) {
    cppos(format("  return oalex::match(ctx, i, {});\n", dquoted(s->val)));
  }else if(auto* wp = dynamic_cast<const WordPreserving*>(&r)) {
    codegen(ruleset, *wp, cppos);
  }else if(auto* regex = dynamic_cast<const RegexRule*>(&r)) {
    codegen(ruleset, *regex, cppos);
  }else if(auto* sp = dynamic_cast<const SkipPoint*>(&r)) {
    codegen(ruleset, *sp, cppos);
  }else if(auto* seq = dynamic_cast<const ConcatFlatRule*>(&r)) {
    // TODO suppress hos output, produce static forward decls.
    codegen(ruleset, *seq, cppos);
  }else if(auto* out = dynamic_cast<const OutputTmpl*>(&r)) {
    codegen(ruleset, *out, cppos);
  }else if(auto* loop = dynamic_cast<const LoopRule*>(&r)) {
    codegen(ruleset, *loop, cppos);
  }else if(auto* ext = dynamic_cast<const ExternParser*>(&r)) {
    codegen(ruleset, *ext, cppos);
  }else if(auto* err = dynamic_cast<const ErrorRule*>(&r)) {
    codegen(*err, cppos);
  }else if(auto* qm = dynamic_cast<const QuietMatch*>(&r)) {
    codegen(ruleset, *qm, cppos);
  }else if(auto* ors = dynamic_cast<const OrRule*>(&r)) {
    codegen(ruleset, *ors, cppos);
  }else if(auto* me = dynamic_cast<const MatchOrError*>(&r)) {
    codegen(ruleset, *me, cppos);
  }else if(auto* alias = dynamic_cast<const AliasRule*>(&r)) {
    codegen(ruleset, *alias, cppos);
  // TODO Implement errors, error-recovery scanner.
  }else Bug("Unknown rule type {} in codegen()", r.specifics_typename());
  cppos("}\n");
}

}  // namespace oalex
