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
#include <functional>
#include <map>
#include <memory>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>
#include "oalex.h"
#include "fmt/core.h"
using fmt::format;
using oalex::is_in;
using oalex::Regex;
using oalex::RegexOptions;
using std::exchange;
using std::function;
using std::get_if;
using std::holds_alternative;
using std::map;
using std::string;
using std::string_view;
using std::unique_ptr;
using std::vector;

namespace oalex {

// eval()
// ------

static JsonLoc
skip(InputDiags& ctx, ssize_t& i, const SkipPoint& sp) {
  const Input& input = ctx.input;
  const ssize_t oldi = i;
  i = sp.stayWithinLine ? sp.skip->withinLine(input, i)
                        : sp.skip->acrossLines(input, i);
  if(i == ssize_t(string::npos)) {
    ssize_t com = sp.skip->whitespace(input, oldi);
    if(!ctx.input.sizeGt(com)) Bug("skipper returned npos without a comment");
    Error(ctx, com, "Unfinished comment");
    return JsonLoc::ErrorValue{};
  } else return JsonLoc::Map();  // Just something non-error and flattenable.
}

// TODO use std::source_locatio when moving to C++20.
static const Rule&
ruleAtImpl(const RuleSet& rs, ssize_t ruleidx, const char* context) {
  const unique_ptr<Rule>& r = rs.rules.at(ruleidx);
  if(r) return *r;
  else Bug("{}: Dereferencing null rule {}", context, ruleidx);
}
#define ruleAt(rs, idx) ruleAtImpl(rs, idx, __func__)

static bool
resultFlattenableOrError(const RuleSet& rs, ssize_t ruleidx) {
  const Rule& rule = ruleAt(rs, ruleidx);
  if(auto* orRule = dynamic_cast<const OrRule*>(&rule))
    return orRule->flattenOnDemand;
  if(auto* mor = dynamic_cast<const MatchOrError*>(&rule))
    return resultFlattenableOrError(rs, mor->compidx);
  if(auto* qm = dynamic_cast<const QuietMatch*>(&rule))
    return resultFlattenableOrError(rs, qm->compidx);
  else {
    auto& t = typeid(rule);
    return t == typeid(ConcatFlatRule) || t == typeid(LoopRule)
        || t == typeid(ErrorRule) || t == typeid(SkipPoint);
  }
  // FIXME fix OrList
}

static string
ruleDebugId(const RuleSet& rs, ssize_t i) {
  if(const Ident* opt = ruleAt(rs, i).nameOrNull())
    return format("rule '{}'", opt->preserveCase());
  else return format("rule {}", i);
}

static JsonLoc evalQuiet(const Input& input, ssize_t& i,
                         const RuleSet& rs, ssize_t ruleIndex);

static JsonLoc
eval(InputDiags& ctx, ssize_t& i,
     const ConcatFlatRule& seq, const RuleSet& rs) {
  JsonLoc::Map rv;
  ssize_t j = i;
  for(auto& [idx, outname] : seq.comps) {
    JsonLoc out = eval(ctx, j, rs, idx);
    if(out.holdsError()) return out;
    else if(resultFlattenableOrError(rs,idx)) {
      auto* m = get_if<JsonLoc::Map>(&out);
      if(!m) Bug("Child {} was expected to return a map, got: {}",
                 ruleDebugId(rs, idx), out);
      if(!outname.empty())
        Bug("Flattenable children are not supposed to have names");
      mapUnion(rv, *m);
    }
    else if(!outname.empty()) rv.insert({std::move(outname), std::move(out)});
  }
  i = j;
  return rv;
}

static JsonLoc
eval(InputDiags& ctx, ssize_t& i, const ConcatRule& seq, const RuleSet& rs) {
  JsonLoc rv = seq.outputTmpl;
  JsonLoc::PlaceholderMap pmap = rv.allPlaceholders();
  ssize_t j = i;
  for(auto& [idx, outname] : seq.comps) {
    // TODO move this into substitute in the common case.
    JsonLoc out = eval(ctx, j, rs, idx);
    if(out.holdsError()) return out;
    if(!outname.empty()) rv.substitute(pmap, outname, out);
  }
  for(auto& [id, jsloc] : pmap)
    if(holds_alternative<JsonLoc::Placeholder>(*jsloc))
      Bug("Undefined field '{}', should have been caught by frontend", id);
  i = j;
  return rv;
}

static JsonLoc
eval(InputDiags& ctx, ssize_t& i, const OutputTmpl& out, const RuleSet& rs) {
  JsonLoc outfields = eval(ctx, i, rs, out.childidx);
  if(outfields.holdsError()) return outfields;
  if(out.outputTmpl.substitutionsOk()) return out.outputTmpl;
  JsonLoc::Map container;
  auto* m = get_if<JsonLoc::Map>(&outfields);
  if(!m) {
    if(out.childName.empty())
      Bug("OutputTmpl child must be named, or they need to produce a Map");
    container.insert({out.childName, std::move(outfields)});
    m = &container;
  }
  JsonLoc rv = out.outputTmpl;
  JsonLoc::PlaceholderMap pmap = rv.allPlaceholders();
  for(auto& [id, jsloc] : pmap)
    rv.substitute(pmap, id, moveEltOrEmpty(*m, id));
  return rv;
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
    JsonLoc::Map::iterator it = rv.find(name);
    if(it == rv.end()) {
      if(maxsize > 1) Unimplemented("Late addition needs static key collation");
      it = rv.insert({name, JsonLoc::Vector{}}).first;
    }
    auto v = get_if<JsonLoc::Vector>(&it->second);
    if(!v) Bug("All elements should be vectors");
    v->push_back(std::move(val));
    maxsize = std::max(maxsize, ssize(*v));
  };
  auto recordComponent =
    [&addChild, &rs](string_view desc, JsonLoc comp,
                     ssize_t idx, const string& defname) {
    if(resultFlattenableOrError(rs, idx)) {
      // TODO refactor out this validation between here and ConcatFlatRule.
      auto* m = get_if<JsonLoc::Map>(&comp);
      if(!m) Bug("LoopRule {} {} was expected to return a map, got {}",
                 desc, ruleDebugId(rs, idx), comp);
      for(auto& [k,v] : *m) addChild(k, std::move(v));
    }else if(!defname.empty()) addChild(defname, std::move(comp));
    // else ignore component.
  };

  // Unlike other rules, our fallback_point is different j because we need may
  // need to discard even successful SkipPoints if the next component fails.
  ssize_t j = i, fallback_point = i;
  bool first = true;
  while(true) {
    JsonLoc out = (first || loop.glueidx != -1)
                    ? eval(ctx, j, rs, loop.partidx)
                    : evalQuiet(ctx.input, j, rs, loop.partidx);
    if(out.holdsError()) {
      if(loop.glueidx == -1 && !first) break;
      else return out;
    }else {
      recordComponent("child", std::move(out), loop.partidx, loop.partname);
      fallback_point = j;
    }
    if(sp && evalQuiet(ctx.input, j, rs, loop.skipidx).holdsError()) break;
    if(loop.glueidx != -1) {
      JsonLoc out = evalQuiet(ctx.input, j, rs, loop.glueidx);
      if(out.holdsError()) break;
      else recordComponent("glue", std::move(out), loop.glueidx, loop.gluename);
      if(sp) {
        out = skip(ctx, j, *sp);
        if(out.holdsError()) return out;
      }
    }
    first = false;
  }
  i = fallback_point;  // discard any failures and SkipPoints.
  return rv;
}

static JsonLoc
substituteOnePlaceholder(JsonLoc tmpl, string_view key, const JsonLoc& value) {
  ssize_t count = tmpl.substitute(tmpl.allPlaceholders(), key, value);
  if(count > 1) Bug("OrRule wasn't expected to have more than one child");
  return tmpl;  // Assumes substitutionsOk();
}

// Defined in parser_helpers.cpp, but intentionally not exposed in header.
InputDiags substrProxy(const Input& input, ssize_t i);

// TODO write a proper resemblance-checker. See the rant in parser_helpers.cpp.
static JsonLoc evalQuiet(const Input& input, ssize_t& i,
                         const RuleSet& rs, ssize_t ruleIndex) {
  InputDiags proxy = substrProxy(input, i);
  ssize_t pos = 0;
  JsonLoc res = eval(proxy, pos, rs, ruleIndex);
  if(!res.holdsError()) i += pos;
  return res;
}

static bool
evalPeek(const Input& input, ssize_t i, const RuleSet& rs, ssize_t ruleIndex) {
  return !evalQuiet(input, i, rs, ruleIndex).holdsError();
}

static bool
orBranchFlattenableOrError(const RuleSet& rs, ssize_t partidx,
                           const JsonLoc& tmpl) {
  if(holds_alternative<JsonLoc::Map>(tmpl)) return true;
  if(holds_alternative<JsonLoc::ErrorValue>(tmpl)) return true;
  return isPassthroughTmpl(tmpl) && resultFlattenableOrError(rs, partidx);
}

// TODO we need an `eval test -v` that produces verbose debug logs.
static JsonLoc
eval(InputDiags& ctx, ssize_t& i, const OrRule& ors, const RuleSet& rs) {
  if(ors.comps.empty()) Bug("Found an empty OrList RuleSet");
  JsonLoc out{JsonLoc::ErrorValue{}};
  for(auto& [lidx, pidx, tmpl]: ors.comps) {
    if(ors.flattenOnDemand && !orBranchFlattenableOrError(rs,pidx,tmpl))
      Bug("OrRule branch {} does not produce a map", ruleDebugId(rs, pidx));
    if(lidx != -1 && !evalPeek(ctx.input, i, rs, lidx)) continue;
    out = eval(ctx, i, rs, pidx);
    if(!out.holdsError()) return substituteOnePlaceholder(tmpl, "child", out);

    // If we passed evalPeek(), don't try anything else.
    if(lidx != -1) return out;
  }
  return out;  // Return the last error.
}

static JsonLoc
eval(InputDiags& ctx, ssize_t& i, const MatchOrError& me, const RuleSet& rs) {
  ssize_t oldi = i;
  JsonLoc out = eval(ctx, i, rs, me.compidx);
  if(out.holdsError()) Error(ctx, oldi, i, me.errmsg);
  return out;
}

// TODO recheck if we are properly initializing
// rv.stPos and rv.enPos in all cases.
JsonLoc
eval(InputDiags& ctx, ssize_t& i, const RuleSet& ruleset, ssize_t ruleIndex) {
  const Rule& r = ruleAt(ruleset, ruleIndex);
  if(auto* s = dynamic_cast<const StringRule*>(&r))
    return match(ctx, i, s->val);
  else if(auto* wp = dynamic_cast<const WordPreserving*>(&r))
    return match(ctx, i, ruleset.regexOpts.word, **wp);
  else if(dynamic_cast<const ExternParser*>(&r))  // use dlopen() someday
    UserError("eval() doesn't support 'extern' parsers");
  else if(auto* sp = dynamic_cast<const SkipPoint*>(&r))
    return skip(ctx, i, *sp);
  else if(auto* regex = dynamic_cast<const RegexRule*>(&r))
    return match(ctx, i, *regex->patt, ruleset.regexOpts);
  else if(auto* seq = dynamic_cast<const ConcatRule*>(&r))
    return eval(ctx, i, *seq, ruleset);
  else if(auto* seq = dynamic_cast<const ConcatFlatRule*>(&r))
    return eval(ctx, i, *seq, ruleset);
  else if(auto* out = dynamic_cast<const OutputTmpl*>(&r))
    return eval(ctx, i, *out, ruleset);
  else if(auto* loop = dynamic_cast<const LoopRule*>(&r))
    return eval(ctx, i, *loop, ruleset);
  else if(auto* err = dynamic_cast<const ErrorRule*>(&r)) {
    if(err->msg.empty()) return JsonLoc::ErrorValue{};
    else return errorValue(ctx, i, err->msg);
  }else if(auto* qm = dynamic_cast<const QuietMatch*>(&r))
    return evalQuiet(ctx.input, i, ruleset, qm->compidx);
  else if(auto* ors = dynamic_cast<const OrRule*>(&r))
    return eval(ctx, i, *ors, ruleset);
  else if(auto* me = dynamic_cast<const MatchOrError*>(&r))
    return eval(ctx, i, *me, ruleset);
  Bug("Unknown rule type {} in eval", r.specifics_typename());
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

static void linebreak(const OutputStream& cppos, ssize_t indent) {
  cppos("\n" + string(indent, ' '));
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

template <class T, class Cb> static void
genMakeVector(const string& eltType, const vector<T>& vec,
              Cb genElt, function<void()> br,
              const OutputStream& cppos) {
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

void
codegenDefaultRegexOptions(const RuleSet& ruleset, const OutputStream& cppos) {
  cppos("const oalex::RegexOptions& defaultRegexOpts() {\n");
  cppos("  using oalex::RegexCharSet;\n");
  cppos("  using oalex::RegexOptions;\n");
  cppos("  static const RegexOptions *opts = new RegexOptions{.word =\n  ");
  genRegexCharSet(ruleset.regexOpts.word, cppos, 4);
  cppos("\n  };\n");
  cppos("  return *opts;\n");
  cppos("}\n");
}

static string
parserName(const Ident& rname) {
  return "parse" + rname.toUCamelCase();
}

static void
parserHeaders(const Ident& rname,
              const OutputStream& cppos, const OutputStream& hos) {
  hos(format("oalex::JsonLoc {}(oalex::InputDiags& ctx, ssize_t& i);\n",
             parserName(rname)));

  // TODO complex parsers should have comments with the source line.
  cppos(format("oalex::JsonLoc {}(oalex::InputDiags& ctx, "
                                      "ssize_t& i) ",
               parserName(rname)));
}

static void
codegen(const RegexRule& regex, const OutputStream& cppos) {
  // TODO trim this down whenever possible.
  cppos("  using oalex::makeVector;\n");
  cppos("  using oalex::move_to_unique;\n");
  cppos("  using oalex::Regex;\n");
  cppos("  using oalex::RegexAnchor;\n");
  cppos("  using oalex::RegexCharSet;\n");
  cppos("  using oalex::RegexConcat;\n");
  cppos("  using oalex::RegexOptional;\n");
  cppos("  using oalex::RegexOptions;\n");
  cppos("  using oalex::RegexOrList;\n");
  cppos("  using oalex::RegexRepeat;\n");
  cppos("  using oalex::RegexString;\n");
  cppos("  using std::literals::string_literals::operator\"\"s;\n");
  cppos("  using std::unique_ptr;\n");
  cppos("  static const Regex *r = new ");
  genRegexComponents(*regex.patt, cppos, 4);
  cppos(";\n");
  cppos("  return oalex::match(ctx, i, *r, defaultRegexOpts());\n");
}

static void
codegen(const JsonLoc& jsloc, const OutputStream& cppos,
        const map<string,string>& placeholders, ssize_t indent) {
  auto br = [&]() { linebreak(cppos, indent); };
  if(auto* p = get_if<JsonLoc::Placeholder>(&jsloc)) {
    auto v = placeholders.find(p->key);
    if(v == placeholders.end())
      Bug("Undefined placeholder in codegen: {}", p->key);
    cppos(v->second);
  }else if(auto* s = get_if<JsonLoc::String>(&jsloc)) {
    cppos(format("{}s", dquoted(*s)));
  }else if(auto* v = get_if<JsonLoc::Vector>(&jsloc)) {
    cppos("JsonLoc::Vector{");
    genMakeVector("JsonLoc", *v, [&](auto& child) {
                   codegen(child, cppos, placeholders, indent+2);
                 }, br, cppos);
    cppos("}");
  }else if(auto* m = get_if<JsonLoc::Map>(&jsloc)) {
    cppos("JsonLoc::Map{"); br();
    for(const auto& [k, v] : *m) {
      cppos(format("  {{{}, ", dquoted(k)));
      codegen(v, cppos, placeholders, indent+4);
      cppos("},"); br();
    }
    cppos("}");
  }else cppos("JsonLoc::ErrorValue{}");
}

static bool
hasEmptyPlaceholder(const vector<ConcatRule::Component>& comps) {
  for(auto& comp : comps) if(comp.outputPlaceholder.empty()) return true;
  return false;
}

static void
codegen(const RuleSet& ruleset, const ConcatFlatRule& cfrule,
        const OutputStream& cppos) {
  cppos("  using oalex::JsonLoc;\n");
  cppos("  ssize_t j = i;\n\n");
  cppos("  JsonLoc::Map m;\n");
  cppos("  JsonLoc res = JsonLoc::ErrorValue{};\n");
  for(auto& [childid, key] : cfrule.comps) {
    cppos("\n  res = ");
      codegenParserCall(ruleAt(ruleset, childid), "j", cppos);
      cppos(";\n");
    cppos("  if(res.holdsError()) return res;\n");
    // TODO Check for duplicate keys at compile-time.
    if(resultFlattenableOrError(ruleset,childid))
      cppos("  oalex::mapUnion(m, *oalex::get_if<JsonLoc::Map>(&res));\n");
    else if(!key.empty())
      cppos(format("  m.insert({{{}, std::move(res)}});\n", dquoted(key)));
  }
  cppos("\n  JsonLoc rv{std::move(m)};\n");
  cppos("  rv.stPos = i; rv.enPos = j;\n");
  cppos("  i = j;\n");
  cppos("  return rv;\n");
}

static void
codegen(const RuleSet& ruleset, const ConcatRule& concatRule,
        const OutputStream& cppos) {
  cppos("  using oalex::JsonLoc;\n");
  cppos("  ssize_t j = i;\n\n");
  map<string,string> placeholders;
  if(hasEmptyPlaceholder(concatRule.comps))
    cppos("  JsonLoc res{JsonLoc::ErrorValue{}};\n");
  for(auto& comp : concatRule.comps) {
    const string resvar = "res" + comp.outputPlaceholder;
    const char* decl = comp.outputPlaceholder.empty() ? "" : "JsonLoc ";

    if(!comp.outputPlaceholder.empty() &&
       !placeholders.insert({comp.outputPlaceholder,
                             format("std::move({})", resvar)}).second)
      // This uniqueness is also used in codegen(JsonLoc).
      Bug("Duplicate placeholders at codegen: ", comp.outputPlaceholder);

    cppos(format("\n  {}{} = ", decl, resvar));
      codegenParserCall(ruleAt(ruleset, comp.idx), "j", cppos);
      cppos(";\n");
    cppos(format("  if({0}.holdsError()) return {0};\n", resvar));
  }
  cppos("\n  i = j;\n");
  // TODO add source location.
  cppos("  return ");
    codegen(concatRule.outputTmpl, cppos, placeholders, 2);
    cppos(";\n");
}

// TODO Make it possible to figure out at compile-time whether a rule produces
// a JsonLoc::Map. E.g. by having OrRule also autobox its non-map branches.
// TODO Once we can figure out at compile-time whether something produces a
// Map, and reflect that in the generated code. E.g. by returning the concrete
// type.
static void
codegen(const RuleSet& ruleset, const OutputTmpl& out,
        const OutputStream& cppos) {
  cppos("  using oalex::get_if;\n");
  cppos("  using oalex::JsonLoc;\n");
  cppos("  using oalex::moveEltOrEmpty;\n");
  cppos("  JsonLoc outfields = ");
    codegenParserCall(ruleAt(ruleset, out.childidx), "i", cppos);
    cppos(";\n");
  cppos("  if(outfields.holdsError()) return outfields;\n");
  map<string,string> placeholders;
  // Dev-note: we only produce Bug() if reaching a given control path indicates
  // a bug in the code *generator*.
  if(!out.outputTmpl.substitutionsOk()) {
    cppos("  auto* m = get_if<JsonLoc::Map>(&outfields);\n");
    if(out.childName.empty())
      cppos("  if(m == nullptr) oalex::Bug(\"{} needs a map\", __func__);\n");
    else {
      cppos("  if(m == nullptr) {\n");
      cppos("    return ");
        codegen(out.outputTmpl, cppos,
                map<string,string>{{out.childName, "std::move(outfields)"}}, 4);
        cppos(";\n");
      cppos("  }\n");
    }
    for(auto& [key, jsloc] : out.outputTmpl.allPlaceholders())
      placeholders.insert({key, format("moveEltOrEmpty(*m, {})",
                                       dquoted(key))});
  }
  cppos("  return ");
    codegen(out.outputTmpl, cppos, placeholders, 2);
    cppos(";\n");
}

static void
codegen(const RuleSet& ruleset, const LoopRule& loop,
        const OutputStream& cppos) {
  if(loop.lookidx != -1) Unimplemented("LoopRule lookahead");
  auto recordComponent =
    [&ruleset, &cppos](ssize_t idx, const string& name) {
      if(resultFlattenableOrError(ruleset,idx)) {
        cppos("    mapCreateOrAppendAllElts(m,\n");
        cppos("      std::move(*oalex::get_if<JsonLoc::Map>(&res)), first);\n");
      }else if(!name.empty()) {
        cppos(format("    mapCreateOrAppend(m, {}, std::move(res), first);\n",
                     dquoted(name)));
      }// else drop this component.
    };

  Ident skipname;
  if(loop.skipidx != -1) {
    if(!dynamic_cast<const SkipPoint*>(&ruleAt(ruleset, loop.skipidx)))
      Bug("LoopRule skipidx {} is not a SkipPoint rule",
          ruleDebugId(ruleset, loop.skipidx));
    if(auto name = ruleAt(ruleset, loop.skipidx).nameOrNull()) skipname = *name;
    else Bug("LoopRule skipidx {} rule needs a name",
             ruleDebugId(ruleset, loop.skipidx));
  }

  cppos("  using oalex::JsonLoc;\n");
  cppos("  using oalex::mapCreateOrAppend;\n");
  cppos("  using oalex::mapCreateOrAppendAllElts;\n");
  cppos("  using oalex::quietMatch;\n");
  cppos("  ssize_t j = i, fallback_point = i;\n\n");
  cppos("  JsonLoc::Map m;\n");
  cppos("  bool first = true;\n");
  cppos("  while(true) {\n");
  cppos("    JsonLoc res = JsonLoc::ErrorValue{};\n\n");

  if(loop.glueidx == -1) {
    // TODO resolve this `first` case at compile-time.
    cppos("    if(first) res = ");
      codegenParserCall(ruleAt(ruleset, loop.partidx), "j", cppos);
      cppos(";\n");
    cppos(format("    else res = quietMatch(ctx.input, j, {});\n",
                 parserName(*ruleAt(ruleset, loop.partidx).nameOrNull())));
    cppos("    if(res.holdsError()) {\n");
    cppos("      if(first) return res;\n");
    cppos("      else break;\n");
    cppos("    }\n");
  }else {
    cppos("    res = ");
      codegenParserCall(ruleAt(ruleset, loop.partidx), "j", cppos);
      cppos(";\n");
    cppos("    if(res.holdsError()) return res;\n");
  }
  recordComponent(loop.partidx, loop.partname);

  cppos("    fallback_point = j;\n");
  cppos("\n");
  if(loop.skipidx != -1) {
    cppos(format("    res = quietMatch(ctx.input, j, {});\n",
                 parserName(skipname)));
    cppos("    if(res.holdsError()) break;\n");
  }
  if(loop.glueidx != -1) {
    if(auto gluename = ruleAt(ruleset, loop.glueidx).nameOrNull())
      cppos(format("    res = quietMatch(ctx.input, j, {});\n",
                   parserName(*gluename)));
    else Bug("Glue rules need a name for codegen(LoopRule)");
    cppos("    if(res.holdsError()) break;\n");
    recordComponent(loop.glueidx, loop.gluename);
    if(loop.skipidx != -1) {
      cppos("    res = ");
        codegenParserCall(ruleAt(ruleset, loop.skipidx), "j", cppos);
        cppos(";\n");
      cppos("    if(res.holdsError())\n");
      cppos("      return oalex::errorValue(ctx, j, "
                                            "\"Unfinished comment\");\n");
    }
  }
  cppos("    first = false;\n");
  cppos("  }\n");
  cppos("  JsonLoc rv{std::move(m)};\n");
  cppos("  rv.stPos = i; rv.enPos = fallback_point;\n");
  cppos("  i = fallback_point;\n");
  cppos("  return rv;\n");
}

static void
codegen(const ErrorRule& errRule, const OutputStream& cppos) {
  if(errRule.msg.empty()) cppos("  return JsonLoc::ErrorValue{};\n");
  else cppos(format("  return oalex::errorValue(ctx, i, {});\n",
                    dquoted(errRule.msg)));
}

static void
codegen(const RuleSet& ruleset, const QuietMatch& qm,
        const OutputStream& cppos) {
  if(const Ident* name = ruleAt(ruleset, qm.compidx).nameOrNull())
    cppos(format("  return oalex::quietMatch(ctx.input, i, {});\n",
                 parserName(*name)));
  else Bug("QuietMatch::compidx targets need to have names");
}

static void
codegenLookahead(const RuleSet& ruleset, ssize_t lidx,
                 const OutputStream& cppos) {
  const Rule& rule = ruleAt(ruleset, lidx);
  if(auto* s = dynamic_cast<const StringRule*>(&rule))
    cppos(format("ctx.input.hasPrefix(i, {})", dquoted(s->val)));
  else if(auto* wp = dynamic_cast<const WordPreserving*>(&rule))
    cppos(format("oalex::peekMatch(ctx, i, defaultRegexOpts().word, {})",
                 dquoted(**wp)));
  // When adding a new branch here, remember to change StringRule::needsName().
  else {
    if(const Ident* name = rule.nameOrNull())
      cppos(format("oalex::peekMatch(ctx.input, i, {})", parserName(*name)));
    else Bug("The frontend must always name lookidx for {} rules",
             rule.specifics_typename());
  }
}

static void
codegenReturnErrorOrTmpl(string_view resvar, const JsonLoc& tmpl,
                         const OutputStream& cppos) {
  if(isPassthroughTmpl(tmpl)) {
    cppos(format("    return {};\n", resvar));
  }else {
    cppos(format("    if(!{}.holdsError()) return ", resvar));
      codegen(tmpl, cppos, {{"child", string(resvar)}}, 4);
      cppos(";\n");
    cppos(format("    else return {};\n", resvar));
  }
}

static void
codegen(const RuleSet& ruleset, const OrRule& orRule,
        const OutputStream& cppos) {
  cppos("  using std::literals::string_literals::operator\"\"s;\n");
  cppos("  JsonLoc res{JsonLoc::ErrorValue{}};\n");
  for(auto& [lidx, pidx, tmpl] : orRule.comps) {
    // Frontend should make sure even string-producing rules are
    // wrapped in empty maps.
    if(orRule.flattenOnDemand && !orBranchFlattenableOrError(ruleset,pidx,tmpl))
      Bug("OrRule branch {} does not produce a map",
          ruleDebugId(ruleset, pidx));
    if(lidx == -1) {
      cppos("  res = ");
        codegenParserCall(ruleAt(ruleset, pidx), "i", cppos);
        cppos(";\n");
      cppos("  if(!res.holdsError()) return ");
        codegen(tmpl, cppos, {{"child", "res"}}, 2);
        cppos(";\n");
    }else {
      cppos("  if(");
        codegenLookahead(ruleset, lidx, cppos);
        cppos(") {\n");
      cppos("    res = ");
        codegenParserCall(ruleAt(ruleset, pidx), "i", cppos);
        cppos(";\n");
      codegenReturnErrorOrTmpl("res", tmpl, cppos);
      cppos("  }\n");
    }
  }
  cppos("  return res;\n");
}

static void
codegen(const RuleSet& ruleset, const MatchOrError& me,
        const OutputStream& cppos) {
  cppos("  using oalex::Error;\n");
  cppos("  JsonLoc  res = ");
    codegenParserCall(ruleAt(ruleset, me.compidx), "i", cppos);
    cppos(";\n");
  cppos("  if(res.holdsError())\n");
  cppos(format("    Error(ctx, i, {});\n", dquoted(me.errmsg)));
  cppos("  return res;\n");
}

static void
codegen(const SkipPoint& sp, const OutputStream& cppos) {
  cppos("  using oalex::Skipper;\n");
  cppos("  static Skipper* skip = new Skipper{\n");
  cppos("    .unnestedComments{\n");
  for(auto& [st,en] : sp.skip->unnestedComments)
    cppos(format("     {{ {}, {} }},\n", dquoted(st), dquoted(en)));
  cppos("    },\n");
  if(sp.skip->nestedComment.has_value()) {
    auto& [st,en] = *sp.skip->nestedComment;
    cppos(format("    .nestedComment{{ {{ {}, {} }} }},\n",
                 dquoted(st), dquoted(en)));
  }else {
    cppos("    .nestedComment{}, \n");
  }
  cppos(format("    .indicateBlankLines = {},\n",
               alphabool(sp.skip->indicateBlankLines)));
  cppos("  };\n");
  if(sp.stayWithinLine)
    cppos("  ssize_t j = skip->withinLine(ctx.input, i);\n");
  else cppos("  ssize_t j = skip->acrossLines(ctx.input, i);\n");
  cppos("  if (static_cast<size_t>(j) != oalex::Input::npos) {\n");
  cppos("    i = j;\n");
  cppos("    return oalex::JsonLoc::Map();  // dummy non-error value\n");
  cppos("  }else return oalex::JsonLoc::ErrorValue();\n");
}

static void
codegen(const WordPreserving& wp, const OutputStream& cppos) {
  cppos(format("  return oalex::match(ctx, i, defaultRegexOpts().word, {});\n",
               dquoted(*wp)));
}

void
Rule::deferred_name(Ident name) {
  if(name_) Bug("Cannot rename rule {} to {}",
                name_.preserveCase(), name.preserveCase());
  name_ = name;
}

bool needsName(const Rule& rule, bool isTentativeTarget) {
  if(auto* rvar = dynamic_cast<const StringRule*>(&rule)) {
    if(dynamic_cast<const StringRule*>(rvar) ||
       dynamic_cast<const WordPreserving*>(rvar) ||
       false) return false;
    if(dynamic_cast<const ErrorRule*>(rvar)) return isTentativeTarget;
  }
  return true;
}

// Generate an inlined call to oalex::match() when possible, but falls back to
// the main parser for other cases.
static void
codegenParserCall(const Rule& rule, string_view posVar,
                  const OutputStream& cppos) {
  if(auto* s = dynamic_cast<const StringRule*>(&rule))
    cppos(format("oalex::match(ctx, {}, {})", posVar, dquoted(s->val)));
  else if(auto* wp = dynamic_cast<const WordPreserving*>(&rule))
    cppos(format("oalex::match(ctx, {}, defaultRegexOpts().word, {})",
                 posVar, dquoted(**wp)));
  else if(auto* err = dynamic_cast<const ErrorRule*>(&rule)) {
    if(err->msg.empty()) cppos("JsonLoc::ErrorValue{}");
    else cppos(format("oalex::errorValue(ctx, {}, {})",
                      posVar, dquoted(err->msg)));
  }
  else if(const Ident* rname = rule.nameOrNull()) {
    if(dynamic_cast<const ExternParser*>(&rule))
      cppos(format("{}(ctx, {});", rname->preserveCase(), posVar));
    else cppos(format("{}(ctx, {})", parserName(*rname), posVar));
  }
  // When adding a new branch here, remember to change StringRule::needsName().
  else Unimplemented("nameless component of type {}",
                     rule.specifics_typename());
}

static void
genExternDeclaration(const OutputStream& hos, const Ident& rname) {
  hos(format("extern oalex::JsonLoc {}(oalex::InputDiags& ctx, ssize_t& j);\n",
             rname.preserveCase()));
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

  if(dynamic_cast<const ExternParser*>(&r)) {
    genExternDeclaration(hos, rname);
    return;
  }
  parserHeaders(rname, cppos, hos); cppos("{\n");
  if(auto* s = dynamic_cast<const StringRule*>(&r)) {
    cppos(format("  return oalex::match(ctx, i, {});\n", dquoted(s->val)));
  }else if(auto* wp = dynamic_cast<const WordPreserving*>(&r)) {
    codegen(*wp, cppos);
  }else if(auto* regex = dynamic_cast<const RegexRule*>(&r)) {
    codegen(*regex, cppos);
  }else if(auto* sp = dynamic_cast<const SkipPoint*>(&r)) {
    codegen(*sp, cppos);
  }else if(auto* seq = dynamic_cast<const ConcatRule*>(&r)) {
    codegen(ruleset, *seq, cppos);
  }else if(auto* seq = dynamic_cast<const ConcatFlatRule*>(&r)) {
    // TODO suppress hos output, produce static forward decls.
    codegen(ruleset, *seq, cppos);
  }else if(auto* out = dynamic_cast<const OutputTmpl*>(&r)) {
    codegen(ruleset, *out, cppos);
  }else if(auto* loop = dynamic_cast<const LoopRule*>(&r)) {
    codegen(ruleset, *loop, cppos);
  }else if(auto* err = dynamic_cast<const ErrorRule*>(&r)) {
    codegen(*err, cppos);
  }else if(auto* qm = dynamic_cast<const QuietMatch*>(&r)) {
    codegen(ruleset, *qm, cppos);
  }else if(auto* ors = dynamic_cast<const OrRule*>(&r)) {
    codegen(ruleset, *ors, cppos);
  }else if(auto* me = dynamic_cast<const MatchOrError*>(&r)) {
    codegen(ruleset, *me, cppos);
  // TODO Implement errors, error-recovery scanner.
  }else Bug("Unknown rule type {} in codegen()", r.specifics_typename());
  cppos("}\n");
}

}  // namespace oalex
