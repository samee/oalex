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
using std::optional;
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
  } else return JsonLoc::String();  // Just something non-error.
}

static bool
makesFlattenableMap(const Rule& rule) {
  if(auto* orRule = get_if<OrRule>(&rule)) return orRule->flattenOnDemand;
  else return holds_alternative<ConcatFlatRule>(rule);
}

static string
ruleDebugId(const RuleSet& rs, ssize_t i) {
  if(optional<Ident> opt = rs.rules[i].name())
    return format("rule '{}'", opt->preserveCase());
  else return format("rule {}", i);
}

static JsonLoc
eval(InputDiags& ctx, ssize_t& i,
     const ConcatFlatRule& seq, const RuleSet& rs) {
  JsonLoc::Map rv;
  ssize_t j = i;
  for(auto& [idx, outname] : seq.comps) {
    JsonLoc out = eval(ctx, j, rs, idx);
    if(out.holdsError()) return out;
    else if(makesFlattenableMap(rs.rules[idx])) {
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

static JsonLoc
eval(InputDiags& ctx, ssize_t& i, const OrRule& ors, const RuleSet& rs) {
  if(ors.comps.empty()) Bug("Found an empty OrList RuleSet");
  JsonLoc out{JsonLoc::ErrorValue{}};
  for(auto& [lidx, pidx, tmpl]: ors.comps) {
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

// Using std::visit(), since we want to catch missing types at compile-time.
static string
specifics_typename(const std::monostate&) { return "(uninitialized)"; }
static string
specifics_typename(const string&) { return "string"; }
static string
specifics_typename(const WordPreserving&) { return "WordPreserving"; }
static string
specifics_typename(const ExternParser&) { return "ExternParser"; }
static string
specifics_typename(const unique_ptr<const Regex>&) { return "Regex"; }
static string
specifics_typename(const SkipPoint&) { return "SkipPoint"; }
static string
specifics_typename(const ConcatFlatRule&) { return "ConcatFlatRule"; }
static string
specifics_typename(const ConcatRule&) { return "ConcatRule"; }
static string
specifics_typename(const OutputTmpl&) { return "OutputTmpl"; }
static string
specifics_typename(const ErrorRule&) { return "ErrorRule"; }
static string
specifics_typename(const QuietMatch&) { return "QuietMatch"; }
static string
specifics_typename(const OrRule&) { return "OrRule"; }
static string
specifics_typename(const MatchOrError&) { return "MatchOrError"; }

string
Rule::specifics_typename() const {
  return std::visit([](auto& spec) { return oalex::specifics_typename(spec); },
                    this->specifics_);
}

JsonLoc
eval(InputDiags& ctx, ssize_t& i, const RuleSet& ruleset, ssize_t ruleIndex) {
  const Rule& r = ruleset.rules[ruleIndex];
  if(const string* s = get_if<string>(&r)) return match(ctx, i, *s);
  else if(const auto* wp = get_if<WordPreserving>(&r))
    return match(ctx, i, ruleset.regexOpts.word, **wp);
  else if(holds_alternative<ExternParser>(r))  // use dlopen() someday
    UserError("eval() doesn't support 'extern' parsers");
  else if(const auto* sp = get_if<SkipPoint>(&r)) return skip(ctx, i, *sp);
  else if(const auto* regex = get_if<unique_ptr<const Regex>>(&r))
    return match(ctx, i, **regex, ruleset.regexOpts);
  else if(const auto* seq = get_if<ConcatRule>(&r))
    return eval(ctx, i, *seq, ruleset);
  else if(const auto* seq = get_if<ConcatFlatRule>(&r))
    return eval(ctx, i, *seq, ruleset);
  else if(const auto* out = get_if<OutputTmpl>(&r))
    return eval(ctx, i, *out, ruleset);
  else if(const auto* err = get_if<ErrorRule>(&r)) {
    if(err->msg.empty()) return JsonLoc::ErrorValue{};
    else return errorValue(ctx, i, err->msg);
  }else if(const auto* qm = get_if<QuietMatch>(&r))
    return evalQuiet(ctx.input, i, ruleset, qm->compidx);
  else if(const auto* ors = get_if<OrRule>(&r))
    return eval(ctx, i, *ors, ruleset);
  else if(const auto* me = get_if<MatchOrError>(&r))
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

static void
parserHeaders(const Ident& rname,
              const OutputStream& cppos, const OutputStream& hos) {
  hos(format("oalex::JsonLoc parse{}(oalex::InputDiags& ctx, ssize_t& i);\n",
             rname.toUCamelCase()));

  // TODO complex parsers should have comments with the source line.
  cppos(format("oalex::JsonLoc parse{}(oalex::InputDiags& ctx, "
                                      "ssize_t& i) ",
               rname.toUCamelCase()));
}

static void
codegen(const unique_ptr<const Regex>& regex, const OutputStream& cppos) {
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
  genRegexComponents(*regex, cppos, 4);
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
      codegenParserCall(ruleset.rules[childid], "j", cppos);
      cppos(";\n");
    cppos("  if(res.holdsError()) return res;\n");
    // TODO Check for duplicate keys at compile-time.
    if(makesFlattenableMap(ruleset.rules[childid]))
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
      codegenParserCall(ruleset.rules[comp.idx], "j", cppos);
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
    codegenParserCall(ruleset.rules[out.childidx], "i", cppos);
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
codegen(const ErrorRule& errRule, const OutputStream& cppos) {
  if(errRule.msg.empty()) cppos("  return JsonLoc::ErrorValue{};\n");
  else cppos(format("  return oalex::errorValue(ctx, i, {});\n",
                    dquoted(errRule.msg)));
}

// FIXME these targets need names.
// TODO factor out parser-name synthesis.
static void
codegen(const RuleSet& ruleset, const QuietMatch& qm,
        const OutputStream& cppos) {
  cppos(format("  return oalex::quietMatch(ctx.input, i, parse{});\n",
               ruleset.rules[qm.compidx].name()->toUCamelCase()));
}

// TODO refactor parser name synthesis.
static void
codegenLookahead(const RuleSet& ruleset, ssize_t lidx,
                 const OutputStream& cppos) {
  const Rule& rule = ruleset.rules[lidx];
  if(const auto* s = get_if<string>(&rule))
    cppos(format("ctx.input.hasPrefix(i, {})", dquoted(*s)));
  else if(const auto* wp = get_if<WordPreserving>(&rule))
    cppos(format("oalex::peekMatch(ctx, i, defaultRegexOpts().word, {})",
                 dquoted(**wp)));
  // When adding a new branch here, remember to change Rule::needsName().
  else {
    if(!rule.name().has_value())
      Bug("The frontend must always name lookidx for {} rules",
          rule.specifics_typename());
    cppos(format("oalex::peekMatch(ctx.input, i, parse{})",
                  rule.name()->toUCamelCase()));
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
    if(lidx == -1) {
      cppos("  res = ");
        codegenParserCall(ruleset.rules[pidx], "i", cppos);
        cppos(";\n");
      cppos("  if(!res.holdsError()) return ");
        codegen(tmpl, cppos, {{"child", "res"}}, 2);
        cppos(";\n");
    }else {
      cppos("  if(");
        codegenLookahead(ruleset, lidx, cppos);
        cppos(") {\n");
      cppos("    res = ");
        codegenParserCall(ruleset.rules[pidx], "i", cppos);
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
    codegenParserCall(ruleset.rules[me.compidx], "i", cppos);
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
  cppos("    return oalex::JsonLoc::String();  // dummy non-error value\n");
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

bool
Rule::needsName(bool isTentativeTarget) const {
  if(holds_alternative<std::string>(specifics_) ||
     holds_alternative<WordPreserving>(specifics_) ||
     false) return false;
  if(holds_alternative<ErrorRule>(specifics_)) return isTentativeTarget;
  else return true;
}

// Generate an inlined call to oalex::match() when possible, but falls back to
// the main parser for other cases.
static void
codegenParserCall(const Rule& rule, string_view posVar,
                  const OutputStream& cppos) {
  if(const auto* s = get_if<string>(&rule))
    cppos(format("oalex::match(ctx, {}, {})", posVar, dquoted(*s)));
  else if(const auto* wp = get_if<WordPreserving>(&rule))
    cppos(format("oalex::match(ctx, {}, defaultRegexOpts().word, {})",
                 posVar, dquoted(**wp)));
  else if(const auto* err = get_if<ErrorRule>(&rule)) {
    if(err->msg.empty()) cppos("JsonLoc::ErrorValue{}");
    else cppos(format("oalex::errorValue(ctx, {}, {})",
                      posVar, dquoted(err->msg)));
  }
  else if(optional<Ident> rname = rule.name()) {
    if(holds_alternative<ExternParser>(rule))
      cppos(format("{}(ctx, {});", rname->preserveCase(), posVar));
    else cppos(format("parse{}(ctx, {})", rname->toUCamelCase(), posVar));
  }
  // When adding a new branch here, remember to change Rule::needsName().
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
  const Rule& r = ruleset.rules[ruleIndex];
  if(!r.name().has_value()) Bug("Cannot codegen for unnamed rules");
  Ident rname = *r.name();

  if(holds_alternative<ExternParser>(r)) {
    genExternDeclaration(hos, rname);
    return;
  }
  parserHeaders(rname, cppos, hos); cppos("{\n");
  if(const auto* s = get_if<string>(&r)) {
    cppos(format("  return oalex::match(ctx, i, {});\n", dquoted(*s)));
  }else if(const auto* wp = get_if<WordPreserving>(&r)) {
    codegen(*wp, cppos);
  }else if(const auto* regex = get_if<unique_ptr<const Regex>>(&r)) {
    codegen(*regex, cppos);
  }else if(const auto* sp = get_if<SkipPoint>(&r)) {
    codegen(*sp, cppos);
  }else if(const auto* seq = get_if<ConcatRule>(&r)) {
    codegen(ruleset, *seq, cppos);
  }else if(const auto* seq = get_if<ConcatFlatRule>(&r)) {
    // TODO suppress hos output, produce static forward decls.
    codegen(ruleset, *seq, cppos);
  }else if(const auto* out = get_if<OutputTmpl>(&r)) {
    codegen(ruleset, *out, cppos);
  }else if(const auto* err = get_if<ErrorRule>(&r)) {
    codegen(*err, cppos);
  }else if(const auto* qm = get_if<QuietMatch>(&r)) {
    codegen(ruleset, *qm, cppos);
  }else if(const auto* ors = get_if<OrRule>(&r)) {
    codegen(ruleset, *ors, cppos);
  }else if(const auto* me = get_if<MatchOrError>(&r)) {
    codegen(ruleset, *me, cppos);
  // TODO Implement errors, error-recovery scanner.
  }else Bug("Unknown rule type {} in codegen()", r.specifics_typename());
  cppos("}\n");
}

}  // namespace oalex
