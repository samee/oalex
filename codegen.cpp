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

#include "codegen.h"
#include <functional>
#include <map>
#include <type_traits>
#include <utility>
#include <vector>
#include "util.h"
#include "oalex.h"
#include "fmt/format.h"
using fmt::format;
using oalex::Regex;
using oalex::RegexOptions;
using std::exchange;
using std::function;
using std::get_if;
using std::holds_alternative;
using std::map;
using std::optional;
using std::string;
using std::vector;

namespace oalex {

// eval()
// ------

static JsonLoc skip(InputDiags& ctx, ssize_t& i,
                 const SkipPoint& sp) {
  const Input& input = ctx.input;
  const ssize_t oldi = i;
  i = sp.stayWithinLine ? sp.skip->withinLine(input, i)
                        : sp.skip->acrossLines(input, i);
  if(i == ssize_t(string::npos)) {
    ssize_t com = oldi;
    while(ctx.input.sizeGt(com) && is_in(ctx.input[com], " \n\t")) ++com;
    if(!ctx.input.sizeGt(com)) Bug("skipper returned npos without a comment");
    Error(ctx, com, "Unfinished comment");
    return JsonLoc::ErrorValue{};
  } else return JsonLoc::String();  // Just something non-error.
}

JsonLoc eval(InputDiags& ctx, ssize_t& i,
             const ConcatRule& seq, const RuleSet& rs) {
  JsonLoc rv = seq.outputTmpl;
  JsonLoc::PlaceholderMap pmap = rv.allPlaceholders();
  ssize_t j = i;
  for(auto& [idx, outname] : seq.comps) {
    // TODO move this into substitute in the common case.
    JsonLoc out = eval(ctx, j, rs, idx);
    if(out.holdsError()) return out;
    if(!outname.empty()) rv.substitute(pmap, outname, out);
  }
  i = j;
  return rv;
}

// Using std::visit(), since we want to catch missing types at compile-time.
static string specifics_typename(const string&) { return "string"; }
static string specifics_typename(const Regex&) { return "Regex"; }
static string specifics_typename(const SkipPoint&) { return "SkipPoint"; }
static string specifics_typename(const ConcatRule&) { return "ConcatRule"; }

string Rule::specifics_typename() const {
  return std::visit([](auto& spec) { return oalex::specifics_typename(spec); },
                    this->specifics_);
}

JsonLoc eval(InputDiags& ctx, ssize_t& i,
             const RuleSet& ruleset, ssize_t ruleIndex) {
  const Rule& r = ruleset.rules[ruleIndex];
  if(const string* s = get_if<string>(&r)) return match(ctx, i, *s);
  else if(const auto* sp = get_if<SkipPoint>(&r)) return skip(ctx, i, *sp);
  else if(const auto* regex = get_if<Regex>(&r))
    return match(ctx, i, *regex, ruleset.regexOpts);
  else if(const auto* seq = get_if<ConcatRule>(&r))
    return eval(ctx,i, *seq, ruleset);
  Unimplemented("eval() for {} rule", r.specifics_typename());
}


// codegen()
// ---------

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

static string cEscaped(const string& s) {
  string rv;
  for(char c : s) rv += cEscaped(c);
  return rv;
}

static string squoted(char ch) { return format("'{}'", cEscaped(ch)); }

static string anchorName(RegexAnchor a) {
  switch(a) {
    case RegexAnchor::wordEdge: return "wordEdge";
    case RegexAnchor::bol: return "bol";
    case RegexAnchor::eol: return "eol";
    default: Bug("Unknown RegexAnchor of type {}", static_cast<int>(a));
  }
}

static void linebreak(const OutputStream& cppos, ssize_t indent) {
  cppos("\n" + string(indent, ' '));
}

static void genRegexCharSet(const RegexCharSet& cset,
                            const OutputStream& cppos, ssize_t indent) {
  auto br = [&]() { linebreak(cppos, indent); };
  cppos("RegexCharSet{.ranges = {"); br();
  for(auto& range : cset.ranges) {
    cppos(format("  {{ {}, {} }},", squoted(range.from), squoted(range.to)));
    br();
  }
  cppos(format("}}, .negated = {}}}",
               cset.negated ? "true" : "false"));
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
  if(auto* cset = get_if_unique<const RegexCharSet>(&regex)) {
    cppos("move_to_unique(");
    genRegexCharSet(*cset, cppos, indent);
    cppos(")");
  }else if(auto* s = get_if_unique<const string>(&regex)) {
    cppos("move_to_unique(\"");  cppos(cEscaped(*s)); cppos("\"s)");
  }else if(auto* a = get_if_unique<const RegexAnchor>(&regex)) {
    cppos(format("move_to_unique(RegexAnchor::{})", anchorName(*a)));
  }else if(auto* seq = get_if_unique<const RegexConcat>(&regex)) {
    cppos("move_to_unique(RegexConcat{.parts{");
    genMakeVector("Regex", seq->parts, [&](auto& part) {
                    genRegexComponents(part, cppos, indent+2);
                  }, br, cppos);
    cppos("}})");
  }else if(auto* opt = get_if_unique<const RegexOptional>(&regex)) {
    cppos("move_to_unique(RegexOptional{.part{");
    genRegexComponents(opt->part, cppos, indent);
    cppos("}})");
  }else if(auto* rep = get_if_unique<const RegexRepeat>(&regex)) {
    cppos("move_to_unique(RegexRepeat{.part{");
    genRegexComponents(rep->part, cppos, indent);
    cppos("}})");
  }else if(auto ors = get_if_unique<const RegexOrList>(&regex)) {
    cppos("move_to_unique(RegexOrList{.parts{"); br();
    genMakeVector("Regex", ors->parts, [&](auto& part) {
                    genRegexComponents(part, cppos, indent+2);
                  }, br, cppos);
    cppos("}})");
  }else Unimplemented("Regex codegen for index {}", regex.index());
}

void codegenDefaultRegexOptions(const RuleSet& ruleset,
                                const OutputStream& cppos) {
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
codegen(const Regex& regex, const string& rname,
        const OutputStream& cppos, const OutputStream& hos) {
  hos(format("oalex::JsonLoc parse{}(oalex::InputDiags& ctx, ssize_t& i);\n",
             rname));
  cppos(format("oalex::JsonLoc parse{}(oalex::InputDiags& ctx, "
                                      "ssize_t& i) {{\n", rname));
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
  cppos("  using std::literals::string_literals::operator\"\"s;\n");
  cppos("  static Regex *r = new Regex{\n    ");
  genRegexComponents(regex, cppos, 4);
  cppos("\n  };\n");
  cppos("  return oalex::match(ctx, i, *r, defaultRegexOpts());\n");
  cppos("}\n");
}

static void
codegen(const JsonLoc& jsloc, const OutputStream& cppos,
        const map<string,string>& placeholders, ssize_t indent) {
  auto br = [&]() { linebreak(cppos, indent); };
  if(auto* p = get_if<JsonLoc::Placeholder>(&jsloc)) {
    auto v = placeholders.find(p->key);
    if(v == placeholders.end())
      Bug("Undefined placeholder in codegen: {}", p->key);
    cppos(format("std::move({})", v->second));
  }else if(auto* s = get_if<JsonLoc::String>(&jsloc)) {
    cppos(format("\"{}\"", cEscaped(*s)));
  }else if(auto* v = get_if<JsonLoc::Vector>(&jsloc)) {
    cppos("JsonLoc::Vector{");
    genMakeVector("JsonLoc", *v, [&](auto& child) {
                   codegen(child, cppos, placeholders, indent+2);
                 }, br, cppos);
    cppos("}");
  }else if(auto* m = get_if<JsonLoc::Map>(&jsloc)) {
    cppos("JsonLoc::Map{"); br();
    for(const auto& [k, v] : *m) {
      cppos(format("  {{\"{}\", ", cEscaped(k)));
      codegen(v, cppos, placeholders, indent+4);
      cppos("},"); br();
    }
    cppos("}");
  }else cppos("JsonLoc::ErrorValue{}");
}

static void
codegen(const ConcatRule& concatRule, const string& rname,
        const OutputStream& cppos, const OutputStream& hos,
        function<optional<string>(ssize_t)> rulename) {
  hos(format("oalex::JsonLoc parse{}(oalex::InputDiags& ctx, ssize_t& i);\n",
             rname));
  cppos(format("oalex::JsonLoc parse{}(oalex::InputDiags& ctx, "
                                      "ssize_t& i) {{\n", rname));
  cppos("  using oalex::JsonLoc;\n");
  cppos("  ssize_t j = i;\n\n");
  map<string,string> placeholders;
  for(auto& comp : concatRule.comps) {
    const optional<string> nm = rulename(comp.idx);
    if(!nm.has_value()) Unimplemented("Nameless concat component");
    string resvar = "res" + *nm;
    cppos(format("  JsonLoc {} = parse{}(ctx, j);\n", resvar, *nm));
    cppos(format("  if({0}.holdsError()) return {0};\n", resvar));
    if(!comp.outputPlaceholder.empty() &&
       !placeholders.insert({comp.outputPlaceholder, resvar}).second)
      // This uniqueness is also used in codegen(JsonLoc).
      Bug("Duplicate placeholders at codegen: ", comp.outputPlaceholder);
  }
  cppos("\n  i = j;\n");
  cppos("  return ");
    codegen(concatRule.outputTmpl, cppos, placeholders, 2);
    cppos(";\n");
  cppos("}\n");
}

/*
// TODO: Implement this. It should generate an inlined call to oalex::match()
// when possible, but falls back to the main codegen() for other cases.
static void codegenInlineOneLiners(const RuleSet& ruleset, ssize_t ruleIndex,
                                   OutputStream& os);
*/

void codegen(const RuleSet& ruleset, ssize_t ruleIndex,
             const OutputStream& cppos, const OutputStream& hos) {
  const Rule& r = ruleset.rules[ruleIndex];
  // TODO check if some rule already uses the name start().
  string fname = (r.name().has_value() ? *r.name() : "start");
  auto rulename = [&](ssize_t i) { return ruleset.rules[i].name(); };
  if(const auto* s = get_if<string>(&r)) {
    hos(format("oalex::JsonLoc parse{}(oalex::InputDiags& ctx, ssize_t& i);\n",
               fname));

    // TODO complex parsers should have comments with the source line.
    cppos(format("oalex::JsonLoc parse{}(oalex::InputDiags& ctx, "
                                        "ssize_t& i) {{\n",
                 fname));
    cppos(format("  return oalex::match(ctx, i, \"{}\");\n", cEscaped(*s)));
    cppos("}\n");
  }else if(const auto* regex = get_if<Regex>(&r)) {
    codegen(*regex, fname, cppos, hos);
  }else if(const auto* seq = get_if<ConcatRule>(&r)) {
    codegen(*seq, fname, cppos, hos, rulename);
  // TODO Implement SkipPoint, errors, OrListRule
  }else Unimplemented("codegen() for {} rule", r.specifics_typename());
}

}  // namespace oalex
