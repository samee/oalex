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
#include <type_traits>
#include <utility>
#include "util.h"
#include "oalex.h"
#include "fmt/format.h"
using fmt::format;
using oalex::Regex;
using oalex::RegexOptions;
using std::exchange;
using std::get_if;
using std::holds_alternative;
using std::string;

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

static string cEscaped(const string& s) {
  string rv;
  for(char c : s) switch(c) {
    // Technically, we should only need \", \n, and \\, but this should help
    // readability.
    case '"' : rv += "\\\""; continue;
    case '\\': rv += "\\\\"; continue;
    case '\a': rv += "\\a"; continue;
    case '\b': rv += "\\b"; continue;
    case '\f': rv += "\\f"; continue;
    case '\n': rv += "\\n"; continue;
    case '\r': rv += "\\r"; continue;
    case '\t': rv += "\\t"; continue;
    case '\v': rv += "\\v"; continue;
    case '\0': rv += "\\0"; continue;
    // TODO hex code for bytes > 0x7f
    default: rv += c;
  }
  return rv;
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
  // TODO generate better names than just `start()`.
  if(const auto* s = get_if<string>(&r)) {
    hos("oalex::JsonLoc start(oalex::InputDiags& ctx, ssize_t& i);\n");

    cppos("oalex::JsonLoc start(oalex::InputDiags& ctx, ssize_t& i) {\n");
    cppos(format("  return oalex::match(ctx, i, \"{}\");\n", cEscaped(*s)));
    cppos("}\n");
    return;
  }
  Unimplemented("codegen() for {} rule", r.specifics_typename());
}

}  // namespace oalex
