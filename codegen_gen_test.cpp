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
#include "codegen_test_util.h"

#include "jsonloc_io_test.h"
#include "runtime/diags_test_util.h"
#include "runtime/oalex.h"
#include <cstdio>
#include <cstring>
#include <functional>
#include <libgen.h>
#include <memory>
#include <string>
#include <string_view>
#include <unistd.h>
#include <utility>
using oalex::Bug;
using oalex::codegenDefaultRegexOptions;
using oalex::ConcatRule;
using oalex::Input;
using oalex::InputDiags;
using oalex::JsonLoc;
using oalex::OutputStream;
using oalex::parseJsonLoc;
using oalex::parseRegex;
using oalex::Rule;
using oalex::RuleSet;
using oalex::SkipPoint;
using oalex::UserError;
using oalex::WordPreserving;
using oalex::test::cskip;
using oalex::test::regexOpts;
using oalex::test::singletonRuleSet;
using std::bind;
using std::pair;
using std::size;
using std::string;
using std::string_view;
using std::unique_ptr;

namespace {

auto pathSplit(const string& s) {
  unique_ptr<char,decltype(&free)> dir(strdup(s.c_str()), free);
  unique_ptr<char,decltype(&free)> base(strdup(s.c_str()), free);
  return pair<string,string>{dirname(dir.get()), basename(base.get())};
}

auto pathSplitSuffix(const string& s, ssize_t len) -> pair<string,string> {
  if(len <= 0) Bug("{}: len must be positive, not {}", __func__, len);
  if(len == 1) return pathSplit(s);
  auto [pref, suff] = pathSplitSuffix(s, len-1);
  if(pref == ".") return {pref, suff};
  auto [pref2, suff2] = pathSplit(pref);
  return {pref2, suff2 + "/" + suff};
}

struct CmdLineOpts {
  string outputCppPath, outputHPath, hPathAsIncluded;
};

CmdLineOpts parseCmdLine(int argc, char* argv[]) {
  int opt;
  string rv;
  while((opt = getopt(argc, argv, "o:")) != -1) {
    if(opt == 'o') {
      if(!optarg) Bug("Null argument to -o");
      rv = optarg;
    }else if(opt == '?') UserError("Please check the command line");
    else {
     if(optarg && *optarg)
       Bug("Got unexpected parameter -{} {}", char(opt), optarg);
     else Bug("Got unexpected option -{}", char(opt));
    }
  }
  if(rv.empty()) UserError("Missing -o option for output filename");
  if(optind < argc) UserError("Extra parameter: {}", argv[optind]);
  if(optind > argc) Bug("getopt() produced too large an optind");
  return CmdLineOpts{.outputCppPath = rv + ".cpp",
                     .outputHPath = rv + ".h",
                     .hPathAsIncluded = pathSplitSuffix(rv+".h", 1).second};
}

auto fopenw(const string& s) -> unique_ptr<FILE, decltype(&fclose)> {
  unique_ptr<FILE, decltype(&fclose)> fp(fopen(s.c_str(), "w"), fclose);
  if(!fp) UserError("Couldn't write to file {}", s);
  return fp;
}

auto writeOrFail(FILE* fp, string_view s) {
  if(fwrite(s.data(), s.size(), 1, fp) < 1)
    UserError("File write error");
}

void generateSingleStringTest(const OutputStream& cppos,
                              const OutputStream& hos) {
  RuleSet rs = singletonRuleSet(Rule{"hello", "HelloPrefix"});
  codegen(rs, 0, cppos, hos);
  rs = singletonRuleSet(Rule{WordPreserving{"hello"}, "HelloKeyword"});
  codegen(rs, 0, cppos, hos);
}

Rule regexRule(const string& testName,
               const string& regex_pattern, const string& fname) {
  InputDiags regex_input{Input{regex_pattern}};
  size_t i = 0;
  auto regex = parseRegex(regex_input, i);
  assertEmptyDiags(testName, regex_input.diags);
  return Rule{std::move(*regex), fname};
}

void generateSingleRegexTest(const OutputStream& cppos,
                             const OutputStream& hos) {
  const pair<string,string> inputs[] = {
    {"/fo[ox]/", "FooOrFox"},
    {"/foo+d/", "LongFood"},
    {"/abc|xy+z/", "AbcXyz"},
    {"/^abc$/", "AbcWholeLine"},
    {"/^abc\\b/", "AbcWord"},
  };
  for(auto& [pat, fname] : inputs) {
    cppos("\n");
    RuleSet rs = singletonRuleSet(regexRule(__func__, pat, fname));
    codegen(rs, 0, cppos, hos);
  }
}

void generateConcatTest(const OutputStream& cppos,
                        const OutputStream& hos) {
  RuleSet rs { oalex::makeVector<Rule>(
    Rule{WordPreserving{"int"}, "Type"},
    regexRule(__func__, "/[a-zA-Z_][a-zA-Z_0-9]*\\b/", "Identifier"),
    Rule{"=", ""},
    regexRule(__func__, "/-?[0-9]+\\b/", "IntegerLiteral"),
    Rule{";", ""},
    Rule{SkipPoint{false, &rs.skip}, "CommentsAndWhitespace"},
    Rule{"", ""}
    ), cskip, regexOpts
  };
  // Produce the helpers: all but the last rule.
  for(size_t i=0; i<size(rs.rules)-1; ++i)
    if(rs.rules[i].name().has_value()) codegen(rs, i, cppos, hos);

  Rule testRules[] = {
    {ConcatRule{{{0,""}, {5,""}, {1,"id"}, {5,""}, {2,""}, {5,""},
                 {3,"value"}, {5,""}, {4,""}},
                *parseJsonLoc("{id, value}")}, "Definition"},
    {ConcatRule{{{1,"lhs"}, {5,""}, {2,""}, {5,""}, {1,"rhs"}, {5,""}, {4,""}},
                *parseJsonLoc("{rhs, lhs}")}, "Assignment"},
  };
  for(size_t i=0; i<size(testRules); ++i) {
    rs.rules[size(rs.rules)-1] = std::move(testRules[i]);
    codegen(rs, size(rs.rules)-1, cppos, hos);
  }
}

}  // namespace

int main(int argc, char* argv[]) {
  try {
    CmdLineOpts opts = parseCmdLine(argc, argv);
    auto cppfp = fopenw(opts.outputCppPath);
    auto hfp = fopenw(opts.outputHPath);
    fputs("#pragma once\n"
          "#include <cstdint>\n"
          "#include <oalex.h>\n\n"
          "extern oalex::JsonLoc\n"
          "  parseAsgnStmt(oalex::InputDiags& ctx, size_t& i);\n"
          "extern bool goodFunc();\n"
          "extern bool badFunc();\n\n", hfp.get());
    fprintf(cppfp.get(),
            "#include \"%s\"\n"
            "using oalex::InputDiags;\n"
            "using oalex::JsonLoc;\n\n"
            "JsonLoc parseAsgnStmt(InputDiags&, size_t&) {\n"
            "  // Unimplemented\n"
            "  return JsonLoc::ErrorValue{};\n"
            "}\n\n"
            "bool goodFunc() { return true; }\n"
            "bool badFunc()  { return false; }\n\n",
            opts.hPathAsIncluded.c_str());
    using std::placeholders::_1;
    auto cppos = bind(writeOrFail, cppfp.get(), _1);
    auto hos = bind(writeOrFail, hfp.get(), _1);
    auto linebreaks = [&](){ cppos("\n"); hos("\n"); };

    // TODO first-class support for multiple RuleSets in a file.
    codegenDefaultRegexOptions(RuleSet{{}, cskip, regexOpts}, cppos);
    linebreaks(); generateSingleStringTest(cppos, hos);
    linebreaks();
    linebreaks();
    generateSingleRegexTest(cppos, hos);
    linebreaks();
    generateConcatTest(cppos, hos);
    return 0;
  }catch(const oalex::UserErrorEx& ex) {
    fprintf(stderr, "%s: %s\n", argv[0], ex.what());
    return 1;
  }
}
