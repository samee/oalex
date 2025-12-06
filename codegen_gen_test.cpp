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

#include "codegen.h"
#include "codegen_test_util.h"

#include "analysis.h"
#include "jsontmpl_parsers.h"
#include "regex_io.h"
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
using oalex::AliasRule;
using oalex::Bug;
using oalex::codegenDefaultRegexOptions;
using oalex::ConcatFlatRule;
using oalex::ErrorRule;
using oalex::ExternParser;
using oalex::Ident;
using oalex::Input;
using oalex::InputDiags;
using oalex::JsonLoc;
using oalex::JsonTmpl;
using oalex::LoopRule;
using oalex::makeVectorUnique;
using oalex::MatchOrError;
using oalex::OutputStream;
using oalex::OutputTmpl;
using oalex::OrRule;
using oalex::parseJsonTmpl;
using oalex::parseRegexCharSet;
using oalex::passthroughTmpl;
using oalex::QuietMatch;
using oalex::RegexOptions;
using oalex::Rule;
using oalex::RuleSet;
using oalex::StringRule;
using oalex::Skipper;
using oalex::SkipPoint;
using oalex::UserError;
using oalex::WordPreserving;
using oalex::test::cskip;
using oalex::test::flatcat;
using oalex::test::nmRule;
using oalex::test::parseRegexRule;
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

struct fcloser {
  void operator()(FILE* fp) const noexcept { fclose(fp); }
};

auto fopenw(const string& s) -> unique_ptr<FILE,fcloser> {
  unique_ptr<FILE,fcloser> fp(fopen(s.c_str(), "w"));
  if(!fp) UserError("Couldn't write to file {}", s);
  return fp;
}

class WriteOrFail final : public OutputStream {
  FILE* fp_;
 public:
  explicit WriteOrFail(FILE* fp) : fp_{fp} {}
  void operator()(string_view s) const override {
    if(fwrite(s.data(), s.size(), 1, fp_) < 1)
      UserError("File write error");
  }
};

void generateSingleStringTest(const OutputStream& cppos,
                              const OutputStream& hos) {
  RuleSet rs = singletonRuleSet(nmRule("hello", "HelloPrefix"));
  codegen(rs, 0, cppos, hos);
  rs = singletonRuleSet(nmRule(WordPreserving{"hello", 0}, "HelloKeyword"));
  codegen(rs, 0, cppos, hos);
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
    RuleSet rs = singletonRuleSet(nmRule(parseRegexRule(pat), fname));
    codegen(rs, 0, cppos, hos);
  }
}

void codegenNamedRules(RuleSet& rs,
                       const OutputStream& cppos, const OutputStream& hos) {
  resolveWrapperTypes(rs);
  populateFlatFields(rs);
  normalizeCompRead(rs);
  for(size_t i=0; i<size(rs.rules); ++i)
    if(rs.rules[i]->nameOrNull() != nullptr) codegen(rs, i, cppos, hos);
}

void generateWordPreservingWithOverride(const OutputStream& cppos,
                                        const OutputStream& hos) {
  // First, try the normal rule. Then, ones with overrides.
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
               nmRule(WordPreserving{"hello", 0}, "WordHello1"),
               nmRule(WordPreserving{"hello", 1}, "WordHello2"),
               nmRule(WordPreserving{"hell", 0}, "WordHello3"),
               nmRule(WordPreserving{"hell", 2}, "WordHello4")),
    .skips{},
    .regexOpts = {regexOpts, RegexOptions{parseRegexCharSet("[a-z-]")},
                             RegexOptions{parseRegexCharSet("[e-l]")}
                 },
  };
  codegenNamedRules(rs, cppos, hos);
}

void generateRegexWordOverride(const OutputStream& cppos,
                               const OutputStream& hos) {
  // First, try the normal rule. Then, ones with overrides.
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
               nmRule(parseRegexRule("/[a-z]+\\b/"), "RegexWord1"),
               nmRule(parseRegexRule("/[a-z]+\\b/", 1), "RegexWord2"),
               nmRule(parseRegexRule("/[a-z]+\\b/", 2), "RegexWord3")),
    .skips{},
    .regexOpts = {regexOpts, RegexOptions{parseRegexCharSet("[a-z-]")},
                             RegexOptions{parseRegexCharSet("[e-l]")}
                 },
  };
  codegenNamedRules(rs, cppos, hos);
}


void generateConcatFlatTest(const OutputStream& cppos,
                            const OutputStream& hos) {
  const ssize_t varTypeIndex = 7;
  const ssize_t declIndex = 8;
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
               WordPreserving{"var", 0},
               nmRule(parseRegexRule("/[a-zA-Z]+/"), "FlatIdent"),
               StringRule{":"}, StringRule{"="},
               nmRule(parseRegexRule("/[0-9]+/"), "FlatNumber"),
               StringRule{";"},
               nmRule(SkipPoint{0}, "FlatSpace"),
               nmRule(ConcatFlatRule{{
                   {1, "var_name"}, {6}, {2}, {6}, {1, "type"},
               }}, "FlatVarAndType"),
               nmRule(ConcatFlatRule{{
                 {0}, {6}, flatcat(varTypeIndex), {6}, {3},
                 {6}, {4, "rhs"}, {6}, {5}
               }}, "FlatDefn"),
               nmRule(OutputTmpl{declIndex, {},
                 *parseJsonTmpl("{var_name, init_value: {type, value: rhs}}")
               }, "FlatThenAssembled")),
    .skips = {cskip},
    .regexOpts = {regexOpts},
  };
  codegenNamedRules(rs, cppos, hos);
}

void generateSingleWordTemplate(const OutputStream& cppos,
                                const OutputStream& hos) {
  JsonTmpl jstmpl = JsonTmpl::Map{{"keyword",
    JsonTmpl{JsonTmpl::Placeholder{"the_word"}}}};
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        StringRule{"word"},
        nmRule(OutputTmpl{0, "the_word", jstmpl}, "WordTmpl")),
    .skips = {},
    .regexOpts = {regexOpts},
  };
  codegen(rs, 1, cppos, hos);
}

void generateConcatTest(const OutputStream& cppos,
                        const OutputStream& hos) {
  RuleSet rs { oalex::makeVectorUnique<Rule>(
    nmRule(WordPreserving{"int", 0}, "Type"),
    nmRule(parseRegexRule("/[a-zA-Z_][a-zA-Z_0-9]*\\b/"), "Identifier"),
    StringRule{"="},
    nmRule(parseRegexRule("/-?[0-9]+\\b/"), "IntegerLiteral"),
    StringRule{";"},
    nmRule(SkipPoint{0}, "CommentsAndWhitespace"),
    nmRule(ConcatFlatRule{{{0}, {5}, {1,"id"}, {5}, {2}, {5},
                           {3,"value"}, {5}, {4}}}, "DefinitionFields"),
    nmRule(ConcatFlatRule{{{1,"lhs"}, {5}, {2}, {5}, {1,"rhs"}, {5}, {4}}},
           "AssignmentFields"),
    nmRule(OutputTmpl{6, {}, *parseJsonTmpl("{id, value}")}, "Definition"),
    nmRule(OutputTmpl{7, {}, *parseJsonTmpl("{rhs, lhs}")}, "Assignment")
    ), {cskip}, {regexOpts}
  };
  codegenNamedRules(rs, cppos, hos);
}

void generateNestedTypesTest(const OutputStream& cppos,
                             const OutputStream& hos) {
  RuleSet rs { oalex::makeVectorUnique<Rule>(
    nmRule(parseRegexRule("/[a-zA-Z_][a-zA-Z_0-9]*\\b/"), "ThreeAddrLhs"),
    StringRule{"+"},
    nmRule(SkipPoint{0}, "ThreeAddrSkip"),
    nmRule(ConcatFlatRule{{ {0, "src_1"}, {2}, {1}, {2}, {0, "src_2"} }},
           "ThreeAddrRhsFields"),
    nmRule(OutputTmpl{3, {}, *parseJsonTmpl("{src_1, src_2}")}, "ThreeAddrRhs"),
    StringRule{"="},
    nmRule(ConcatFlatRule{{{0, "lhs"}, {2}, {5}, {2}, {4, "expr"}}},
           "ThreeAddrFields"),
    nmRule(OutputTmpl{6, {}, *parseJsonTmpl("{lhs, expr}")},
           "ThreeAddrOperation")
    ), {cskip}, {regexOpts}
  };
  rs.rules[4]->nestedIn(7);

  codegenNamedRules(rs, cppos, hos);
}

void generateTupleTypesTest(const OutputStream& cppos,
                            const OutputStream& hos) {
  RuleSet rs { oalex::makeVectorUnique<Rule>(
    nmRule(parseRegexRule("/[0-9][0-9]/"), "DateComp"),
    StringRule{"-"},
    nmRule(ConcatFlatRule{{ {0, "day"}, {1}, {0, "month"}, {1}, {0, "year"} }},
           "DateFields"),
    nmRule(OutputTmpl{2, {},
      *parseJsonTmpl("{date_fields: [day, month, year]}")},
      "Date")
    ), {cskip}, {regexOpts}
  };

  codegenNamedRules(rs, cppos, hos);
}

void generateExternParserDeclaration(const OutputStream& cppos,
                                     const OutputStream& hos) {
  const Skipper shskip{ {{"#", "\n"}}, {}, Skipper::Newlines::keep_all };
  RuleSet rs { oalex::makeVectorUnique<Rule>(
      WordPreserving{"let", 0},
      nmRule(parseRegexRule("/[a-zA-Z_][a-zA-Z_0-9]*\\b/"), "ExtTmplId"),
      StringRule{":"},
      nmRule(ExternParser{"oalexPluginIndentedTmpl", {}}, "IndentedTmpl"),
      nmRule(SkipPoint{0}, "ExtSpace"),
      nmRule(ConcatFlatRule{{{0}, {4}, {1,"id"}, {4}, {2}, {4}, {3,"tmpl"}}},
             "ExtTmplFields"),
      nmRule(OutputTmpl{5, {}, *parseJsonTmpl("{id, tmpl}")}, "ExtTmpl")
    ), {shskip}, {regexOpts}
  };
  codegenNamedRules(rs, cppos, hos);
}

void generateExternParserParams(const OutputStream& cppos,
                                const OutputStream& hos) {
  RuleSet rs { oalex::makeVectorUnique<Rule>(
      nmRule(parseRegexRule("/[a-zA-Z_]+\\b/"), "ParamId"),
      nmRule(ExternParser{"oalexPluginBulletedList", {0}}, "BulletListIds")
    ), {}, {regexOpts}
  };
  codegenNamedRules(rs, cppos, hos);
}

void generateIndentedListBuiltin(const OutputStream& cppos,
                                 const OutputStream& hos) {
  RuleSet rs { oalex::makeVectorUnique<Rule>(
      StringRule{"my_list"},
      nmRule(SkipPoint{0}, "ListLeaderSkip"),
      StringRule{":"},
      nmRule(ConcatFlatRule{{ {0}, {1}, {2} }}, "ListLeader"),
      nmRule(StringRule{"item"}, "ListItemKeyword"),
      nmRule(parseRegexRule("/[0-9]+/"), "ListItemNumber"),
      nmRule(ConcatFlatRule{{ {4}, {1}, {5, "num"} }}, "ListItem"),
      nmRule(ExternParser{"oalexBuiltinIndentedList", {3,6}},
             "SimpleIndentedList")
    ), {cskip}, {regexOpts}
  };
  codegenNamedRules(rs, cppos, hos);
}

void generateOrTest(const OutputStream& cppos, const OutputStream& hos) {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        StringRule{"if"}, StringRule{"while"},
        nmRule(parseRegexRule("/[0-9]+/"), "OrCompNumber"),
        nmRule(OrRule{{
          {-1, 0, passthroughTmpl}, {-1, 1, passthroughTmpl},
          {-1, 2, passthroughTmpl},
        }, /* flattenOnDemand */ false}, "OneWordOrList")),
    .skips{},
    .regexOpts = {regexOpts},
  };
  codegenNamedRules(rs, cppos, hos);
}

void generateMatchOrErrorTest(const OutputStream& cppos,
                              const OutputStream& hos) {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        StringRule{"hello-world"},
        nmRule(MatchOrError{0, "Was expecting a greeting"},
               "HelloWorldOrError")),
    .skips{},
    .regexOpts = {regexOpts},
  };
  resolveWrapperTypes(rs);
  codegen(rs, 1, cppos, hos);
}

void generateOptionalComponent(const OutputStream& cppos,
                               const OutputStream& hos) {
  // var var_name [ = init_value ]
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        nmRule(SkipPoint{0}, "OptionalCompSkip"),
        WordPreserving{"var", 0},
        nmRule(parseRegexRule("/[a-zA-Z_][a-zA-Z_0-9]*\\b/"),
               "OptionalCompVarName"),
        StringRule{"="},
        nmRule(parseRegexRule("/[0-9]+\\b/"), "OptionalCompValue"),
        nmRule(ConcatFlatRule{{ {3}, {0}, {4, "init_value"} }},
               "OptionalCompInititializer"),
        nmRule(ConcatFlatRule{{ }}, "OptionalCompAbsent"),
        nmRule(OrRule{{
          {-1, 5, passthroughTmpl}, {-1, 6, passthroughTmpl},
        }, /* flattenOnDemand */ true}, "OptionalCompOptInitializer"),
        nmRule( ConcatFlatRule{{ {1}, {0}, {2, "var_name"}, {0}, flatcat(7) }},
                "VarOptionalInitValue" )
    ),
    .skips{cskip},
    .regexOpts = {regexOpts},
  };

  codegenNamedRules(rs, cppos, hos);
}

void generateAliasRuleTest(const OutputStream& cppos,
                           const OutputStream& hos) {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        StringRule{"hello-world"},
        nmRule(AliasRule{0}, "AliasedHelloWorld")),
    .skips{},
    .regexOpts = {regexOpts},
  };
  resolveWrapperTypes(rs);
  codegen(rs, 1, cppos, hos);
}

void generateErrorRuleTest(const OutputStream& cppos,
                           const OutputStream& hos) {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        StringRule{"hello-world"},
        ErrorRule{"Was expecting a greeting"},
        nmRule(OrRule{
          {{-1, 0, passthroughTmpl}, {-1, 1, passthroughTmpl}},
          /* flattenOnDemand */ false,
        }, "ErrorRuleHelloWorld")),
    .skips{},
    .regexOpts = {regexOpts},
  };
  resolveWrapperTypes(rs);
  codegen(rs, 2, cppos, hos);
}

void generateFlattenOnDemand(const OutputStream& cppos,
                             const OutputStream& hos) {
  OrRule orrule{{
    {-1, 4, passthroughTmpl},
    {-1, 3, passthroughTmpl},
  }, /* flattenOnDemand */ false};
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        StringRule{"let"}, nmRule(parseRegexRule("/[0-9]+/"), "FlattenNumber"),
        nmRule(ConcatFlatRule{{ {0, "keyword"} }}, "FlattenKeywordQuiet"),
        nmRule(ConcatFlatRule{{ {1, "number"} }}, "FlattenNumberTagged"),
        nmRule(MatchOrError{2, "Expected keyword 'let'"}, "FlattenKeyword"),
        nmRule(OrRule{orrule.comps, /* flattenOnDemand */ false},
               "UnflattenKeywordOrNumber"),
        nmRule(ConcatFlatRule{{{5, "next_token"}}}, "UnflattenSingleConcat"),
        nmRule(OrRule{orrule.comps, /* flattenOnDemand */ true},
               "FlattenKeywordOrNumber"),
        nmRule(ConcatFlatRule{{flatcat(7)}}, "FlattenSingleConcat")
      ), .skips{}, .regexOpts = {regexOpts},
  };

  codegenNamedRules(rs, cppos, hos);
}

void generateLookaheads(const OutputStream& cppos, const OutputStream& hos) {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        nmRule(SkipPoint{0}, "lookahead_space"),
        WordPreserving{"var", 0},
        nmRule(parseRegexRule("/[a-z]+/"), "lookahead_ident"),
        StringRule{"="}, StringRule{";"},
        nmRule(ConcatFlatRule{{
          {1}, {0}, {2, "var"}, {0}, {3}, {0}, {2, "init_value"}, {0}, {4},
        }}, "decl"),
        nmRule(ConcatFlatRule{{
          {2, "lhs"}, {0}, {3}, {0}, {2, "rhs"}, {0}, {4},
        }}, "asgn"),
        StringRule{"."},
        nmRule(ConcatFlatRule{{ {7}, {2, "directive"} }}, "directive"),
        nmRule(parseRegexRule("/[0-9]+/"), "lookahead_line_number_regex"),
        StringRule{":"},
        nmRule(ConcatFlatRule{{ {9, "line_number"}, {10} }},
               "lookahead_line_num"),
        nmRule(OrRule{{ {1, 5, passthroughTmpl},
                        {7, 8, passthroughTmpl},
                        {9, 11, passthroughTmpl},
                        {-1, 6, passthroughTmpl} },
                      /* flattenOnDemand */ true}, "lookahead_simple_stmt")),
    .skips{cskip},
    .regexOpts = {regexOpts},
  };

  codegenNamedRules(rs, cppos, hos);
}

void generateQuietTest(const OutputStream& cppos, const OutputStream& hos) {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        StringRule{"string1"}, StringRule{"string2"},
        nmRule(MatchOrError{0, "Expecting 'string1'"}, "string1_or_error"),
        nmRule(QuietMatch{2}, "string1_quiet"),
        nmRule(OrRule{{{-1, 3, passthroughTmpl}, {-1, 1, passthroughTmpl}},
                      /* flattenOnDemand */ false}, "quiet_match_test")),
    .skips{},
    .regexOpts = {regexOpts},
  };
  codegenNamedRules(rs, cppos, hos);
}

void generateMiscFlatteningTest(const OutputStream& cppos,
                                const OutputStream& hos) {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        nmRule("hello", "flat_hello_comp"),
        nmRule(ConcatFlatRule{{ {0, "hello_for_qm"} }}, "flat_hello_flat1"),
        nmRule(QuietMatch{1}, "flat_hello_quiet_passing_thru_concat_flat"),
        nmRule(ConcatFlatRule{{ {0, "hello_for_mor"} }}, "flat_hello_flat2"),
        nmRule(MatchOrError{3, "Expected keyword 'hello'"},
          "flat_match_or_error_passing_thru_concat_flat"),
        nmRule(ConcatFlatRule{{ flatcat(2), flatcat(4) }},
          "flat_hello_flat3"),
        nmRule(QuietMatch{0}, "flat_hello_quiet_dropped_by_concat_flat"),
        nmRule(MatchOrError{0, "Expected keyword 'hello'"},
          "flat_match_or_error_dropped_by_concat_flat"),
        nmRule(ConcatFlatRule{{ {6}, {7} }}, "flat_hello_flat4")
     ),
    .skips{},
    .regexOpts = {regexOpts},
  };
  codegenNamedRules(rs, cppos, hos);
}

void generateLoopRuleTest(const OutputStream& cppos, const OutputStream& hos) {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        nmRule(MatchOrError{4, "Expected an identifier"}, "LoopIdent"),
        nmRule("+", "LoopPlusOperator"),
        nmRule(SkipPoint{0}, "LoopSkip"),
        nmRule(LoopRule::fold(6, 5).skip(2), "LoopSum"),
        nmRule(parseRegexRule("/[a-z]+/"), "LoopIdentRegex"),
        nmRule(MatchOrError{1, "Expected operator '+'"}, "LoopPlusOrError"),
        nmRule(ConcatFlatRule{{ {0, "operand"} }}, "LoopOperand"),

        // Test glueidx == -1
        nmRule(",", "LoopComma"),
        nmRule(
          ConcatFlatRule{{ {0, "elements"}, {2}, {7} }},
          "ListPrefixPart"),
        nmRule(LoopRule::repeat(8).skip(2), "ListPrefix"),

        // Flattenable child.
        nmRule(parseRegexRule("/[-+]/"), "LoopPlusOrMinus"),
        nmRule(ConcatFlatRule{{ {10, "sign"}, {0, "elements"} }},
               "LoopFlatElt"),
        nmRule(LoopRule::fold(11, 7).skip(2), "SignedListContents"),
        StringRule{"["}, StringRule{"]"},
        nmRule(ConcatFlatRule{{ {13}, flatcat(12), {14} }}, "SignedList")
    ),
    .skips{cskip},
    .regexOpts = {regexOpts},
  };

  codegenNamedRules(rs, cppos, hos);
}

void generateGluePartSwappedTest(const OutputStream& cppos,
                                 const OutputStream& hos) {
  RuleSet rs{
    .rules = makeVectorUnique<Rule>(
        StringRule{"-"},
        nmRule(parseRegexRule("/[a-z]+/"), "GpSwappedIdent"),
        nmRule(ConcatFlatRule{{ { 1, "words" } }}, "GpSwappedWord"),
        nmRule(LoopRule::fold(0, 2), "GpSwappedString")
    ),
    .skips{},
    .regexOpts = {regexOpts},
  };

  codegenNamedRules(rs, cppos, hos);
}

}  // namespace

int main(int argc, char* argv[]) {
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
          "using oalex::dropOnError;\n"
          "using oalex::InputDiags;\n"
          "using oalex::JsonLoc;\n"
          "using oalex::keepOnError;\n"
          "using namespace std::string_literals;\n\n"
          "JsonLoc parseAsgnStmt(InputDiags&, size_t&) {\n"
          "  // Unimplemented\n"
          "  return JsonLoc::ErrorValue{};\n"
          "}\n\n"
          "bool goodFunc() { return true; }\n"
          "bool badFunc()  { return false; }\n\n",
          opts.hPathAsIncluded.c_str());
  WriteOrFail cppos{cppfp.get()}, hos{hfp.get()};
  auto linebreaks = [&](){ cppos("\n"); hos("\n"); };

  // TODO first-class support for multiple RuleSets in a file.
  codegenDefaultRegexOptions(RuleSet{{}, {}, {regexOpts}}, cppos);
  linebreaks(); generateSingleStringTest(cppos, hos);
  linebreaks();
  linebreaks();
  generateSingleRegexTest(cppos, hos);
  linebreaks();
  generateWordPreservingWithOverride(cppos, hos);
  linebreaks();
  generateRegexWordOverride(cppos, hos);
  linebreaks();
  generateConcatFlatTest(cppos, hos);
  linebreaks();
  generateSingleWordTemplate(cppos, hos);
  linebreaks();
  generateConcatTest(cppos, hos);
  linebreaks();
  generateNestedTypesTest(cppos, hos);
  linebreaks();
  generateTupleTypesTest(cppos, hos);
  linebreaks();
  generateExternParserDeclaration(cppos, hos);
  linebreaks();
  generateExternParserParams(cppos, hos);
  linebreaks();
  generateIndentedListBuiltin(cppos, hos);
  linebreaks();
  generateOrTest(cppos, hos);
  linebreaks();
  generateMatchOrErrorTest(cppos, hos);
  linebreaks();
  generateOptionalComponent(cppos, hos);
  linebreaks();
  generateAliasRuleTest(cppos, hos);
  linebreaks();
  generateErrorRuleTest(cppos, hos);
  linebreaks();
  generateFlattenOnDemand(cppos, hos);
  linebreaks();
  generateLookaheads(cppos, hos);
  linebreaks();
  generateQuietTest(cppos, hos);
  linebreaks();
  generateMiscFlatteningTest(cppos, hos);
  linebreaks();
  generateLoopRuleTest(cppos, hos);
  linebreaks();
  generateGluePartSwappedTest(cppos, hos);
  return 0;
}
