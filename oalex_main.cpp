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

/*
13 Dec 2020: As I make this initial commit, this is supposed to be a fairly
scrappy frontend. It will accept a language that represents codegen.h RuleSet
fairly directly. Slowly, I'll evolve it into something more featureful.
*/

#include <cstdio>
#include <cstring>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>
#include <getopt.h>
#include <libgen.h>
#include "codegen.h"
#include "frontend.h"
#include "oalex.h"
#include "runtime/util.h"
#include "fmt/core.h"
using fmt::format;
using oalex::Bug;
using oalex::codegen;
using oalex::codegenDefaultRegexOptions;
using oalex::defaultSkip;
using oalex::Diag;
using oalex::Example;
using oalex::Ident;
using oalex::Input;
using oalex::InputDiags;
using oalex::is_in;
using oalex::JsonLoc;
using oalex::ParsedSource;
using oalex::parseOalexSource;
using oalex::RegexOptions;
using oalex::Rule;
using oalex::RuleSet;
using oalex::Skipper;
using oalex::UserError;
using std::nullopt;
using std::optional;
using std::size;
using std::string;
using std::string_view;
using std::unique_ptr;
using std::vector;

namespace {

const char usage[] = R"(
Usage:  oalex [eval] filename                    # Parses stdin
        oalex [eval] test filename               # Runs all examples
        oalex build [--cpp-out=outname.cpp] [--h-out=outname.h] \
              filename                           # Generate all parsers
        oalex build test [--cpp-out=outname.cpp] \
                         [--h-out=outname.h] \
                         [--test-cpp-out=outname_test.cpp] \
                         filename                # Generate test drivers

        oalex --help                             # This message
        oalex help                               #
)";

enum class CmdMode { eval, evalTest, build, buildTest };

struct CmdlineOptions {
  CmdMode mode;
  string inFilename;  // TODO support more than a single filename.
  string cppOutFilename;
  string hOutFilename;
  string testOutFilename;
};

struct CmdModeTableEntry {
  vector<string> selectors;
  CmdMode mode;
  optional<CmdlineOptions> (*parse)(int argc, char *argv[], int start);
};

optional<CmdlineOptions>
getRulesetFilename(int argc, char *argv[], int start) {
  argc-=start; argv+=start;
  if(argc > 1) {
    fprintf(stderr, "oalex currently supports only a single input file\n");
    return nullopt;
  }else if(argc < 1) {
    fprintf(stderr, "Input filename missing\n");
    return nullopt;
  }
  CmdlineOptions rv;
  rv.inFilename = argv[0];
  return rv;
}

bool knownArg(string_view arg, const struct option *opts) {
  arg = arg.substr(2);  // Assume first two chars are "--".
  for(int i=0; opts[i].name; ++i) {
    if(opts[i].name == arg) return true;
    size_t n = strlen(opts[i].name);
    if(arg.substr(0, n) == opts[i].name && arg.size() > n && arg[n]=='=')
      return true;
  }
  return false;
}

// getopt() allows prefixes that we want to disallow. E.g. --test-out can
// be shortened to just --test. getopt() accepts it, but we don't.
bool hasUnknownArg(int argc, char *argv[], int start,
                   const struct option *opts) {
  for(int i=start; i<argc; ++i)
    if(string_view(argv[i]).substr(0, 2) == "--" && !knownArg(argv[i], opts)) {
      fprintf(stderr, "Unknown option '%s'\n", argv[i]);
      return true;
    }
  return false;
}

optional<CmdlineOptions>
getInputOutputFilenames(int argc, char *argv[], int start) {
  enum : int { cppOutFlag, hOutFlag, testOutFlag };
  const struct option opts[] = {
    {"cpp-out",  required_argument, nullptr, cppOutFlag},
    {"h-out",    required_argument, nullptr, hOutFlag},
    {"test-out", required_argument, nullptr, testOutFlag},
    {0, 0, 0, 0}
  };
  optind = start;
  CmdlineOptions rv{};
  if(hasUnknownArg(argc, argv, start, opts)) return nullopt;
  while(1) {
    int c = getopt_long(argc, argv, "", opts, nullptr);
    switch(c) {
      case -1: {
        optional<CmdlineOptions> in = getRulesetFilename(argc, argv, optind);
        if(!in.has_value()) return nullopt;
        rv.inFilename = std::move(in->inFilename);
        return rv;
      }
      case cppOutFlag:
        rv.cppOutFilename = optarg;
        break;
      case hOutFlag:
        rv.hOutFilename = optarg;
        break;
      case testOutFlag:
        rv.testOutFilename = optarg;
        break;
      default:
        return nullopt;
    }
  }
}

optional<CmdlineOptions>
showUsageAndAbort(int, char**, int) {
  fprintf(stderr, "%s\n", usage);
  return nullopt;
}

// This table is matched from bottom to top in parseCmdlineOptions. So the most
// specific command prefix should appear at the end.
const CmdModeTableEntry
cmdModeTable[] = {
  { {"oalex"}, CmdMode::eval, getRulesetFilename },
  { {"oalex", "eval"}, CmdMode::eval, getRulesetFilename },
  { {"oalex", "test"}, CmdMode::evalTest, getRulesetFilename },
  { {"oalex", "eval", "test"}, CmdMode::evalTest, getRulesetFilename },
  { {"oalex", "build"}, CmdMode::build, getInputOutputFilenames },
  { {"oalex", "build", "test"}, CmdMode::buildTest, getInputOutputFilenames },
  { {"oalex", "help"}, CmdMode{}, showUsageAndAbort },
  { {"oalex", "--help"}, CmdMode{}, showUsageAndAbort },
};

optional<CmdlineOptions>
parseCmdlineOptions(int argc, char *argv[]) {
  if(argc > 0 && argv[0]) argv[0] = basename(argv[0]);  // my/oalex --> oalex
  for(int i=size(cmdModeTable)-1; i>=0; --i) {
    auto& entry = cmdModeTable[i];
    int n = entry.selectors.size();
    if(argc < n) continue;
    if(!equal(entry.selectors.begin(), entry.selectors.end(), argv)) continue;
    optional<CmdlineOptions> rv = entry.parse(argc, argv, n);
    if(rv.has_value()) rv->mode = entry.mode;
    return rv;
  }
  fprintf(stderr, "%s\n", usage);
  return nullopt;
}

bool validate(const CmdlineOptions& opts) {
  if(opts.mode == CmdMode::build) {
    if(!opts.testOutFilename.empty()) {
      fprintf(stderr, "'oalex build' doesn't use a --test-cpp-out flag\n");
      return false;
    }else if(opts.cppOutFilename.empty() != opts.hOutFilename.empty()) {
      fprintf(stderr, "Both --cpp-out and --h-out must be provided, "
                      "or neither should be\n");
      return false;
    }
  }else if(opts.mode == CmdMode::buildTest) {
    if(opts.cppOutFilename.empty() != opts.hOutFilename.empty() ||
       opts.cppOutFilename.empty() != opts.testOutFilename.empty()) {
      fprintf(stderr, "All of --cpp-out, --h-out, and --test-cpp-out must be "
                      "specified, or none of them should be.\n");
      return false;
    }
  }
  return true;
}

string_view tryRemovingSuffix(string_view s, string_view suff) {
  if(s.size() < suff.size() || s.substr(s.size()-suff.size()) != suff)
    return s;
  else return s.substr(0, s.size()-suff.size());
}

void fillOutputFilenames(CmdlineOptions& opts) {
  if(opts.mode != CmdMode::build && opts.mode != CmdMode::buildTest) return;
  string sansExt{tryRemovingSuffix(opts.inFilename, ".oalex")};
  opts.cppOutFilename = sansExt + ".cpp";
  opts.hOutFilename = sansExt + ".h";
  if(opts.mode == CmdMode::buildTest)
    opts.testOutFilename = std::move(sansExt) + "_test.cpp";
}

optional<string> fileContents(const string& filename) {
  unique_ptr<FILE, decltype(&fclose)> fp(
      fopen(filename.c_str(), "r"), &fclose
  );
  if(!fp) {
    fprintf(stderr, "Could not open %s for reading\n", filename.c_str());
    return nullopt;
  }
  if(fseek(&*fp, 0, SEEK_END) != 0) {
    fprintf(stderr, "Seeking failed for file %s\n", filename.c_str());
    return nullopt;
  }
  long off = ftell(&*fp);
  if(off == -1) {
    fprintf(stderr, "Couldn't find the filesize of %s\n", filename.c_str());
    return nullopt;
  }
  if(fseek(&*fp, 0, SEEK_SET) != 0) {
    fprintf(stderr, "Seekback failed for file %s\n", filename.c_str());
    return nullopt;
  }
  string s(off, ' ');
  if(fread(s.data(), 1, off, &*fp) < size_t(off)) {
    fprintf(stderr, "Failed to read from file %s\n", filename.c_str());
    return nullopt;
  }
  return s;
}

void diagsToStderr(const vector<Diag>& diags) {
  for(const auto& d : diags)
    fprintf(stderr, "  %s\n", string(d).c_str());
}

// Replace this class with std::scope_exit when that's available.
class DiagsExitPrinter {
 public:
  explicit DiagsExitPrinter(const vector<Diag>& diags) : diags_{&diags} {}
  ~DiagsExitPrinter() { diagsToStderr(*diags_); }
 private:
  const vector<Diag>* diags_;
};

auto parseOalexFile(const string& filename) -> optional<ParsedSource> {
  optional<string> s = fileContents(filename);
  if(!s.has_value()) return nullopt;
  InputDiags ctx{Input{*s}};
  DiagsExitPrinter exit_printer{ctx.diags};
  auto rv = parseOalexSource(ctx);
  return rv;
}

// It's a heuristic for the first rule as the user sees it,
// not the first in how we arranged it in RuleSet.
ssize_t firstRule(const RuleSet& rs) {
  size_t idpos = size_t(-1);
  ssize_t rv = -1;
  for(size_t i=0; i<rs.rules.size(); ++i) {
    const Ident* name = rs.rules[i]->nameOrNull();
    if(name && name->stPos() > 0 && name->stPos() < idpos) {
      rv = i;
      idpos = name->stPos();
    }
  }
  return rv;
}

class StdinStream : public oalex::InputStream {
 public:
  int16_t getch() override
    { int ch = getchar(); return ch == EOF ? -1 : ch; }
};

JsonLoc processStdin(const RuleSet& rs) {
  StdinStream ss;
  InputDiags ctx(Input{&ss});
  ssize_t pos = 0;
  ssize_t rule_i = firstRule(rs);
  if(rule_i == -1) {
    fprintf(stderr, "No rule defined");
    return JsonLoc::ErrorValue{};
  }
  pos = defaultSkip().acrossLines(ctx.input, pos);
  JsonLoc jsloc = eval(ctx, pos, rs, rule_i);
  if(!jsloc.holdsErrorValue()) return jsloc;
  for(const auto& d : ctx.diags) fprintf(stderr, "%s\n", string(d).c_str());
  return JsonLoc::ErrorValue{};
}

ssize_t findRule(const RuleSet& ruleSet, const Ident& ruleName) {
  for(ssize_t i=0; i*1ul<ruleSet.rules.size(); ++i)
    if(const Ident* nm = ruleSet.rules[i]->nameOrNull())
      if(*nm == ruleName) return i;
  return -1;
}

bool atInputEnd(const Input& input, size_t pos) {
  return !input.sizeGt(pos) ||
         (!input.sizeGt(pos+1) && input[pos] == '\n');
}

// Returns true on success.
bool testExample(const RuleSet& rs, const Example& ex) {
  InputDiags ctx{Input{ex.sampleInput}};
  ssize_t pos = 0;
  ssize_t ruleIndex = findRule(rs, ex.ruleName);
  if(ruleIndex < 0)
    Bug("Rule {} not found. The frontend should have already "
        "detected this error", ex.ruleName.preserveCase());
  JsonLoc jsloc = eval(ctx, pos, rs, ruleIndex);

  if (ex.expectation.matches(jsloc, ctx.diags)) {
    if (ex.expectation.isForSuccess() && !atInputEnd(ctx.input, pos)) {
      fprintf(stderr, "Did not consume the entire input at success. "
                      "Stopped parsing at position %ld '%s'", pos,
                      debugPrefix(ctx.input, pos).c_str());
      return false;
    }else return true;
  }

  bool success = Example::runSucceeded(jsloc, ctx.diags);
  fprintf(stderr, "%s\n", describeTestFailure(ex, success).c_str());
  if(!jsloc.holdsErrorValue())
    fprintf(stderr, "Output: %s\n", jsloc.prettyPrint().c_str());
  if(!ctx.diags.empty()) {
    fprintf(stderr, "Errors received:\n");
    diagsToStderr(ctx.diags);
  }
  return false;
}

bool testAllExamples(const ParsedSource& src) {
  bool rv = true;
  for(const Example& ex : src.examples)
    if(!testExample(src.ruleSet, ex)) rv = false;
  return rv;
}

// Combination of unique_ptr<FILE, declttype(&fclose)> and a closure that
// can write to file, so it can be used directly in codegen.h.
// fp_ remains private.
// TODO export this class to codegen tests.
class FileStream final : public oalex::OutputStream {
 public:
  explicit FileStream(FILE* fp) : fp_(fp) {}
  ~FileStream() { fclose(fp_); }

  // Boilerplate: movable type but not copyable.
  FileStream(const FileStream&) = delete;
  FileStream(FileStream&& that) noexcept : fp_(that.fp_) { that.fp_ = nullptr; }
  FileStream& operator=(const FileStream&) = delete;
  FileStream& operator=(FileStream&& that) noexcept {
    fp_ = that.fp_;
    that.fp_ = nullptr;
    return *this;
  }

  explicit operator bool() const { return fp_; }

  void operator()(string_view s) const override {
    if(fwrite(s.data(), s.size(), 1, fp_) < 1) UserError("File write error");
  }
 private:
  FILE* fp_;
};

FileStream fopenw(const string& s) {
  FileStream fs{fopen(s.c_str(), "w")};
  if(!fs) UserError("Couldn't open file '{}' for writing.", s);
  return fs;
}

bool needsCodegen(const Rule& r) {
  return r.nameOrNull() != nullptr;
}

void produceSourceFiles(const ParsedSource& src,
    const string& cppFname, const string& hFname) {
  if(is_in('"', hFname))
    UserError("Output header filename cannot contain '\"'");

  FileStream cppos = fopenw(cppFname);
  FileStream hos = fopenw(hFname);

  hos("#pragma once\n"
      "#include <cstdint>\n"
      "#include <oalex.h>\n\n");
  cppos(format("#include \"{}\"\n"
               "using oalex::InputDiags;\n"
               "using oalex::JsonLoc;\n"
               "using namespace std::string_literals;\n"
               "\n",
               hFname));

  codegenDefaultRegexOptions(src.ruleSet, std::ref(cppos));
  cppos("\n");

  for(size_t i=0; i<src.ruleSet.rules.size(); ++i)
    if (needsCodegen(*src.ruleSet.rules[i])) {
      codegen(src.ruleSet, i, std::ref(cppos), std::ref(hos));
      cppos("\n");
      hos("\n");
    }
}

}  // namespace

int main(int argc, char *argv[]) {
  optional<CmdlineOptions> cmdlineOpts = parseCmdlineOptions(argc, argv);
  if(!cmdlineOpts.has_value()) return 1;

  auto& in = cmdlineOpts->inFilename;
  auto& cpp = cmdlineOpts->cppOutFilename;
  auto& h = cmdlineOpts->hOutFilename;
  auto& test = cmdlineOpts->testOutFilename;
  if(!validate(*cmdlineOpts)) return 1;
  if(cpp.empty()) fillOutputFilenames(*cmdlineOpts);

  if(cmdlineOpts->mode == CmdMode::eval) {
    optional<ParsedSource> src = parseOalexFile(cmdlineOpts->inFilename);
    if(!src.has_value()) return 1;
    JsonLoc res = processStdin(src->ruleSet);
    printf("%s\n", res.prettyPrintJson().c_str());
    return res.holdsErrorValue() ? 1 : 0;
  }else if(cmdlineOpts->mode == CmdMode::evalTest) {
    optional<ParsedSource> src = parseOalexFile(cmdlineOpts->inFilename);
    if(!src.has_value()) return 1;
    return testAllExamples(*src) ? 0 : 1;
  }else if(cmdlineOpts->mode == CmdMode::build) {
    optional<ParsedSource> src = parseOalexFile(cmdlineOpts->inFilename);
    if(!src.has_value()) return 1;
    if(!cmdlineOpts->testOutFilename.empty()) {
      fprintf(stderr, "Test file generation not yet implemented");
      return 1;
    }
    produceSourceFiles(*src, cmdlineOpts->cppOutFilename,
                             cmdlineOpts->hOutFilename);
    return 0;
  }else {
    fprintf(stderr, "This mode isn't implmented yet");
    return 1;
  }

  // Parsing result, to be removed once we have proper tests that use files.
  if(!in.empty())
    fprintf(stderr, "Input file: \"%s\"\n", in.c_str());
  if(!cpp.empty() || !h.empty() || !test.empty())
    fprintf(stderr, "Output files:\n");
  if(!cpp.empty())
    fprintf(stderr, "  Parser .cpp file: \"%s\"\n", cpp.c_str());
  if(!h.empty())
    fprintf(stderr, "  Parser .h file: \"%s\"\n", h.c_str());
  if(!test.empty())
    fprintf(stderr, "  Test-driver .cpp file: \"%s\"\n", test.c_str());
  fprintf(stderr, "Nothing is implemented yet.\n");
  return 1;
}
