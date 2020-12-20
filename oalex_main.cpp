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
#include "oalex.h"
using oalex::Input;
using oalex::JsonLoc;
using oalex::RuleSet;
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

// This table is matched from bottom to top in parseCmdlineOptions. So the most
// specific command prefix should appear at the end.
// TODO `oalex help`
const CmdModeTableEntry
cmdModeTable[] = {
  { {"oalex"}, CmdMode::eval, getRulesetFilename },
  { {"oalex", "eval"}, CmdMode::eval, getRulesetFilename },
  { {"oalex", "test"}, CmdMode::evalTest, getRulesetFilename },
  { {"oalex", "eval", "test"}, CmdMode::evalTest, getRulesetFilename },
  { {"oalex", "build"}, CmdMode::build, getInputOutputFilenames },
  { {"oalex", "build", "test"}, CmdMode::buildTest, getInputOutputFilenames },
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

auto parseOalexFile(const string& filename) -> optional<RuleSet> {
  optional<string> s = fileContents(filename);
  if(!s.has_value()) return nullopt;
  Input in(*s);
  if(in.hasPrefix(0, "require_politeness\n")) return RuleSet{};
  fprintf(stderr, "Doesn't insist on politeness\n");
  return nullopt;
}

JsonLoc processStdin(const RuleSet&) { return JsonLoc::ErrorValue{}; }

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
    optional<RuleSet> rs = parseOalexFile(cmdlineOpts->inFilename);
    if(!rs.has_value()) return 1;
    JsonLoc res = processStdin(*rs);
    printf("%s\n", res.prettyPrintJson().c_str());
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
