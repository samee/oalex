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
#include <optional>
#include <string>
#include <vector>
#include <libgen.h>
using std::nullopt;
using std::optional;
using std::size;
using std::string;
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
  string filename;  // TODO support more than a single filename.
  string cppoutFilename;
  string houtFilename;
  string testoutFilename;
};

struct CmdModeTableEntry {
  vector<string> selectors;
  CmdMode mode;
  optional<CmdlineOptions> (*parse)(int argc, char *argv[]);
};

// TODO
optional<CmdlineOptions>
getRulesetFilename(int, char **) {
  return CmdlineOptions{};
}

// TODO
optional<CmdlineOptions>
getInputOutputFilenames(int, char **) {
  return CmdlineOptions{};
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
    optional<CmdlineOptions> rv = entry.parse(argc-n, argv+n);
    if(rv.has_value()) rv->mode = entry.mode;
    return rv;
  }
  return nullopt;
}

}  // namespace

int main(int argc, char *argv[]) {
  optional<CmdlineOptions> cmdlineOpts = parseCmdlineOptions(argc, argv);
  if(cmdlineOpts.has_value()) fprintf(stderr, "Nothing is implemented yet.\n");
  fprintf(stderr, "%s\n", usage);
  return 1;
}
