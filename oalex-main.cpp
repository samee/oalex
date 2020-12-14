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
#include <string>
#include <vector>
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
  CmdMode (*parse)(int argc, char *argv[]);
};

CmdMode getRulesetFilename(int, char **) { return {}; }  // TODO
CmdMode getInputOutputFilenames(int, char **) { return {}; }  // TODO

CmdModeTableEntry cmdModeTable[] = {
  { {"oalex"}, CmdMode::eval, getRulesetFilename },
  { {"oalex", "eval"}, CmdMode::eval, getRulesetFilename },
  { {"oalex", "test"}, CmdMode::evalTest, getRulesetFilename },
  { {"oalex", "eval", "test"}, CmdMode::evalTest, getRulesetFilename },
  { {"oalex", "build"}, CmdMode::build, getInputOutputFilenames },
  { {"oalex", "build", "test"}, CmdMode::buildTest, getInputOutputFilenames },
};

}  // namespace

int main(int argc, char *argv[]) {
  if(argc > 1 && argv) fprintf(stderr, "Nothing is implemented yet.\n");
  fprintf(stderr, "%s\n", usage);
  return 1;
}
