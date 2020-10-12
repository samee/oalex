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

#include "runtime/util.h"
#include <cstdio>
#include <memory>
#include <string>
#include <unistd.h>
using oalex::Bug;
using oalex::UserError;
using std::string;
using std::unique_ptr;

namespace {

struct CmdLineOpts {
  string outputCppPath;
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
  return CmdLineOpts{.outputCppPath = rv + ".cpp"};
}

auto fopenw(const string& s) -> unique_ptr<FILE, decltype(&fclose)> {
  unique_ptr<FILE, decltype(&fclose)> fp(fopen(s.c_str(), "w"), fclose);
  if(!fp) UserError("Couldn't write to file {}", s);
  return fp;
}

}  // namespace

int main(int argc, char* argv[]) {
  try {
    CmdLineOpts opts = parseCmdLine(argc, argv);
    auto fp = fopenw(opts.outputCppPath);
    fputs("bool goodFunc() { return true; }\n", fp.get());
    fputs("bool badFunc()  { return false; }\n", fp.get());
    return 0;
  }catch(const oalex::UserErrorEx& ex) {
    fprintf(stderr, "%s: %s\n", argv[0], ex.what());
    return 1;
  }
}
