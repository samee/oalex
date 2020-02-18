// With all the sudo, I couldn't integrate this into cmake. Manual fuzzing:
// ~/src/afl/afl-g++ -O3 --std=c++17 ../lookahead_regex.cpp \
     lookahead_regex_fuzz.cpp -I.. -L../build/runtime -loalex
// echo core | sudo tee /proc/sys/kernel/core_pattern
// echo performance | sudo tee \
     /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor
// ~/src/afl/afl-fuzz -i tests -o findings ./a.out

#include "lookahead_regex.h"
#include <cctype>
#include <iostream>
#include <string_view>
#include <vector>
#include "runtime/test_util.h"
using oalex::BugDie;
using oalex::Diag;
using oalex::Input;
using oalex::InputDiags;
using oalex::regex::CharRange;
using oalex::regex::CharSet;
using oalex::regex::Regex;
using std::cerr;
using std::cin;
using std::endl;
using std::nullopt;
using std::optional;
using std::string;
using std::string_view;
using std::vector;

namespace regex = oalex::regex;

namespace {

void abortScreaming(string_view testName, const vector<Diag>& diags) {
  for(const auto& d:diags) cerr<<string(d)<<endl;
  BugDie()<<testName<<" had unexpected errors";
}

auto tryParsing(const string& input, size_t& i) -> optional<Regex> {
  // Caller of regex::parse() is expected to ensure input starts with '/'.
  if(input.empty() || input[0] != '/') return nullopt;
  optional<Regex> res;
  bool hasDiags;
  try {
    InputDiags ctx{Input{input}, {}};
    res = regex::parse(ctx, i);
    hasDiags = !ctx.diags.empty();
  }
  // Silently discard invalid inputs.
  catch(oalex::UserErrorEx&) { return nullopt; }
  // TODO stop catching bugs once all the "Unimplemented" throws
  // have been removed.
  catch(oalex::BugEx&) { return nullopt; }
  if(!res && !hasDiags) BugMe<<"Regex "<<input<<" silently failed to parse.";
  if(hasDiags) return nullopt;
  return res;
}

}  // namespace

int main() {
  string input;
  getline(cin, input);
  size_t i = 0;
  optional<Regex> parseResult = tryParsing(input, i);
  if(!parseResult) return 0;
  string output = regex::prettyPrint(*parseResult);
  input.resize(i);  // fuzzer might provide trailing garbage.
  if(input != output)
    BugMe<<"Regex has changed after pretty-printing: "
         <<input<<" became "<<output;
}
