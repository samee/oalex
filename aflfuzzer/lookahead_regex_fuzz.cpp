// With all the sudo, I couldn't integrate this into cmake. Manual fuzzing:
// ~/src/afl/afl-g++ -O3 --std=c++17 ../lookahead_regex.cpp \
     lookahead_regex_test.cpp -I.. -L../build/runtime -loalex
// echo core | sudo tee /proc/sys/kernel/core_pattern
// echo performance | sudo tee \
     /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor
// ~/src/afl/afl-fuzz -i tests -o findings ./a.out

#include "lookahead_regex.h"
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

auto tryParsing(const string& input) -> optional<Regex> {
  optional<Regex> res;
  bool hasDiags;
  try {
    InputDiags ctx{Input{input}, {}};
    size_t i = 0;
    res = regex::parse(ctx, i);
    hasDiags = !ctx.diags.empty();
  }
  // Silently discard invalid inputs.
  catch(oalex::UserErrorEx&) { return nullopt; }
  // TODO stop catching bugs once all the "Unimplemented" throws
  // have been removed.
  catch(oalex::BugEx&) { return nullopt; }
  if(!res && !hasDiags) BugMe<<"Regex "<<input<<" silently failed to parse.";
  return res;
}

}  // namespace

int main() {
  string input;
  getline(cin, input);
  optional<Regex> parseResult = tryParsing(input);
  if(!parseResult) return 0;
  string output = regex::prettyPrint(*parseResult);
  if(input != output)
    BugMe<<"Regex has changed after pretty-printing: "
         <<input<<" became "<<output;
}
