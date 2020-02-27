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
using std::stoi;
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
  if(input.empty() || input[i] != '/') return nullopt;
  optional<Regex> res;
  bool hasDiags;
  try {
    InputDiags ctx{Input{input}, {}};
    res = regex::parse(ctx, i);
    hasDiags = !ctx.diags.empty();
  }
  // Silently discard invalid inputs.
  catch(oalex::UserErrorEx&) { return nullopt; }
  // TODO stop catching these once all the "Unimplemented" throws
  // have been removed.
  catch(oalex::UnimplementedEx&) { return nullopt; }
  if(!res && !hasDiags) BugMe<<"Regex "<<input<<" silently failed to parse.";
  if(hasDiags) return nullopt;
  return res;
}

auto parseHex(const string& s, size_t& i) -> optional<char> {
  if(s.size() < i+4) return nullopt;
  if(s.compare(i, 2, "\\x") != 0) return nullopt;
  i += 4;
  // We can expect conversion to succeed since this is used after
  // regex::parse() succeeds.
  return stoi(string(s.substr(i-2,2)),nullptr,16);
}

auto parseEsc(const string& s, size_t& i) -> optional<char> {
  if(s[i] != '\\') return nullopt;
  if(auto ch = parseHex(s, i)) return ch;
  if(s.compare(i, 2, "\\]") == 0) { i+=2; return ']'; }
  if(s.compare(i, 2, "\\-") == 0) { i+=2; return '-'; }
  if(s.compare(i, 2, "\\^") == 0) { i+=2; return '^'; }
  if(s.compare(i, 2, "\\\\") == 0) { i+=2; return '\\'; }
  return nullopt;
}

// This might go a little overboard in allowing equivalence where it shouldn't.
bool regexEqual(const string& a, const string& b) {
  size_t i=0, j=0;
  while(i<a.size() && j<b.size()) {
    char ai, bj;
    if(auto ch=parseEsc(a,i)) ai=*ch; else ai=a[i++];
    if(auto ch=parseEsc(b,j)) bj=*ch; else bj=b[j++];
    if(ai != bj) return false;
  }
  return i==a.size() && j==b.size();
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
  if(!regexEqual(input, output))
    BugMe<<"Regex has changed after pretty-printing: "
         <<input<<" became "<<output;
}
