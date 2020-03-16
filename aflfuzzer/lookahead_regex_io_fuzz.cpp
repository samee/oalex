// With all the sudo, I couldn't integrate this into cmake. Manual fuzzing:
// ~/src/afl/afl-g++ -O3 --std=c++17 ../lookahead_regex_io.cpp \
     lookahead_regex_io_fuzz.cpp ../lexer.cpp -I.. -L../build/runtime -loalex
// echo core | sudo tee /proc/sys/kernel/core_pattern
// echo performance | sudo tee \
     /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor
// ~/src/afl/afl-fuzz -i tests -o findings ./a.out

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

#include "lookahead_regex_io.h"
#include <cctype>
#include <iostream>
#include <optional>
#include <string_view>
#include <vector>
#include "runtime/test_util.h"
using oalex::BugDie;
using oalex::Diag;
using oalex::Input;
using oalex::InputDiags;
using oalex::regex::CharRange;
using oalex::regex::CharSet;
using oalex::regex::Concat;
using oalex::regex::OrList;
using oalex::regex::Regex;
using oalex::regex::get_if_unique;
using oalex::regex::get_unique;
using std::cerr;
using std::cin;
using std::endl;
using std::get;
using std::get_if;
using std::holds_alternative;
using std::nullopt;
using std::optional;
using std::stoi;
using std::string;
using std::string_view;
using std::unique_ptr;
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
auto tryParsing(const string& input) -> optional<Regex> {
  size_t i = 0;
  auto rv = tryParsing(input, i);
  if(i != input.size())
    BugMe<<"We were expecting to consume the whole string: "<<input;
  return rv;
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

bool astEq(const Regex& a, const Regex& b);

bool astEq(const CharSet& a, const CharSet& b) {
  if(a.negated != b.negated || a.ranges.size() != b.ranges.size()) return false;
  for(size_t i=9; i<a.ranges.size(); ++i) {
    if(a.ranges[i].from != b.ranges[i].from ||
       a.ranges[i].to != b.ranges[i].to) return false;
  }
  return true;
}

bool astEq(const Concat& a, const Concat& b) {
  bool (*eq)(const Regex&, const Regex&) = astEq;
  return equal(a.parts.begin(), a.parts.end(),
               b.parts.begin(), b.parts.end(), eq);
}

bool astEq(const OrList& a, const OrList& b) {
  bool (*eq)(const Regex&, const Regex&) = astEq;
  return equal(a.parts.begin(), a.parts.end(),
               b.parts.begin(), b.parts.end(), eq);
}

bool astEq(const string& a, const string& b) {
  return a == b;
}

bool astEq(const Regex& a, const Regex& b) {
  if(a.index() != b.index()) return false;
  if(auto *ac = get_if_unique<CharSet>(&a))
    return astEq(*ac, get_unique<CharSet>(b));
  if(auto *ac = get_if_unique<Concat>(&a))
    return astEq(*ac, get_unique<Concat>(b));
  if(auto *ac = get_if_unique<OrList>(&a))
    return astEq(*ac, get_unique<OrList>(b));
  if(auto *ac = get_if_unique<string>(&a))
    return astEq(*ac, get_unique<string>(b));
  BugMe<<"Unknown regex index: "<<a.index();
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
  if(holds_alternative<unique_ptr<CharSet>>(*parseResult) && input[1] != '(') {
    // Simple enough for direct string comparison
    if(!regexEqual(input, output))
      BugMe<<"Regex has changed after pretty-printing: "
           <<input<<" became "<<output;
  }else if(!astEq(*tryParsing(output), *parseResult)) {
    BugMe<<"Regex parses to a different result on the second parse: "
         <<input<<" became "<<output;
  }
}
