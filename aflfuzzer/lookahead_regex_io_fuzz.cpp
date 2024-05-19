// With all the sudo, I couldn't integrate this into cmake. Manual fuzzing:
// ~/src/afl/afl-g++ -O3 --std=c++17 ../lookahead_regex_io.cpp \
     lookahead_regex_io_fuzz.cpp ../lexer.cpp -I.. -L../build/runtime -loalex \
     -I../build/_deps/fmt-src/include -L../build/_deps/fmt-build/ -lfmt
// echo core | sudo tee /proc/sys/kernel/core_pattern
// echo performance | sudo tee \
     /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor
// ~/src/afl/afl-fuzz -i tests -o findings ./a.out

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

#include "lookahead_regex_io.h"
#include <cctype>
#include <cstdio>
#include <optional>
#include <string_view>
#include <vector>
#include "runtime/test_util.h"
#include "fmt/core.h"
using oalex::Bug;
using oalex::Diag;
using oalex::Input;
using oalex::InputDiags;
using oalex::isSubstr;
using oalex::regex::Anchor;
using oalex::regex::CharRange;
using oalex::regex::CharSet;
using oalex::regex::Concat;
using oalex::regex::Optional;
using oalex::regex::OrList;
using oalex::regex::Regex;
using oalex::regex::Repeat;
using oalex::regex::get_if_unique;
using oalex::regex::get_unique;
using fmt::format;
using fmt::print;
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
  for(const auto& d:diags) print(stderr, "{}\n", string(d));
  Bug("{} had unexpected errors", testName);
}

auto tryParsing(const string& input, size_t& i) -> optional<Regex> {
  // Caller of regex::parse() is expected to ensure input starts with '/'.
  if(input.empty() || input[i] != '/') return nullopt;
  optional<Regex> res;
  bool hasDiags;
  try {
    InputDiags ctx{Input{input}};
    res = regex::parse(ctx, i);
    hasDiags = !ctx.diags.empty();
  }
  // Silently discard invalid inputs.
  catch(oalex::UserErrorEx&) { return nullopt; }
  // TODO stop catching these once all the "Unimplemented" throws
  // have been removed.
  catch(oalex::UnimplementedEx&) { return nullopt; }
  if(!res && !hasDiags) BugMe("Regex {} silently failed to parse.", input);
  if(hasDiags) return nullopt;
  return res;
}
auto tryParsing(const string& input) -> optional<Regex> {
  size_t i = 0;
  auto rv = tryParsing(input, i);
  if(rv.has_value() && i != input.size())
    BugMe("We were expecting to consume the whole string: {}", input);
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
  if(s.compare(i, 2, "\\/") == 0) { i+=2; return '/'; }
  if(s.compare(i, 2, "\\\\") == 0) { i+=2; return '\\'; }
  if(s.compare(i, 2, "\\t") == 0) { i+=2; return '\t'; }
  if(s.compare(i, 2, "\\n") == 0) { i+=2; return '\n'; }
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

bool astEq(Anchor a, Anchor b) {
  return int(a) == int(b);
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
  if(auto *ac = get_if_unique<Repeat>(&a))
    return astEq(ac->part, get_unique<Repeat>(b).part);
  if(auto *ac = get_if_unique<Optional>(&a))
    return astEq(ac->part, get_unique<Optional>(b).part);
  if(auto *ac = get_if_unique<Anchor>(&a))
    return astEq(*ac, get_unique<Anchor>(b));
  BugMe("Unknown regex index: {}", a.index());
}

template <size_t len> string getline() {
  char buf[len];
  if(!fgets(buf, len, stdin)) return "";
  return string(buf);
}

}  // namespace

int main() {
  try {
    string input = getline<3000>();
    size_t i = 0;
    optional<Regex> parseResult = tryParsing(input, i);
    if(!parseResult) return 0;
    string output = prettyPrint(*parseResult);
    print("Input {} bytes: {}\n", input.size(), input);
    print("Result of first parse: {}\n",  output);
    input.resize(i);  // fuzzer might provide trailing garbage.
    if(holds_alternative<unique_ptr<CharSet>>(*parseResult) &&
       input[1] != '(') {
      // Simple enough for direct string comparison
      if(!regexEqual(input, output))
        BugMe("Regex has changed after pretty-printing: {} became {}",
              input, output);
    }else if(optional<Regex> secondParse = tryParsing(output)) {
      if(!astEq(*secondParse, *parseResult)) {
        BugMe("Regex parses to a different result on the second parse: "
              "{} became {}", input, output);
      }
    }else BugMe("Second parse failed after pretty-printing: {}", output);
  }catch(const std::runtime_error& ex) {
    // This is to ignore fuzzing limits internally in libfmt.
    if(isSubstr("fuzz mode - ", ex.what())) return 0;
    else throw;
  }
}
