/*  Copyright 2019-2020 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "fmt/format.h"
#include "test_util.h"
#include "util_impl.h"
using oalex::assertHasDiagWithSubstr;
using oalex::Bug;
using oalex::BugWarn;
using oalex::Diag;
using oalex::Input;
using oalex::InputDiags;
using oalex::isSubstr;
using oalex::showDiags;
using oalex::UserErrorEx;
using fmt::format_to;
using fmt::memory_buffer;
using fmt::print;
using fmt::to_string;
using std::string;
using std::string_view;
using std::vector;

auto fmt::formatter<std::vector<std::string>>::format(
    const vector<string>& v, fmt::format_context& ctx)
  -> decltype(format_to(ctx.out(), "")) {
  if(v.empty()) return format_to(ctx.out(), "{{}}");
  format_to(ctx.out(), "{{{}", v[0]);
  for(size_t i=1;i<v.size();++i) format_to(ctx.out(), ", {}", v[i]);
  return format_to(ctx.out(), "}}");
}

namespace oalex{

void showDiags(const vector<Diag>& diags) {
  memory_buffer buf;
  format_to(buf, "diags:\n");
  for(const auto& d : diags) format_to(buf, "  {}\n", string(d));
  BugWarn("{}", to_string(buf));
}

void assertHasDiagWithSubstr(string_view testName, const vector<Diag>& diags,
                             string_view expectedDiag) {
  for(const Diag& d : diags) if(isSubstr(expectedDiag, d.msg)) return;
  showDiags(diags);
  Bug("{} didn't get the expected diag: {}", testName, expectedDiag);
}

void assertHasDiagWithSubstrOnce(
    string_view testName, const vector<Diag>& diags,
    string_view expectedDiag) {
  size_t c = 0;
  for(const Diag& d : diags) if(isSubstr(expectedDiag, d.msg)) c++;
  if(c == 1) return;
  showDiags(diags);
  if(c == 0)
    Bug("{} didn't get the expected diag: {}", testName, expectedDiag);
  else
    Bug("{} emitted the expected diag too many times", testName);
}

void assertHasDiagWithSubstrAt(string_view testName, const vector<Diag>& diags,
                               string_view expectedDiag, size_t expectedStPos) {
  for(const Diag& d : diags) {
    if(d.stPos != expectedStPos+1) continue;  // The +1 is from Diags() ctor
    if(!isSubstr(expectedDiag, d.msg)) continue;
    return;
  }
  showDiags(diags);
  Bug("{} didn't get the expected diag at position {}: {}",
      testName, expectedStPos, expectedDiag);
}

void assertEmptyDiags(string_view testName, const vector<Diag>& diags) {
  if(diags.empty()) return;
  for(const auto& d:diags) print(stderr, "{}\n", string(d));
  Bug("{} had unexpected errors", testName);
}

class GetFromString : public InputStream {
  std::string src;
  size_t i=0;
 public:
  explicit GetFromString(std::string_view src):src(src) {}
  int16_t getch() override { return i<src.size()?src[i++]:-1; }
};

void assertProducesDiag(std::string_view testName, std::string_view input,
                        std::string_view err,
                        void (*cb)(oalex::InputDiags&, size_t&)) {
  GetFromString si{input};
  InputDiags ctx{Input{&si}};
  size_t i = 0;
  try {
    cb(ctx, i);
  }catch(UserErrorEx& ex) {
    Error(ctx,0,0,ex.what());  // demote Fatal() error to non-fatal.
  }
  assertHasDiagWithSubstr(testName, ctx.diags, err);
}

}  // namespace oalex
