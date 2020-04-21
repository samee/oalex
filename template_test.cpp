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

#include "template.h"
#include "runtime/test_util.h"
using std::optional;
using std::pair;
using std::string;
using std::string_view;
using std::vector;
using namespace std::literals::string_literals;
using oalex::Bug;
using oalex::DelimPair;
using oalex::matchAllParts;

namespace {

void assertHasSubstr(string_view testName, string_view s,
                     size_t st, string_view t) {
  string_view m = s.substr(st, t.size());
  if(m != t)
    Bug()<<testName<<": Matched the wrong part. Found '"<<m
         <<"' instead of '"<<t<<"'";
}

void testMatchAll() {
  const string tmpl = "if (cond) { ... } else { ... }";

  // Test a single match.
  optional<vector<pair<size_t, size_t>>> out = matchAllParts("cond"s, tmpl);

  if(!out) BugMe<<"Couldn't find 'cond' in: "<<tmpl;
  if(out->size() != 1)
    BugMe<<"Was expecting a single match. Found "<<out->size();
  assertHasSubstr(__func__, tmpl, out->at(0).first, "cond");

  out = matchAllParts(DelimPair{"{", "}"}, tmpl);
  if(!out) BugMe<<"Couldn't find '{ ... }' in: "<<tmpl;
  if(out->size() != 2)
    BugMe<<"Was expecting two matches. Found "<<out->size();
  for(auto& bound : *out) {
    assertHasSubstr(__func__, tmpl, bound.first, "{");
    assertHasSubstr(__func__, tmpl, bound.second-"}"s.size(), "}");
  }
}

}  // namespace

int main() {testMatchAll();}
