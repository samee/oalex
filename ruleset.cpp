/*  Copyright 2019-2025 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "ruleset.h"

#include <format>
#include <string>
#include <string_view>
#include <utility>
#include <vector>
#include "runtime/diags.h"
#include "runtime/jsonloc.h"
#include "runtime/util.h"
using std::format;
using std::pair;
using std::string;
using std::string_view;
using std::vector;

namespace oalex {

void
Rule::deferred_name(Ident name) {
  if(name_) Bug("Cannot rename rule {} to {}",
                name_.preserveCase(), name.preserveCase());
  if(name) exposure().state(UserExposure::topLevel);
  name_ = name;
}
void
Rule::context_skipper(ssize_t skipper_index) {
  if(contextSkipper_ != Rule::helperRuleNoContext)
    Bug("We shouldn't have to assign context_skipper() to the same rule "
        "multiple times.");
  contextSkipper_ = skipper_index;
}

OutputTypeInfo
WrapperRule::outType(const RuleSet& rs) const {
  const Rule& r = *rs.rules.at(ts_);
  auto ot = r.outType(rs).type();
  return {&rs, r, ot};
}

static bool validExtName(string_view name) {
  return name.find("oalexPlugin") == 0 || name.find("oalexBuiltin") == 0;
}

ssize_t
expectedParamCount(string_view builtinSuff) {
  const vector<pair<string, ssize_t>> builtin_param_counts {
    {"Hello", 0}, {"IndentedList", 2}
  };
  for(auto& [name,count] : builtin_param_counts) if(name == builtinSuff) {
    return count;
  }
  return -1;
}

bool ExternParser::requireValidNameAndParamCount(
    const StringLoc& extName, ssize_t providedParamCount, DiagsDest ctx) {
  if(extName->find("oalexPlugin") == 0) return true;
  else if(extName->find("oalexBuiltin") != 0) {
    Error(ctx, extName.stPos(), extName.enPos(),
          "External parser names need to start either with 'oalexPlugin...'"
          " or 'oalexBuiltin...'");
    return false;
  }
  string_view suff = *extName;
  suff.remove_prefix(sizeof("oalexBuiltin")-1);
  ssize_t expected = expectedParamCount(suff);
  if(expected == providedParamCount) return true;
  else if(expected == -1) {
    Error(ctx, extName.stPos(), extName.enPos(),
          format("{} is not a known builtin parser", *extName));
  }else {
    Error(ctx, extName.stPos(), extName.enPos(), format(
            "oalexBuiltin{}() expects {} parameters, but {} was provided",
            suff, expected, providedParamCount));
  }
  return false;
}

ExternParser::ExternParser(string_view extName, vector<ssize_t> params)
  : externalName_{extName}, params_{std::move(params)} {
  if(!validExtName(extName))
    Bug("External names need to start either with oalexPlugin or oalexBuiltin");
}

const string&
ExternParser::externalName() const {
  if(!externalName_.empty()) return externalName_;
  else Bug("External parsers must have an external name");
}

UserExposure::State UserExposure::state() const {
  switch(state_) {
    case unknown:
    case topLevel:
    case builtin:
    case notExposed:
    case notGenerated:
      return State{state_};
    // case nested intentionally left out. It should never
    // be explicitly represented.
    default:
      if(state_ >= 0) return State::nested;
      else Bug("Invalid UserExposure::Status {}", state_);
  }
}

bool
resultFlattenableOrError(const RuleSet& rs, ssize_t ruleidx) {
  OutputType t = outType(rs, ruleidx).type();
  return t == OutputType::flatStruct || t == OutputType::nullopt;
}

bool
makesFlatStruct(const RuleSet& rs, ssize_t ruleidx) {
  OutputType t = outType(rs, ruleidx).type();
  return t == OutputType::flatStruct;
}


}  // namespace oalex
