/*  Copyright 2019-2021 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#pragma once

#include <limits>
#include <map>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include <fmt/core.h>

namespace oalex {

// Even though we call it json, we don't in fact support numbers, booleans,
// or null. We do support an error type. Together with strings, those two are
// the only atomic types.
// stPos and enPos can remain JsonLoc::npos, if they only have hardcoded values.
// The other difference is that we treat strings as byte-strings, not utf-8.
// This also causes a difference in our backslash escaping conventions.
// prettyPrint() also returns something closer to protobufs than json.
// In other words, this is a complete abuse of the term "json".
//
// Another design wart added later: originally the stPos and enPos fields were
// meant to indicate locations in user input, not in oalex source. But an
// exception was later made for JsonLoc::Placeholder, since it never shows up
// in outputs parsed out of user input.
//
// At some point in the future, we might separate out these into two types:
//
//   * The output produced by parsing user input.
//   * The braced portion of an 'outputs' stanza in a rule or example.
//     This will include ellipsis as a variant, but not error values.
struct JsonLoc {

  static constexpr size_t npos = std::numeric_limits<size_t>::max();
  struct ErrorValue {};
  using String = std::string;
  using Vector = std::vector<JsonLoc>;
  using Map = std::map<std::string,JsonLoc, std::less<>>;
  struct Placeholder { std::string key; };
  using Value = std::variant<ErrorValue,Placeholder,String,Vector,Map>;

  Value value;
  size_t stPos=npos, enPos=npos;

  // conversion constructors.
  JsonLoc() = delete;
  JsonLoc(ErrorValue) : value(ErrorValue{}) {}
  JsonLoc(Placeholder p, size_t st, size_t en)
    : value(p), stPos(st), enPos(en) {}
  JsonLoc(String s) : value(s) {}
  JsonLoc(Vector v) : value(v) {}
  JsonLoc(Map m) : value(m) {}

  bool holdsError() const { return std::holds_alternative<ErrorValue>(value); }

  // Note that allPlaceholders() is a non-const member method, since it returns
  // non-const JsonLoc pointers to various internal components. Pretty much any
  // direct mutation to this->value will invalidate these pointers: only
  // substitute() is safe.
  using PlaceholderMap = std::multimap<std::string, JsonLoc*, std::less<>>;
  PlaceholderMap allPlaceholders();

  // The Const version is used for pre-substitution processing, by functions
  // that only accept const JsonLoc objects but extract the identifier list.
  // The type is typically used as a vector<pair<string,const JsonLoc*>>, and
  // not as a map. But this reduces cognitive complexity as users have to
  // remember only one type of methods (the map-returning kind), and we have
  // to implement only one recursive visitor.
  using ConstPlaceholderMap =
    std::multimap<std::string, const JsonLoc*, std::less<>>;
  ConstPlaceholderMap allPlaceholders() const;

  // Returns number of substitutions made. Zero if key doesn't exist.
  size_t substitute(const PlaceholderMap& pmap, std::string_view key,
                    const JsonLoc& json);

  // Check if all placeholders have been substituted.
  // Check if child intervals are covered by parent intervals (ignoring npos).
  // Check if child has a valid interval, so does parent.
  // Check if all (stPos==npos) == (enPos==npos).
  // Typically, this should be checked after all substitutions are made.
  bool substitutionsOk() const;

  // The first line is never indented. No newline follows the last character.
  // Corollary: String and Placeholders are never indented,
  //   and are not newline-terminated.
  std::string prettyPrint(size_t indent=0) const;
  std::string prettyPrintJson(size_t indent=0) const;
  std::string prettyPrintWithLocs(size_t indent=0) const;  // TODO

  // This is false iff we have any ErrorValue or Placeholder nodes.
  //
  // Dev-notes: we may later replace this with hasErrorValue().
  //   See notes on operator==() for reasons.
  bool supportsEquality() const;

  // Dev-notes: Right now this equality check is only used for `oalex test`.
  //   If those tests later acquire support for placeholders or omitted fields,
  //   we will have to generalize this to some relaxed notion of matching.
  bool operator==(const JsonLoc& that) const;
  bool operator!=(const JsonLoc& that) const { return !(*this == that); }
};

template <class X> X* get_if(JsonLoc* json) {
  return std::get_if<X>(&json->value);
}

template <class X> const X* get_if(const JsonLoc* json) {
  return std::get_if<X>(&json->value);
}

template <class X> bool holds_alternative(const JsonLoc& json) {
  return std::holds_alternative<X>(json.value);
}

inline JsonLoc moveEltOrEmpty(JsonLoc::Map& m, std::string_view key) {
  auto it = m.find(key);
  if(it == m.end()) return JsonLoc::Map{};
  else return std::move(it->second);
}

}  // namespace oalex

template <> class fmt::formatter<oalex::JsonLoc> {
  unsigned indent_ = 0;
 public:
  auto parse(format_parse_context& ctx) -> decltype(ctx.begin());

  template <class FormatContext>
  auto format(const oalex::JsonLoc& jsloc, FormatContext& ctx) {
    for(char ch : jsloc.prettyPrint(indent_)) *ctx.out() = ch;
    return ctx.out();
  }
};
