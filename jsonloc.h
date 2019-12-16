/*  Copyright 2019 Google LLC

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

namespace oalex {

// Even though we call it json, we don't in fact support numbers, booleans,
// or null. It's all strings here, that's the only atomic datatype.
// stpos and enpos can remain JsonLoc::npos, if they only have hardcoded values.
// The other difference is that we treat strings as byte-strings, not utf-8.
// This also causes a difference in our backslash escaping conventions.
// prettyPrint() also returns something closer to protobufs than json.
// In other words, this is a complete abuse of the term "json".
struct JsonLoc {

  static constexpr size_t npos = std::numeric_limits<size_t>::max();
  using String = std::string;
  using Vector = std::vector<JsonLoc>;
  using Map = std::map<std::string,JsonLoc>;
  struct Placeholder { std::string key; };
  using Value = std::variant<Placeholder,String,Vector,Map>;

  Value value;
  size_t stpos=npos, enpos=npos;

  // conversion constructors.
  JsonLoc() : value() {}
  JsonLoc(Placeholder p) : value(p) {}
  JsonLoc(String s) : value(s) {}
  JsonLoc(Vector v) : value(v) {}
  JsonLoc(Map m) : value(m) {}

  // Note that allPlaceholders() is a non-const member method, since it returns
  // non-const JsonLoc pointers to various internal components. Pretty much any
  // direct mutation to this->value will invalidate these pointers: only
  // substitute() is safe.
  using PlaceholderMap = std::multimap<std::string, JsonLoc*, std::less<>>;
  PlaceholderMap allPlaceholders();

  // Returns number of substitutions made. Zero if key doesn't exist.
  size_t substitute(const PlaceholderMap& pmap, std::string_view key,
                    const JsonLoc& json);

  // Check if all placeholders have been substituted.
  // Check if child intervals are covered by parent intervals (ignoring npos).
  // Check if child has a valid interval, so does parent.
  // Check if all (stpos==npos) == (enpos==npos).
  // Typically, this should be checked after all substitutions are made.
  bool substitutionsOk() const;

  // The first line is never indented. No newline follows the last character.
  // Corollary: String and Placeholders are never indented,
  //   and are not newline-terminated.
  std::string prettyPrint(size_t indent=0) const;
  std::string prettyPrintWithLocs(size_t indent=0) const;  // TODO
};

template <class X> X* get_if(JsonLoc* json) {
  return std::get_if<X>(&json->value);
}

template <class X> const X* get_if(const JsonLoc* json) {
  return std::get_if<X>(&json->value);
}

}  // namespace oalex
