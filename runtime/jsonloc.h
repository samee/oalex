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
#include <string>
#include <string_view>
#include <vector>

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
class JsonLoc {
 public:
  enum class Tag { ErrorValue, String, Vector, Map, Placeholder };
  static constexpr size_t npos = std::numeric_limits<size_t>::max();
  struct ErrorValue {};
  using String = std::string;
  using Vector = std::vector<JsonLoc>;
  using Map = std::vector<std::pair<std::string, JsonLoc>>;  // sorted keys.
  struct Placeholder { std::string key; };

  size_t stPos=npos, enPos=npos;
  static ssize_t mapLinearFind(const Map& m, std::string_view k);
  static void mapSort(Map& m);

  // conversion constructors.
  JsonLoc() = delete;
  JsonLoc(ErrorValue) : tag_{Tag::ErrorValue}, errorValue_{} {}
  JsonLoc(Placeholder p, size_t st, size_t en) : stPos{st}, enPos{en},
    tag_{Tag::Placeholder}, placeholderValue_{std::move(p)} {}
  JsonLoc(String s) : tag_{Tag::String}, stringValue_{std::move(s)} {}
  JsonLoc(Vector v) : tag_{Tag::Vector}, vectorValue_(std::move(v)) {}
  JsonLoc(Map m) : tag_{Tag::Map}, mapValue_(std::move(m)) {}

  JsonLoc(JsonLoc&& that);
  JsonLoc(const JsonLoc& that);
  JsonLoc& operator=(JsonLoc&& that);
  JsonLoc& operator=(const JsonLoc& that);
  ~JsonLoc();

  // Or rename type to Tag::Error.
  bool holdsErrorValue() const { return tag_ == Tag::ErrorValue; }
  bool holdsString() const { return tag_ == Tag::String; }
  bool holdsVector() const { return tag_ == Tag::Vector; }
  bool holdsMap() const { return tag_ == Tag::Map; }
  bool holdsPlaceholder() const { return tag_ == Tag::Placeholder; }

  String* getIfString() { return holdsString() ? &stringValue_ : nullptr; }
  const String* getIfString() const
    { return holdsString() ? &stringValue_ : nullptr; }
  Placeholder* getIfPlaceholder()
    { return holdsPlaceholder() ? &placeholderValue_ : nullptr; }
  const Placeholder* getIfPlaceholder() const
    { return holdsPlaceholder() ? &placeholderValue_ : nullptr; }
  Vector* getIfVector() { return holdsVector() ? &vectorValue_ : nullptr; }
  const Vector* getIfVector() const
    { return holdsVector() ? &vectorValue_ : nullptr; }
  Map* getIfMap() { return holdsMap() ? &mapValue_ : nullptr; }
  const Map* getIfMap() const { return holdsMap() ? &mapValue_ : nullptr; }

  Tag tag() const { return tag_; }
  std::string_view tagName() const;

  // The Const version is used for pre-substitution processing, by functions
  // that only accept const JsonLoc objects but extract the identifier list.
  using ConstPlaceholderMap
    = std::vector<std::pair<std::string, const JsonLoc*>>;
  ConstPlaceholderMap allPlaceholders() const;

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
 private:
  Tag tag_;
  union {
    ErrorValue errorValue_;
    String stringValue_;
    Vector vectorValue_;
    Map mapValue_;
    Placeholder placeholderValue_;
  };
  void destroyValue();
  void copyValue(const JsonLoc& that);
  void moveValue(JsonLoc&& that);
};

inline bool isPlaceholder(const JsonLoc& jsloc, std::string_view pname) {
  if(auto* p = jsloc.getIfPlaceholder()) return p->key == pname;
  else return false;
}

inline JsonLoc moveEltOrEmpty(JsonLoc::Map& m, std::string_view key) {
  for(ssize_t i=0; i<(ssize_t)m.size(); ++i)
    if(m[i].first == key) return std::move(m[i].second);
  return JsonLoc::Map{};
}

}  // namespace oalex
