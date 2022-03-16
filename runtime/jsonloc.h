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

/* class StringLoc: just a pair of a string and location. Can be easily
converted to and from JsonLoc. No way to modify it after construction, other
than by releasing.

Dev notes: It would have been a lot more efficient to use an unowned
std::string_view instead of std::string. However, this will often point to
strings a bit too ephemeral, such as:

  * Input objects that "forget" inputs already processed
  * JsonLoc objects that don't live long enough.
*/
class StringLoc {
 public:
  StringLoc() : s_{}, stPos_{std::string::npos} {}
  StringLoc(std::string s, size_t stPos) : s_{std::move(s)}, stPos_{stPos} {}
  size_t stPos() const { return stPos_; }
  size_t enPos() const { return stPos_+s_.size(); }
  const std::string& operator*() const { return s_; }
  const std::string* operator->() const { return &s_; }
  std::string release() { stPos_ = std::string::npos; return std::move(s_); }
  explicit operator bool() const { return stPos_ != std::string::npos; }
 private:
  std::string s_;
  size_t stPos_;
};

// Even though we call it json, we don't in fact support numbers, booleans,
// or null. We do support an error type. Together with strings, those two are
// the only atomic types.
// stPos and enPos can remain JsonLoc::npos, if they only have hardcoded values.
// The other difference is that we treat strings as byte-strings, not utf-8.
// This also causes a difference in our backslash escaping conventions.
// prettyPrint() also returns something closer to protobufs than json.
// In other words, this is a complete abuse of the term "json".
//
// The stPos and enPos fields indicate locations in user input,
// not in oalex source.
class JsonLoc {
 public:
  enum class Tag { ErrorValue, String, Vector, Map };
  static constexpr size_t npos = std::numeric_limits<size_t>::max();
  struct ErrorValue {};
  using String = std::string;
  using Vector = std::vector<JsonLoc>;
  using Map = std::vector<std::pair<std::string, JsonLoc>>;  // sorted keys.

  size_t stPos=npos, enPos=npos;
  static ssize_t mapScanForIndex(const Map& m, std::string_view k);
  static const JsonLoc* mapScanForValue(const Map& m, std::string_view k);
  static void mapSort(Map& m);

  // conversion constructors.
  JsonLoc() = delete;
  JsonLoc(ErrorValue) : tag_{Tag::ErrorValue}, errorValue_{} {}
  JsonLoc(String s) : tag_{Tag::String}, stringValue_{std::move(s)} {}
  JsonLoc(StringLoc s) :
    stPos{s.stPos()}, enPos{s.enPos()}, tag_{Tag::String},
    stringValue_{s.release()} {}
  JsonLoc(Vector v) : tag_{Tag::Vector}, vectorValue_(std::move(v)) {}
  JsonLoc(Map m) : tag_{Tag::Map}, mapValue_(std::move(m)) {
    mapSort(mapValue_);
  }

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

  String* getIfString() { return holdsString() ? &stringValue_ : nullptr; }
  const String* getIfString() const
    { return holdsString() ? &stringValue_ : nullptr; }
  StringLoc getIfStringLoc() const {
    return holdsString() ? StringLoc{stringValue_, stPos} : StringLoc{};
  }
  Vector* getIfVector() { return holdsVector() ? &vectorValue_ : nullptr; }
  const Vector* getIfVector() const
    { return holdsVector() ? &vectorValue_ : nullptr; }
  Map* getIfMap() { return holdsMap() ? &mapValue_ : nullptr; }
  const Map* getIfMap() const { return holdsMap() ? &mapValue_ : nullptr; }

  Tag tag() const { return tag_; }
  std::string_view tagName() const;

  // Check if child intervals are covered by parent intervals (ignoring npos).
  // Check if child has a valid interval, so does parent.
  // Check if all (stPos==npos) == (enPos==npos).
  bool substitutionsOk() const;

  // The first line is never indented. No newline follows the last character.
  // Corollary: Strings are never indented, and are not newline-terminated.
  // The output cannot be parsed back as a template if there are any
  // JsonLoc::ErrorValue{} components.
  std::string prettyPrint(size_t indent=0) const;
  std::string prettyPrintJson(size_t indent=0) const;
  std::string prettyPrintWithLocs(size_t indent=0) const;  // TODO

  // Dev-notes: Right now this equality check is only used for `oalex test`.
  //   If those tests later acquire support for placeholders or omitted fields,
  //   we will have to use a more complicated matching between JsonLoc and
  //   JsonTmpl.
  bool operator==(const JsonLoc& that) const;
  bool operator!=(const JsonLoc& that) const { return !(*this == that); }
 private:
  Tag tag_;
  union {
    ErrorValue errorValue_;
    String stringValue_;
    Vector vectorValue_;
    Map mapValue_;
  };
  void destroyValue();
  void copyValue(const JsonLoc& that);
  void moveValue(JsonLoc&& that);
};

inline JsonLoc moveEltOrEmpty(JsonLoc::Map& m, std::string_view key) {
  for(ssize_t i=0; i<(ssize_t)m.size(); ++i)
    if(m[i].first == key) return std::move(m[i].second);
  return JsonLoc::Map{};
}

}  // namespace oalex
