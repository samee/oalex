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

class JsonLoc;

// Unlike JsonLoc, the stPos and enPos fields here indicate locations in oalex
// source, not user input. Right now, they are not populated very accurately.
class JsonTmpl {
 public:
  enum class Tag { String, Vector, Map, Placeholder };
  static constexpr size_t npos = std::numeric_limits<size_t>::max();
  using String = std::string;
  using Vector = std::vector<JsonTmpl>;
  using Map = std::vector<std::pair<std::string, JsonTmpl>>;  // sorted keys.
  struct Placeholder { std::string key; };

  size_t stPos=npos, enPos=npos;
  static ssize_t mapScanForIndex(const Map& m, std::string_view k);
  static const JsonTmpl* mapScanForValue(const Map& m, std::string_view k);
  static void mapSort(Map& m);

  // conversion constructors.
  JsonTmpl() = delete;
  JsonTmpl(Placeholder p, size_t st, size_t en) : stPos{st}, enPos{en},
    tag_{Tag::Placeholder}, placeholderValue_{std::move(p)} {}
  JsonTmpl(String s) : tag_{Tag::String}, stringValue_{std::move(s)} {}
  JsonTmpl(Vector v) : tag_{Tag::Vector}, vectorValue_(std::move(v)) {}
  JsonTmpl(Map m) : tag_{Tag::Map}, mapValue_(std::move(m)) {}

  JsonTmpl(JsonTmpl&& that);
  JsonTmpl(const JsonTmpl& that);
  JsonTmpl& operator=(JsonTmpl&& that);
  JsonTmpl& operator=(const JsonTmpl& that);
  ~JsonTmpl();

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

  using ConstPlaceholderMap
    = std::vector<std::pair<std::string, const JsonTmpl*>>;
  ConstPlaceholderMap allPlaceholders() const;

  JsonLoc substituteAll(const std::vector<std::pair<std::string,
                                                    JsonLoc>>& subs) const;

  // Recursively check for Placeholder.
  bool substitutionsNeeded() const;

  // Equivalent to substituteAll({}).
  JsonLoc outputIfFilled() const;

  // The first line is never indented. No newline follows the last character.
  // Corollary: String and Placeholders are never indented,
  //   and are not newline-terminated.
  std::string prettyPrint(size_t indent=0) const;
  std::string prettyPrintJson(size_t indent=0) const;
  std::string prettyPrintWithLocs(size_t indent=0) const;  // TODO
 private:
  Tag tag_;
  union {
    String stringValue_;
    Vector vectorValue_;
    Map mapValue_;
    Placeholder placeholderValue_;
  };
  void destroyValue();
  void copyValue(const JsonTmpl& that);
  void moveValue(JsonTmpl&& that);
};

inline bool isPlaceholder(const JsonTmpl& jstmpl, std::string_view pname) {
  if(auto* p = jstmpl.getIfPlaceholder()) return p->key == pname;
  else return false;
}

inline JsonTmpl moveEltOrEmpty(JsonTmpl::Map& m, std::string_view key) {
  for(ssize_t i=0; i<(ssize_t)m.size(); ++i)
    if(m[i].first == key) return std::move(m[i].second);
  return JsonTmpl::Map{};
}

}  // namespace oalex
