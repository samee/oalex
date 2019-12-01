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

  using PlaceholderMap = std::multimap<std::string, JsonLoc*, std::less<>>;
  PlaceholderMap allPlaceholders();

  // Returns number of substitutions made. Zero if key doesn't exist.
  size_t substitute(const PlaceholderMap& pmap, std::string_view key,
                    const JsonLoc& json);

  // Check if all placeholders have been substituted.
  // Check if child intervals are covered by parent intervals (ignoring npos).
  // Check if child has a valid interval, so does parent.
  // Check if all (stpos==npos) == (enpos==npos).
  // Typically, this should be checked before a substitution is made.
  bool substitutionsOk() const;
 private:
  void allPlaceholdersImpl(PlaceholderMap& result);
};

template <class X> X* get_if(JsonLoc* json) {
  return std::get_if<X>(json->value);
}

template <class X> const X* get_if(const JsonLoc* json) {
  return std::get_if<X>(json->value);
}

}  // namespace oalex
