/*  Copyright 2020-2021 The oalex authors.

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
#include <string>
#include <string_view>
#include <memory>
#include <vector>

#include "input_view.h"

namespace oalex {

enum struct RegexNodeType {
  charSet, string, anchor, concat,
  repeat, optional, orList
};

class Regex {
 public:
  RegexNodeType nodeType;
  explicit Regex(RegexNodeType t) : nodeType(t) {}
  virtual ~Regex() {}
};

struct CharRange { unsigned char from, to; };

class RegexCharSet final : public Regex {
 public:
  RegexCharSet() : Regex(RegexNodeType::charSet) {}
  explicit RegexCharSet(std::vector<CharRange> r, bool neg = false)
    : Regex(RegexNodeType::charSet), ranges(std::move(r)), negated(neg) {}
  std::vector<CharRange> ranges;
  bool negated = false;
};

class RegexString final : public Regex {
 public:
  explicit RegexString(std::string v)
    : Regex(RegexNodeType::string), value(std::move(v)) {}
  std::string value;
};

class RegexAnchor final : public Regex {
 public:
  enum AnchorType { wordEdge, bol, eol } anchorType;
  explicit RegexAnchor(AnchorType t)
    : Regex(RegexNodeType::anchor), anchorType(t) {}
};

class RegexConcat final : public Regex {
 public:
  RegexConcat() : Regex(RegexNodeType::concat) {}
  explicit RegexConcat(std::vector<std::unique_ptr<const Regex>> parts)
    : Regex(RegexNodeType::concat), parts(std::move(parts)) {}
  std::vector<std::unique_ptr<const Regex>> parts;
};

class RegexRepeat final : public Regex {
 public:
  explicit RegexRepeat(std::unique_ptr<const Regex> part)
    : Regex(RegexNodeType::repeat), part(std::move(part)) {}
  std::unique_ptr<const Regex> part;
};

class RegexOptional final : public Regex {
 public:
  explicit RegexOptional(std::unique_ptr<const Regex> part)
    : Regex(RegexNodeType::optional), part(std::move(part)) {}
  std::unique_ptr<const Regex> part;
};

class RegexOrList final : public Regex {
 public:
  RegexOrList() : Regex(RegexNodeType::orList) {}
  explicit RegexOrList(std::vector<std::unique_ptr<const Regex>> parts)
    : Regex(RegexNodeType::orList), parts(std::move(parts)) {}
  std::vector<std::unique_ptr<const Regex>> parts;
};

struct RegexOptions {
  RegexCharSet word;  // Used for \b matches.
};

bool matchesRegexCharSet(char ch, const RegexCharSet& cset);

// Used for lookaheads
bool startsWithRegex(const Input& input, size_t i,
                     const Regex& regex, const RegexOptions& opts);

// Not used for lookaheads.
bool consumeGreedily(const Input& input, size_t& i,
                     const Regex& regex, const RegexOptions& opts);

}  // namespace oalex
