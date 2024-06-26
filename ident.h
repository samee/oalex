/*  Copyright 2019-2024 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

/*
   Type Ident is meant for identifiers from input language that show up in
   generated code. It tries to canonicalize common conventions. They are parsed
   from oalex code. For equality comparison, we will ignore case and
   underscores. Some of those information is kept around, though, to help
   pretty-printing. E.g. bitEnum will compare equal to biteNum, but they can
   still be printed as bit_enum vs. bite_num.

   Right now, we won't allow prefix or suffix underscores or double underscores,
   since we might later change how they get printed. But for now, we can keep it
   internally as a list of words. As usual in many languages, we won't allow
   identifiers to start with a digit. And we can't have an identifier that's all
   underscores, since we will likely use it for other purposes.

   With those constraints in place, parsing can be quite simple:

     * Split at every underscore
     * For each piece
       - Group digits and letters into separate words
       - For each letter-word, if it is not all-caps, break before each
         capital letter.

    With that done, we can serve case-conversion requests.

   */

#pragma once
#include <optional>
#include <string>

namespace oalex {

// Forward declaration, since we don't want to depend on lexer.h in the header.
// TODO refactor this class out of lexer.h, so we don't need to link
// ident_test.cpp with regex_io.cpp.
struct WholeSegment;
struct InputDiags;
class StringLoc;
class DiagsDest;

// This class is meant to represent an identifier in the _generated_ code, not
// in the input oalex source code. It is only useful if we might need to change
// case or remove underscores to conform to some target style. Otherwise, use
// a WholeSegment.
// Dev-note: This really could be a subclass of WholeSegment.
class Ident {
  std::string orig_;
  size_t stPos_ = std::string::npos, enPos_ = std::string::npos;
  Ident(std::nullopt_t) {}
  static Ident parseFromString(DiagsDest ctx,
                               std::string s, size_t stPos);
 public:
  Ident() = default;
  static Ident parse(InputDiags& ctx, size_t& i);
  static Ident parse(DiagsDest ctx, const WholeSegment& s);
  static Ident parse(DiagsDest ctx, const StringLoc& s);

  /* parseGenerated() dies on invalid input. It is expected to be used only
  for hardcoded inputs and sanitized inputs. It also preserves the invariant
  that for all valid input s, Ident::parseGenerated(s).preserveCase() == s. */
  static Ident parseGenerated(std::string s);

  std::string toSnakeCase() const;
  std::string toUCamelCase() const;
  std::string toLCamelCase() const;
  std::string preserveCase() const { return orig_; }
  friend std::strong_ordering operator<=>(const Ident& a, const Ident& b);
  friend class std::hash<Ident>;
  explicit operator bool() const { return !orig_.empty(); }
  size_t stPos() const { return stPos_; }
  size_t enPos() const { return enPos_; }
};

inline bool operator==(const Ident& a, const Ident& b) { return (a<=>b) == 0; }
}  // namespace oalex

template <> struct std::hash<oalex::Ident> {
  hash<string> hash_helper;
  size_t operator()(const oalex::Ident& ident) const;
};
