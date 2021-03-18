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

#include"ident.h"
#include<algorithm>
#include<cctype>
#include<vector>
#include "lexer.h"
using oalex::lex::WholeSegment;
using std::all_of;
using std::string;
using std::string_view;
using std::vector;

size_t std::hash<oalex::Ident>::operator()(const oalex::Ident& ident) const {
  std::string s;
  for(char ch : ident.orig_) if(ch != '_') s += tolower(ch);
  // Maybe memoize s as a member of Ident if this becomes a bottleneck.
  return hash_helper(s);
}

namespace oalex {

bool operator==(const Ident& a, const Ident& b) {
  size_t i=0, j=0;
  while(i<a.orig_.size() && j<b.orig_.size()) {
    char ai = a.orig_[i], bj = b.orig_[j];
    if(ai == '_') { ++i; continue; }
    if(bj == '_') { ++j; continue; }
    if(tolower(ai) != tolower(bj)) return false;
    ++i; ++j;
  }
  return i==a.orig_.size() && j==b.orig_.size();
}

// This allows us to use std::map.
bool operator<(const Ident& a, const Ident& b) {
  size_t i=0, j=0;
  while(i<a.orig_.size() && j<b.orig_.size()) {
    char ai = tolower(a.orig_[i]), bj = tolower(b.orig_[j]);
    if(ai == '_') { ++i; continue; }
    if(bj == '_') { ++j; continue; }
    if(ai != bj) return ai < bj;
    ++i; ++j;
  }
  return i==a.orig_.size() && j!=b.orig_.size();
}

const size_t kMaxIdentLen = 100;

// Note: this returns false for an empty string.
static bool hasAlnum(const string& s) {
  for(char ch : s) if(isalnum(ch)) return true;
  return false;
}

// TODO use LocPair
Ident Ident::parseFromString(InputDiagsRef ctx, string s, size_t stPos) {
  const size_t enPos = stPos + s.size();
  if(!hasAlnum(s))
    return Error(ctx, stPos, enPos, "Identifier must have a digit or letter");
  if(s[0] == '_' || s.back()  == '_')
    return Error(ctx, stPos, enPos,
                 "Identifiers with leading or trailing underscores"
                 " are not supported for forward compatibility");
  if(isdigit(s[0]))
    return Error(ctx, stPos, stPos+1, "Identifiers cannot start with a digit");
  for(size_t j=1; j<s.size(); ++j) if(s[j] == '_' && s[j-1] == '_')
    return Error(ctx, j+stPos-1, j+stPos+1,
                 "Consecutive underscores are not allowed for "
                 "forward compatibility");
  Ident rv;
  rv.orig_ = std::move(s);
  rv.stPos_ = stPos;
  rv.enPos_ = enPos;
  return rv;
}

static bool isIdentChar(char ch) { return ch=='_' || isalnum(ch); }

Ident Ident::parse(InputDiagsRef ctx, size_t& i) {
  const InputPiece& input = *ctx.input;
  size_t l;
  for(l=0; input.sizeGt(i+l); ++l) {
    if(l >= kMaxIdentLen) Fatal(ctx, i, i+l, "Identifier too long");
    if(!isIdentChar(input[i+l])) break;
  }
  const Ident rv = parseFromString(ctx, input.substr(i,l), input.inputPos(i));
  if(rv) i += l;
  return rv;
}

Ident Ident::parse(InputDiagsRef ctx, const WholeSegment& s) {
  if(s->size() > kMaxIdentLen)
    return Error(ctx, s.stPos, s.enPos, "Identifier too long");
  for(size_t i=0; i<s->size(); ++i) if(!isIdentChar(s.data[i]))
    return Error(ctx, s.stPos+i, s.stPos+i+1, "Invalid identifier character");
  return parseFromString(ctx, *s, s.stPos);
}

template <class Cb>
auto splitAt(string_view s, Cb isSeparator) -> vector<string> {
  vector<string> rv;
  size_t i=0, j;
  for(j=0; j<s.size(); ++j) {
    ssize_t r = isSeparator(j);
    if(r < 0) continue;
    rv.emplace_back(s.substr(i, j-i));
    i = j+r;
    if(r > 1) j += r-1;  // Skip over separator.
  }
  rv.emplace_back(s.substr(i));
  return rv;
}

static auto underscoreSplit(string_view s) {
  return splitAt(s, [&](size_t i) { return ssize_t(s[i]=='_' ? 1 : -1); });
}

static auto splitLettersAndDigits(string_view s) {
  return splitAt(s, [&](size_t i) {
      return (i > 0 && isdigit(s[i-1]) != isdigit(s[i])) ? 0 : -1;
  });
}

static bool allCaps(string_view s) {
  return all_of(s.begin(), s.end(), isupper);
}

static string lower(string s) {
  for(char& ch : s) ch = tolower(ch);
  return s;
}

static auto splitBeforeCaps(string_view s) {
  return splitAt(s, [&](size_t i) {
      return (i > 0 && isupper(s[i])) ? 0 : -1;
  });
}

static
auto splitWords(string_view s) -> vector<string> {
  vector<string> rv;
  for(const auto& word : underscoreSplit(s)) {
    for(const auto& word_piece : splitLettersAndDigits(word)) {
      if(allCaps(word_piece)) rv.push_back(word_piece);
      else for(const auto& word_piece_2 : splitBeforeCaps(word_piece))
        rv.push_back(word_piece_2);
    }
  }
  return rv;
}

string Ident::toSnakeCase() const {
  string rv;
  for(auto& word : splitWords(orig_)) {
    if(!rv.empty()) rv += "_";
    rv += lower(word);
  }
  return rv;
}

string Ident::toUCamelCase() const {
  string rv;
  for(auto& word : splitWords(orig_)) {
    word = lower(std::move(word));
    if(!word.empty()) word[0] = toupper(word[0]);
    rv += word;
  }
  return rv;
}

string Ident::toLCamelCase() const {
  string rv;
  for(auto& word : splitWords(orig_)) {
    word = lower(std::move(word));
    if(!rv.empty() && !word.empty()) word[0] = toupper(word[0]);
    rv += word;
  }
  return rv;
}

}  // namespace oalex
