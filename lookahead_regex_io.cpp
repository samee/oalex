/*  Copyright 2020 Google LLC

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "lookahead_regex_io.h"
#include <cctype>
#include <cstring>
#include "lexer.h"
#include "runtime/util.h"
using std::get_if;
using std::holds_alternative;
using std::isprint;
using std::make_unique;
using std::nullopt;
using std::optional;
using std::ostringstream;
using std::strchr;
using std::string;
using std::unique_ptr;
using oalex::InputDiags;

namespace oalex::regex {

namespace {

char hexdigit(uint8_t ch) {
  if(ch <= 9) return '0' + ch;
  else if(ch <= 15) return ch-10+'a';
  else Bug()<<"hexdigit("<<int(ch)<<") input is not a hex digit.";
}

bool is_in(char ch, const char s[]) {
  const char* c = strchr(s, ch);
  return c && *c;
}

auto xlat(const char* from, const char* to, char ch) -> optional<char> {
  const char* p = strchr(from, ch);
  if(p && *p) return to[p-from];
  else return nullopt;
}

auto needsEscapingForSet(char ch, bool is_first, bool is_last)
  -> optional<char> {
  const static char escaped_from[]="\t" "\n" "\\" "/";
  const static char escaped_to[]  ="t"  "n"  "\\" "/";
  static_assert(sizeof(escaped_from) == sizeof(escaped_to));

  if(ch == '-' && !(is_first||is_last)) return '-';
  if(ch == ']' && !is_first) return ']';
  return xlat(escaped_from, escaped_to, ch);
}

// Caret '^' is handled specially, outside of this function, since it also
// requires knowing whether the set was negated.
string escapedForSet(unsigned char ch, size_t pos, size_t count) {
  char s[5]="\\x55";
  if(optional<char> opt = needsEscapingForSet(ch, pos==0, pos+1==count)) {
    s[1]=*opt;
    s[2]='\0';
  }else if(isprint(ch)) { s[0]=ch; s[1]='\0'; }
  else { s[2]=hexdigit(ch/16); s[3]=hexdigit(ch%16); }
  return s;
}

bool needsEscapingForString(char ch) {
  return is_in(ch, "\\.[]{}^$|+*?");
}

string escapedForString(const string& s) {
  string rv;
  for(char ch : s) {
    if(needsEscapingForString(ch)) rv += '\\';
    rv += ch;
  }
  return rv;
}

// This allows us to not handle some corner cases, e.g. "[--x]".
bool isPlainRange(unsigned char from, unsigned char to) {
  return (isdigit(from) && isdigit(to)) ||
         (isupper(from) && isupper(to)) ||
         (islower(from) && islower(to));
}

bool isPlainRange(const CharRange& range) {
  return isPlainRange(range.from, range.to);
}

auto prettyPrintRec(const Regex& regex) -> string;

bool hasAllChars(const CharSet& set) {
  return set.negated && set.ranges.empty();
}

string prettyPrintSet(const CharSet& set) {
  if(hasAllChars(set)) return ".";
  size_t n = set.ranges.size();
  ostringstream os;

  if(set.negated) os<<"[^";
  else os<<'[';

  for(size_t i=0; i<n; ++i) {
    auto& r = set.ranges[i];
    if(r.from > r.to) Bug()<<"Invalid regex range";
    else if(r.from == r.to) {
      if(r.from == '^' && i == 0 && !set.negated) os<<"\\^";
      else os<<escapedForSet(r.from, i, n);
    }else if(isPlainRange(r.from, r.to)) os<<r.from<<'-'<<r.to;
    else Bug()<<"Complicated range found: "<<r.from<<" to "<<r.to;
  }
  os<<']';
  return os.str();
}

template <class T>
struct is_variant { static constexpr bool value = false; };
template <class ... Ts>
struct is_variant<std::variant<Ts...>> { static constexpr bool value = true; };
template <class T> inline constexpr
bool is_variant_v = is_variant<T>::value;

template <class ... Ts> struct holds_one_of_helper;  // undefined

template <>
struct holds_one_of_helper<> {
  template <class V> static bool check(const V&) { return false; }
};

template <class T, class ... Ts>
struct holds_one_of_helper<T, Ts...> {
  template <class V> static bool check(const V& v) {
    static_assert(is_variant_v<V>);
    return holds_alternative<T>(v) || holds_one_of_helper<Ts...>::check(v);
  }
};

template <class ... Ts, class V> bool holds_one_of(const V& v) {
  return holds_one_of_helper<Ts...>::check(v);
}
template <class ... Ts, class V> bool holds_one_of_unique(const V& v) {
  return holds_one_of<unique_ptr<Ts>...>(v);
}

template <class ... Ts>
void surroundUnless(ostringstream& os, const Regex& regex) {
  if(!holds_one_of_unique<Ts...>(regex)) os<<'('<<prettyPrintRec(regex)<<')';
  else os<<prettyPrintRec(regex);
}

string prettyPrintSeq(const Concat& seq) {
  ostringstream os;
  for(auto& part : seq.parts) {
    surroundUnless<CharSet, string, Repeat, Optional>(os, part);
  }
  return os.str();
}

string prettyPrintOrs(const OrList& ors) {
  if(ors.parts.empty()) return "";
  ostringstream os;

  surroundUnless<CharSet, string, Concat, Repeat, Optional>(os, ors.parts[0]);
  for(size_t i=1; i<ors.parts.size(); ++i) {
    os<<'|';
    surroundUnless<CharSet, string, Concat, Repeat, Optional>(os, ors.parts[i]);
  }
  return os.str();
}

string surround(string s) { return "(" + s + ")"; }

// `op` can be one of [?+*{], where '{' indicates numeric repeat.
string prettyPrintRepeatPart(const Regex& part, char op) {
  string op_s(1, op);
  if(auto* s = get_if_unique<string>(&part)) {
    if(s->size() == 1) return prettyPrintRec(part) + op_s;
    else return surround(prettyPrintRec(part)) + op_s;
  }
  else if(holds_one_of_unique<Concat,OrList>(part))
    return surround(prettyPrintRec(part)) + op_s;
  else return prettyPrintRec(part) + op_s;
}

string prettyPrintOpt(const Optional& opt) {
  // Collapse /(...)+?/ into /(...)*/
  if(auto* rep = get_if_unique<Repeat>(&opt.part))
    return prettyPrintRepeatPart(rep->part, '*');
  else return prettyPrintRepeatPart(opt.part, '?');
}

string prettyPrintRep(const Repeat& rep) {
  return prettyPrintRepeatPart(rep.part, '+');
}

  /*
  using Regex = std::variant<
    CharRange,
    std::string,
    std::unique_ptr<struct Concat>,
    std::unique_ptr<struct Repeat>,
    std::unique_ptr<struct Optional>,
    std::unique_ptr<struct OrList>,
  >;

  struct Concat { std::vector<Regex> parts; };
  struct Repeat { Regex part; };
  struct Optional { Regex part; };
  struct OrList { std::vector<Regex> parts; };

  */

auto prettyPrintRec(const Regex& regex) -> string {
  if(auto* set = get_if_unique<CharSet>(&regex)) return prettyPrintSet(*set);
  else if(auto* seq = get_if_unique<Concat>(&regex))
    return prettyPrintSeq(*seq);
  else if(auto* s = get_if_unique<string>(&regex)) return escapedForString(*s);
  else if(auto* r = get_if_unique<Repeat>(&regex)) return prettyPrintRep(*r);
  else if(auto* r = get_if_unique<Optional>(&regex)) return prettyPrintOpt(*r);
  else if(auto* r = get_if_unique<OrList>(&regex)) return prettyPrintOrs(*r);
  else Unimplemented()<<"prettyPrint(regex) for variant "<<regex.index();
}

constexpr uint8_t kMaxDepth = 255;
auto parseRec(InputDiags& ctx, size_t& i, uint8_t depth) -> optional<Regex>;

// Regex and string literals are among the few places that don't ignore spaces
// before checking for a specific character.
bool hasChar(const Input& input, size_t pos, char ch) {
  return input.sizeGt(pos) && input[pos]==ch;
}

bool parseCharSetNegation(const Input& input, size_t& i) {
  if(!hasChar(input,i,'^')) return false;
  ++i;
  return true;
}

auto unescaped(char ch) -> optional<unsigned char> {
  const static char escaped_from[]="\t" "\n" "\\" "/" "-" "]";
  const static char escaped_to[]  ="t"  "n"  "\\" "/" "-" "]";
  static_assert(sizeof(escaped_from) == sizeof(escaped_to));
  return xlat(escaped_to, escaped_from, ch);
}

// Assumes caller has already checked for "\x" prefix.
auto parseHexCode(InputDiags& ctx, size_t& i) -> optional<unsigned char> {
  size_t j = i+2;  // skip over "\x"
  if(auto res = oalex::lex::lexHexCode(ctx, j)) { i = j; return res; }
  else return ctx.Error(i, i+4, "Invalid hex code");
}

auto parseEscapeCode(InputDiags& ctx, size_t& i) -> optional<unsigned char> {
  const Input& input = ctx.input;
  if(!hasChar(input,i,'\\')) return nullopt;
  if(!input.sizeGt(i+1)) ctx.Fatal(i, i+1, "Incomplete escape code");
  if(input[i+1] == 'x') return parseHexCode(ctx, i);
  if(auto res = unescaped(input[i+1])) { i=i+2; return res; }
  else { i += 2; return ctx.Error(i-2, i, "Unknown escape code"); }
}

auto parseCharSetElt(InputDiags& ctx, size_t& i) -> optional<unsigned char> {
  const Input& input = ctx.input;
  if(!input.sizeGt(i)) return nullopt;
  if(input[i] == '/') {
    ctx.Fatal(i, i+1, "Expected closing ']'");
    return nullopt;
  }
  if(input[i] == '\\') return parseEscapeCode(ctx, i);
  char ch = input[i];
  if(!isprint(ch)) {
    ctx.Error(i, i+1, "Invalid character");
    return nullopt;
  }
  ++i;
  return ch;
}

auto parseCharSet(InputDiags& ctx, size_t& i) -> optional<Regex> {
  const Input& input = ctx.input;
  if(!hasChar(input,i,'['))
    Bug()<<"parseCharSet called at invalid location "<<i;
  CharSet cset;
  size_t j = i+1;

  cset.negated = parseCharSetNegation(input, j);

  // One or more set elements, not zero or more.
  // Allow ']' as the first set element. It does not close the set.
  do {
    if(!input.sizeGt(j)) ctx.Fatal(i, i+1, "Unmatched '['");
    if(auto st = parseCharSetElt(ctx, j)) cset.ranges.push_back({*st,*st});
    else { ++j; continue; }

    // We have more to do if we are starting a range.
    if(!hasChar(input,j,'-')) continue;

    // Treat the '-' literally if this is the last character.
    if(hasChar(input,j+1,']')) continue;
    ++j;

    // Parse out the end character.
    auto& new_range = cset.ranges.back();
    if(auto en = parseCharSetElt(ctx, j)) new_range.to = *en;
    else { ++j; continue; }

    // TODO remove this check. It exists only to help fuzzing.
    if(new_range.from == new_range.to)
      ctx.Error(i, j, "Redundant range. Use a single character.");

    if(new_range.from > new_range.to)
      ctx.Error(i, j, "Invalid range going backwards.");

    if(!isPlainRange(new_range))
      ctx.Error(i, j, "Ranges can only span 0-9, A-Z, or a-z.");

    if(hasChar(input,j,'-') && !hasChar(input,j+1,']'))
      ctx.Error(j, j+1, "Character range has no start");
  } while(!hasChar(input,j,']'));
  i = j+1;
  return makeUniqueRegex(std::move(cset));
}

auto parseGroup(InputDiags& ctx, size_t& i, uint8_t depth) -> optional<Regex> {
  if(depth == kMaxDepth) ctx.Fatal(i, i+1, "Parentheses nested too deep");
  const Input& input = ctx.input;
  size_t j = i;
  if(!hasChar(input,i,'('))
    ctx.FatalBug(i, i+1, "parseGroup() must start with '('");
  optional<Regex> res = parseRec(ctx, ++j, depth+1);
  if(!res) return nullopt;
  if(!hasChar(input,j,')')) return ctx.Error(i, j, "Unmatched '('");
  i = j+1;
  return res;
}

// Temporary function to detect unimplemented feature.
bool startsRepeat(char ch) { return is_in(ch, "+*?{"); }

auto parseSingleChar(InputDiags& ctx, size_t& i) -> optional<Regex> {
  char ch = ctx.input[i];
  if(ch == '\\') Unimplemented()<<"Bare escape codes";
  else if(ch == '^' || ch == '$') Unimplemented()<<"Anchors";
  else return makeUniqueRegex(string(1, ctx.input[i++]));
}

// Consequtive string parts get joined into a single string.
auto contractStrings(Concat concat) -> Concat {
  Concat rv;
  for(Regex& part: concat.parts) {
    string* s = get_if_unique<string>(&part);
    string* t = rv.parts.empty() ? nullptr
                                 : get_if_unique<string>(&rv.parts.back());
    if(!s || !t) rv.parts.push_back(std::move(part));
    else t->append(std::move(*s));
  }
  return rv;
}

// op is assumed to be one of [+*?].
Regex repeatWith(Regex regex, char op) {
  switch(op) {
    case '+': return makeUniqueRegex<Repeat>({std::move(regex)});
    case '?': return makeUniqueRegex<Optional>({std::move(regex)});
    case '*': return makeUniqueRegex<Optional>({
                       makeUniqueRegex<Repeat>({std::move(regex)})
                     });
    default: Bug()<<"repeatWith called with invalid op: "<<op;
  }
}

// Assumes startsRepeat(ctx.input[i]) == true.
bool repeatBack(InputDiags& ctx, size_t& i, Concat& concat) {
  char ch = ctx.input[i];
  ++i;
  if(ch == '{') Unimplemented()<<"'{}'";
  if(concat.parts.empty()) return ctx.Error(i-1, i, "Nothing to repeat"), false;
  concat.parts.back() = repeatWith(std::move(concat.parts.back()), ch);
  return true;
}

// Used with T being one of Concat or OrList.
template <class T>
Regex unpackSingleton(T t) {
  if(t.parts.size() == 1) return Regex{std::move(t.parts[0])};
  else return Regex{make_unique<T>(std::move(t))};
}

auto parseBranch(InputDiags& ctx, size_t& i, uint8_t depth) -> optional<Regex> {
  const Input& input = ctx.input;
  size_t j = i;
  regex::Concat concat;
  optional<Regex> subres;

  while(input.sizeGt(j)) {
    if(input[j] == '[') subres = parseCharSet(ctx, j);
    else if(input[j] == '(') subres = parseGroup(ctx, j, depth);
    else if(is_in(input[j], ")/|")) {  // end pattern.
      i = j;
      return unpackSingleton(contractStrings(std::move(concat)));
    }else if(startsRepeat(input[j])) {
      if(!repeatBack(ctx, j, concat)) return nullopt;
      else continue;  // Skip checking subres.
    }else if(input[j] == '.') {
      subres = makeUniqueRegex<CharSet>({{}, true});
      ++j;
    }else subres = parseSingleChar(ctx, j);

    if(!subres) return nullopt;
    concat.parts.push_back(std::move(*subres));
  }

  ctx.Error(i, j, "Unterminated regex, expected '/'");
  i = j;
  return nullopt;
}

auto parseRec(InputDiags& ctx, size_t& i, uint8_t depth) -> optional<Regex> {
  const Input& input = ctx.input;
  size_t j = i;
  regex::OrList ors;
  optional<Regex> subres;

  while(input.sizeGt(j)) {
    subres = parseBranch(ctx, j, depth);
    if(!subres) return nullopt;
    ors.parts.push_back(std::move(*subres));
    if(!input.sizeGt(j)) break;
    if(is_in(input[j], ")/")) {
      i = j;
      return unpackSingleton(std::move(ors));
    }else if(input[j] == '|') ++j;
    else ctx.FatalBug(j, j+1, "Branch parsing finished unexpectedly");
  }

  ctx.Error(i, j, "Unterminated regex, expected '/'");
  i = j;
  return nullopt;
}

}  // namespace

auto prettyPrint(const Regex& regex) -> string {
  return "/" + prettyPrintRec(regex) + "/";
}

// Current state: only parses concatenation of character sets.
auto parse(InputDiags& ctx, size_t& i) -> optional<Regex> {
  const Input& input = ctx.input;
  if(!hasChar(input,i,'/')) return nullopt;
  size_t j = i+1;
  auto rv = parseRec(ctx, j, 0);
  if(!rv) return rv;
  else if(hasChar(input,j,')')) return ctx.Error(j, j+1, "Unmatched ')'");
  else if(!hasChar(input,j,'/'))
    ctx.FatalBug(j, j+1, "Pattern parsing ended unexpectedly without '/'");
  i = j+1;
  return rv;
}

}  // namespace oalex::regex
