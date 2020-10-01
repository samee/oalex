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

#include "regex_io.h"
#include <cctype>
#include <cstring>
#include "fmt/format.h"
#include "lexer.h"
#include "util.h"
#include "runtime/util.h"
using std::get_if;
using std::isprint;
using std::make_unique;
using std::nullopt;
using std::optional;
using std::strchr;
using std::string;
using std::unique_ptr;
using oalex::InputDiags;

namespace oalex {
namespace regex {

namespace {

char hexdigit(uint8_t ch) {
  if(ch <= 9) return '0' + ch;
  else if(ch <= 15) return ch-10+'a';
  else Bug("hexdigit({}) input is not a hex digit.", int(ch));
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

auto escaped(char ch, const char meta[]) -> optional<char> {
  if(is_in(ch, meta)) return ch;
  if(ch == '\n') return 'n';
  if(ch == '\t') return 't';
  return nullopt;
}

const char stringMeta[] = "\\/.[{()^$|+*?";

string escapedForString(const string& s) {
  string rv;
  for(char ch : s) {
    if(auto esc = escaped(ch, stringMeta)) rv += string(1,'\\') + *esc;
    else rv += ch;
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
  fmt::memory_buffer buf;

  if(set.negated) format_to(buf, "[^");
  else format_to(buf, "[");

  for(size_t i=0; i<n; ++i) {
    auto& r = set.ranges[i];
    if(r.from > r.to) Bug("Invalid regex range: {} > {}", r.from, r.to);
    else if(r.from == r.to) {
      if(r.from == '^' && i == 0 && !set.negated) format_to(buf, "\\^");
      else format_to(buf, "{}", escapedForSet(r.from, i, n));
    }else if(isPlainRange(r.from, r.to))
      format_to(buf, "{}-{}", char(r.from), char(r.to));
    else Bug("Complicated range found: {} to {}", r.from, r.to);
  }
  format_to(buf, "]");
  return fmt::to_string(buf);
}

string prettyPrintAnchor(Anchor a) {
  switch(a) {
    case Anchor::wordEdge: return "\\b";
    case Anchor::bol: return "^";
    case Anchor::eol: return "$";
    default: Bug("Unknown Anchor type {}", static_cast<int>(a));
  }
}

bool printsToNull(const Regex& regex) {
  if(holds_one_of_unique<CharSet,Anchor,Optional,Repeat,OrList>(regex))
    return false;
  else if(auto* s = get_if_unique<string>(&regex)) return s->empty();
  else if(auto* seq = get_if_unique<Concat>(&regex)) {
    for(const Regex& p : seq->parts) if(!printsToNull(p)) return false;
    return true;
  }else Bug("printsToNull() called with unknown index {}", regex.index());
}

template <class ... Ts>
void surroundUnless(fmt::memory_buffer& buf, const Regex& regex) {
  if(!holds_one_of_unique<Ts...>(regex))
    format_to(buf, "({})", prettyPrintRec(regex));
  else format_to(buf, "{}", prettyPrintRec(regex));
}

string prettyPrintSeq(const Concat& seq) {
  fmt::memory_buffer buf;
  for(auto& part : seq.parts) {
    if(printsToNull(part)) format_to(buf, "()");
    else surroundUnless<CharSet, string, Anchor, Repeat, Optional>(buf, part);
  }
  return fmt::to_string(buf);
}

string prettyPrintOrs(const OrList& ors) {
  if(ors.parts.empty()) return "";
  fmt::memory_buffer buf;

  surroundUnless<CharSet, string, Anchor,
                 Concat, Repeat, Optional>(buf, ors.parts[0]);
  for(size_t i=1; i<ors.parts.size(); ++i) {
    format_to(buf, "|");
    surroundUnless<CharSet, string, Anchor,
                   Concat, Repeat, Optional>(buf, ors.parts[i]);
  }
  return fmt::to_string(buf);
}

string surround(string s) { return "(" + s + ")"; }

// `op` can be one of [?+*{], where '{' indicates numeric repeat.
string prettyPrintRepeatPart(const Regex& part, char op) {
  string op_s(1, op);
  if(auto* s = get_if_unique<string>(&part)) {
    if(s->size() == 1) return prettyPrintRec(part) + op_s;
    else return surround(prettyPrintRec(part)) + op_s;
  }
  else if(holds_one_of_unique<Concat,Repeat,OrList>(part))
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
  else if(auto* a = get_if_unique<Anchor>(&regex)) return prettyPrintAnchor(*a);
  else if(auto* r = get_if_unique<Repeat>(&regex)) return prettyPrintRep(*r);
  else if(auto* r = get_if_unique<Optional>(&regex)) return prettyPrintOpt(*r);
  else if(auto* r = get_if_unique<OrList>(&regex)) return prettyPrintOrs(*r);
  else Unimplemented("prettyPrint(regex) for variant {}", regex.index());
}

constexpr uint8_t kMaxDepth = 255;
constexpr uint8_t kMaxRepDepth = 5;
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

auto unescaped(char ch, const char meta[]) -> optional<unsigned char> {
  if(ch == 't') return '\t';
  else if(ch == 'n') return '\n';
  else if(is_in(ch, meta)) return ch;
  else return nullopt;
}

// Assumes caller has already checked for "\x" prefix.
auto parseHexCode(InputDiags& ctx, size_t& i) -> optional<unsigned char> {
  size_t j = i+2;  // skip over "\x"
  if(auto res = oalex::lex::lexHexCode(ctx, j)) { i = j; return res; }
  else return Error(ctx, i, i+4, "Invalid hex code");
}

auto parseEscapeCode(InputDiags& ctx, size_t& i, const char meta[]) ->
optional<unsigned char> {
  const Input& input = ctx.input;
  if(!hasChar(input,i,'\\')) return nullopt;
  if(!input.sizeGt(i+1)) Fatal(ctx, i, i+1, "Incomplete escape code");
  if(input[i+1] == 'x') return parseHexCode(ctx, i);
  if(auto res = unescaped(input[i+1], meta)) { i=i+2; return res; }
  else { i += 2; return Error(ctx, i-2, i, "Unknown escape code"); }
}

auto parseCharSetElt(InputDiags& ctx, size_t& i) -> optional<unsigned char> {
  const Input& input = ctx.input;
  if(!input.sizeGt(i)) return nullopt;
  if(input[i] == '/') {
    Fatal(ctx, i, i+1, "Expected closing ']'");
    return nullopt;
  }
  if(input[i] == '\\') return parseEscapeCode(ctx, i, "^\\/-]");
  char ch = input[i];
  if(!isprint(ch)) {
    Error(ctx, i, i+1, "Invalid character");
    return nullopt;
  }
  ++i;
  return ch;
}

auto parseCharSetUnq(InputDiags& ctx, size_t& i) -> unique_ptr<CharSet> {
  const Input& input = ctx.input;
  if(!hasChar(input,i,'['))
    Bug("parseCharSetUnq called at invalid location {}", i);
  CharSet cset;
  size_t j = i+1;

  cset.negated = parseCharSetNegation(input, j);

  // One or more set elements, not zero or more.
  // Allow ']' as the first set element. It does not close the set.
  do {
    if(!input.sizeGt(j)) Fatal(ctx, i, i+1, "Unmatched '['");
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
      Error(ctx, i, j, "Redundant range. Use a single character.");

    if(new_range.from > new_range.to)
      Error(ctx, i, j, "Invalid range going backwards.");

    if(!isPlainRange(new_range))
      Error(ctx, i, j, "Ranges can only span 0-9, A-Z, or a-z.");

    if(hasChar(input,j,'-') && !hasChar(input,j+1,']'))
      Error(ctx, j, j+1, "Character range has no start");
  } while(!hasChar(input,j,']'));
  i = j+1;
  return move_to_unique(cset);
}

auto parseGroup(InputDiags& ctx, size_t& i, uint8_t depth) -> optional<Regex> {
  if(depth == kMaxDepth) Fatal(ctx, i, i+1, "Parentheses nested too deep");
  const Input& input = ctx.input;
  size_t j = i;
  if(!hasChar(input,i,'('))
    FatalBug(ctx, i, i+1, "parseGroup() must start with '('");
  optional<Regex> res = parseRec(ctx, ++j, depth+1);
  if(!res) return nullopt;
  if(!hasChar(input,j,')')) return Error(ctx, i, j, "Unmatched '('");
  i = j+1;
  return res;
}

// Temporary function to detect unimplemented feature.
bool startsRepeat(char ch) { return is_in(ch, "+*?{"); }

auto parseSingleChar(InputDiags& ctx, size_t& i) -> optional<Regex> {
  auto reta = [&i](Anchor a, size_t off) {
    i += off;
    return make_unique<Anchor>(a);
  };
  char ch = ctx.input[i];
  if(ch == '\\') {
    if(hasChar(ctx.input,i+1,'b')) return reta(Anchor::wordEdge, 2);
    else if(auto opt = parseEscapeCode(ctx, i, stringMeta))
      return make_unique<string>(1, *opt);
    else return nullopt;
  }
  else if(ch == '^') return reta(Anchor::bol, 1);
  else if(ch == '$') return reta(Anchor::eol, 1);
  else return make_unique<string>(1, ctx.input[i++]);
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
    case '+': return move_to_unique(Repeat{std::move(regex)});
    case '?': return move_to_unique(Optional{std::move(regex)});
    case '*': return move_to_unique(Optional{
                       move_to_unique(Repeat{std::move(regex)})
                     });
    default: Bug("repeatWith called with invalid op: {}", op);
  }
}

// Assumes startsRepeat(ctx.input[i]) == true.
bool repeatBack(InputDiags& ctx, size_t& i, Concat& concat) {
  char ch = ctx.input[i];
  ++i;
  if(ch == '{') Unimplemented("'{{}}'");
  if(concat.parts.empty()) {
    Error(ctx, i-1, i, "Nothing to repeat");
    return false;
  }
  concat.parts.back() = repeatWith(std::move(concat.parts.back()), ch);
  return true;
}

// Used with T being one of Concat or OrList.
template <class T>
Regex unpackSingleton(T t) {
  if(t.parts.size() == 0) return make_unique<string>();
  else if(t.parts.size() == 1) return Regex{std::move(t.parts[0])};
  else return Regex{make_unique<T>(std::move(t))};
}

auto parseBranch(InputDiags& ctx, size_t& i, uint8_t depth) -> optional<Regex> {
  const Input& input = ctx.input;
  size_t j = i;
  size_t repdepth = 0;
  regex::Concat concat;
  optional<Regex> subres;

  while(input.sizeGt(j)) {
    if(input[j] == '[') subres = parseCharSetUnq(ctx, j);
    else if(input[j] == '(') subres = parseGroup(ctx, j, depth + repdepth);
    else if(is_in(input[j], ")/|")) {  // end pattern.
      i = j;
      return unpackSingleton(contractStrings(std::move(concat)));
    }else if(startsRepeat(input[j])) {
      if(++repdepth > kMaxRepDepth)
        Fatal(ctx, j, j+1, "Too many consecutive repeat operators.");
      if(!repeatBack(ctx, j, concat)) return nullopt;
      else continue;  // Skip checking subres.
    }else if(input[j] == '.') {
      subres = move_to_unique(CharSet{{}, true});
      ++j;
    }else subres = parseSingleChar(ctx, j);

    repdepth = 0;
    if(!subres) return nullopt;
    concat.parts.push_back(std::move(*subres));
  }

  Error(ctx, i, j, "Unterminated regex, expected '/'");
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
    else FatalBug(ctx, j, j+1, "Branch parsing finished unexpectedly");
  }

  Error(ctx, i, j, "Unterminated regex, expected '/'");
  i = j;
  return nullopt;
}

}  // namespace

auto prettyPrint(const Regex& regex) -> string {
  return "/" + prettyPrintRec(regex) + "/";
}

CharSet parseCharSet(string input) {
  InputDiags ctx{Input{input}};
  size_t i = 0;
  if(auto cs = parseCharSetUnq(ctx, i)) return *cs;
  else {
    for(const auto& d : ctx.diags) BugWarn("{}", string(d));
    Bug("parseCharSet() input was invalid: {}", input);
  }
}

}  // namespace regex

// Current state: only parses concatenation of character sets.
auto parseRegex(InputDiags& ctx, size_t& i) -> optional<regex::Regex> {
  using regex::hasChar;
  using regex::parseRec;
  const Input& input = ctx.input;
  if(!hasChar(input,i,'/')) return nullopt;
  size_t j = i+1;
  auto rv = parseRec(ctx, j, 0);
  if(!rv) return rv;
  else if(hasChar(input,j,')')) return Error(ctx, j, j+1, "Unmatched ')'");
  else if(!hasChar(input,j,'/'))
    FatalBug(ctx, j, j+1, "Pattern parsing ended unexpectedly without '/'");
  i = j+1;
  return rv;
}

}  // namespace oalex
