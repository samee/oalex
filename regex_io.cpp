/*  Copyright 2020 The oalex authors.

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
#include <initializer_list>
#include <iterator>
#include <vector>
#include "fmt/format.h"
#include "oalex.h"
#include "util.h"
using std::array;
using std::back_insert_iterator;
using std::get_if;
using std::initializer_list;
using std::isprint;
using std::make_unique;
using std::nullopt;
using std::optional;
using std::strchr;
using std::string;
using std::string_view;
using std::unique_ptr;
using std::vector;
using oalex::InputDiags;
using oalex::is_in;

namespace oalex {

// Forward declaration from lexer.h. Can be moved to some parser util later,
// so we don't have to include all of lexer.h and dependencies.
namespace lex {

optional<char> lexHexCode(string_view s, size_t& i,
                          DiagsDest ctx, size_t iPos);

optional<char> lexHexCode(InputDiags& ctx, size_t& i) {
  size_t j=0;
  auto rv = lexHexCode(ctx.input().substr(i,2), j, ctx, i);
  i += j;
  return rv;
}

}  // namespace lex

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

bool hasAllChars(const RegexCharSet& set) {
  return set.negated && set.ranges.empty();
}

string prettyPrintSet(const RegexCharSet& set) {
  if(hasAllChars(set)) return ".";
  size_t n = set.ranges.size();
  fmt::memory_buffer buf;
  back_insert_iterator buf_app{buf};

  if(set.negated) format_to(buf_app, "[^");
  else format_to(buf_app, "[");

  for(size_t i=0; i<n; ++i) {
    auto& r = set.ranges[i];
    if(r.from > r.to) Bug("Invalid regex range: {} > {}", r.from, r.to);
    else if(r.from == r.to) {
      if(r.from == '^' && i == 0 && !set.negated) format_to(buf_app, "\\^");
      else format_to(buf_app, "{}", escapedForSet(r.from, i, n));
    }else if(isPlainRange(r.from, r.to))
      format_to(buf_app, "{}-{}", char(r.from), char(r.to));
    else Bug("Complicated range found: {} to {}", r.from, r.to);
  }
  format_to(buf_app, "]");
  return fmt::to_string(buf);
}

string prettyPrintAnchor(RegexAnchor::AnchorType a) {
  switch(a) {
    case RegexAnchor::wordEdge: return "\\b";
    case RegexAnchor::bol: return "^";
    case RegexAnchor::eol: return "$";
    default: Bug("Unknown RegexAnchor type {}", static_cast<int>(a));
  }
}

bool printsToNull(const Regex& regex) {
  switch(regex.nodeType) {
    case RegexNodeType::charSet:
    case RegexNodeType::anchor:
    case RegexNodeType::optional:
    case RegexNodeType::repeat:
    case RegexNodeType::orList:
      return false;
    case RegexNodeType::string:
      return static_cast<const RegexString&>(regex).value.empty();
    case RegexNodeType::concat: {
      auto& seq = static_cast<const RegexConcat&>(regex);
      for(const auto& p : seq.parts) if(!printsToNull(*p)) return false;
      return true;
    }
    default:
      Bug("printsToNull() called with unknown index {}", int(regex.nodeType));
  }
}

void surroundUnless(const initializer_list<RegexNodeType>& baretypes,
                    fmt::memory_buffer& buf, const Regex& regex) {
  back_insert_iterator buf_app{buf};
  for(RegexNodeType t : baretypes) if(t == regex.nodeType) {
    format_to(buf_app, "{}", prettyPrintRec(regex));
    return;
  }
  format_to(buf_app, "({})", prettyPrintRec(regex));
}

string prettyPrintSeq(const RegexConcat& seq) {
  fmt::memory_buffer buf;
  back_insert_iterator buf_app{buf};
  for(auto& part : seq.parts) {
    if(printsToNull(*part)) format_to(buf_app, "()");
    else surroundUnless({RegexNodeType::charSet, RegexNodeType::string,
                         RegexNodeType::anchor, RegexNodeType::repeat,
                         RegexNodeType::optional}, buf, *part);
  }
  return fmt::to_string(buf);
}

string prettyPrintOrs(const RegexOrList& ors) {
  if(ors.parts.empty()) return "";
  fmt::memory_buffer buf;
  back_insert_iterator buf_app{buf};

  surroundUnless({RegexNodeType::charSet, RegexNodeType::string,
                  RegexNodeType::anchor, RegexNodeType::concat,
                  RegexNodeType::repeat, RegexNodeType::optional},
                 buf, *ors.parts[0]);
  for(size_t i=1; i<ors.parts.size(); ++i) {
    format_to(buf_app, "|");
    surroundUnless({RegexNodeType::charSet, RegexNodeType::string,
                    RegexNodeType::anchor, RegexNodeType::concat,
                    RegexNodeType::repeat, RegexNodeType::optional},
                   buf, *ors.parts[i]);
  }
  return fmt::to_string(buf);
}

string surround(string s) { return "(" + s + ")"; }

// `op` can be one of [?+*{], where '{' indicates numeric repeat.
string prettyPrintRepeatPart(const Regex& part, char op) {
  string op_s(1, op);
  switch(part.nodeType) {
    case RegexNodeType::string: {
      auto& s = static_cast<const RegexString&>(part);
      if(s.value.size() == 1) return prettyPrintRec(part) + op_s;
      else return surround(prettyPrintRec(part)) + op_s;
    }
    case RegexNodeType::concat:
    case RegexNodeType::repeat:
    case RegexNodeType::orList:
      return surround(prettyPrintRec(part)) + op_s;
    default:
      return prettyPrintRec(part) + op_s;
  }
}

string prettyPrintOpt(const RegexOptional& opt) {
  // Collapse /(...)+?/ into /(...)*/
  if(opt.part->nodeType == RegexNodeType::repeat)
    return prettyPrintRepeatPart(
        *static_cast<const RegexRepeat&>(*opt.part).part, '*');
  else return prettyPrintRepeatPart(*opt.part, '?');
}

string prettyPrintRep(const RegexRepeat& rep) {
  return prettyPrintRepeatPart(*rep.part, '+');
}

auto prettyPrintRec(const Regex& regex) -> string {
  switch(regex.nodeType) {
    case RegexNodeType::charSet:
      return prettyPrintSet(static_cast<const RegexCharSet&>(regex));
    case RegexNodeType::concat:
      return prettyPrintSeq(static_cast<const RegexConcat&>(regex));
    case RegexNodeType::string:
      return escapedForString(static_cast<const RegexString&>(regex).value);
    case RegexNodeType::anchor:
      return prettyPrintAnchor(
          static_cast<const RegexAnchor&>(regex).anchorType);
    case RegexNodeType::repeat:
      return prettyPrintRep(static_cast<const RegexRepeat&>(regex));
    case RegexNodeType::optional:
      return prettyPrintOpt(static_cast<const RegexOptional&>(regex));
    case RegexNodeType::orList:
      return prettyPrintOrs(static_cast<const RegexOrList&>(regex));
    default:
      Unimplemented("prettyPrint(regex) for variant {}", int(regex.nodeType));
  }
}

constexpr uint8_t kMaxDepth = 255;
constexpr uint8_t kMaxRepDepth = 5;
auto parseRec(InputDiags& ctx, size_t& i, uint8_t depth)
  -> unique_ptr<const Regex>;

// Regex and string literals are among the few places that don't ignore spaces
// before checking for a specific character.
bool hasChar(const InputPiece& input, size_t pos, char ch) {
  return input.sizeGt(pos) && input[pos]==ch;
}

bool parseCharSetNegation(const InputPiece& input, size_t& i) {
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
  const InputPiece& input = ctx.input();
  if(!hasChar(input,i,'\\')) return nullopt;
  if(!input.sizeGt(i+1)) return Error(ctx, i, i+1, "Incomplete escape code");
  if(input[i+1] == 'x') return parseHexCode(ctx, i);
  if(auto res = unescaped(input[i+1], meta)) { i=i+2; return res; }
  else return Error(ctx, i, i+2, "Unknown escape code");
}

auto parseCharSetElt(InputDiags& ctx, size_t& i) -> optional<unsigned char> {
  const InputPiece& input = ctx.input();
  if(!input.sizeGt(i)) return nullopt;
  if(input[i] == '/') {
    Error(ctx, i, i+1, "Expected closing ']'");
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

auto parseCharSetUnq(InputDiags& ctx, size_t& i) -> unique_ptr<RegexCharSet> {
  const InputPiece& input = ctx.input();
  if(!hasChar(input,i,'['))
    Bug("parseCharSetUnq called at invalid location {}", i);
  RegexCharSet cset;
  size_t j = i+1;

  cset.negated = parseCharSetNegation(input, j);

  // One or more set elements, not zero or more.
  // Allow ']' as the first set element. It does not close the set.
  do {
    if(!input.sizeGt(j)) {
      Error(ctx, i, i+1, "Unmatched '['");
      i = j;
      return nullptr;
    }
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

auto parseGroup(InputDiags& ctx, size_t& i, uint8_t depth)
  -> unique_ptr<const Regex> {
  if(depth == kMaxDepth) {
    Error(ctx, i, i+1, "Parentheses nested too deep");
    return nullptr;
  }
  const InputPiece& input = ctx.input();
  size_t j = i;
  if(!hasChar(input,i,'('))
    FatalBug(ctx, i, i+1, "parseGroup() must start with '('");
  unique_ptr<const Regex> res = parseRec(ctx, ++j, depth+1);
  if(!res) return nullptr;
  if(!hasChar(input,j,')')) {
    Error(ctx, i, j, "Unmatched '('");
    return nullptr;
  }
  i = j+1;
  return res;
}

// Temporary function to detect unimplemented feature.
bool startsRepeat(char ch) { return is_in(ch, "+*?{"); }

// TODO reduce heap allocation. Make a string parser.
auto parseSingleChar(InputDiags& ctx, size_t& i) -> unique_ptr<const Regex> {
  auto reta = [&i](RegexAnchor::AnchorType a, size_t off) {
    i += off;
    return make_unique<RegexAnchor>(a);
  };
  char ch = ctx.input()[i];
  if(ch == '\\') {
    if(hasChar(ctx.input(),i+1,'b')) return reta(RegexAnchor::wordEdge, 2);
    else if(auto opt = parseEscapeCode(ctx, i, stringMeta))
      return make_unique<RegexString>(string(1, *opt));
    else return nullptr;
  }
  else if(ch == '^') return reta(RegexAnchor::bol, 1);
  else if(ch == '$') return reta(RegexAnchor::eol, 1);
  else return make_unique<RegexString>(string(1, ctx.input()[i++]));
}

// Consequtive string parts get joined into a single string.
auto contractStrings(RegexConcat concat) -> RegexConcat {
  vector<unique_ptr<const Regex>> rvparts;
  string acc;
  for(unique_ptr<const Regex>& part: concat.parts) {
    if(part->nodeType == RegexNodeType::string) {
      acc.append(static_cast<const RegexString&>(*part).value);
    }else {
      if(!acc.empty())
        rvparts.push_back(make_unique<RegexString>(std::move(acc)));
      acc.clear();
      rvparts.push_back(std::move(part));
    }
  }
  if(!acc.empty())
    rvparts.push_back(make_unique<RegexString>(std::move(acc)));
  return RegexConcat(std::move(rvparts));
}

// op is assumed to be one of [+*?].
unique_ptr<const Regex> repeatWith(unique_ptr<const Regex> regex, char op) {
  switch(op) {
    case '+': return move_to_unique(RegexRepeat{std::move(regex)});
    case '?': return move_to_unique(RegexOptional{std::move(regex)});
    case '*': return move_to_unique(RegexOptional{
                       move_to_unique(RegexRepeat{std::move(regex)})
                     });
    default: Bug("repeatWith called with invalid op: {}", op);
  }
}

// Assumes startsRepeat(ctx.input()[i]) == true.
bool repeatBack(InputDiags& ctx, size_t& i, RegexConcat& concat) {
  char ch = ctx.input()[i];
  ++i;
  if(ch == '{') Unimplemented("'{{}}'");
  if(concat.parts.empty()) {
    Error(ctx, i-1, i, "Nothing to repeat");
    return false;
  }
  concat.parts.back() = repeatWith(std::move(concat.parts.back()), ch);
  return true;
}

// Used with T being one of RegexConcat or RegexOrList.
template <class T>
auto unpackSingleton(T t) -> unique_ptr<const Regex> {
  if(t.parts.size() == 0) return make_unique<RegexString>(string());
  else if(t.parts.size() == 1) return std::move(t.parts[0]);
  else return make_unique<T>(std::move(t));
}

auto parseBranch(InputDiags& ctx, size_t& i, uint8_t depth)
  -> unique_ptr<const Regex> {
  const InputPiece& input = ctx.input();
  size_t j = i;
  size_t repdepth = 0;
  RegexConcat concat;
  unique_ptr<const Regex> subres;

  while(input.sizeGt(j)) {
    if(input[j] == '[') subres = parseCharSetUnq(ctx, j);
    else if(input[j] == '(') subres = parseGroup(ctx, j, depth + repdepth);
    else if(is_in(input[j], ")/|")) {  // end pattern.
      i = j;
      return unpackSingleton(contractStrings(std::move(concat)));
    }else if(startsRepeat(input[j])) {
      if(++repdepth > kMaxRepDepth) {
        Error(ctx, j, j+1, "Too many consecutive repeat operators.");
        ++j;
      }else if(!repeatBack(ctx, j, concat)) return nullptr;
      else continue;  // Skip checking subres.
    }else if(input[j] == '.') {
      subres = move_to_unique(RegexCharSet{{}, true});
      ++j;
    }else subres = parseSingleChar(ctx, j);

    repdepth = 0;
    if(!subres) return nullptr;
    concat.parts.push_back(std::move(subres));
  }

  Error(ctx, i, j, "Unterminated regex, expected '/'");
  i = j;
  return nullptr;
}

auto parseRec(InputDiags& ctx, size_t& i, uint8_t depth)
  -> unique_ptr<const Regex> {
  const InputPiece& input = ctx.input();
  size_t j = i;
  RegexOrList ors;
  unique_ptr<const Regex> subres;

  while(input.sizeGt(j)) {
    subres = parseBranch(ctx, j, depth);
    if(!subres) return nullptr;
    ors.parts.push_back(std::move(subres));
    if(!input.sizeGt(j)) break;
    if(is_in(input[j], ")/")) {
      i = j;
      return unpackSingleton(std::move(ors));
    }else if(input[j] == '|') ++j;
    else FatalBug(ctx, j, j+1, "Branch parsing finished unexpectedly");
  }

  Error(ctx, i, j, "Unterminated regex, expected '/'");
  i = j;
  return nullptr;
}

}  // namespace

auto prettyPrint(const Regex& regex) -> string {
  return "/" + prettyPrintRec(regex) + "/";
}

RegexCharSet parseRegexCharSet(string input) {
  InputDiags ctx{Input{input}};
  size_t i = 0;
  if(auto cs = parseCharSetUnq(ctx, i)) return std::move(*cs);
  else {
    for(const auto& d : ctx.diags) BugWarn("{}", string(d));
    Bug("parseRegexCharSet() input was invalid: {}", input);
  }
}

auto parseRegex(InputDiags& ctx, size_t& i) -> unique_ptr<const Regex> {
  const InputPiece& input = ctx.input();
  if(!hasChar(input,i,'/')) return nullptr;
  size_t j = i+1;
  auto rv = parseRec(ctx, j, 0);
  if(!rv) return rv;
  else if(hasChar(input,j,')')) {
    Error(ctx, j, j+1, "Unmatched ')'");
    return nullptr;
  } else if(!hasChar(input,j,'/'))
    FatalBug(ctx, j, j+1, "Pattern parsing ended unexpectedly without '/'");
  i = j+1;
  return rv;
}

}  // namespace oalex
