#include "lookahead_regex.h"
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

auto xlat(const char* from, const char* to, char ch) -> optional<char> {
  if(!ch) return nullopt;
  const char* p = strchr(from, ch);
  if(p) return to[p-from];
  else return nullopt;
}

auto needsEscaping(char ch, bool is_first, bool is_last) -> optional<char> {
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
  if(optional<char> opt = needsEscaping(ch, pos==0, pos+1==count)) {
    s[1]=*opt;
    s[2]='\0';
  }else if(isprint(ch)) { s[0]=ch; s[1]='\0'; }
  else { s[2]=hexdigit(ch/16); s[3]=hexdigit(ch%16); }
  return s;
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

string prettyPrintSet(const CharSet& set) {
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

string prettyPrintSeq(const Concat& seq) {
  ostringstream os;
  for(auto& part : seq.parts) {
    if(holds_one_of_unique<Concat, OrList, Negate>(part))
      os<<'('<<prettyPrintRec(part)<<')';
    else os<<prettyPrintRec(part);
  }
  return os.str();
}

  /*
  using Regex = std::variant<
    CharRange,
    std::string,
    std::unique_ptr<struct Concat>,
    std::unique_ptr<struct Repeat>,
    std::unique_ptr<struct Optional>,
    std::unique_ptr<struct OrList>,
    std::unique_ptr<struct Negate>
  >;

  struct Concat { std::vector<Regex> parts; };
  struct Repeat { Regex part; };
  struct Optional { Regex part; };
  struct OrList { std::vector<Regex> parts; };
  struct Negate { Regex part; };

  */

auto prettyPrintRec(const Regex& regex) -> string {
  if(const auto* set = get_if<CharSet>(&regex)) return prettyPrintSet(*set);
  else if(const auto* seq = get_if<unique_ptr<Concat>>(&regex))
    return prettyPrintSeq(**seq);
  else Unimplemented()<<"prettyPrint(regex) for variant "<<regex.index();
}

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

auto parseCharSet(InputDiags& ctx, size_t& i) -> optional<CharSet> {
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
  return cset;
}

}  // namespace

auto prettyPrint(const Regex& regex) -> string {
  return "/" + prettyPrintRec(regex) + "/";
}

auto parseRec(InputDiags& ctx, size_t& i) -> optional<Regex> {
  const Input& input = ctx.input;
  size_t j = i;
  regex::Concat concat;

  while(input.sizeGt(j)) {
    if(input[j] == '[') {
      optional<CharSet> cset = parseCharSet(ctx, j);
      if(!cset) return nullopt;
      concat.parts.push_back(std::move(*cset));
    }else if(input[j] == '(') {
      size_t k = j;
      optional<Regex> res = parseRec(ctx, ++j);
      if(!res) return nullopt;
      if(!hasChar(input,j,')')) return ctx.Error(k, j, "Unmatched '('");
      ++j;
      concat.parts.push_back(std::move(*res));
    }else if(input[j] == '/' || input[j] == ')') {  // end pattern.
      i = j;
      if(concat.parts.size() == 1) return Regex{std::move(concat.parts[0])};
      else return Regex{make_unique<Concat>(std::move(concat))};
    }else
      Unimplemented()<<"regex::parse() features beyond character sets.";
  }

  ctx.Error(i, j, "Unterminated regex, expected '/'");
  i = j;
  return nullopt;
}

// Current state: only parses concatenation of character sets.
auto parse(InputDiags& ctx, size_t& i) -> optional<Regex> {
  const Input& input = ctx.input;
  if(!hasChar(input,i,'/')) return nullopt;
  size_t j = i+1;
  auto rv = parseRec(ctx, j);
  if(!rv) return rv;
  else if(hasChar(input,j,')')) return ctx.Error(j, j+1, "Unmatched ')'");
  else if(!hasChar(input,j,'/'))
    ctx.FatalBug(j, j+1, "Pattern parsing ended unexpectedly without '/'");
  i = j+1;
  return rv;
}

}  // namespace oalex::regex
