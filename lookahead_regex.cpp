#include "lookahead_regex.h"
#include <cctype>
#include <cstring>
#include "runtime/util.h"
using std::get_if;
using std::isprint;
using std::nullopt;
using std::optional;
using std::ostringstream;
using std::strchr;
using std::string;
using oalex::InputDiags;

namespace oalex::regex {

namespace {

char hexdigit(uint8_t ch) {
  if(ch <= 9) return '0' + ch;
  else if(ch <= 15) return ch-10+'a';
  else Bug()<<"hexdigit() input is not a hex digit.";
}

auto needsEscaping(char ch, bool is_first, bool is_last) -> optional<char> {
  const static char escaped_from[]="\t" "\n" "\\" "/";
  const static char escaped_to[]  ="t"  "n"  "\\" "/";
  static_assert(sizeof(escaped_from) == sizeof(escaped_to));

  if(ch == '-' && !(is_first||is_last)) return '-';
  if(ch == ']' && !is_first) return ']';
  const char* p = strchr(escaped_from, ch);
  if(p) return escaped_to[p-escaped_from];
  else return nullopt;
}

// Caret '^' is handled specially, outside of this function, since it also
// requires knowing whether the set was negated.
string escapedForSet(char ch, size_t pos, size_t count) {
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
  else Bug()<<"prettyPrint(regex) Unimplemented for variant "<<regex.index();
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

auto parseCharSetElt(InputDiags& ctx, size_t& i) -> optional<unsigned char> {
  const Input& input = ctx.input;
  if(!input.sizeGt(i)) return nullopt;
  if(input[i] == '\\' || input[i] == '/')
    Bug()<<"Parsing escape codes not yet implemented";
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

auto parse(InputDiags& ctx, size_t& i) -> optional<Regex> {
  const Input& input = ctx.input;
  if(!hasChar(input,i,'/')) return nullopt;
  size_t j = i+1;
  optional<Regex> rv;

  if(hasChar(input,j,'[')) {
    rv = parseCharSet(ctx, j);
    if(!rv.has_value()) return nullopt;
  }else Bug()<<"regex::parse() only implements a single character set.";

  if(!hasChar(input,j,'/'))
    Bug()<<"Was expecting '/' since nothing else is implemented.";
  i = j+1;
  return rv;
}

}  // namespace oalex::regex
