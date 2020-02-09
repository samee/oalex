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

namespace oalex::regex {

namespace {

char hexdigit(uint8_t ch) {
  if(ch <= 9) return '0' + ch;
  else if(ch <= 15) return ch-10+'a';
  else Bug()<<"hexdigit() input is not a hex digit.";
}

auto needsEscaping(char ch, size_t pos, size_t count) -> optional<char> {
  const static char escaped_from[]="\t" "\n" "\\" "/";
  const static char escaped_to[]  ="t"  "n"  "\\" "/";
  static_assert(sizeof(escaped_from) == sizeof(escaped_to));

  bool is_first = (pos==0), is_last = (pos+1==count);
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
  if(optional<char> opt = needsEscaping(ch,pos,count)) {
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

}  // namespace

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

auto prettyPrint(const Regex& regex) -> string {
  if(const auto* set = get_if<CharSet>(&regex)) return prettyPrintSet(*set);
  else Bug()<<"prettyPrint(regex) Unimplemented for variant "<<regex.index();
}

}  // namespace oalex::regex
