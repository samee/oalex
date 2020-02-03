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

auto needsEscaping(char ch) -> optional<char> {
  const static char escaped_from[]="\t" "\n" "\\" "/";
  const static char escaped_to[]  ="t"  "n"  "\\" "/";
  static_assert(sizeof(escaped_from) == sizeof(escaped_to));
  const char* p = strchr(escaped_from, ch);
  if(p) return escaped_to[p-escaped_from];
  else return nullopt;
}

string escaped(char ch) {
  char s[5]="\\x55";
  if(optional<char> opt = needsEscaping(ch)) { s[1]=*opt; s[2]='\0'; }
  else if(isprint(ch)) { s[0]=ch; s[1]='\0'; }
  else { s[2]=hexdigit(ch/16); s[3]=hexdigit(ch%16); }
  return s;
}

bool isPlainRange(unsigned char from, unsigned char to) {
  return (isdigit(from) && isdigit(to)) ||
         (isupper(from) && isupper(to)) ||
         (islower(from) && islower(to));
}

string plainRange(const CharRange& range) {
  return Str()<<"["<<range.from<<'-'<<range.to<<"]";
}

string disjunctedRange(const CharRange& range) {
  ostringstream str;
  str<<"("<<escaped(range.from);
  for(size_t i=range.from+1; i<=range.to; ++i) str<<"|"<<escaped(i);
  str<<")";
  return str.str();
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
  if(const auto* range = get_if<CharRange>(&regex)) {
    if(range->from > range->to) Bug()<<"Invalid regex range";
    else if(range->from == range->to) return escaped(range->from);
    else if(isPlainRange(range->from, range->to)) return plainRange(*range);
    else return disjunctedRange(*range);
  }else Bug()<<"prettyPrint(regex) Unimplemented for variant "<<regex.index();
}

}  // namespace oalex::regex
