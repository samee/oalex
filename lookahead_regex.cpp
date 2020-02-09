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
  const static char escaped_from[]="\t" "\n" "\\" "/" "]";
  const static char escaped_to[]  ="t"  "n"  "\\" "/" "]";
  static_assert(sizeof(escaped_from) == sizeof(escaped_to));
  const char* p = strchr(escaped_from, ch);
  if(p) return escaped_to[p-escaped_from];
  else return nullopt;
}

string escapedForSet(char ch) {
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

// Used to make sure '^' does not get printed like a negation.
// E.g. [\^#@] and not [^#@]. For any object `CaretEscaper fixCaret;`
// fixCaret('^') == "\\^" if this is the first fixCaret call. All other inputs
// are returned unchanged (other than a char->string conversion).
class CaretEscaper {
  bool first = true;
 public:
  string operator()(char ch) {
    if(first && ch=='^') return "\\^";
    first = false;
    return string(1,ch);
  }
  string operator()(string s) {
    return s.size()==1?(*this)(s[0]):s;
  }
};

string prettyPrintSet(const CharSet& set) {
  ostringstream os;
  CaretEscaper fixCaret;
  os<<'[';
  for(auto r : set.ranges) {
    if(r.from > r.to) Bug()<<"Invalid regex range";
    else if(r.from == r.to) os<<fixCaret(escapedForSet(r.from));
    else if(isPlainRange(r.from, r.to)) os<<fixCaret(r.from)<<'-'<<r.to;
    else Bug()<<"Complicated range found: "<<fixCaret(r.from)<<" to "<<r.to;
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
