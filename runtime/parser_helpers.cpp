#include "parser_helpers.h"
#include <utility>
using std::exchange;
using std::string;
using std::string_view;

namespace oalex {

static JsonLoc quote(string input, size_t stPos, size_t enPos) {
  JsonLoc rv = std::move(input);
  rv.stPos = stPos; rv.enPos = enPos;
  return rv;
}

JsonLoc match(InputDiags& ctx, ssize_t& i, string_view s) {
  if(!ctx.input.hasPrefix(i, s)) return JsonLoc::ErrorValue{};
  ssize_t oldi = exchange(i, i+s.size());
  return quote(string(s), oldi, i);
}

JsonLoc match(InputDiags& ctx, ssize_t& i, const Regex& regex,
              const RegexOptions& ropts) {
  size_t oldi = i;
  if(consumeGreedily(ctx.input, sign_cast<size_t&>(i), regex, ropts))
    return quote(ctx.input.substr(oldi, i-oldi), oldi, i);
  else return JsonLoc::ErrorValue{};
}

JsonLoc match(InputDiags& ctx, ssize_t& i, const RegexCharSet& wordChars,
              string_view s) {
  if(s.empty()) return quote("", i, 0);  // Frontend should disallow this.
  if(!ctx.input.hasPrefix(i, s)) return JsonLoc::ErrorValue{};
  const ssize_t j = i+s.size();
  if(ctx.input.sizeGt(j) && matchesRegexCharSet(ctx.input[j-1], wordChars)
                         && matchesRegexCharSet(ctx.input[j], wordChars))
    return JsonLoc::ErrorValue{};
  return quote(string(s), std::exchange(i, j), j);
}

bool quietMatch(const Input& input, ssize_t i, GeneratedParser parser) {
  /* TODO codegen proper resemblance checkers.

  <rant>
  Instead, we are now abusing normal parsers as resemblance checkers,
  using hacks to discard their diagnostics. This is especially egregious
  when one notices that we are needlessly keeping copies of inputs here,
  in a function that is called *more* frequently than normal parsers. At least,
  we should be able to make Input a more lightweight type that doesn't need to
  make input copies.

  Human-written parsers don't usually have such adapter functions.
  We shouldn't have them either.
  </rant>
  */
  InputDiags proxy{Input{[&]() { return input[i++]; } }};
  ssize_t pos = 0;
  JsonLoc res = parser(proxy, pos);
  return !res.holdsError() && !hasError(proxy.diags);
}

}  // namespace oalex