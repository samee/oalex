#include "parser_helpers.h"
#include <utility>
#include "util.h"
using std::exchange;
using std::string;
using std::string_view;

namespace oalex {

JsonLoc errorValue(DiagsDest ctx, ssize_t i, string msg) {
  Error(ctx, i, std::move(msg));
  return JsonLoc::ErrorValue{};
}

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

bool peekMatch(InputDiags& ctx, ssize_t i, const RegexCharSet& wordChars,
               string_view s) {
  if(s.empty()) return true;  // Frontend should disallow this.
  if(!ctx.input.hasPrefix(i, s)) return false;
  const ssize_t j = i+s.size();
  if(ctx.input.sizeGt(j) && matchesRegexCharSet(ctx.input[j-1], wordChars)
                         && matchesRegexCharSet(ctx.input[j], wordChars))
    return false;
  else return true;
}

JsonLoc match(InputDiags& ctx, ssize_t& i, const RegexCharSet& wordChars,
              string_view s) {
  if(peekMatch(ctx, i, wordChars, s)) {
    const ssize_t j = i+s.size();
    return quote(string(s), std::exchange(i, j), j);
  }else return JsonLoc::ErrorValue();
}

class InputWrapper : public InputStream {
  const Input* input;
  ssize_t i;
 public:
  InputWrapper(const Input& input, ssize_t i) : input{&input}, i{i} {}
  int16_t getch() override
    { return input->sizeGt(i) ? (*input)[i++] : -1; }
};
// It's not static because we reuse this in codegen.cpp:eval(), but it's
// not in the header file since I have no intention of supporting this hack in
// the long run.
// This proxy object both discards diags and defends against
// overly enthusiastic forgetBefore().
InputDiags substrProxy(const Input& input, ssize_t i) {
  return InputDiags{Input{std::make_unique<InputWrapper>(input, i)}};
}

bool peekMatch(const Input& input, ssize_t i, GeneratedParser parser) {
  // The only difference between this and quietMatch() is that
  // peekMatch() accepts `i` by value.
  return !quietMatch(input, i, parser).holdsError();
}

JsonLoc quietMatch(const Input& input, ssize_t& i, GeneratedParser parser) {
  /* TODO codegen proper resemblance checkers.
     TODO while this is here, add a flag to InputDiags to disable errors
     with an RAII-controlled flag. Although, I'd have to then think about how
     to do this whiel preserving the type signature to accept an Input type
     and not InputDiags or DiagsDest. Speaking of DiagsDest, that should then
     have to support this too, or we should at least forbid DiagsDest
     construction in this state.

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
  InputDiags proxy = substrProxy(input, i);
  ssize_t pos = 0;
  JsonLoc rv = parser(proxy, pos);
  if(!rv.holdsError()) i += pos;
  return rv;
}

void mapUnion(JsonLoc::Map& m1, JsonLoc::Map& m2) {
  for(auto& p : m2) if(!m1.insert(std::move(p)).second)
    Bug("maps are supposed to be key-disjoint");
}

void mapCreateOrAppend(JsonLoc::Map& m, const std::string& k,
                       JsonLoc v, bool doCreate) {
  if(doCreate) {
    if(!m.insert({k, JsonLoc::Vector{std::move(v)}}).second)
      Bug("Collision in loop elements with name '{}'", k);
  }else {
    auto it = m.find(k);
    if(it == m.end())
      Bug("Need to create element after the first iteration");
    oalex::get_if<JsonLoc::Vector>(&it->second)->push_back(std::move(v));
  }
}

void mapCreateOrAppendAllElts(JsonLoc::Map& m1, JsonLoc::Map m2,
                              bool doCreate) {
  for(auto& [k,v] : m2) mapCreateOrAppend(m1, k, std::move(v), doCreate);
}

void assertMap(JsonLoc& jsloc, string_view errctx) {
  auto* m = get_if<JsonLoc::Map>(&jsloc);
  if(m == nullptr) Bug("{}: needs a map", errctx);
}

void assertNotNull(void* p, string_view fname, string_view errmsg) {
  if(!p) Bug("{}: {}", fname, errmsg);
}

}  // namespace oalex
