#include "frontend_pieces.h"
using oalex::InputDiags;
using oalex::JsonLoc;
using namespace std::string_literals;

const oalex::RegexOptions& defaultRegexOpts() {
  using oalex::RegexCharSet;
  using oalex::RegexOptions;
  static const RegexOptions *opts = new RegexOptions{
    .word = RegexCharSet({
              { '_', '_' },
              { 'a', 'z' },
              { 'A', 'Z' },
              { '0', '9' },
            }, false)
  };
  return *opts;
}

std::optional<oalex::StringLoc> parseWord(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = parseRule1(ctx, i);
  if(holdsErrorValue(res))
    Error(ctx, i, "Does not match expected pattern");
  return res;
}

std::optional<oalex::StringLoc> parseRule1(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::makeVector;
  using oalex::move_to_unique;
  using oalex::Regex;
  using oalex::RegexAnchor;
  using oalex::RegexCharSet;
  using oalex::RegexConcat;
  using oalex::RegexOptional;
  using oalex::RegexOrList;
  using oalex::RegexRepeat;
  using oalex::RegexString;
  using std::literals::string_literals::operator""s;
  using std::unique_ptr;
  static const Regex *r = new RegexRepeat(move_to_unique(
      RegexCharSet({
        { 'a', 'z' },
        { 'A', 'Z' },
        { '_', '_' },
        { '0', '9' },
      }, false)
    ));
  return oalex::match(ctx, i, *r, defaultRegexOpts());
}

std::optional<oalex::StringLoc> parseDoubleQuotedLiteral(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = parseRule2(ctx, i);
  if(holdsErrorValue(res))
    Error(ctx, i, "Does not match expected pattern");
  return res;
}

std::optional<oalex::StringLoc> parseRule2(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::makeVector;
  using oalex::move_to_unique;
  using oalex::Regex;
  using oalex::RegexAnchor;
  using oalex::RegexCharSet;
  using oalex::RegexConcat;
  using oalex::RegexOptional;
  using oalex::RegexOrList;
  using oalex::RegexRepeat;
  using oalex::RegexString;
  using std::literals::string_literals::operator""s;
  using std::unique_ptr;
  static const Regex *r = new RegexConcat({makeVector<unique_ptr<const Regex>>(
      move_to_unique(RegexString("\"")),
      move_to_unique(RegexOptional(move_to_unique(
        RegexRepeat(move_to_unique(
          RegexOrList({makeVector<unique_ptr<const Regex>>(
            move_to_unique(RegexCharSet({
              { '\"', '\"' },
              { '\\', '\\' },
            }, true)),
            move_to_unique(RegexConcat({makeVector<unique_ptr<const Regex>>(
              move_to_unique(RegexString("\\")),
              move_to_unique(RegexCharSet({
              }, true))
            )}))
          )})
        ))
      ))),
      move_to_unique(RegexString("\""))
    )});
  return oalex::match(ctx, i, *r, defaultRegexOpts());
}

std::optional<oalex::StringLoc> parseRule3(oalex::InputDiags& ctx, ssize_t& i) {
  return parseWord(ctx, i);
}

std::optional<oalex::StringLoc> parseRule4(oalex::InputDiags& ctx, ssize_t& i) {
  return parseWord(ctx, i);
}

std::optional<oalex::StringLoc> parseRule5(oalex::InputDiags& ctx, ssize_t& i) {
  return parseWord(ctx, i);
}

ParsedExternRule::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
    {"external_name", fields.external_name},
    {"param", fields.param},
    {"rule_name", fields.rule_name},
  }, loc.first, loc.second);
}

std::optional<ParsedExternRule> parseExternRule(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::assertNotNull;
  using oalex::JsonLoc;
  using oalex::moveEltOrEmpty;
  ssize_t oldi = i;
  JsonLoc outfields = parseRule26(ctx, i);
  if(outfields.holdsErrorValue()) return std::nullopt;
  auto* m = outfields.getIfMap();
  assertNotNull(m, __func__, "needs a map");
  ParsedExternRule rv{
    .loc{oldi, i},
    .fields{
      .external_name = moveEltOrEmpty(*m, "external_name"),
      .param = moveEltOrEmpty(*m, "param"),
      .rule_name = moveEltOrEmpty(*m, "rule_name"),
    },
  };
  return rv;
}

std::optional<oalex::StringLoc> parseRule6(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, defaultRegexOpts().word, "extern");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected 'extern'");
  return res;
}

ParsedRule7::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

oalex::JsonLoc parseRule7(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .newlines = Skipper::Newlines::ignore_all,
  };
  ssize_t j = skip->next(ctx.input(), i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

std::optional<oalex::StringLoc> parseRule8(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, defaultRegexOpts().word, "rule");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected 'rule'");
  return res;
}

ParsedRule9::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

oalex::JsonLoc parseRule9(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .newlines = Skipper::Newlines::ignore_all,
  };
  ssize_t j = skip->next(ctx.input(), i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

ParsedRule10::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"rule_name", JsonLoc{fields.rule_name}},
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

[[maybe_unused]]
static void mergePart0IntoParsedRule10(oalex::StringLoc src, ParsedRule10& dest) {
  dest.fields.rule_name = std::move(src);
}

oalex::JsonLoc parseRule10(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = oalex::toJsonLoc(parseRule3(ctx, j));
  if(res.holdsErrorValue()) return res;
  m.emplace_back("rule_name", std::move(res));
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

ParsedRule11::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

oalex::JsonLoc parseRule11(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .newlines = Skipper::Newlines::ignore_all,
  };
  ssize_t j = skip->next(ctx.input(), i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

std::optional<oalex::StringLoc> parseRule12(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, "=");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected '='");
  return res;
}

ParsedRule13::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

oalex::JsonLoc parseRule13(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .newlines = Skipper::Newlines::ignore_all,
  };
  ssize_t j = skip->next(ctx.input(), i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

ParsedRule14::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"external_name", JsonLoc{fields.external_name}},
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

[[maybe_unused]]
static void mergePart0IntoParsedRule14(oalex::StringLoc src, ParsedRule14& dest) {
  dest.fields.external_name = std::move(src);
}

oalex::JsonLoc parseRule14(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = oalex::toJsonLoc(parseRule4(ctx, j));
  if(res.holdsErrorValue()) return res;
  m.emplace_back("external_name", std::move(res));
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

ParsedRule15::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

oalex::JsonLoc parseRule15(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .newlines = Skipper::Newlines::ignore_all,
  };
  ssize_t j = skip->next(ctx.input(), i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

std::optional<oalex::StringLoc> parseRule16(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, "(");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected '('");
  return res;
}

ParsedRule17::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

oalex::JsonLoc parseRule17(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .newlines = Skipper::Newlines::ignore_all,
  };
  ssize_t j = skip->next(ctx.input(), i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

ParsedRule18::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"param", JsonLoc{fields.param}},
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

[[maybe_unused]]
static void mergePart0IntoParsedRule18(oalex::StringLoc src, ParsedRule18& dest) {
  dest.fields.param = std::move(src);
}

oalex::JsonLoc parseRule18(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = oalex::toJsonLoc(parseRule5(ctx, j));
  if(res.holdsErrorValue()) return res;
  m.emplace_back("param", std::move(res));
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

std::optional<oalex::StringLoc> parseRule19(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, ",");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected ','");
  return res;
}

ParsedRule20::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

oalex::JsonLoc parseRule20(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .newlines = Skipper::Newlines::ignore_all,
  };
  ssize_t j = skip->next(ctx.input(), i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

ParsedRule21::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"param", oalex::toJsonLoc(fields.param)},
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

[[maybe_unused]]
static void mergePartIntoParsedRule21(ParsedRule18 src, ParsedRule21& dest) {
  dest.fields.param.push_back(std::move(src.fields.param));
}

[[maybe_unused]]
static void mergeGlueIntoParsedRule21(oalex::StringLoc /*src*/, ParsedRule21& /*dest*/) {}
oalex::JsonLoc parseRule21(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  using oalex::mapCreateOrAppend;
  using oalex::mapCreateOrAppendAllElts;
  using oalex::quietMatch;
  using oalex::toJsonLoc;
  ssize_t j = i, fallback_point = i;

  JsonLoc::Map m;
  bool first = true;
  while(true) {
    JsonLoc res = JsonLoc::ErrorValue{};

    res = parseRule18(ctx, j);
    if(res.holdsErrorValue()) return res;
    mapCreateOrAppendAllElts(m,
      std::move(*res.getIfMap()), first);
    fallback_point = j;

    res = toJsonLoc(quietMatch(ctx.input(), j, parseRule20));
    if(res.holdsErrorValue()) break;
    res = toJsonLoc(quietMatch(ctx.input(), j, parseRule19));
    if(res.holdsErrorValue()) break;
    res = parseRule20(ctx, j);
    if(res.holdsErrorValue())
      return oalex::errorValue(ctx, j, "Unfinished comment");
    first = false;
  }
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = fallback_point;
  i = fallback_point;
  return rv;
}

ParsedRule22::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"param", oalex::toJsonLoc(fields.param)},
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

oalex::JsonLoc parseRule22(oalex::InputDiags& ctx, ssize_t& i) {
  return oalex::quietMatch(ctx.input(), i, parseRule21);
}

ParsedRule23::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"param", oalex::toJsonLoc(fields.param)},
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

[[maybe_unused]]
static ParsedRule23 convertBranch0IntoParsedRule23(ParsedRule22 src) {
  ParsedRule23 dest = {};
  dest.fields.param = std::move(src.fields.param);
  return dest;
}

[[maybe_unused]]
static ParsedRule23 convertBranch1IntoParsedRule23(const oalex::StringLoc&) { return {}; }
oalex::JsonLoc parseRule23(oalex::InputDiags& ctx, ssize_t& i) {
  using std::literals::string_literals::operator""s;
  JsonLoc res{JsonLoc::ErrorValue{}};
  res = parseRule22(ctx, i);
  if(!res.holdsErrorValue())
    return JsonLoc::withPos(res, res.stPos, res.enPos);
  res = oalex::toJsonLoc(oalex::match(ctx, i, ""));
  if(!res.holdsErrorValue())
    return JsonLoc::withPos(JsonLoc::Map{
    }, res.stPos, res.enPos);
  return res;
}

ParsedRule24::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

oalex::JsonLoc parseRule24(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .newlines = Skipper::Newlines::ignore_all,
  };
  ssize_t j = skip->next(ctx.input(), i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

std::optional<oalex::StringLoc> parseRule25(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, ")");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected ')'");
  return res;
}

ParsedRule26::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"rule_name", JsonLoc{fields.rule_name}},
    {"external_name", JsonLoc{fields.external_name}},
    {"param", oalex::toJsonLoc(fields.param)},
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

[[maybe_unused]]
static void mergePart0IntoParsedRule26(oalex::StringLoc /*src*/, ParsedRule26& /*dest*/) {}
[[maybe_unused]]
static void mergePart1IntoParsedRule26(ParsedRule7 /*src*/, ParsedRule26& /*dest*/) {}
[[maybe_unused]]
static void mergePart2IntoParsedRule26(oalex::StringLoc /*src*/, ParsedRule26& /*dest*/) {}
[[maybe_unused]]
static void mergePart3IntoParsedRule26(ParsedRule9 /*src*/, ParsedRule26& /*dest*/) {}
[[maybe_unused]]
static void mergePart4IntoParsedRule26(ParsedRule10 src, ParsedRule26& dest) {
  dest.fields.rule_name = std::move(src.fields.rule_name);
}

[[maybe_unused]]
static void mergePart5IntoParsedRule26(ParsedRule11 /*src*/, ParsedRule26& /*dest*/) {}
[[maybe_unused]]
static void mergePart6IntoParsedRule26(oalex::StringLoc /*src*/, ParsedRule26& /*dest*/) {}
[[maybe_unused]]
static void mergePart7IntoParsedRule26(ParsedRule13 /*src*/, ParsedRule26& /*dest*/) {}
[[maybe_unused]]
static void mergePart8IntoParsedRule26(ParsedRule14 src, ParsedRule26& dest) {
  dest.fields.external_name = std::move(src.fields.external_name);
}

[[maybe_unused]]
static void mergePart9IntoParsedRule26(ParsedRule15 /*src*/, ParsedRule26& /*dest*/) {}
[[maybe_unused]]
static void mergePart10IntoParsedRule26(oalex::StringLoc /*src*/, ParsedRule26& /*dest*/) {}
[[maybe_unused]]
static void mergePart11IntoParsedRule26(ParsedRule17 /*src*/, ParsedRule26& /*dest*/) {}
[[maybe_unused]]
static void mergePart12IntoParsedRule26(ParsedRule23 src, ParsedRule26& dest) {
  dest.fields.param = std::move(src.fields.param);
}

[[maybe_unused]]
static void mergePart13IntoParsedRule26(ParsedRule24 /*src*/, ParsedRule26& /*dest*/) {}
[[maybe_unused]]
static void mergePart14IntoParsedRule26(oalex::StringLoc /*src*/, ParsedRule26& /*dest*/) {}
oalex::JsonLoc parseRule26(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = oalex::toJsonLoc(parseRule6(ctx, j));
  if(res.holdsErrorValue()) return res;

  res = parseRule7(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = oalex::toJsonLoc(parseRule8(ctx, j));
  if(res.holdsErrorValue()) return res;

  res = parseRule9(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule10(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule11(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = oalex::toJsonLoc(parseRule12(ctx, j));
  if(res.holdsErrorValue()) return res;

  res = parseRule13(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule14(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule15(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = oalex::toJsonLoc(parseRule16(ctx, j));
  if(res.holdsErrorValue()) return res;

  res = parseRule17(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule23(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule24(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = oalex::toJsonLoc(parseRule25(ctx, j));
  if(res.holdsErrorValue()) return res;
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

std::optional<oalex::StringLoc> parseRule27(oalex::InputDiags& ctx, ssize_t& i) {
  return parseWord(ctx, i);
}

std::optional<oalex::StringLoc> parseRule28(oalex::InputDiags& ctx, ssize_t& i) {
  return parseDoubleQuotedLiteral(ctx, i);
}

ParsedErrorStanzaLine::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
    {"error_msg", fields.error_msg},
    {"ident", fields.ident},
  }, loc.first, loc.second);
}

std::optional<ParsedErrorStanzaLine> parseErrorStanzaLine(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::assertNotNull;
  using oalex::JsonLoc;
  using oalex::moveEltOrEmpty;
  ssize_t oldi = i;
  JsonLoc outfields = parseRule34(ctx, i);
  if(outfields.holdsErrorValue()) return std::nullopt;
  auto* m = outfields.getIfMap();
  assertNotNull(m, __func__, "needs a map");
  ParsedErrorStanzaLine rv{
    .loc{oldi, i},
    .fields{
      .error_msg = moveEltOrEmpty(*m, "error_msg"),
      .ident = moveEltOrEmpty(*m, "ident"),
    },
  };
  return rv;
}

ParsedRule29::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"ident", JsonLoc{fields.ident}},
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

[[maybe_unused]]
static void mergePart0IntoParsedRule29(oalex::StringLoc src, ParsedRule29& dest) {
  dest.fields.ident = std::move(src);
}

oalex::JsonLoc parseRule29(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = oalex::toJsonLoc(parseRule27(ctx, j));
  if(res.holdsErrorValue()) return res;
  m.emplace_back("ident", std::move(res));
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

ParsedRule30::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

oalex::JsonLoc parseRule30(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .newlines = Skipper::Newlines::ignore_blank,
  };
  ssize_t j = skip->next(ctx.input(), i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

std::optional<oalex::StringLoc> parseRule31(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, ":");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected ':'");
  return res;
}

ParsedRule32::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

oalex::JsonLoc parseRule32(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .newlines = Skipper::Newlines::ignore_blank,
  };
  ssize_t j = skip->next(ctx.input(), i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

ParsedRule33::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"error_msg", JsonLoc{fields.error_msg}},
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

[[maybe_unused]]
static void mergePart0IntoParsedRule33(oalex::StringLoc src, ParsedRule33& dest) {
  dest.fields.error_msg = std::move(src);
}

oalex::JsonLoc parseRule33(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = oalex::toJsonLoc(parseRule28(ctx, j));
  if(res.holdsErrorValue()) return res;
  m.emplace_back("error_msg", std::move(res));
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

ParsedRule34::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"ident", JsonLoc{fields.ident}},
    {"error_msg", JsonLoc{fields.error_msg}},
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

[[maybe_unused]]
static void mergePart0IntoParsedRule34(ParsedRule29 src, ParsedRule34& dest) {
  dest.fields.ident = std::move(src.fields.ident);
}

[[maybe_unused]]
static void mergePart1IntoParsedRule34(ParsedRule30 /*src*/, ParsedRule34& /*dest*/) {}
[[maybe_unused]]
static void mergePart2IntoParsedRule34(oalex::StringLoc /*src*/, ParsedRule34& /*dest*/) {}
[[maybe_unused]]
static void mergePart3IntoParsedRule34(ParsedRule32 /*src*/, ParsedRule34& /*dest*/) {}
[[maybe_unused]]
static void mergePart4IntoParsedRule34(ParsedRule33 src, ParsedRule34& dest) {
  dest.fields.error_msg = std::move(src.fields.error_msg);
}

oalex::JsonLoc parseRule34(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseRule29(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule30(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = oalex::toJsonLoc(parseRule31(ctx, j));
  if(res.holdsErrorValue()) return res;

  res = parseRule32(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule33(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

ParsedErrorStanzaLeader::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
  }, loc.first, loc.second);
}

std::optional<ParsedErrorStanzaLeader> parseErrorStanzaLeader(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::assertNotNull;
  using oalex::JsonLoc;
  using oalex::moveEltOrEmpty;
  ssize_t oldi = i;
  JsonLoc outfields = parseRule42(ctx, i);
  if(outfields.holdsErrorValue()) return std::nullopt;
  ParsedErrorStanzaLeader rv{
    .loc{oldi, i},
    .fields{
    },
  };
  return rv;
}

std::optional<oalex::StringLoc> parseRule35(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, defaultRegexOpts().word, "errors");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected 'errors'");
  return res;
}

ParsedRule36::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

oalex::JsonLoc parseRule36(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .newlines = Skipper::Newlines::ignore_blank,
  };
  ssize_t j = skip->next(ctx.input(), i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

std::optional<oalex::StringLoc> parseRule37(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, defaultRegexOpts().word, "after");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected 'after'");
  return res;
}

ParsedRule38::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

oalex::JsonLoc parseRule38(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .newlines = Skipper::Newlines::ignore_blank,
  };
  ssize_t j = skip->next(ctx.input(), i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

std::optional<oalex::StringLoc> parseRule39(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, defaultRegexOpts().word, "failing");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected 'failing'");
  return res;
}

ParsedRule40::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

oalex::JsonLoc parseRule40(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .newlines = Skipper::Newlines::ignore_blank,
  };
  ssize_t j = skip->next(ctx.input(), i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

std::optional<oalex::StringLoc> parseRule41(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, ":");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected ':'");
  return res;
}

ParsedRule42::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
  };
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

[[maybe_unused]]
static void mergePart0IntoParsedRule42(oalex::StringLoc /*src*/, ParsedRule42& /*dest*/) {}
[[maybe_unused]]
static void mergePart1IntoParsedRule42(ParsedRule36 /*src*/, ParsedRule42& /*dest*/) {}
[[maybe_unused]]
static void mergePart2IntoParsedRule42(oalex::StringLoc /*src*/, ParsedRule42& /*dest*/) {}
[[maybe_unused]]
static void mergePart3IntoParsedRule42(ParsedRule38 /*src*/, ParsedRule42& /*dest*/) {}
[[maybe_unused]]
static void mergePart4IntoParsedRule42(oalex::StringLoc /*src*/, ParsedRule42& /*dest*/) {}
[[maybe_unused]]
static void mergePart5IntoParsedRule42(ParsedRule40 /*src*/, ParsedRule42& /*dest*/) {}
[[maybe_unused]]
static void mergePart6IntoParsedRule42(oalex::StringLoc /*src*/, ParsedRule42& /*dest*/) {}
oalex::JsonLoc parseRule42(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = oalex::toJsonLoc(parseRule35(ctx, j));
  if(res.holdsErrorValue()) return res;

  res = parseRule36(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = oalex::toJsonLoc(parseRule37(ctx, j));
  if(res.holdsErrorValue()) return res;

  res = parseRule38(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = oalex::toJsonLoc(parseRule39(ctx, j));
  if(res.holdsErrorValue()) return res;

  res = parseRule40(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = oalex::toJsonLoc(parseRule41(ctx, j));
  if(res.holdsErrorValue()) return res;
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

oalex::JsonLike parseErrorStanza(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::InputDiags;
  using oalex::JsonLike;
  using oalex::Parser;
  using oalex::ParserPtr;
  extern JsonLike oalexBuiltinIndentedList(InputDiags& ctx, ssize_t& i, const oalex::Parser&, const oalex::Parser&);
  const static Parser* errorStanzaLeaderWrapper = new ParserPtr(
    +[](InputDiags& ctx, ssize_t& i) {
      return JsonLike(parseErrorStanzaLeader(ctx, i));
    });
  const static Parser* errorStanzaLineWrapper = new ParserPtr(
    +[](InputDiags& ctx, ssize_t& i) {
      return JsonLike(parseErrorStanzaLine(ctx, i));
    });
  return oalexBuiltinIndentedList(ctx, i, *errorStanzaLeaderWrapper, *errorStanzaLineWrapper);
}

