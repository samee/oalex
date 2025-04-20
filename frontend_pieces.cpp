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

std::optional<oalex::StringLoc> parseDoubleQuotedLiteral(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = parseRule2(ctx, i);
  if(holdsErrorValue(res))
    Error(ctx, i, "Does not match expected pattern");
  return res;
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

ParsedExternRule::operator JsonLoc() const {
  using oalex::toJsonLoc;
  auto rv = JsonLoc::withPos(JsonLoc::Map{
    {"external_name", toJsonLoc(fields.external_name)},
    {"param", toJsonLoc(fields.param)},
    {"rule_name", toJsonLoc(fields.rule_name)},
  }, loc.first, loc.second);

  return rv;
}

std::optional<ParsedExternRule> parseExternRule(oalex::InputDiags& ctx, ssize_t& i) {
  ssize_t oldi = i;
  std::optional<ParsedRule26> outfields = parseRule26(ctx, i);
  if(oalex::holdsErrorValue(outfields)) return std::nullopt;
  ParsedExternRule rv{
    .loc{oldi, i},
    .fields{
      .external_name = std::move(outfields->fields.external_name),
      .param = std::move(outfields->fields.param),
      .rule_name = std::move(outfields->fields.rule_name),
    },
  };
  return rv;
}

std::optional<oalex::StringLoc> parseRule6(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, defaultRegexOpts().word, "extern");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected \'extern\'");
  return res;
}

bool parseRule7(oalex::InputDiags& ctx, ssize_t& i) {
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
    return true;
  } else {
    Error(ctx, i, "Unfinished comment");
    return false;
  }
}

std::optional<oalex::StringLoc> parseRule8(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, defaultRegexOpts().word, "rule");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected \'rule\'");
  return res;
}

bool parseRule9(oalex::InputDiags& ctx, ssize_t& i) {
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
    return true;
  } else {
    Error(ctx, i, "Unfinished comment");
    return false;
  }
}

ParsedRule10::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"rule_name", JsonLoc{fields.rule_name}},
  };
  using oalex::holdsErrorValue;
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

static void mergePart0IntoParsedRule10(oalex::StringLoc src, ParsedRule10& dest) {
  dest.fields.rule_name = std::move(src);
}

std::optional<ParsedRule10> parseRule10(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::holdsErrorValue;
  ssize_t j = i;

  ParsedRule10 rv;

  std::optional<oalex::StringLoc> res0 = parseRule3(ctx, j);
  if(holdsErrorValue(res0)) return std::nullopt;
  mergePart0IntoParsedRule10(std::move(res0.value()), rv);
  rv.loc.first = i; rv.loc.second = j;
  i = j;
  return rv;
}

bool parseRule11(oalex::InputDiags& ctx, ssize_t& i) {
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
    return true;
  } else {
    Error(ctx, i, "Unfinished comment");
    return false;
  }
}

std::optional<oalex::StringLoc> parseRule12(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, "=");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected \'=\'");
  return res;
}

bool parseRule13(oalex::InputDiags& ctx, ssize_t& i) {
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
    return true;
  } else {
    Error(ctx, i, "Unfinished comment");
    return false;
  }
}

ParsedRule14::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"external_name", JsonLoc{fields.external_name}},
  };
  using oalex::holdsErrorValue;
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

static void mergePart0IntoParsedRule14(oalex::StringLoc src, ParsedRule14& dest) {
  dest.fields.external_name = std::move(src);
}

std::optional<ParsedRule14> parseRule14(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::holdsErrorValue;
  ssize_t j = i;

  ParsedRule14 rv;

  std::optional<oalex::StringLoc> res0 = parseRule4(ctx, j);
  if(holdsErrorValue(res0)) return std::nullopt;
  mergePart0IntoParsedRule14(std::move(res0.value()), rv);
  rv.loc.first = i; rv.loc.second = j;
  i = j;
  return rv;
}

bool parseRule15(oalex::InputDiags& ctx, ssize_t& i) {
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
    return true;
  } else {
    Error(ctx, i, "Unfinished comment");
    return false;
  }
}

std::optional<oalex::StringLoc> parseRule16(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, "(");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected \'(\'");
  return res;
}

bool parseRule17(oalex::InputDiags& ctx, ssize_t& i) {
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
    return true;
  } else {
    Error(ctx, i, "Unfinished comment");
    return false;
  }
}

ParsedRule18::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"param", JsonLoc{fields.param}},
  };
  using oalex::holdsErrorValue;
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

static void mergePart0IntoParsedRule18(oalex::StringLoc src, ParsedRule18& dest) {
  dest.fields.param = std::move(src);
}

std::optional<ParsedRule18> parseRule18(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::holdsErrorValue;
  ssize_t j = i;

  ParsedRule18 rv;

  std::optional<oalex::StringLoc> res0 = parseRule5(ctx, j);
  if(holdsErrorValue(res0)) return std::nullopt;
  mergePart0IntoParsedRule18(std::move(res0.value()), rv);
  rv.loc.first = i; rv.loc.second = j;
  i = j;
  return rv;
}

std::optional<oalex::StringLoc> parseRule19(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, ",");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected \',\'");
  return res;
}

bool parseRule20(oalex::InputDiags& ctx, ssize_t& i) {
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
    return true;
  } else {
    Error(ctx, i, "Unfinished comment");
    return false;
  }
}

ParsedRule21::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"param", oalex::toJsonLoc(fields.param)},
  };
  using oalex::holdsErrorValue;
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

static void mergeInitPartIntoParsedRule21(ParsedRule18 src, ParsedRule21& dest) {
  dest.fields.param.push_back(std::move(src.fields.param));
}

static void mergePart0IntoParsedRule21(bool /*src*/, ParsedRule21& /*dest*/) {}
static void mergePart1IntoParsedRule21(oalex::StringLoc /*src*/, ParsedRule21& /*dest*/) {}
static void mergePart2IntoParsedRule21(bool /*src*/, ParsedRule21& /*dest*/) {}
static void mergePart3IntoParsedRule21(ParsedRule18 src, ParsedRule21& dest) {
  dest.fields.param.push_back(std::move(src.fields.param));
}

std::optional<ParsedRule21> parseRule21(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::holdsErrorValue;
  using oalex::quietMatch;
  ssize_t j = i, fallback_point = i;

  ParsedRule21 rv;
  std::optional<ParsedRule18> init = parseRule18(ctx, j);
  if(holdsErrorValue(init)) return std::nullopt;
  mergeInitPartIntoParsedRule21(std::move(init.value()), rv);

  while(true) {
    fallback_point = j;

    bool res0 = quietMatch(ctx.input(), j, parseRule20);
    if(holdsErrorValue(res0)) break;
    mergePart0IntoParsedRule21(std::move(res0), rv);

    std::optional<oalex::StringLoc> res1 = quietMatch(ctx.input(), j, parseRule19);
    if(holdsErrorValue(res1)) break;
    mergePart1IntoParsedRule21(std::move(res1.value()), rv);

    bool res2 = parseRule20(ctx, j);
    if(holdsErrorValue(res2)) return std::nullopt;
    mergePart2IntoParsedRule21(std::move(res2), rv);

    std::optional<ParsedRule18> res3 = parseRule18(ctx, j);
    if(holdsErrorValue(res3)) return std::nullopt;
    mergePart3IntoParsedRule21(std::move(res3.value()), rv);
  }
  rv.loc.first = i; rv.loc.second = fallback_point;
  i = fallback_point;
  return rv;
}

ParsedRule22::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"param", oalex::toJsonLoc(fields.param)},
  };
  using oalex::holdsErrorValue;
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

std::optional<ParsedRule21> parseRule22(oalex::InputDiags& ctx, ssize_t& i) {
  return oalex::quietMatch(ctx.input(), i, parseRule21);
}

ParsedRule23::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"param", oalex::toJsonLoc(fields.param)},
  };
  using oalex::holdsErrorValue;
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

static ParsedRule23 convertBranch0IntoParsedRule23(ParsedRule21 src) {
  ParsedRule23 dest = {};
  dest.fields.param = std::move(src.fields.param);
  return dest;
}

static ParsedRule23 convertBranch1IntoParsedRule23(const oalex::StringLoc&) { return {}; }
std::optional<ParsedRule23> parseRule23(oalex::InputDiags& ctx, ssize_t& i) {
  using std::literals::string_literals::operator""s;
  using oalex::holdsErrorValue;
  std::optional<ParsedRule21> res0 = parseRule22(ctx, i);
  if(!holdsErrorValue(res0))
    return convertBranch0IntoParsedRule23(res0.value());
  std::optional<oalex::StringLoc> res1 = oalex::match(ctx, i, "");
  if(!holdsErrorValue(res1))
    return convertBranch1IntoParsedRule23(res1.value());
  return std::nullopt;
}

bool parseRule24(oalex::InputDiags& ctx, ssize_t& i) {
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
    return true;
  } else {
    Error(ctx, i, "Unfinished comment");
    return false;
  }
}

std::optional<oalex::StringLoc> parseRule25(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, ")");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected \')\'");
  return res;
}

ParsedRule26::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"rule_name", JsonLoc{fields.rule_name}},
    {"external_name", JsonLoc{fields.external_name}},
    {"param", oalex::toJsonLoc(fields.param)},
  };
  using oalex::holdsErrorValue;
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

static void mergePart0IntoParsedRule26(oalex::StringLoc /*src*/, ParsedRule26& /*dest*/) {}
static void mergePart1IntoParsedRule26(bool /*src*/, ParsedRule26& /*dest*/) {}
static void mergePart2IntoParsedRule26(oalex::StringLoc /*src*/, ParsedRule26& /*dest*/) {}
static void mergePart3IntoParsedRule26(bool /*src*/, ParsedRule26& /*dest*/) {}
static void mergePart4IntoParsedRule26(ParsedRule10 src, ParsedRule26& dest) {
  dest.fields.rule_name = std::move(src.fields.rule_name);
}

static void mergePart5IntoParsedRule26(bool /*src*/, ParsedRule26& /*dest*/) {}
static void mergePart6IntoParsedRule26(oalex::StringLoc /*src*/, ParsedRule26& /*dest*/) {}
static void mergePart7IntoParsedRule26(bool /*src*/, ParsedRule26& /*dest*/) {}
static void mergePart8IntoParsedRule26(ParsedRule14 src, ParsedRule26& dest) {
  dest.fields.external_name = std::move(src.fields.external_name);
}

static void mergePart9IntoParsedRule26(bool /*src*/, ParsedRule26& /*dest*/) {}
static void mergePart10IntoParsedRule26(oalex::StringLoc /*src*/, ParsedRule26& /*dest*/) {}
static void mergePart11IntoParsedRule26(bool /*src*/, ParsedRule26& /*dest*/) {}
static void mergePart12IntoParsedRule26(ParsedRule23 src, ParsedRule26& dest) {
  dest.fields.param = std::move(src.fields.param);
}

static void mergePart13IntoParsedRule26(bool /*src*/, ParsedRule26& /*dest*/) {}
static void mergePart14IntoParsedRule26(oalex::StringLoc /*src*/, ParsedRule26& /*dest*/) {}
std::optional<ParsedRule26> parseRule26(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::holdsErrorValue;
  ssize_t j = i;

  ParsedRule26 rv;

  std::optional<oalex::StringLoc> res0 = parseRule6(ctx, j);
  if(holdsErrorValue(res0)) return std::nullopt;
  mergePart0IntoParsedRule26(std::move(res0.value()), rv);

  bool res1 = parseRule7(ctx, j);
  if(holdsErrorValue(res1)) return std::nullopt;
  mergePart1IntoParsedRule26(std::move(res1), rv);

  std::optional<oalex::StringLoc> res2 = parseRule8(ctx, j);
  if(holdsErrorValue(res2)) return std::nullopt;
  mergePart2IntoParsedRule26(std::move(res2.value()), rv);

  bool res3 = parseRule9(ctx, j);
  if(holdsErrorValue(res3)) return std::nullopt;
  mergePart3IntoParsedRule26(std::move(res3), rv);

  std::optional<ParsedRule10> res4 = parseRule10(ctx, j);
  if(holdsErrorValue(res4)) return std::nullopt;
  mergePart4IntoParsedRule26(std::move(res4.value()), rv);

  bool res5 = parseRule11(ctx, j);
  if(holdsErrorValue(res5)) return std::nullopt;
  mergePart5IntoParsedRule26(std::move(res5), rv);

  std::optional<oalex::StringLoc> res6 = parseRule12(ctx, j);
  if(holdsErrorValue(res6)) return std::nullopt;
  mergePart6IntoParsedRule26(std::move(res6.value()), rv);

  bool res7 = parseRule13(ctx, j);
  if(holdsErrorValue(res7)) return std::nullopt;
  mergePart7IntoParsedRule26(std::move(res7), rv);

  std::optional<ParsedRule14> res8 = parseRule14(ctx, j);
  if(holdsErrorValue(res8)) return std::nullopt;
  mergePart8IntoParsedRule26(std::move(res8.value()), rv);

  bool res9 = parseRule15(ctx, j);
  if(holdsErrorValue(res9)) return std::nullopt;
  mergePart9IntoParsedRule26(std::move(res9), rv);

  std::optional<oalex::StringLoc> res10 = parseRule16(ctx, j);
  if(holdsErrorValue(res10)) return std::nullopt;
  mergePart10IntoParsedRule26(std::move(res10.value()), rv);

  bool res11 = parseRule17(ctx, j);
  if(holdsErrorValue(res11)) return std::nullopt;
  mergePart11IntoParsedRule26(std::move(res11), rv);

  std::optional<ParsedRule23> res12 = parseRule23(ctx, j);
  if(holdsErrorValue(res12)) return std::nullopt;
  mergePart12IntoParsedRule26(std::move(res12.value()), rv);

  bool res13 = parseRule24(ctx, j);
  if(holdsErrorValue(res13)) return std::nullopt;
  mergePart13IntoParsedRule26(std::move(res13), rv);

  std::optional<oalex::StringLoc> res14 = parseRule25(ctx, j);
  if(holdsErrorValue(res14)) return std::nullopt;
  mergePart14IntoParsedRule26(std::move(res14.value()), rv);
  rv.loc.first = i; rv.loc.second = j;
  i = j;
  return rv;
}

std::optional<oalex::StringLoc> parseRule27(oalex::InputDiags& ctx, ssize_t& i) {
  return parseWord(ctx, i);
}

std::optional<oalex::StringLoc> parseRule28(oalex::InputDiags& ctx, ssize_t& i) {
  return parseDoubleQuotedLiteral(ctx, i);
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

ParsedErrorStanzaLine::operator JsonLoc() const {
  using oalex::toJsonLoc;
  auto rv = JsonLoc::withPos(JsonLoc::Map{
    {"error_msg", toJsonLoc(fields.error_msg)},
    {"ident", toJsonLoc(fields.ident)},
  }, loc.first, loc.second);

  return rv;
}

std::optional<ParsedErrorStanzaLine> parseErrorStanzaLine(oalex::InputDiags& ctx, ssize_t& i) {
  ssize_t oldi = i;
  std::optional<ParsedRule34> outfields = parseRule34(ctx, i);
  if(oalex::holdsErrorValue(outfields)) return std::nullopt;
  ParsedErrorStanzaLine rv{
    .loc{oldi, i},
    .fields{
      .error_msg = std::move(outfields->fields.error_msg),
      .ident = std::move(outfields->fields.ident),
    },
  };
  return rv;
}

ParsedRule29::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"ident", JsonLoc{fields.ident}},
  };
  using oalex::holdsErrorValue;
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

static void mergePart0IntoParsedRule29(oalex::StringLoc src, ParsedRule29& dest) {
  dest.fields.ident = std::move(src);
}

std::optional<ParsedRule29> parseRule29(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::holdsErrorValue;
  ssize_t j = i;

  ParsedRule29 rv;

  std::optional<oalex::StringLoc> res0 = parseRule27(ctx, j);
  if(holdsErrorValue(res0)) return std::nullopt;
  mergePart0IntoParsedRule29(std::move(res0.value()), rv);
  rv.loc.first = i; rv.loc.second = j;
  i = j;
  return rv;
}

bool parseRule30(oalex::InputDiags& ctx, ssize_t& i) {
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
    return true;
  } else {
    Error(ctx, i, "Unfinished comment");
    return false;
  }
}

std::optional<oalex::StringLoc> parseRule31(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, ":");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected \':\'");
  return res;
}

bool parseRule32(oalex::InputDiags& ctx, ssize_t& i) {
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
    return true;
  } else {
    Error(ctx, i, "Unfinished comment");
    return false;
  }
}

ParsedRule33::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"error_msg", JsonLoc{fields.error_msg}},
  };
  using oalex::holdsErrorValue;
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

static void mergePart0IntoParsedRule33(oalex::StringLoc src, ParsedRule33& dest) {
  dest.fields.error_msg = std::move(src);
}

std::optional<ParsedRule33> parseRule33(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::holdsErrorValue;
  ssize_t j = i;

  ParsedRule33 rv;

  std::optional<oalex::StringLoc> res0 = parseRule28(ctx, j);
  if(holdsErrorValue(res0)) return std::nullopt;
  mergePart0IntoParsedRule33(std::move(res0.value()), rv);
  rv.loc.first = i; rv.loc.second = j;
  i = j;
  return rv;
}

ParsedRule34::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
    {"ident", JsonLoc{fields.ident}},
    {"error_msg", JsonLoc{fields.error_msg}},
  };
  using oalex::holdsErrorValue;
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

static void mergePart0IntoParsedRule34(ParsedRule29 src, ParsedRule34& dest) {
  dest.fields.ident = std::move(src.fields.ident);
}

static void mergePart1IntoParsedRule34(bool /*src*/, ParsedRule34& /*dest*/) {}
static void mergePart2IntoParsedRule34(oalex::StringLoc /*src*/, ParsedRule34& /*dest*/) {}
static void mergePart3IntoParsedRule34(bool /*src*/, ParsedRule34& /*dest*/) {}
static void mergePart4IntoParsedRule34(ParsedRule33 src, ParsedRule34& dest) {
  dest.fields.error_msg = std::move(src.fields.error_msg);
}

std::optional<ParsedRule34> parseRule34(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::holdsErrorValue;
  ssize_t j = i;

  ParsedRule34 rv;

  std::optional<ParsedRule29> res0 = parseRule29(ctx, j);
  if(holdsErrorValue(res0)) return std::nullopt;
  mergePart0IntoParsedRule34(std::move(res0.value()), rv);

  bool res1 = parseRule30(ctx, j);
  if(holdsErrorValue(res1)) return std::nullopt;
  mergePart1IntoParsedRule34(std::move(res1), rv);

  std::optional<oalex::StringLoc> res2 = parseRule31(ctx, j);
  if(holdsErrorValue(res2)) return std::nullopt;
  mergePart2IntoParsedRule34(std::move(res2.value()), rv);

  bool res3 = parseRule32(ctx, j);
  if(holdsErrorValue(res3)) return std::nullopt;
  mergePart3IntoParsedRule34(std::move(res3), rv);

  std::optional<ParsedRule33> res4 = parseRule33(ctx, j);
  if(holdsErrorValue(res4)) return std::nullopt;
  mergePart4IntoParsedRule34(std::move(res4.value()), rv);
  rv.loc.first = i; rv.loc.second = j;
  i = j;
  return rv;
}

ParsedErrorStanzaLeader::operator JsonLoc() const {
  using oalex::toJsonLoc;
  auto rv = JsonLoc::withPos(JsonLoc::Map{
  }, loc.first, loc.second);

  return rv;
}

std::optional<ParsedErrorStanzaLeader> parseErrorStanzaLeader(oalex::InputDiags& ctx, ssize_t& i) {
  ssize_t oldi = i;
  std::optional<ParsedRule42> outfields = parseRule42(ctx, i);
  if(oalex::holdsErrorValue(outfields)) return std::nullopt;
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
    Error(ctx, i, "Expected \'errors\'");
  return res;
}

bool parseRule36(oalex::InputDiags& ctx, ssize_t& i) {
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
    return true;
  } else {
    Error(ctx, i, "Unfinished comment");
    return false;
  }
}

std::optional<oalex::StringLoc> parseRule37(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, defaultRegexOpts().word, "after");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected \'after\'");
  return res;
}

bool parseRule38(oalex::InputDiags& ctx, ssize_t& i) {
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
    return true;
  } else {
    Error(ctx, i, "Unfinished comment");
    return false;
  }
}

std::optional<oalex::StringLoc> parseRule39(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, defaultRegexOpts().word, "failing");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected \'failing\'");
  return res;
}

bool parseRule40(oalex::InputDiags& ctx, ssize_t& i) {
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
    return true;
  } else {
    Error(ctx, i, "Unfinished comment");
    return false;
  }
}

std::optional<oalex::StringLoc> parseRule41(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  using oalex::holdsErrorValue;
  std::optional<oalex::StringLoc> res = oalex::match(ctx, i, ":");
  if(holdsErrorValue(res))
    Error(ctx, i, "Expected \':\'");
  return res;
}

ParsedRule42::operator JsonLoc() const {
  auto rv = JsonLoc::Map{
  };
  using oalex::holdsErrorValue;
  return JsonLoc::withPos(rv, loc.first, loc.second);
}

static void mergePart0IntoParsedRule42(oalex::StringLoc /*src*/, ParsedRule42& /*dest*/) {}
static void mergePart1IntoParsedRule42(bool /*src*/, ParsedRule42& /*dest*/) {}
static void mergePart2IntoParsedRule42(oalex::StringLoc /*src*/, ParsedRule42& /*dest*/) {}
static void mergePart3IntoParsedRule42(bool /*src*/, ParsedRule42& /*dest*/) {}
static void mergePart4IntoParsedRule42(oalex::StringLoc /*src*/, ParsedRule42& /*dest*/) {}
static void mergePart5IntoParsedRule42(bool /*src*/, ParsedRule42& /*dest*/) {}
static void mergePart6IntoParsedRule42(oalex::StringLoc /*src*/, ParsedRule42& /*dest*/) {}
std::optional<ParsedRule42> parseRule42(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::holdsErrorValue;
  ssize_t j = i;

  ParsedRule42 rv;

  std::optional<oalex::StringLoc> res0 = parseRule35(ctx, j);
  if(holdsErrorValue(res0)) return std::nullopt;
  mergePart0IntoParsedRule42(std::move(res0.value()), rv);

  bool res1 = parseRule36(ctx, j);
  if(holdsErrorValue(res1)) return std::nullopt;
  mergePart1IntoParsedRule42(std::move(res1), rv);

  std::optional<oalex::StringLoc> res2 = parseRule37(ctx, j);
  if(holdsErrorValue(res2)) return std::nullopt;
  mergePart2IntoParsedRule42(std::move(res2.value()), rv);

  bool res3 = parseRule38(ctx, j);
  if(holdsErrorValue(res3)) return std::nullopt;
  mergePart3IntoParsedRule42(std::move(res3), rv);

  std::optional<oalex::StringLoc> res4 = parseRule39(ctx, j);
  if(holdsErrorValue(res4)) return std::nullopt;
  mergePart4IntoParsedRule42(std::move(res4.value()), rv);

  bool res5 = parseRule40(ctx, j);
  if(holdsErrorValue(res5)) return std::nullopt;
  mergePart5IntoParsedRule42(std::move(res5), rv);

  std::optional<oalex::StringLoc> res6 = parseRule41(ctx, j);
  if(holdsErrorValue(res6)) return std::nullopt;
  mergePart6IntoParsedRule42(std::move(res6.value()), rv);
  rv.loc.first = i; rv.loc.second = j;
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

