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

oalex::JsonLoc parseWord(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = parseRule1(ctx, i);
  if(res.holdsErrorValue())
    Error(ctx, i, "Does not match expected pattern");
  return res;
}

oalex::JsonLoc parseRule1(oalex::InputDiags& ctx, ssize_t& i) {
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
  return toJsonLoc(oalex::match(ctx, i, *r, defaultRegexOpts()));
}

oalex::JsonLoc parseDoubleQuotedLiteral(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = parseRule2(ctx, i);
  if(res.holdsErrorValue())
    Error(ctx, i, "Does not match expected pattern");
  return res;
}

oalex::JsonLoc parseRule2(oalex::InputDiags& ctx, ssize_t& i) {
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
  return toJsonLoc(oalex::match(ctx, i, *r, defaultRegexOpts()));
}

oalex::JsonLoc parseRule3(oalex::InputDiags& ctx, ssize_t& i) {
  return parseWord(ctx, i);
}

oalex::JsonLoc parseRule4(oalex::InputDiags& ctx, ssize_t& i) {
  return parseWord(ctx, i);
}

oalex::JsonLoc parseRule5(oalex::InputDiags& ctx, ssize_t& i) {
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

oalex::JsonLoc parseRule6(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::toJsonLoc(oalex::match(ctx, i, defaultRegexOpts().word, "extern"));
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected 'extern'");
  return res;
}

ParsedRule7::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
  }, loc.first, loc.second);
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

oalex::JsonLoc parseRule8(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::toJsonLoc(oalex::match(ctx, i, defaultRegexOpts().word, "rule"));
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected 'rule'");
  return res;
}

ParsedRule9::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
  }, loc.first, loc.second);
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
  return JsonLoc::withPos(JsonLoc::Map{
    {"rule_name", JsonLoc{fields.rule_name}},
  }, loc.first, loc.second);
}

oalex::JsonLoc parseRule10(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseRule3(ctx, j);
  if(res.holdsErrorValue()) return res;
  m.emplace_back("rule_name", std::move(res));
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

ParsedRule11::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
  }, loc.first, loc.second);
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

oalex::JsonLoc parseRule12(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::toJsonLoc(oalex::match(ctx, i, "="));
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected '='");
  return res;
}

ParsedRule13::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
  }, loc.first, loc.second);
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
  return JsonLoc::withPos(JsonLoc::Map{
    {"external_name", JsonLoc{fields.external_name}},
  }, loc.first, loc.second);
}

oalex::JsonLoc parseRule14(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseRule4(ctx, j);
  if(res.holdsErrorValue()) return res;
  m.emplace_back("external_name", std::move(res));
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

ParsedRule15::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
  }, loc.first, loc.second);
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

oalex::JsonLoc parseRule16(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::toJsonLoc(oalex::match(ctx, i, "("));
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected '('");
  return res;
}

ParsedRule17::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
  }, loc.first, loc.second);
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
  return JsonLoc::withPos(JsonLoc::Map{
    {"param", JsonLoc{fields.param}},
  }, loc.first, loc.second);
}

oalex::JsonLoc parseRule18(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseRule5(ctx, j);
  if(res.holdsErrorValue()) return res;
  m.emplace_back("param", std::move(res));
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

oalex::JsonLoc parseRule19(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::toJsonLoc(oalex::match(ctx, i, ","));
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected ','");
  return res;
}

ParsedRule20::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
  }, loc.first, loc.second);
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
  return JsonLoc::withPos(JsonLoc::Map{
    {"param", oalex::toJsonLoc(fields.param)},
  }, loc.first, loc.second);
}

oalex::JsonLoc parseRule21(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  using oalex::mapCreateOrAppend;
  using oalex::mapCreateOrAppendAllElts;
  using oalex::quietMatch;
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

    res = quietMatch(ctx.input(), j, parseRule20);
    if(res.holdsErrorValue()) break;
    res = quietMatch(ctx.input(), j, parseRule19);
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
  return JsonLoc::withPos(JsonLoc::Map{
    {"param", oalex::toJsonLoc(fields.param)},
  }, loc.first, loc.second);
}

oalex::JsonLoc parseRule22(oalex::InputDiags& ctx, ssize_t& i) {
  return oalex::quietMatch(ctx.input(), i, parseRule21);
}

ParsedRule23::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
    {"param", oalex::toJsonLoc(fields.param)},
  }, loc.first, loc.second);
}

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
  return JsonLoc::withPos(JsonLoc::Map{
  }, loc.first, loc.second);
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

oalex::JsonLoc parseRule25(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::toJsonLoc(oalex::match(ctx, i, ")"));
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected ')'");
  return res;
}

ParsedRule26::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
    {"rule_name", JsonLoc{fields.rule_name}},
    {"external_name", JsonLoc{fields.external_name}},
    {"param", oalex::toJsonLoc(fields.param)},
  }, loc.first, loc.second);
}

oalex::JsonLoc parseRule26(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseRule6(ctx, j);
  if(res.holdsErrorValue()) return res;

  res = parseRule7(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule8(ctx, j);
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

  res = parseRule12(ctx, j);
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

  res = parseRule16(ctx, j);
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

  res = parseRule25(ctx, j);
  if(res.holdsErrorValue()) return res;
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

oalex::JsonLoc parseRule27(oalex::InputDiags& ctx, ssize_t& i) {
  return parseWord(ctx, i);
}

oalex::JsonLoc parseRule28(oalex::InputDiags& ctx, ssize_t& i) {
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
  return JsonLoc::withPos(JsonLoc::Map{
    {"ident", JsonLoc{fields.ident}},
  }, loc.first, loc.second);
}

oalex::JsonLoc parseRule29(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseRule27(ctx, j);
  if(res.holdsErrorValue()) return res;
  m.emplace_back("ident", std::move(res));
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

ParsedRule30::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
  }, loc.first, loc.second);
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

oalex::JsonLoc parseRule31(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::toJsonLoc(oalex::match(ctx, i, ":"));
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected ':'");
  return res;
}

ParsedRule32::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
  }, loc.first, loc.second);
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
  return JsonLoc::withPos(JsonLoc::Map{
    {"error_msg", JsonLoc{fields.error_msg}},
  }, loc.first, loc.second);
}

oalex::JsonLoc parseRule33(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseRule28(ctx, j);
  if(res.holdsErrorValue()) return res;
  m.emplace_back("error_msg", std::move(res));
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

ParsedRule34::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
    {"ident", JsonLoc{fields.ident}},
    {"error_msg", JsonLoc{fields.error_msg}},
  }, loc.first, loc.second);
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

  res = parseRule31(ctx, j);
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

oalex::JsonLoc parseRule35(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::toJsonLoc(oalex::match(ctx, i, defaultRegexOpts().word, "errors"));
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected 'errors'");
  return res;
}

ParsedRule36::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
  }, loc.first, loc.second);
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

oalex::JsonLoc parseRule37(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::toJsonLoc(oalex::match(ctx, i, defaultRegexOpts().word, "after"));
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected 'after'");
  return res;
}

ParsedRule38::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
  }, loc.first, loc.second);
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

oalex::JsonLoc parseRule39(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::toJsonLoc(oalex::match(ctx, i, defaultRegexOpts().word, "failing"));
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected 'failing'");
  return res;
}

ParsedRule40::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
  }, loc.first, loc.second);
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

oalex::JsonLoc parseRule41(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::toJsonLoc(oalex::match(ctx, i, ":"));
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected ':'");
  return res;
}

ParsedRule42::operator JsonLoc() const {
  return JsonLoc::withPos(JsonLoc::Map{
  }, loc.first, loc.second);
}

oalex::JsonLoc parseRule42(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseRule35(ctx, j);
  if(res.holdsErrorValue()) return res;

  res = parseRule36(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule37(ctx, j);
  if(res.holdsErrorValue()) return res;

  res = parseRule38(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule39(ctx, j);
  if(res.holdsErrorValue()) return res;

  res = parseRule40(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule41(ctx, j);
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

