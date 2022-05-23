#include "frontend_pieces.h"
using oalex::InputDiags;
using oalex::JsonLoc;
using namespace std::string_literals;

const oalex::RegexOptions& defaultRegexOpts() {
  using oalex::RegexCharSet;
  using oalex::RegexOptions;
  static const RegexOptions *opts = new RegexOptions{.word =
  RegexCharSet({
      { '0', '9' },
      { 'A', 'Z' },
      { 'a', 'z' },
      { '_', '_' },
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
  using oalex::RegexOptions;
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
  using oalex::RegexOptions;
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

oalex::JsonLoc parseRule3(oalex::InputDiags& ctx, ssize_t& i) {
  return oalex::match(ctx, i, defaultRegexOpts().word, "extern");
}

oalex::JsonLoc parseRule4(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, defaultRegexOpts().word, "extern");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected 'extern'");
  return res;
}

oalex::JsonLoc parseRule5(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .indicateBlankLines = false,
  };
  ssize_t j = skip->acrossLines(ctx.input, i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

oalex::JsonLoc parseRule6(oalex::InputDiags& ctx, ssize_t& i) {
  return oalex::match(ctx, i, defaultRegexOpts().word, "rule");
}

oalex::JsonLoc parseRule7(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, defaultRegexOpts().word, "rule");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected 'rule'");
  return res;
}

oalex::JsonLoc parseRule8(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .indicateBlankLines = false,
  };
  ssize_t j = skip->acrossLines(ctx.input, i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

oalex::JsonLoc parseRule9(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseWord(ctx, j);
  if(res.holdsErrorValue()) return res;
  m.emplace_back("rule_name", std::move(res));
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

oalex::JsonLoc parseRule10(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .indicateBlankLines = false,
  };
  ssize_t j = skip->acrossLines(ctx.input, i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

oalex::JsonLoc parseRule11(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, "=");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected '='");
  return res;
}

oalex::JsonLoc parseRule12(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .indicateBlankLines = false,
  };
  ssize_t j = skip->acrossLines(ctx.input, i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

oalex::JsonLoc parseRule13(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseWord(ctx, j);
  if(res.holdsErrorValue()) return res;
  m.emplace_back("external_name", std::move(res));
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

oalex::JsonLoc parseRule14(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .indicateBlankLines = false,
  };
  ssize_t j = skip->acrossLines(ctx.input, i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

oalex::JsonLoc parseRule15(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, "(");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected '('");
  return res;
}

oalex::JsonLoc parseRule16(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .indicateBlankLines = false,
  };
  ssize_t j = skip->acrossLines(ctx.input, i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

oalex::JsonLoc parseRule17(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseWord(ctx, j);
  if(res.holdsErrorValue()) return res;
  m.emplace_back("param", std::move(res));
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

oalex::JsonLoc parseRule18(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, ",");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected ','");
  return res;
}

oalex::JsonLoc parseRule19(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .indicateBlankLines = false,
  };
  ssize_t j = skip->acrossLines(ctx.input, i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

oalex::JsonLoc parseRule20(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  using oalex::mapCreateOrAppend;
  using oalex::mapCreateOrAppendAllElts;
  using oalex::quietMatch;
  ssize_t j = i, fallback_point = i;

  JsonLoc::Map m;
  bool first = true;
  while(true) {
    JsonLoc res = JsonLoc::ErrorValue{};

    res = parseRule17(ctx, j);
    if(res.holdsErrorValue()) return res;
    mapCreateOrAppendAllElts(m,
      std::move(*res.getIfMap()), first);
    fallback_point = j;

    res = quietMatch(ctx.input, j, parseRule19);
    if(res.holdsErrorValue()) break;
    res = quietMatch(ctx.input, j, parseRule18);
    if(res.holdsErrorValue()) break;
    res = parseRule19(ctx, j);
    if(res.holdsErrorValue())
      return oalex::errorValue(ctx, j, "Unfinished comment");
    first = false;
  }
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = fallback_point;
  i = fallback_point;
  return rv;
}

oalex::JsonLoc parseRule21(oalex::InputDiags& ctx, ssize_t& i) {
  return oalex::quietMatch(ctx.input, i, parseRule20);
}

oalex::JsonLoc parseRule22(oalex::InputDiags& ctx, ssize_t& i) {
  using std::literals::string_literals::operator""s;
  JsonLoc res{JsonLoc::ErrorValue{}};
  res = parseRule21(ctx, i);
  if(!res.holdsErrorValue())
    return JsonLoc::withPos(res, res.stPos, res.enPos);
  res = oalex::match(ctx, i, "");
  if(!res.holdsErrorValue())
    return JsonLoc::withPos(JsonLoc::Map{
    }, res.stPos, res.enPos);
  return res;
}

oalex::JsonLoc parseRule23(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .indicateBlankLines = false,
  };
  ssize_t j = skip->acrossLines(ctx.input, i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

oalex::JsonLoc parseRule24(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, ")");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected ')'");
  return res;
}

oalex::JsonLoc parseRule25(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseRule15(ctx, j);
  if(res.holdsErrorValue()) return res;

  res = parseRule16(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule22(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule23(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule24(ctx, j);
  if(res.holdsErrorValue()) return res;
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

oalex::JsonLoc parseRule26(oalex::InputDiags& ctx, ssize_t& i) {
  return oalex::quietMatch(ctx.input, i, parseRule25);
}

oalex::JsonLoc parseRule27(oalex::InputDiags& ctx, ssize_t& i) {
  using std::literals::string_literals::operator""s;
  JsonLoc res{JsonLoc::ErrorValue{}};
  res = parseRule26(ctx, i);
  if(!res.holdsErrorValue())
    return JsonLoc::withPos(res, res.stPos, res.enPos);
  res = oalex::match(ctx, i, "");
  if(!res.holdsErrorValue())
    return JsonLoc::withPos(JsonLoc::Map{
    }, res.stPos, res.enPos);
  return res;
}

oalex::JsonLoc parseRule28(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .indicateBlankLines = false,
  };
  ssize_t j = skip->acrossLines(ctx.input, i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

oalex::JsonLoc parseRule29(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, ";");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected ';'");
  return res;
}

oalex::JsonLoc parseRule30(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseRule4(ctx, j);
  if(res.holdsErrorValue()) return res;

  res = parseRule5(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule7(ctx, j);
  if(res.holdsErrorValue()) return res;

  res = parseRule8(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule9(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule10(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule11(ctx, j);
  if(res.holdsErrorValue()) return res;

  res = parseRule12(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule13(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule14(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule27(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule28(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule29(ctx, j);
  if(res.holdsErrorValue()) return res;
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

oalex::JsonLoc parseExternRule(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::assertNotNull;
  using oalex::JsonLoc;
  using oalex::moveEltOrEmpty;
  ssize_t oldi = i;
  JsonLoc outfields = parseRule30(ctx, i);
  if(outfields.holdsErrorValue()) return outfields;
  auto* m = outfields.getIfMap();
  assertNotNull(m, __func__, "needs a map");
  JsonLoc rv = JsonLoc::Map{
    {"external_name", moveEltOrEmpty(*m, "external_name")},
    {"param", moveEltOrEmpty(*m, "param")},
    {"rule_name", moveEltOrEmpty(*m, "rule_name")},
  };
  rv.stPos = oldi; rv.enPos = i;
  return rv;
}

oalex::JsonLoc parseRule31(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseWord(ctx, j);
  if(res.holdsErrorValue()) return res;
  m.emplace_back("ident", std::move(res));
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

oalex::JsonLoc parseRule32(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .indicateBlankLines = false,
  };
  ssize_t j = skip->acrossLines(ctx.input, i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

oalex::JsonLoc parseRule33(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, ":");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected ':'");
  return res;
}

oalex::JsonLoc parseRule34(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .indicateBlankLines = false,
  };
  ssize_t j = skip->acrossLines(ctx.input, i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

oalex::JsonLoc parseRule35(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseDoubleQuotedLiteral(ctx, j);
  if(res.holdsErrorValue()) return res;
  m.emplace_back("error_msg", std::move(res));
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

oalex::JsonLoc parseRule36(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseRule31(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule32(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule33(ctx, j);
  if(res.holdsErrorValue()) return res;

  res = parseRule34(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule35(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

oalex::JsonLoc parseErrorStanzaLine(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::assertNotNull;
  using oalex::JsonLoc;
  using oalex::moveEltOrEmpty;
  ssize_t oldi = i;
  JsonLoc outfields = parseRule36(ctx, i);
  if(outfields.holdsErrorValue()) return outfields;
  auto* m = outfields.getIfMap();
  assertNotNull(m, __func__, "needs a map");
  JsonLoc rv = JsonLoc::Map{
    {"error_msg", moveEltOrEmpty(*m, "error_msg")},
    {"ident", moveEltOrEmpty(*m, "ident")},
  };
  rv.stPos = oldi; rv.enPos = i;
  return rv;
}

oalex::JsonLoc parseRule37(oalex::InputDiags& ctx, ssize_t& i) {
  return oalex::match(ctx, i, defaultRegexOpts().word, "errors");
}

oalex::JsonLoc parseRule38(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, defaultRegexOpts().word, "errors");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected 'errors'");
  return res;
}

oalex::JsonLoc parseRule39(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .indicateBlankLines = false,
  };
  ssize_t j = skip->acrossLines(ctx.input, i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

oalex::JsonLoc parseRule40(oalex::InputDiags& ctx, ssize_t& i) {
  return oalex::match(ctx, i, defaultRegexOpts().word, "after");
}

oalex::JsonLoc parseRule41(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, defaultRegexOpts().word, "after");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected 'after'");
  return res;
}

oalex::JsonLoc parseRule42(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .indicateBlankLines = false,
  };
  ssize_t j = skip->acrossLines(ctx.input, i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

oalex::JsonLoc parseRule43(oalex::InputDiags& ctx, ssize_t& i) {
  return oalex::match(ctx, i, defaultRegexOpts().word, "failing");
}

oalex::JsonLoc parseRule44(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, defaultRegexOpts().word, "failing");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected 'failing'");
  return res;
}

oalex::JsonLoc parseRule45(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Skipper;
  static Skipper* skip = new Skipper{
    .unnestedComments{
     { "#", "\n" },
    },
    .nestedComment{},
    .indicateBlankLines = false,
  };
  ssize_t j = skip->acrossLines(ctx.input, i);
  if (static_cast<size_t>(j) != oalex::Input::npos) {
    i = j;
    return oalex::JsonLoc::Map();  // dummy non-error value
  }else return oalex::JsonLoc::ErrorValue();
}

oalex::JsonLoc parseRule46(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, ":");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected ':'");
  return res;
}

oalex::JsonLoc parseRule47(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseRule38(ctx, j);
  if(res.holdsErrorValue()) return res;

  res = parseRule39(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule41(ctx, j);
  if(res.holdsErrorValue()) return res;

  res = parseRule42(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule44(ctx, j);
  if(res.holdsErrorValue()) return res;

  res = parseRule45(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule46(ctx, j);
  if(res.holdsErrorValue()) return res;
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

oalex::JsonLoc parseErrorStanzaLeader(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::assertNotNull;
  using oalex::JsonLoc;
  using oalex::moveEltOrEmpty;
  ssize_t oldi = i;
  JsonLoc outfields = parseRule47(ctx, i);
  if(outfields.holdsErrorValue()) return outfields;
  JsonLoc rv = JsonLoc::Map{
  };
  rv.stPos = oldi; rv.enPos = i;
  return rv;
}

oalex::JsonLoc parseErrorStanza(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::InputDiags;
  using oalex::JsonLoc;
  using oalex::Parser;
  using oalex::ParserPtr;
  extern JsonLoc oalexBuiltinIndentedList(InputDiags& ctx, ssize_t& i, const oalex::Parser&, const oalex::Parser&);
  const static Parser* errorStanzaLeaderWrapper = new ParserPtr(&parseErrorStanzaLeader);
  const static Parser* errorStanzaLineWrapper = new ParserPtr(&parseErrorStanzaLine);
  return oalexBuiltinIndentedList(ctx, i, *errorStanzaLeaderWrapper, *errorStanzaLineWrapper);
}

