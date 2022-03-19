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
    Error(ctx, i, "Expected word");
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

oalex::JsonLoc parseRule2(oalex::InputDiags& ctx, ssize_t& i) {
  return oalex::match(ctx, i, defaultRegexOpts().word, "extern");
}

oalex::JsonLoc parseRule3(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, defaultRegexOpts().word, "extern");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected 'extern'");
  return res;
}

oalex::JsonLoc parseRule4(oalex::InputDiags& ctx, ssize_t& i) {
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

oalex::JsonLoc parseRule5(oalex::InputDiags& ctx, ssize_t& i) {
  return oalex::match(ctx, i, defaultRegexOpts().word, "rule");
}

oalex::JsonLoc parseRule6(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, defaultRegexOpts().word, "rule");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected 'rule'");
  return res;
}

oalex::JsonLoc parseRule7(oalex::InputDiags& ctx, ssize_t& i) {
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

oalex::JsonLoc parseRule8(oalex::InputDiags& ctx, ssize_t& i) {
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

oalex::JsonLoc parseRule9(oalex::InputDiags& ctx, ssize_t& i) {
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

oalex::JsonLoc parseRule10(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, "=");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected '='");
  return res;
}

oalex::JsonLoc parseRule11(oalex::InputDiags& ctx, ssize_t& i) {
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

oalex::JsonLoc parseRule12(oalex::InputDiags& ctx, ssize_t& i) {
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

oalex::JsonLoc parseRule13(oalex::InputDiags& ctx, ssize_t& i) {
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

oalex::JsonLoc parseRule14(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, "(");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected '('");
  return res;
}

oalex::JsonLoc parseRule15(oalex::InputDiags& ctx, ssize_t& i) {
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

oalex::JsonLoc parseRule16(oalex::InputDiags& ctx, ssize_t& i) {
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

oalex::JsonLoc parseRule17(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, ",");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected ','");
  return res;
}

oalex::JsonLoc parseRule18(oalex::InputDiags& ctx, ssize_t& i) {
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

oalex::JsonLoc parseRule19(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  using oalex::mapCreateOrAppend;
  using oalex::mapCreateOrAppendAllElts;
  using oalex::quietMatch;
  ssize_t j = i, fallback_point = i;

  JsonLoc::Map m;
  bool first = true;
  while(true) {
    JsonLoc res = JsonLoc::ErrorValue{};

    res = parseRule16(ctx, j);
    if(res.holdsErrorValue()) return res;
    mapCreateOrAppendAllElts(m,
      std::move(*res.getIfMap()), first);
    fallback_point = j;

    res = quietMatch(ctx.input, j, parseRule18);
    if(res.holdsErrorValue()) break;
    res = quietMatch(ctx.input, j, parseRule17);
    if(res.holdsErrorValue()) break;
    res = parseRule18(ctx, j);
    if(res.holdsErrorValue())
      return oalex::errorValue(ctx, j, "Unfinished comment");
    first = false;
  }
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = fallback_point;
  i = fallback_point;
  return rv;
}

oalex::JsonLoc parseRule20(oalex::InputDiags& ctx, ssize_t& i) {
  return oalex::quietMatch(ctx.input, i, parseRule19);
}

oalex::JsonLoc parseRule21(oalex::InputDiags& ctx, ssize_t& i) {
  using std::literals::string_literals::operator""s;
  JsonLoc res{JsonLoc::ErrorValue{}};
  res = parseRule20(ctx, i);
  if(!res.holdsErrorValue())
    return JsonLoc::withPos(res, res.stPos, res.enPos);
  res = oalex::match(ctx, i, "");
  if(!res.holdsErrorValue())
    return JsonLoc::withPos(JsonLoc::Map{
    }, res.stPos, res.enPos);
  return res;
}

oalex::JsonLoc parseRule22(oalex::InputDiags& ctx, ssize_t& i) {
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

oalex::JsonLoc parseRule23(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, ")");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected ')'");
  return res;
}

oalex::JsonLoc parseRule24(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseRule14(ctx, j);
  if(res.holdsErrorValue()) return res;

  res = parseRule15(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule21(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule22(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule23(ctx, j);
  if(res.holdsErrorValue()) return res;
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

oalex::JsonLoc parseRule25(oalex::InputDiags& ctx, ssize_t& i) {
  return oalex::quietMatch(ctx.input, i, parseRule24);
}

oalex::JsonLoc parseRule26(oalex::InputDiags& ctx, ssize_t& i) {
  using std::literals::string_literals::operator""s;
  JsonLoc res{JsonLoc::ErrorValue{}};
  res = parseRule25(ctx, i);
  if(!res.holdsErrorValue())
    return JsonLoc::withPos(res, res.stPos, res.enPos);
  res = oalex::match(ctx, i, "");
  if(!res.holdsErrorValue())
    return JsonLoc::withPos(JsonLoc::Map{
    }, res.stPos, res.enPos);
  return res;
}

oalex::JsonLoc parseRule27(oalex::InputDiags& ctx, ssize_t& i) {
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

oalex::JsonLoc parseRule28(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::Error;
  JsonLoc  res = oalex::match(ctx, i, ";");
  if(res.holdsErrorValue())
    Error(ctx, i, "Expected ';'");
  return res;
}

oalex::JsonLoc parseRule29(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseRule3(ctx, j);
  if(res.holdsErrorValue()) return res;

  res = parseRule4(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule6(ctx, j);
  if(res.holdsErrorValue()) return res;

  res = parseRule7(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule8(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule9(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule10(ctx, j);
  if(res.holdsErrorValue()) return res;

  res = parseRule11(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule12(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule13(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule26(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule27(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));

  res = parseRule28(ctx, j);
  if(res.holdsErrorValue()) return res;
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

oalex::JsonLoc parseExternRule(oalex::InputDiags& ctx, ssize_t& i) {
  using oalex::JsonLoc;
  ssize_t j = i;

  JsonLoc::Map m;
  JsonLoc res = JsonLoc::ErrorValue{};

  res = parseRule29(ctx, j);
  if(res.holdsErrorValue()) return res;
  oalex::mapAppend(m, std::move(*res.getIfMap()));
  JsonLoc rv{std::move(m)};
  rv.stPos = i; rv.enPos = j;
  i = j;
  return rv;
}

