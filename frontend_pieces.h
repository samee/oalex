#pragma once
#include <cstdint>
#include <oalex.h>

std::optional<oalex::StringLoc> parseWord(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseRule1(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseDoubleQuotedLiteral(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseRule2(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseRule3(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseRule4(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseRule5(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedExternRule {
  oalex::LocPair loc;
  struct Fields {
    oalex::JsonLoc external_name;
    oalex::JsonLoc param;
    oalex::JsonLoc rule_name;
  } fields;
  struct TypedFields {
    oalex::StringLoc external_name;
    std::vector<oalex::StringLoc> param;
    oalex::StringLoc rule_name;
  } typed_fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedExternRule> parseExternRule(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseRule6(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule7 {
  oalex::LocPair loc;
  struct Fields {
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule7> parseRule7(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseRule8(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule9 {
  oalex::LocPair loc;
  struct Fields {
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule9> parseRule9(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule10 {
  oalex::LocPair loc;
  struct Fields {
    oalex::StringLoc rule_name;
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule10> parseRule10(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule11 {
  oalex::LocPair loc;
  struct Fields {
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule11> parseRule11(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseRule12(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule13 {
  oalex::LocPair loc;
  struct Fields {
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule13> parseRule13(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule14 {
  oalex::LocPair loc;
  struct Fields {
    oalex::StringLoc external_name;
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule14> parseRule14(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule15 {
  oalex::LocPair loc;
  struct Fields {
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule15> parseRule15(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseRule16(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule17 {
  oalex::LocPair loc;
  struct Fields {
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule17> parseRule17(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule18 {
  oalex::LocPair loc;
  struct Fields {
    oalex::StringLoc param;
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule18> parseRule18(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseRule19(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule20 {
  oalex::LocPair loc;
  struct Fields {
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule20> parseRule20(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule21 {
  oalex::LocPair loc;
  struct Fields {
    std::vector<oalex::StringLoc> param;
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule21> parseRule21(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule22 {
  oalex::LocPair loc;
  struct Fields {
    std::vector<oalex::StringLoc> param;
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule21> parseRule22(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule23 {
  oalex::LocPair loc;
  struct Fields {
    std::vector<oalex::StringLoc> param;
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule23> parseRule23(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule24 {
  oalex::LocPair loc;
  struct Fields {
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule24> parseRule24(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseRule25(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule26 {
  oalex::LocPair loc;
  struct Fields {
    oalex::StringLoc rule_name;
    oalex::StringLoc external_name;
    std::vector<oalex::StringLoc> param;
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule26> parseRule26(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseRule27(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseRule28(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedErrorStanzaLine {
  oalex::LocPair loc;
  struct Fields {
    oalex::JsonLoc error_msg;
    oalex::JsonLoc ident;
  } fields;
  struct TypedFields {
    oalex::StringLoc error_msg;
    oalex::StringLoc ident;
  } typed_fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedErrorStanzaLine> parseErrorStanzaLine(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule29 {
  oalex::LocPair loc;
  struct Fields {
    oalex::StringLoc ident;
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule29> parseRule29(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule30 {
  oalex::LocPair loc;
  struct Fields {
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule30> parseRule30(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseRule31(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule32 {
  oalex::LocPair loc;
  struct Fields {
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule32> parseRule32(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule33 {
  oalex::LocPair loc;
  struct Fields {
    oalex::StringLoc error_msg;
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule33> parseRule33(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule34 {
  oalex::LocPair loc;
  struct Fields {
    oalex::StringLoc ident;
    oalex::StringLoc error_msg;
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule34> parseRule34(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedErrorStanzaLeader {
  oalex::LocPair loc;
  struct Fields {
  } fields;
  struct TypedFields {
  } typed_fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedErrorStanzaLeader> parseErrorStanzaLeader(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseRule35(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule36 {
  oalex::LocPair loc;
  struct Fields {
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule36> parseRule36(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseRule37(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule38 {
  oalex::LocPair loc;
  struct Fields {
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule38> parseRule38(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseRule39(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule40 {
  oalex::LocPair loc;
  struct Fields {
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule40> parseRule40(oalex::InputDiags& ctx, ssize_t& i);

std::optional<oalex::StringLoc> parseRule41(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedRule42 {
  oalex::LocPair loc;
  struct Fields {
  } fields;
  explicit operator oalex::JsonLoc() const;
};

std::optional<ParsedRule42> parseRule42(oalex::InputDiags& ctx, ssize_t& i);

extern oalex::JsonLike oalexBuiltinIndentedList(oalex::InputDiags& ctx, ssize_t& j, const oalex::Parser&, const oalex::Parser&);
oalex::JsonLike parseErrorStanza(oalex::InputDiags& ctx, ssize_t& i);

