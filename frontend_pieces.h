#pragma once
#include <cstdint>
#include <oalex.h>

oalex::JsonLoc parseWord(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule1(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseDoubleQuotedLiteral(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule2(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule3(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule4(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule5(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedExternRule {
  oalex::LocPair loc;
  struct Fields {
    oalex::JsonLoc external_name;
    oalex::JsonLoc param;
    oalex::JsonLoc rule_name;
  } fields;
  operator oalex::JsonLoc() const;
};

oalex::JsonLoc parseExternRule(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule6(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule7(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule8(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule9(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule10(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule11(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule12(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule13(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule14(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule15(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule16(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule17(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule18(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule19(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule20(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule21(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule22(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule23(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule24(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule25(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule26(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule27(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule28(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedErrorStanzaLine {
  oalex::LocPair loc;
  struct Fields {
    oalex::JsonLoc error_msg;
    oalex::JsonLoc ident;
  } fields;
  operator oalex::JsonLoc() const;
};

oalex::JsonLoc parseErrorStanzaLine(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule29(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule30(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule31(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule32(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule33(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule34(oalex::InputDiags& ctx, ssize_t& i);

struct ParsedErrorStanzaLeader {
  oalex::LocPair loc;
  struct Fields {
  } fields;
  operator oalex::JsonLoc() const;
};

oalex::JsonLoc parseErrorStanzaLeader(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule35(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule36(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule37(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule38(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule39(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule40(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule41(oalex::InputDiags& ctx, ssize_t& i);

oalex::JsonLoc parseRule42(oalex::InputDiags& ctx, ssize_t& i);

extern oalex::JsonLoc oalexBuiltinIndentedList(oalex::InputDiags& ctx, ssize_t& j, const oalex::Parser&, const oalex::Parser&);
oalex::JsonLoc parseErrorStanza(oalex::InputDiags& ctx, ssize_t& i);

