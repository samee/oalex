#pragma once
#include <cstddef>
#include <cstdint>
#include <type_traits>

// TODO change dfa.cpp to use segment. dynamic_cast is too heavyweight.

// As of now (C++17) sum types are still a bit awkward in C++.
// dynamic_cast is designed for unbounded type families. Here's my attempt at
// lowering the overhead, which doesn't use a polymorphic base class. But
// unlike std::variant, it still uses a non-polymorphic base class to share some
// fields among various subtypes. Also unlike std::variant, we don't need a
// fixed size since these types will typically be heap-allocated, and referred
// through const pointers (often std::shared_ptr).


// Example definition of semantic values:
//
//   enum class MyTypeTags { BinOp = Segment::tagReservedLim+1, UnOp, Literal };
//
//   struct BinOpExpr : Segment {
//     static constexpr tag_type = MyTypeTags::BinOp;
//     (...other params here...)
//   };
//
//   (...repeat with UnOpExpr, LiteralExpr...)
//
// Now we can either use:
//
//   if (auto* b = segment_cast<BinOpExpr>(sv)) ...;
//
// Or just vanilla switch case:
//
//   switch(sv->tag) {
//     case MyTypeTags::BinOp:   ...;
//     case MyTypeTags::UnOp:    ...;
//     case MyTypeTags::Literal: ...;
//   }

struct Segment {
  size_t stPos,enPos;
  using tagint_t = uint16_t;
  tagint_t tag;
  static constexpr tagint_t lastReservedTag = 31;
};

template <class T>
const T* segment_cast(const Segment* sv) {
  static_assert(std::is_base_of_v<Segment,T>,
                "segment_cast called on non-Segment type");
  return sv->tag==T::type_tag ? static_cast<const T*>(sv) : nullptr;
}
