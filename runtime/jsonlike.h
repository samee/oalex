/*  Copyright 2023 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

/* This header provides JsonLike, a type-erased version of JsonLoc. Any struct
   that can be converted into a JsonLoc can be assigned to a JsonLike object. It
   will remain copyable, convertible to JsonLoc, and can be dynamically cast
   back to the original type.

   Dev-note: The approach taken here is to wrap every struct in our own little
   class hierarchy, where the wrapper all derives from JsonLikeWrapper. This
   allows us to use any_of<JsonLikeWrapper> to store our object.

   An alternate approach would have been to directly use any_of<SomeInterface>.
   However, that means our oalex-generated structs will have to derive from
   that new interface, while we want to keep them aggregate types.

   We can also merge this into any_of instead of building on top of it. This
   would have reduced the number of indirections. We can revisit this if
   performance becomes an issue.
*/
#pragma once
#include <utility>
#include "any_of.h"
#include "jsonloc.h"

namespace oalex {

namespace internal {

class JsonLikeWrapper {
 public:
  virtual operator JsonLoc() const = 0;
  virtual const std::type_info& type() const = 0;
  virtual ~JsonLikeWrapper() {}
};

template <class V>
class JsonLikeWrapperSpecific : public JsonLikeWrapper {
  V value_;
 public:
  explicit JsonLikeWrapperSpecific(V value) : value_{std::move(value)} {}
  operator JsonLoc() const override { return JsonLoc{value_}; }
  V& value() { return value_; }
  const V& value() const { return value_; }
  const std::type_info& type() const override { return typeid(value_); }
};

}  // namespace internal

class JsonLike {
  template <class V> using wrapper = internal::JsonLikeWrapperSpecific<V>;
 public:
  JsonLike() {}
  template <class V> JsonLike(V value) : data_{wrapper<V>(std::move(value))} {}
  void reset() { data_.reset(); }

  // Forwarding methods to JsonLikeWrapper interface.
  operator JsonLoc() const {
    return data_ ? JsonLoc{*data_} : JsonLoc::ErrorValue{};
  }
  const std::type_info& type() const {
    return data_ ? data_->type() : typeid(JsonLoc::ErrorValue);
  }

  // downcast
  template <class V> V* try_cast() {
    auto* p = any_of_cast<wrapper<V>>(&data_);
    return p ? &p->value(): nullptr;
  }
  template <class V> const V* try_cast() const {
    auto* p = any_of_cast<wrapper<V>>(&data_);
    return p ? &p->value(): nullptr;
  }
 private:
  any_of<internal::JsonLikeWrapper> data_;
};

}  // namespace oalex
