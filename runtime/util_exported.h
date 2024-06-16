#pragma once
#include <memory>
#include <optional>
#include <vector>
#include "any_of.h"

namespace oalex {

// Enables brace-initialization for variants without naming the type twice.
template <class T>
auto move_to_unique(T&& t) -> std::unique_ptr<std::remove_reference_t<T>> {
  return std::make_unique<std::remove_reference_t<T>>(std::move(t));
}

template <class V, class ... Args> std::vector<V>
makeVector(Args ... args) {
  std::vector<V> rv;
  (rv.push_back(std::move(args)), ...);
  return rv;
}

template <class T>
std::optional<T> to_std_optional(sz::any_of<T> opt) {
  return opt.has_value() ? std::optional{std::move(*opt)} : std::optional<T>{};
}

}  // namespace oalex
