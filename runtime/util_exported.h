#pragma once
#include <memory>
#include <vector>

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

}  // namespace oalex
