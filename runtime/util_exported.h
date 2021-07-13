#pragma once

namespace oalex {

// Enables brace-initialization for variants without naming the type twice.
template <class T>
auto move_to_unique(T& t) -> std::unique_ptr<T> {
  return std::make_unique<T>(std::move(t));
}

template <class T>
auto move_to_unique(T&& t) -> std::unique_ptr<T> {
  return std::make_unique<T>(std::move(t));
}

template <class V, class ... Args> std::vector<V>
makeVector(Args ... args) {
  std::vector<V> rv;
  (rv.push_back(std::move(args)), ...);
  return rv;
}

}  // namespace oalex
