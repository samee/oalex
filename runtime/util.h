/*  Copyright 2019 Google LLC

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#pragma once
#include<iostream>
#include<memory>
#include<sstream>
#include<stdexcept>
#include"fmt/format.h"

namespace oalex {

// Usage:
//   if(weird) Bug() << "Tell me more about " << x;
//   if(weird) BugDie() << "Tell me more about " << x;
//   if(weird) BugWarn() << "Tell me more about " << x;
//   if(weird) UserError() << "Tell the user to behave";
//   Debug() << "More message";  // Never in checked-in code.
//
// TODO: adopt std::format() whenever it becomes available and stable.
struct BugEx : std::logic_error {
  BugEx(const std::string& s) : std::logic_error(s) {}
};

struct Bug {
  std::ostringstream os;
  [[noreturn]] ~Bug() noexcept(false) {
    throw BugEx(os.str());
  }
};
template <class X> std::ostream& operator<<(Bug&& b, const X& x) {
  return b.os << x;
}
template <class ... Args>
[[noreturn]] void BugFmt(const char* fmt, const Args& ... args) {
  throw BugEx(fmt::format(fmt, args...));
}

struct BugDie {
  [[noreturn]] ~BugDie() { std::cerr<<std::endl; std::abort(); }
};
template <class X> std::ostream& operator<<(BugDie&&, const X& x) {
  return std::cerr << "Bug: " << x;
}
template <class ... Args>
[[noreturn]] void BugDieFmt(const char* fmt, const Args& ... args) {
  fmt::print(stderr, fmt, args...);
  std::abort();
}

struct UnimplementedEx : BugEx {
  UnimplementedEx(const std::string& msg) : BugEx(msg) {}
};
struct Unimplemented {
  std::ostringstream os;
  [[noreturn]] ~Unimplemented() noexcept(false) {
    throw UnimplementedEx(os.str());
  }
};
template <class X> std::ostream& operator<<(Unimplemented&& b, const X& x) {
  return b.os << "Unimplemented: " << x;
}
template <class ... Args>
[[noreturn]] void UnimplementedFmt(const char* fmt, const Args& ... args) {
  throw UnimplementedEx(fmt::format(fmt, args...));
}


struct Debug {
  ~Debug () { std::cerr<<std::endl; }
};
template <class X> std::ostream& operator<<(Debug&&, const X& x) {
  return std::cerr << x;
}
template <class ... Args>
[[noreturn]] void DebugFmt(const char* fmt, const Args& ... args) {
  fmt::print(stderr, fmt, args...);
  fmt::print(stderr, "\n");
}



struct UserErrorEx : std::runtime_error {
  UserErrorEx(const std::string& s) : std::runtime_error(s) {}
};

struct UserError {
  std::ostringstream os;
  [[noreturn]] ~UserError() noexcept(false) { throw UserErrorEx(os.str()); }
};
template <class X> std::ostream& operator<<(UserError&& b, const X& x) {
  return b.os << x;
}
template <class ... Args>
[[noreturn]] void UserErrorFmt(const char* fmt, const Args& ... args) {
  throw UserErrorEx(fmt::format(fmt, args...));
}

struct BugWarn { ~BugWarn() { std::cerr<<std::endl; } };
template <class X> std::ostream& operator<<(BugWarn&&, const X& x) {
  return std::cerr <<"Bug: " << x;
}
template <class ... Args>
void BugWarnFmt(const char* fmt, const Args& ... args) {
  fmt::print(stderr, fmt::format("Bug: {}\n", fmt), args...);
}

// Usage: std::string s = (Str()<<"hello world "<<5);
struct Str {
  std::ostringstream os;
  operator std::string() const { return os.str(); }
  template <class X> Str& operator<<(const X& x)
    { os<<x; return *this; }
};

// Enables brace-initialization for variants without naming the type twice.
template <class T>
auto move_to_unique(T& t) -> std::unique_ptr<T> {
  return std::make_unique<T>(std::move(t));
}

template <class T>
auto move_to_unique(T&& t) -> std::unique_ptr<T> {
  return std::make_unique<T>(std::move(t));
}

}  // namespace oalex
