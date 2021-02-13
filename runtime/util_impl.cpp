/*  Copyright 2020 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "util_impl.h"
#include "fmt/core.h"

namespace oalex {

void BugImplHelper(const char* fmt, fmt::format_args args) {
  throw BugEx(fmt::vformat(fmt, args));
}

void UnimplementedImplHelper(const char* fmt, fmt::format_args args) {
  throw UnimplementedEx(fmt::vformat(fmt, args));
}

void UserErrorImplHelper(const char* fmt, fmt::format_args args) {
  throw UserErrorEx(fmt::vformat(fmt, args));
}

}  // namespace oalex
