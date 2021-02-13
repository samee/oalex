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

/*  The intent is that this file only needs to be implemented if you plan to
    catch any of the Bug() calls. This allows us to not include stdexcept in
    most files. Our util.h can thus keep BugEx undefined. It does still need to
    be linked everywhere. */

#pragma once
#include<stdexcept>

namespace oalex {

struct BugEx : std::logic_error {
  BugEx(const std::string& s) : std::logic_error(s) {}
};

struct UnimplementedEx : BugEx {
  UnimplementedEx(const std::string& msg) : BugEx(msg) {}
};

// TODO: Audit it's usage. We should be using Error() diags in most cases.
struct UserErrorEx : std::runtime_error {
  UserErrorEx(const std::string& s) : std::runtime_error(s) {}
};

}  // namespace oalex
