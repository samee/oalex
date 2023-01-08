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

#include "jsonlike.h"
#include "util.h"

namespace oalex {

void internal::JsonLikeWrapper::assertNotError(const JsonLoc& jsloc) {
  if(jsloc.holdsErrorValue()) Bug("JsonLike ctors should disallow this.");
}

template <> JsonLike::JsonLike(JsonLoc jsloc) {
  if(!jsloc.holdsErrorValue()) data_ = wrapper<JsonLoc>(std::move(jsloc));
}

}  // namespace oalex
