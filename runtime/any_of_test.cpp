/*  Copyright 2019-2024 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "runtime/any_of.h"
#include "runtime/test_util.h"
#include <string>
using oalex::assertEqual;
using oalex::Bug;
using std::string;
using sz::any_of;
using sz::any_of_cast;

class Greeter {
 public:
  virtual string show_msg() const = 0;
};

class HelloGreeter : public Greeter {
  int val;
 public:
  HelloGreeter(int x) : val(x) {}
  void setx(int x) { val = x; }
  string show_msg() const override { return fmt::format("hello {}", val); }
};

int main() {
  any_of<Greeter> g;
  g = HelloGreeter{5};
  assertEqual("typeid", g.type().name(), typeid(HelloGreeter).name());
  if(!any_of_cast<HelloGreeter>(&g)) {
    BugMe("Any cast failed");
  }
  if(!any_of_cast<HelloGreeter>(const_cast<const any_of<Greeter>*>(&g))) {
    BugMe("Const any cast failed");
  }
  assertEqual("Base method", g->show_msg(), "hello 5");
  if(!g) BugMe("operator bool");

  any_of<Greeter> g2 = g;
  assertEqual("copied", g2->show_msg(), "hello 5");
  any_of_cast<HelloGreeter>(&g2)->setx(6);
  assertEqual("original still intact", g->show_msg(), "hello 5");
  assertEqual("copy modified", g2->show_msg(), "hello 6");

  g2 = std::move(g);
  if(g) Bug("Not null even after move");
  assertEqual("value moved", g2->show_msg(), "hello 5");
}
