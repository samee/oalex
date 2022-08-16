/*
To test this code, first build oalex as described in the project readme. Then,

  SRC=$(realpath $PWD/../../)
  BUILD=$SRC/build

  # Generate simple_sum.{cpp,h}
  $BUILD/oalex build simple_sum.oalex

  # Compile this file
  g++ simple_sum.cpp simple_sum_test.cpp \
    -L $BUILD/runtime -l:liboalex.a -I $SRC/runtime \
    -L $BUILD/submodules/fmt -l:libfmt.a

  # Test it
  ./a.out
*/

#include "simple_sum.h"
#include <iostream>

int main() {
  oalex::InputDiags ctx{oalex::Input{"1+2"}};
  ssize_t pos = 0;
  oalex::JsonLoc jsloc = parseSimpleSum(ctx, pos);
  std::cout << jsloc.prettyPrint(0) << std::endl;
}
