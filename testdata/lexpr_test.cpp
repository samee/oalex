// Compiled and manually tested with:
//
//     build/oalex build testdata/lexpr.cpp
//     g++ -I runtime testdata/lexpr_test.cpp testdata/lexpr.cpp \
//       -loalex -L build/runtime
#include "lexpr.h"
#include <string>
#include <iostream>
using namespace std;
using namespace oalex;

int main() {
  string line;
  while(getline(cin, line)) {
    InputDiags ctx{Input{line}};
    ssize_t pos=0;
    optional<ParsedLexpr> lexpr = parseLexpr(ctx, pos);
    if(lexpr) cout << JsonLoc(*lexpr).prettyPrint(2) << endl;
  }
}
