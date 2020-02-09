#include "lookahead_regex.h"
#include "runtime/test_util.h"
using oalex::regex::CharRange;
using oalex::regex::CharSet;
using oalex::regex::Regex;
using std::string;

int main() {
  std::pair<Regex,string> testVectors[] = {
    {CharSet{{CharRange{'0','9'}}}, "[0-9]"},
    {CharSet{{CharRange{'A','Z'}}}, "[A-Z]"},
    {CharSet{{CharRange{'a','z'}}}, "[a-z]"},
    {CharSet{{CharRange{'c','c'}}}, "[c]"},
    {CharSet{{CharRange{'\n','\n'}}}, "[\\n]"},
    {CharSet{{CharRange{'\\','\\'}}}, "[\\\\]"},
    {CharSet{{CharRange{'/','/'}}}, "[\\/]"},
    {CharSet{{CharRange{'0','8'},
              CharRange{']',']'},
              CharRange{'A','Z'}}},"[0-8\\]A-Z]"},
    {CharSet{{CharRange{'^','^'},
              CharRange{'!','!'},
              CharRange{':',':'}}}, "[\\^!:]"},
    {CharSet{{CharRange{'!','!'},
              CharRange{'^','^'},
              CharRange{':',':'}}}, "[!^:]"},
    {CharSet{{CharRange{']',']'},
              CharRange{'x','x'},
              CharRange{'-','-'}}}, "[]x-]"},
    {CharSet{{CharRange{'x','x'},
              CharRange{'-','-'},
              CharRange{']',']'}}}, "[x\\-\\]]"},
    {CharSet{{CharRange{'a','a'},
              CharRange{'-','-'},
              CharRange{'z','z'}}}, "[a\\-z]"},
  };
  const size_t n = sizeof(testVectors)/sizeof(testVectors[0]);
  for(size_t i=0; i<n; ++i) {
    string observed = prettyPrint(testVectors[i].first);
    if(observed != testVectors[i].second)
      BugMe<<"Regex prettyPrint failed: "<<observed
           <<" != "<<testVectors[i].second;
  }
}
