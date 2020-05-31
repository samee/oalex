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

#include"input_view_auto_forget.h"
#include<random>
#include"runtime/test_util.h"
using namespace std;
using namespace oalex;

#define TestErr TestErrImpl().start(__FILE__,__LINE__)

namespace oalex {

bool someError=false;
struct TestErrImpl {
  ~TestErrImpl() { std::cerr<<std::endl; }
  std::ostream& start(const char* file,int line) {
    someError=true;
    return std::cerr<<file<<':'<<line<<": ";
  }
};

class input_buffer_test {
 public:
  static void valid(const input_buffer& buf) {
    size_t i;
    const auto& starts=buf.starts_;
    // .first must be sorted with no duplicates.
    for(i=1;i<starts.size();++i) {
      if(starts[i-1].first==starts[i].first)
        TestErr<<"Duplicate indices found at position "<<i-1<<": "
               <<starts[i].first;
      else if(starts[i-1].first>starts[i].first)
        TestErr<<"Unsorted indices at "<<i-1<<": "
               <<starts[i-1].first<<" > "<<starts[i].first;
    }
    if(buf.minnz_>starts.size())
      TestErr<<"minnz_ out of bounds: "<<buf.minnz_<<" > "<<starts.size();
    else {
      for(i=0;i<buf.minnz_;++i) if(starts[i].second!=0)
        TestErr<<"minnz is skipping over live elements at "<<i;
      for(i=buf.minnz_;i<starts.size();++i) {
        if(starts[i].first<buf.off_)
          TestErr<<"We are pointing to removed characters: "<<starts[i].first
                 <<" < "<<buf.off_;
      }
      if(buf.minnz_>starts.size()) TestErr<<"Strangely large minnz_";
      else if(buf.minnz_<starts.size() && starts[buf.minnz_].second==0)
        TestErr<<"minnz_ could have advanced farther up";
    }
  }
};

}  // namespace oalex

int main() {
  input_buffer buf;
  input_buffer_test::valid(buf);
  string ref;
  default_random_engine engine;
  engine.seed(42);
  constexpr int ivn=20;
  input_view iv[ivn];
  size_t st[ivn]={},len[ivn]={};
  bool used[ivn]={};
  int usedc=0;
  bernoulli_distribution hard_coin(.1);
  uniform_int_distribution<char> random_char('a','z');
  uniform_int_distribution<int> random_index(0,ivn-1),
    use_type(1,3);
  for(int i=0;i<1000;++i) {
    char ch=random_char(engine);
    buf.push_back(ch);
    ref.push_back(ch);
    for(int j=0;j<ivn;++j) if(hard_coin(engine)) {
      int ut=use_type(engine);
      if(!used[j]) {
        if(!usedc||ut==1) {
          len[j]=string::npos;
          st[j]=ref.size();
          iv[j]=grab_tail(buf);
        }else {
          int j2;
          do { j2=random_index(engine); } while(!used[j2]);
          len[j]=len[j2];
          st[j]=st[j2];
          if(ut==2) iv[j]=iv[j2];
          else {
            iv[j]=std::move(iv[j2]);
            used[j2]=false;
            usedc--;
          }
        }
        used[j]=true;
        usedc++;
      }else if(len[j]==string::npos) {
        len[j]=ref.size()-st[j];
        iv[j].stop_growing();
      }else if(len[j]>5) {
        len[j]-=5;
        iv[j].remove_suffix(5);
      }else {
        used[j]=false;
        usedc--;
        iv[j].reset();
      }
    }
    input_buffer_test::valid(buf);
    for(int j=0;j<ivn;++j) {
      if(!used[j]) continue;
      if(used[j]&&string_view(iv[j])!=ref.substr(st[j],len[j])) {
        TestErr<<"i = "<<i<<", "<<"j = "<<j;
        TestErr<<"Something is wrong: \""
          <<string_view(iv[j])<<"\" !=\" "<<ref.substr(st[j],len[j])<<'"';
      }
    }
  }
  return someError;
}
