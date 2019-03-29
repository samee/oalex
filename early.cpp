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

/* A rough Early-like parser, so I know what the programming API should look
   like. Eventually, I'll want classes for: AstNode, grammar, invoking API,
   error-handling. */

#include<array>
#include<algorithm>
#include<functional>
#include<iostream>
#include<set>
#include<sstream>
#include<string>
#include<string_view>
#include<unordered_set>
#include<vector>
using namespace std;

struct GrammarRule {
  struct Term { string match; bool terminal; };
  string lhs;
  vector<Term> rhs;
};

using Grammar=vector<GrammarRule>;

string quote(string_view s) {
  ostringstream os;
  os<<'"';
  for(char c:s) if(c=='"') os<<"\\\""; else os<<c;
  os<<'"';
  return os.str();
}

void debug(const Grammar& g) {
  for(auto& r:g) {
    cout<<r.lhs<<" :=";
    if(r.rhs.empty()) { cout<<" \"\"\n"; continue; }
    for(const GrammarRule::Term& t:r.rhs) {
      if(t.terminal) cout<<" "<<quote(t.match);
      else cout<<' '<<t.match;
    }
    cout<<'\n';
  }
  cout<<flush;
}

void pairize(Grammar& g) {
  size_t i,j,n=g.size(),rc=0;
  for(i=0;i<n;++i) {
    if(g[i].rhs.size()<=2) continue;
    GrammarRule::Term cur=g[i].rhs[0];
    for(j=1;j<g[i].rhs.size()-1;++j) {
      GrammarRule r;
      r.lhs="$pr."+to_string(rc++);
      r.rhs={cur,g[i].rhs[j]};
      g.push_back(r);
      cur={r.lhs,false};
    }
    g[i].rhs={cur,g[i].rhs.back()};
  }
}

set<pair<string_view,size_t>> revDeps(const Grammar& g) {
  set<pair<string_view,size_t>> rv;
  for(size_t i=0;i<g.size();++i) for(const GrammarRule::Term& t:g[i].rhs)
    if(!t.terminal) rv.insert({string_view(t.match),i});
  return rv;
}

set<pair<string_view,size_t>> removeLoops(const Grammar& g,
    set<pair<string_view,size_t>> rd) {
  auto it=rd.begin();
  while(it!=rd.end())
    if(it->first==g[it->second].lhs) it=rd.erase(it);
    else ++it;
  return rd;
}

set<string_view> rdkeys(const set<pair<string_view,size_t>>& rd) {
  set<string_view> rv;
  for(const auto& [f,_]:rd) rv.insert(f);
  return rv;
}

// Allows me to write for(elt:WithFirst(s,"key")) { ... }
class WithFirst {
  using container=set<pair<string_view,size_t>>;
  using iter=container::const_iterator;
  const container* c_;
  string k_;
  static string incr(string s) { return s+string(1,'\0'); }
 public:
  WithFirst(const set<pair<string_view,size_t>>& c,string_view k)
    : c_(&c), k_(k) {}
  iter begin() const { return c_->lower_bound(make_pair(k_,0)); }
  iter end() const { return c_->lower_bound(make_pair(incr(k_),0)); }
};

// Same as boost::hash_combine.
template <class T>
inline void hash_combine(std::size_t& seed, const T& v) {
    std::hash<T> hasher;
    seed ^= hasher(v) + 0x9e3779b9 + (seed<<6) + (seed>>2);
}

struct SubMatch { size_t st,en; string_view rule; };
size_t smhash(const SubMatch& sm) {
  size_t s=0;
  hash_combine(s,sm.st); hash_combine(s,sm.en);
  hash_combine(s,sm.rule);
  return s;
}

bool operator==(const SubMatch& a, const SubMatch& b) {
  return a.st==b.st&&a.en==b.en&&a.rule==b.rule;
}

struct EarlyContext {
  const Grammar* g;
  string input;
  unordered_set<SubMatch,decltype(smhash)*> subMatches;
  set<pair<string_view,size_t>> rd;
  EarlyContext(string s,const Grammar* g) :
    g(g), input(std::move(s)), subMatches(100,&smhash),
    rd(removeLoops(*g,revDeps(*g))) {}
};

bool singleTermMatches(EarlyContext& ctx,const GrammarRule::Term& t,
    size_t st,size_t en) {
  if(t.terminal) return ctx.input.substr(st,en-st)==t.match;
  else return ctx.subMatches.count({st,en,t.match});
}

void tryMatchAndPropagate(EarlyContext& ctx,size_t a,size_t st,size_t en) {
  const auto& rhs=ctx.g->at(a).rhs;
  string_view lhs=ctx.g->at(a).lhs;
  if(rhs.size()==0) return;
  else if(rhs.size()==1) {
    if(!singleTermMatches(ctx,rhs[0],st,en)) return;
  }else if(rhs.size()==2) {
    size_t i;
    for(i=st;i<=en;++i)
      if(singleTermMatches(ctx,rhs[0],st,i)&&
         singleTermMatches(ctx,rhs[1],i,en)) break;
    if(i>en) return;
  }else throw invalid_argument("Grammar not pairized");
  // Record a match.
  if(!ctx.subMatches.insert({st,en,lhs}).second) return;
  // Propagate if it's new.
  for(const auto& [_,b]:WithFirst(ctx.rd,lhs))
    tryMatchAndPropagate(ctx,b,st,en);
}

// Assumes grammar has gone through pairize().
bool checkValid(string input_param, const Grammar& g) {
  if(g.empty()) return false;
  EarlyContext ctx(std::move(input_param),&g);
  ssize_t i,j,a;
  for(j=0;j<=ctx.input.size();++j) for(i=j;i>=0;--i)
    for(a=0;a<g.size();++a) tryMatchAndPropagate(ctx,a,i,j);
  return ctx.subMatches.count(SubMatch{0,ctx.input.size(),g[0].lhs});
}

void debugRevs(const Grammar& g) {
  const auto& rd=removeLoops(g,revDeps(g));
  for(const string_view nt:rdkeys(rd)) {
    cout<<nt<<" -->";
    for(const auto& [_,ri]:WithFirst(rd,nt)) cout<<' '<<g[ri].lhs<<'-'<<ri;
    cout<<endl;
  }
}

int main() {
  Grammar g{
    GrammarRule{"expr",{{"term",false}}},
    GrammarRule{"expr",{{"term",false},{"+",true},{"expr",false}}},
    GrammarRule{"term",{{"ident",false},{"*",true},{"term",false}}},
    GrammarRule{"term",{{"ident",false}}},
    GrammarRule{"ident",{{"x",true}}},
  };
  debugRevs(g);
  cout<<endl;
  pairize(g);
  debug(g);
  cout<<checkValid("x+x",g)<<endl;
  cout<<checkValid("x+x*x",g)<<endl;
  cout<<checkValid("x+x*",g)<<endl;
}
