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

#include "dfa.h"
#include "util.h"
using namespace oalex;
using namespace std;
// If I need to check BugDie later, I'll use std::set_terminate(). This can
// happen if I worry about accidentally disabling checks. But for now, I am not
// testing them since dying is never part of the API contract. They are all
// internal bugs.
#define BugMe BugDie()<<__func__<<": "

// Check cases:
//   * Shift from multiple
//   * Shift through a mix of StringEdge (st,mid,end) and CharRangeEdge
//   * Shift to erase a GssHead from mismatch.

namespace oalex::internal {

struct GlrCtxTest {
  using SegfaultOnHooks=GlrCtx::SegfaultOnHooks;
  static size_t headSize(const GlrCtx& ctx) { return ctx.heads_.size(); }
  static void resetHeads(GlrCtx& ctx) {
    ctx.heads_.assign(1,ctx.startingHeadAt(ctx.dfa_->stState));
  }
};

}  // namespace oalex


using oalex::internal::GlrCtxTest;

namespace {

void dieIfBad(const Dfa& dfa) {
  string checkRes=dfa.checkError();
  if(!checkRes.empty()) BugDie()<<checkRes;
}

class GetFromString {
  string_view src;
  size_t i=0;
 public:
  explicit GetFromString(string_view src):src(src) {}
  int operator()() { return i<src.size()?src[i++]:-1; }
};

template <class X>
string shared_typeid(const shared_ptr<X>& p)
  { return p?typeid(*p).name():"*nullptr"; }

string debug(const vector<string>& v) {
  if(v.empty()) return "{}";
  ostringstream os;
  os<<'{'<<v[0];
  for(size_t i=1;i<v.size();++i) os<<", "<<v[i];
  os<<'}';
  return os.str();
}

ostream& operator<<(ostream& os,const vector<string>& v) {
  return os<<debug(v);
}

namespace checkCheckError {

void expectError(const Dfa& dfa,string_view b) {
  const string a=dfa.checkError();
  if(a.find(b)==string::npos)
    BugDie()<<"Was expecting '"<<b<<"' got '"<<a<<'\'';
}

const Dfa dfaCanon {
  {{PushEdge{1}},{}},  // adjList
  {{},{}},             // labelsMap
  {0,1},               // statePrioMap
  0,                   // stState
  -1                   // enLabel, glrParse() not invoked.
};

void checksLabelsMapSize() {
  Dfa dfa=dfaCanon;
  dfa.labelsMap.clear();
  expectError(dfa,"adjList and labelsMap has different sizes.");
}

void checksStatePrioMapSize() {
  Dfa dfa=dfaCanon;
  dfa.statePrioMap.clear();
  expectError(dfa,"adjList and statePrioMap has different sizes.");
}

void checksEmptyStringEdge() {
  Dfa dfa=dfaCanon;
  dfa.adjList[1]={StringEdge{"",1}};
  expectError(dfa,"empty string edge");
}

void checksPushLoop() {
  Dfa dfa=dfaCanon;
  dfa.adjList[1]={PushEdge{1}};
  expectError(dfa,"PushEdge self-loop");
}

void checksPrefixFreeStrings() {
  Dfa dfa=dfaCanon;
  dfa.adjList[1]={StringEdge{"a",1},StringEdge{"ab",1}};
  expectError(dfa,"is a prefix of");
}

void checksDistinctLabels() {
  Dfa dfa=dfaCanon;
  dfa.adjList[1]={LabelEdge{DfaLabel{0},0},LabelEdge{DfaLabel{0},1}};
  expectError(dfa,"Duplicate labels");
}

void checksComponentSeparation() {
  Dfa dfa=dfaCanon;
  dfa.adjList[1]={LabelEdge{DfaLabel{0},0},StringEdge{"foo",0}};
  expectError(dfa,"Components mix");
}

void checksPushesFromTerminal() {
  Dfa dfa=dfaCanon;
  dfa.adjList[1]={StringEdge{"foo",1},PushEdge{0}};
  expectError(dfa,"terminal component has outgoing PushEdge");
}

void checksNonPushFromStart() {
  Dfa dfa=dfaCanon;
  dfa.adjList[0].push_back({LabelEdge{DfaLabel{1},1}});
  expectError(dfa,"Start state must have only PusheEdges");
}

void checksAmbiguousPrio() {
  Dfa dfa=dfaCanon;
  dfa.statePrioMap[0]=dfa.statePrioMap[1]=0;
  expectError(dfa,"statePrioMap has duplicates");
}

void test() {
  // Ordered roughly in the same order as code in checkError.
  checksLabelsMapSize();
  checksStatePrioMapSize();
  checksEmptyStringEdge();
  checksPushLoop();
  checksPrefixFreeStrings();
  checksDistinctLabels();
  checksComponentSeparation();
  checksPushesFromTerminal();
  checksNonPushFromStart();
  checksAmbiguousPrio();
}

}  // namespace checkCheckError.

namespace singleShifts {

const Dfa dfa{
  { // adjList
    {PushEdge{1},PushEdge{5}},
    {CharRangeEdge{'a','z',2},CharRangeEdge{'A','Z',3}},
    {StringEdge{"oo",4}},
    {},
    {},
    {LabelEdge{DfaLabel{0},DfaState{6}}},
    {}
  },
  {{},{},{},{},{DfaLabel{0}},{},{}},        // labelsMap
  {0,1,2,3,4,5,6},                          // statePrioMap
  0,                                        // stState
  -1,                                       // enLabel, glrParse() not invoked.
};

void test() {
  dieIfBad(dfa);
  oalex::internal::GlrCtx ctx(dfa,GlrCtxTest::SegfaultOnHooks());
  GlrCtxTest::resetHeads(ctx);
  ctx.shift('z');
  ctx.shift('o');
  ctx.shift('o');
  if(GlrCtxTest::headSize(ctx)!=1)
    BugDie()<<"Was expecting a single parse head. Got "
            <<GlrCtxTest::headSize(ctx);
}

}  // namespace singleShifts


namespace singleStringParse {

struct Hooks : public GssHooks {
  shared_value extend(DfaState  // fromState
                     ,const DfaEdge&  // withEdge
                     ,const shared_value&  // fromVal
                     ,const shared_value&   // withVal
                     ) override {
    BugMe<<"called unexpectedly";
  }
  shared_value use_value(DfaLabel,shared_value val) override {
    return val;
  }
  shared_value merge(DfaState  // en
                    ,shared_value  // v1
                    ,shared_value  // v2
                    ) override {
    BugMe<<"called unexpectedly";
  }
};

const Dfa dfa{
  { // adjList
    {PushEdge{1},PushEdge{3}},
    {StringEdge{"foo",2}},
    {},
    {LabelEdge{DfaLabel{0},DfaState{4}}},
    {}
  },
  {{},{},{DfaLabel{0}},{},{DfaLabel{1}}},    // labelsMap
  {0,1,2,3,4},                               // statePrioMap
  0,                                         // stState
  1,                                         // enLabel
};

void test() {
  dieIfBad(dfa);
  Hooks hooks;
  vector<shared_value> res=glrParse(dfa,hooks,GetFromString("foo"));
  if(res.size()!=1) BugMe<<"res.size == "<<res.size()<<" != 1";
  const StringVal& sv=dynamic_cast<const StringVal&>(*res[0]);
  if(sv.s!="foo") BugMe<<"Parsed '"<<sv.s<<"' != 'foo'";
}

}  // namespace singleStringParse


namespace listParse {

/*

Behold, a needlessly complicated way of doing [trim(p) | for p in split(s)].
Could be useful if we had quoted items with commas in them.

  List := "" | ListNe
  ListNe := item | ListNe "," item
  item := [a-z]+

But let's make it more interesting by allowing spaces:

  List := spaces | ListNe
  ListNe := spaces item spaces | ListNe "," spaces item spaces
  item := [a-z]+
  spaces := ' '*

Notice collapsed spaces. Overall, regular language, does not require a lot of
stack usage. Main part of the DFA:

  0 --lblIdent--> 1 --lblComma--> 2 --lblident--> 1

We can accept in states 0 or 1. But then we have loops for spaces from each:

  0 --lblSpace--> 0
  1 --lblSpace--> 1
  2 --lblSpace--> 2

And each of those have PushEdges to the terminal components:

  3 --' '--> 3             lblSpace
  4 --'a'--> 5 --'a'--> 5  lblIdent
  6 --','--> 7             lblComma
  0 --PushEdge--> 3,4
  1 --PushEdge--> 3,6
  2 --PushEdge--> 3,4


Finally, added state 8 since start state can only have PushEdges.
*/

const DfaLabel lblSpace{0};
const DfaLabel lblIdent{1};
const DfaLabel lblComma{2};
const DfaLabel lblList{3};
const DfaEdge startSpace=PushEdge{3};
const DfaEdge startIdent=PushEdge{4};
const DfaEdge startComma=PushEdge{6};

const Dfa dfa{
  { // adjList
    {LabelEdge{lblSpace,0},LabelEdge{lblIdent,1},startIdent},
    {LabelEdge{lblSpace,1},LabelEdge{lblComma,2},startSpace,startComma},
    {LabelEdge{lblSpace,2},LabelEdge{lblIdent,1},startSpace,startIdent},
    {StringEdge{" ",3}},
    {CharRangeEdge{'a','z',5}},
    {CharRangeEdge{'a','z',5}},
    {StringEdge{",",7}},
    {},
    {PushEdge{0},startSpace,startIdent},
  },
  // labelsMap
  {{lblList},{lblList},{},{lblSpace},{},{lblIdent},{},{lblComma},{lblList}},
  {0,1,2,3,4,5,6,7,8},                                           // statePrioMap
  8,                                                             // stState
  lblList
};

struct ConsListVal : SemVal {
  string ident;
  shared_ptr<const ConsListVal> prev;
  bool hasComma=false;
  ConsListVal(size_t st,size_t en,string ident)
    : SemVal(st,en), ident(std::move(ident)) {}
};

shared_ptr<ConsListVal> makeSingletonList(const StringVal& s) {
  auto l=make_shared<ConsListVal>(s.stPos,s.enPos,s.s);
  return l;
}

shared_ptr<ConsListVal> extendList(shared_ptr<const ConsListVal> list,
                                   StringVal& s) {
  if(!list->hasComma) return nullptr;
  auto rv=make_shared<ConsListVal>(list->stPos,s.enPos,s.s);
  rv->enPos=s.enPos;
  rv->prev=std::move(list);
  rv->hasComma=false;
  return rv;
}

struct Hooks : public GssHooks {
  shared_value extend(
      DfaState fromState,const DfaEdge& withEdge,
      const shared_value& fromVal,const shared_value& withVal
      ) override {
    const LabelEdge& le=get<LabelEdge>(withEdge);
    if(le.lbl==lblSpace) {
      if(fromState!=le.dest) BugMe<<"Spaces causing state change";
      else return fromVal;
    }else if(le.lbl==lblIdent) {
      if(fromState==DfaState{0})
        return makeSingletonList(dynamic_cast<const StringVal&>(*withVal));
      else if(fromState==DfaState{2}) {
        auto ident=dynamic_cast<const StringVal&>(*withVal);
        auto list=dynamic_pointer_cast<const ConsListVal>(fromVal);
        return extendList(list,ident);
      }else BugMe<<"Wasnt' expecting identifier edge out of state "
                 <<fromState.to_int;
    }else if(le.lbl==lblComma) {
      const StringVal& sv=dynamic_cast<const StringVal&>(*withVal);
      if(sv.s!=",") BugMe<<"Non comma string "<<sv.s;
      auto rv=make_shared<ConsListVal>(
          dynamic_cast<const ConsListVal&>(*fromVal));
      rv->hasComma=true;
      return rv;
    }else BugMe<<"Extending with unexpected label "<<le.lbl.to_int;
    BugMe<<"called unexpectedly";
  }
  DfaLabel onlyLabel(DfaState s) {
    vector<DfaLabel> l=dfa.labels(s);
    if(l.size()!=1) BugMe<<"Was expecting 1 label at state "<<s.to_int
                         <<" found "<<l.size();
    return l[0];
  }
  shared_value use_value(DfaLabel lbl,shared_value val) override {
    if(lbl==lblSpace)
      return make_shared<EmptyVal>(val->stPos,val->enPos);
    else if(lbl==lblIdent)
      return makeSingletonList(dynamic_cast<const StringVal&>(*val));
    else BugMe<<"Trying to use strange label "<<lbl.to_int;
  }
  shared_value merge(DfaState  // en
                    ,shared_value  // v1
                    ,shared_value  // v2
                    ) override {
    BugMe<<"called unexpectedly";
  }
};

vector<string> gather(const ConsListVal& val) {
  vector<string> rv;
  const ConsListVal* v=&val;
  while(v) {
    rv.push_back(v->ident);
    v=v->prev.get();
  }
  reverse(rv.begin(),rv.end());
  return rv;
}

void test() {
  dieIfBad(dfa);
  Hooks hooks;

  string inputs[]={"a, b ,c"," a,b,c ",""," "," a "};
  vector<string> outputs[]={{"a","b","c"},{"a","b","c"},{},{},{"a"}};
  constexpr size_t n=(sizeof(inputs)/sizeof(*inputs));
  static_assert(n==sizeof(outputs)/sizeof(*outputs));

  for(size_t i=0;i<n;++i) {
    vector<shared_value> res=glrParse(dfa,hooks,GetFromString(inputs[i]));
    if(res.size()!=1) BugMe<<"res.size == "<<res.size()<<" != 1";
    if(outputs[i].empty()&&dynamic_cast<const EmptyVal*>(res[0].get())!=nullptr)
      continue;
    const ConsListVal& clv=dynamic_cast<const ConsListVal&>(*res[0]);
    vector<string> v=gather(clv);
    if(v!=outputs[i]) BugMe<<"input["<<i<<"] parsed into "<<v;
  }

  string invalid_inputs[]={",,","a b","a, , b","FOO"};
  for(size_t i=0;i<sizeof(invalid_inputs)/sizeof(*invalid_inputs);++i) {
    vector<shared_value> res
      =glrParse(dfa,hooks,GetFromString(invalid_inputs[i]));
    if(!res.empty()) BugMe<<"res.size == "<<res.size()<<", was expecting empty";
  }
}

}  // namespace listParse

namespace slowListParse {
// TODO test ambiguous merges.
// First, the DFA is on paper
// Second, allow PushEdge duplication.
// Third, refactor code after testing merge.
// Fourth, better error API as in paper. Test it.
// Iterate with other future plans, and how they might get used.
/*
Exercises merge() by parsing the slow way. Here's our ambiguous grammar:

  List := "" | ListNe
  ListNe := item | ListNe "," ListNe
  item := [a-z]+

I won't worry about spaces here. Start with state 0:

0 --PushEdge--> 1 --'a'--> 2 --'a'--> 2  // lblList{1,2}
0 --PushEdge--> 3 --lblList1--> 4
                  --lblComma--> 5
                  --lblList2--> 6  // lblList{1,2}
5 --PushEdge--> 3
4 --PushEdge--> 7 --','--> 8 // lblComma

So for input "a,b,c" we can have two paths:
  0 (3 4 5 (3 4 5 6)6)
  0 (3 4 5 6)(3 4 5 6)

'(' means PushEdge, ')' means reduce with label.
Normally accept at state 6, and that's where we merge. But we can accept at 0
or 4 as well, if there is nothing at state 6.

*/

// Semantically equivalent labels for "identifier". Separate just to get around
// the stupid non-dupliate assumption in Dfa::checkError. The fact that you can
// get around it with duplicate labels shows exactly how stupid it is. FIXME.
const DfaLabel lblList{0};
const DfaLabel lblComma{2};
const DfaLabel lblEndMarker{3};
const DfaEdge startComma=PushEdge{7};
const DfaEdge startIdent=PushEdge{1};
const DfaEdge startList=PushEdge{3};

const Dfa dfa{
  { // adjList
    {startIdent,startList},
    {CharRangeEdge{'a','z',2}},
    {CharRangeEdge{'a','z',2}},
    {LabelEdge{lblList,4},startIdent},
    {LabelEdge{lblComma,5},startComma},
    {LabelEdge{lblList,6},startIdent,startList},
    {},
    {StringEdge{",",8}},
    {},
  },
  { // labelsMap
    {lblEndMarker},{},{lblList},{},{lblEndMarker},{},{lblList},{},{lblComma}
  },
  {0,1,2,6,4,5,3,7,8},  // statePrioMap
  0,                    // stState
  lblEndMarker,
};

string posRange(const SemVal& v) { return Str()<<v.stPos<<'-'<<v.enPos; }

// We could have probably used two subclasses or a variant type,
// but I'm feeling lazy.
class ConcatListVal : public SemVal {
 public:
  size_t size;  // Element count, not input string span.

  // ListNe := ListNe "," ListNe
  shared_ptr<const ConcatListVal> l1;
  shared_ptr<const ConcatListVal> l2;

  // ListNe := ident
  optional<string> ident;

  // Singleton, never changes.
  explicit ConcatListVal(const StringVal& s)
    : SemVal(s.stPos,s.enPos), size(1), ident(std::move(s.s)) {}

  // Concat operator, with first argument. Expects a second argument later.
  explicit ConcatListVal(shared_ptr<const ConcatListVal> l1)
    : SemVal(l1->stPos,l1->enPos), size(l1->size), l1(std::move(l1)) {}

  void recordComma(size_t newEnPos) {
    if(isSingleton()) BugMe<<"Can't record comma on singleton";
    if(hasComma) BugMe<<"Duplicate comma";
    hasComma=true;
    enPos=newEnPos;
  }
  void recordSecondList(shared_ptr<const ConcatListVal> l2) {
    if(isSingleton()) BugMe<<"Can't add list onto singleton";
    if(!hasComma)
      BugMe<<"Missing comma separator. Extending "
           <<posRange(*this)<<" with "<<posRange(*l2);
    if(this->l2) BugMe<<"Already have second list";
    this->size+=l2->size;
    enPos=l2->enPos;
    this->l2=std::move(l2);
  }
  bool concatDone() const { return l1&&hasComma&&l2; }
  bool isSingleton() const { return bool(ident); }
  bool waitingForComma() const { return !hasComma&&l1&&!l2&&!ident; }

 private:
  bool hasComma=false;
};

struct Hooks : public GssHooks {
  shared_value extend(
      DfaState fromState,const DfaEdge& withEdge,
      const shared_value& fromVal,const shared_value& withVal
      ) override {
    auto clvCopy=make_shared<ConcatListVal>(
        dynamic_cast<const ConcatListVal&>(*fromVal));

    if(fromState==DfaState{4}&&withEdge==LabelEdge{lblComma,5}) {
      const string& s=dynamic_cast<const StringVal&>(*withVal).s;
      if(s!=",") BugMe<<"Weird comma "<<s;
      if(!clvCopy->waitingForComma())
        clvCopy=make_shared<ConcatListVal>(clvCopy);
      clvCopy->recordComma(withVal->enPos);
    }else if(fromState==DfaState{5}&&withEdge==LabelEdge{lblList,6}) {
      if(auto clv2=dynamic_pointer_cast<const ConcatListVal>(withVal))
        clvCopy->recordSecondList(std::move(clv2));
      else if(auto sv=dynamic_cast<const StringVal*>(withVal.get()))
        clvCopy->recordSecondList(make_shared<ConcatListVal>(*sv));
      else BugMe<<"Weird sublist with typeid "<<shared_typeid(withVal);
    }else BugMe<<"Unexpected transition from DfaState{"<<fromState.to_int
               <<"} along edge "<<edgeDebug(withEdge);
    return clvCopy;
  }
  shared_value use_value(DfaLabel lbl,shared_value val) override {
    // This will happen only on DfaState{3}-->DfaState{4}.
    if(lbl==lblList) {
      if(auto sv=dynamic_cast<const StringVal*>(val.get()))
        return make_shared<ConcatListVal>(*sv);
      else if(auto clv=dynamic_pointer_cast<const ConcatListVal>(val))
        return clv;
      else BugMe<<"Cannot use list with unknown typeid "<<shared_typeid(val);
    }
    else BugMe<<"Trying to use strange label "<<lbl.to_int;
  }
  static int8_t lexiSizeCmp(const ConcatListVal& clv1,
                            const ConcatListVal& clv2){
    if(clv1.size!=clv2.size) BugMe<<"Comparing incompatble concat lists";
    if(clv1.isSingleton()) return 0;
    size_t s1=clv1.l1->size;
    size_t s2=clv2.l1->size;
    if(s1!=s2) return (s1<s2?-1:1);
    else if(int8_t c=lexiSizeCmp(*clv1.l1,*clv2.l1)) return c;
    else return lexiSizeCmp(*clv1.l2,*clv2.l2);
  }
  shared_value merge(DfaState en,shared_value v1,shared_value v2) override {
    if(auto sp=dynamic_cast<const StringVal*>(v1.get())) {
      if(sp->s!=dynamic_cast<const StringVal&>(*v2).s)
        BugMe<<"Unequal strings";
      return v1;
    }
    auto clv1=dynamic_cast<const ConcatListVal&>(*v1);
    auto clv2=dynamic_cast<const ConcatListVal&>(*v2);
    if(clv1.isSingleton()!=clv2.isSingleton())
      BugMe<<"One is singleton while the other isn't";
    if(en==DfaState{6}) {
      checkSingleton(clv1,"v1 going to DfaState{6}");
      checkSingleton(clv2,"v2 going to DfaState{6}");
    }else if(en==DfaState{4}) { if(clv1.isSingleton()) return v1; }
    else BugMe<<"Unexpected merge in state "<<en.to_int;
    return std::move(lexiSizeCmp(clv1,clv2)<0?v1:v2);
  }

  static void checkSingleton(const ConcatListVal& clv,string_view msg) {
    if(clv.isSingleton())
        BugMe<<"We have an ambiguous singleton. "<<msg
             <<" Value: "<<*clv.ident
             <<", Range: "<<posRange(clv);
  }

};

vector<string> gather(shared_value v) {
  if(dynamic_cast<const EmptyVal*>(v.get())) return {};
  auto clv=dynamic_cast<const ConcatListVal*>(v.get());
  vector<string> rv;
  while(!clv->isSingleton()) {
    if(!clv->l1->isSingleton())
      BugMe<<"l1 should be single after merge. Got size "<<clv->l1->size
           <<"+"<<clv->l2->size;
    rv.push_back(*clv->l1->ident);
    clv=clv->l2.get();
  }
  rv.push_back(*clv->ident);
  return rv;
}

// TODO dedup test() harness.
void test() {
  dieIfBad(dfa);
  Hooks hooks;

  string inputs[]={"a,b,c","foo,bar","","a,b,c,d"};
  vector<string> outputs[]={{"a","b","c"},{"foo","bar"},{},{"a","b","c","d"}};
  constexpr size_t n=(sizeof(inputs)/sizeof(*inputs));
  static_assert(n==sizeof(outputs)/sizeof(*outputs));

  for(size_t i=3;i<n;++i) {
    vector<shared_value> res=glrParse(dfa,hooks,GetFromString(inputs[i]));
    if(res.size()!=1) BugMe<<"res.size == "<<res.size()<<" != 1";
    if(res.empty()) BugMe<<"No valid parse on input["<<i<<']';
    vector<string> resg=gather(res[0]);
    if(resg!=outputs[i])
      BugMe<<"input["<<i<<"] parsed into "<<resg;
  }

  string invalid_inputs[]={",,","a b","a, , b","FOO"};
  for(size_t i=0;i<sizeof(invalid_inputs)/sizeof(*invalid_inputs);++i) {
    vector<shared_value> res
      =glrParse(dfa,hooks,GetFromString(invalid_inputs[i]));
    if(!res.empty()) BugMe<<"res.size == "<<res.size()<<", was expecting empty";
  }
}

}  // namespace slowListParse

}  // namespace

int main() {
  checkCheckError::test();
  singleShifts::test();
  singleStringParse::test();
  listParse::test();
  slowListParse::test();
}
