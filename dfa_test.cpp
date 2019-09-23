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

vector<string> diagSetMessages(const set<const Diag*>& diagitems) {
  vector<string> msgs;
  for(const Diag* d:diagitems) msgs.push_back(d->msg);  // die if d==nullptr
  sort(msgs.begin(),msgs.end());
  return msgs;
}

vector<string> diagSetMessages(SharedDiagSet diags) {
  return diagSetMessages(diags->gather());
}

void testDiagSetGather() {
  vector<string> msg={"msg1","msg2","msg3"};

  vector<shared_ptr<const Diag>> diagitems;
  for(const string& m:msg)
    diagitems.push_back(make_shared<const Diag>(0,1,m));

  auto diagbeg=diagitems.begin();
  auto ds1=make_shared<const DiagSet>(diagbeg,diagbeg+2);
  auto ds2=make_shared<const DiagSet>(diagbeg+1,diagbeg+3);
  auto dsroot=concat(ds1,ds2);

  vector<string> msg_observed=diagSetMessages(dsroot->gather());
  if(msg!=msg_observed) {
    BugMe<<"DiagSet::Gather returned unexpected set: "<<debug(msg_observed)
         <<" != "<<debug(msg);
  }
}

void testNullDiagsIgnored() {
  vector<string> msg={"msg1","msg2","msg3"};

  vector<shared_ptr<const Diag>> diagitems;
  for(const string& m:msg)
    diagitems.push_back(make_shared<const Diag>(0,1,m));
  diagitems.push_back(nullptr);

  auto d=make_shared<DiagSet>(diagitems.begin(),diagitems.end());

  vector<string> msg_observed=diagSetMessages(d->gather());
  if(msg!=msg_observed) {
    BugMe<<"null diags caused problems: "<<debug(msg_observed)<<" != "
         <<debug(msg);
  }
}

void testDiagNullConcat() {
  if(auto nullcat=concat(SharedDiagSet(),SharedDiagSet())) {
    BugMe<<"Concat(nullptr,nullptr) returned non-null: "
         <<debug(diagSetMessages(nullcat));
  }
  vector<shared_ptr<const Diag>> diagitems={
    make_shared<const Diag>(0,1,"msg1"),
    make_shared<const Diag>(0,1,"msg2")};
  auto diags=make_shared<const DiagSet>(diagitems.begin(),diagitems.end());
  auto r1=concat(diags,nullptr);
  if(r1!=diags) {
    BugMe<<"Concat(diags,nullptr) produced "<<debug(diagSetMessages(r1))
         <<" != {msg1,msg2}";
  }
  auto r2=concat(nullptr,diags);
  if(r2!=diags) {
    BugMe<<"Concat(diags,nullptr) produced "<<debug(diagSetMessages(r2))
         <<" != {msg1,msg2}";
  }
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
  GssHooksRes reduceList(DfaLabel lbl,SharedListVal lv) override {
    if(lbl!=DfaLabel{0}) BugMe<<"Unexpected "<<lbl;
    else return lv->last;
  }
};

const Dfa dfa{
  { // adjList
    {PushEdge{1}},
    {StringEdge{"foo",2}},
    {},
  },
  {{},{},{DfaLabel{0}}},    // labelsMap
  {0,1,2},                  // statePrioMap
  0,                        // stState
  0,                        // enLabel
};

void test() {
  dieIfBad(dfa);
  Hooks hooks;
  SharedVal v=glrParseUnique(dfa,hooks,GetFromString("foo")).first;
  const StringVal& sv=dynamic_cast<const StringVal&>(*v);
  if(sv.s!="foo") BugMe<<"Parsed '"<<sv.s<<"' != 'foo'";
}

}  // namespace singleStringParse

namespace stringSequenceParse {
// Code started passing all tests even when I hardcoded stPos==0.
// That's why this test exists.

//  0 --PushEdge--> 1 --lbl0--> 2 --lbl0--> 3
//  0 --PushEdge--> 4 --"foo"--> 5
//  2 --PushEdge--> 6 --"bar"--> 7
//  0 --PushEdge--> 8 --lbl1--> 9
const Dfa dfa{
  { {PushEdge{1},PushEdge{4}},
    {LabelEdge{DfaLabel{0},DfaState{2}}},
    {LabelEdge{DfaLabel{0},DfaState{3}},PushEdge{6}},
    {},
    {StringEdge{"foo",5}},
    {},
    {StringEdge{"bar",7}},
    {},
  },  // adjList
  // labelsMap
  {{},{},{},{DfaLabel{1}},{},{DfaLabel{0}},{},{DfaLabel{0}}},
  {0,1,2,3,4,5,6,7},                // statePrioMap
  0,                                // stState
  1,                                // enLabel
};

struct Hooks : public GssHooks {
  GssHooksRes reduceList(DfaLabel lbl,SharedListVal lv) override {
    if(lbl==DfaLabel{1}) {
      if(lv->size!=2) BugMe<<"Expecting pair, got sequence size "<<lv->size;
      // Not a constant-time string-concatenation, but it's okay if this
      // grammar won't be ambiguous.
      auto &s1=dynamic_cast<const StringVal&>(*lv->at(0));
      auto &s2=dynamic_cast<const StringVal&>(*lv->at(1));
      return make_shared<StringVal>(lv->stPos,lv->enPos,s1.s+s2.s);
    }
    BugMe<<"Unexpected "<<lbl;
  }
};

void test() {
  dieIfBad(dfa);
  Hooks hooks;
  SharedVal v=glrParseUnique(dfa,hooks,GetFromString("foobar")).first;
  const StringVal& sv=dynamic_cast<const StringVal&>(*v);
  if(sv.s!="foobar") BugMe<<"Parsed '"<<sv.s<<"' != 'foobar'";
}

}  // namespace stringSequenceParse

namespace nullIgnored {

const Dfa dfa {
  { {PushEdge{1},PushEdge{3}},
    {LabelEdge{DfaLabel{0},DfaState{2}},LabelEdge{DfaLabel{1},DfaState{2}}},
    {},
    {StringEdge{"foo",4}},
    {},
  }, // adjList
  {{},{},{DfaLabel{2}},{},{DfaLabel{0},DfaLabel{1}}},  // labelsMap
  {0,1,2,3,4},  // statePrioMap
  0,            // stState
  2,            // enLabel
};

struct Hooks : public GssHooks {
  GssHooksRes reduceString(DfaLabel lbl,SharedStringVal sv) override {
    if(lbl==DfaLabel{0}) return abandonReduce(sv->stPos,sv->enPos,"no reason");
    else return sv;
  }
  GssHooksRes reduceList(DfaLabel lbl,SharedListVal lv) override {
    if(lbl!=DfaLabel{2}) BugMe<<"Unexpected "<<lbl;
    if(lv->size!=1)
      BugMe<<"Was expecting a single string 'foo'. Got size "<<lv->size;
    return lv->last;
  }
};

void test() {
  dieIfBad(dfa);
  Hooks hooks;
  SharedVal v=glrParseUnique(dfa,hooks,GetFromString("foo")).first;
  const StringVal& sv=dynamic_cast<const StringVal&>(*v);
  if(sv.s!="foo") BugMe<<"Parsed '"<<sv.s<<"' != 'foo'";
}

}  // namespace nullIgnored


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

struct Hooks : GssHooks {
  GssHooksRes reduceList(DfaLabel lbl,SharedListVal lv) override {
    if(lbl!=lblList) BugMe<<"Unexpected "<<lbl<<" != "<<lblList;
    return lv;
  }
  GssHooksRes reduceString(DfaLabel lbl,SharedStringVal sv) override {
    if(lbl==lblSpace||lbl==lblComma)
      return make_shared<EmptyVal>(sv->stPos,sv->enPos);
    else return GssHooks::reduceString(lbl,std::move(sv));
  }
};

vector<string> gather(const ListVal* lv) {
  vector<string> rv;
  while(lv) {
    if(auto *sv=dynamic_cast<const StringVal*>(lv->last.get()))
      rv.push_back(sv->s);
    else if(dynamic_cast<const EmptyVal*>(lv->last.get())==nullptr)
      BugMe<<"Got weird SemVal of typeid "<<typeid(*lv->last).name();
    lv=lv->prev.get();
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
    SharedVal v=glrParseUnique(dfa,hooks,GetFromString(inputs[i])).first;
    if(outputs[i].empty()&&
        dynamic_cast<const EmptyVal*>(v.get())!=nullptr)
      continue;
    vector<string> obs=gather(dynamic_cast<const ListVal*>(v.get()));
    if(obs!=outputs[i])
      BugMe<<"listParse mismatch "<<debug(obs)<<" != "<<debug(outputs[i]);
  }

  string invalid_inputs[]={",,","a b","a, , b","FOO"};
  for(size_t i=0;i<sizeof(invalid_inputs)/sizeof(*invalid_inputs);++i) {
    SharedVal v
      =glrParseUnique(dfa,hooks,GetFromString(invalid_inputs[i])).first;
    if(v!=nullptr) BugMe<<"Got "<<typeid(*v).name()<<" was expecting null";
  }
}

}  // namespace listParse

namespace slowListParse {
// TODO test ambiguous merges.
// First, the DFA is on paper (done)
// Second, allow PushEdge duplication. (done)
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
0 --PushEdge--> 3 --lblList--> 4
                  --lblComma--> 5
                  --lblList--> 6  // lblList{1,2}
5 --PushEdge--> 3
4 --PushEdge--> 7 --','--> 8 // lblComma

So for input "a,b,c" we can have two paths:
  0 (3 4 5 (3 4 5 6)6)
  0 (3 4 5 6)(3 4 5 6)

'(' means PushEdge, ')' means reduce with label.
Normally accept at state 6, and that's where we merge. But we can accept at 0
or 4 as well, if there is nothing at state 6.

*/

const DfaLabel lblList{0};
const DfaLabel lblComma{2};
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
    {},{},{lblList},{},{},{},{lblList},{},{lblComma}
  },
  {0,1,2,6,4,5,3,7,8},  // statePrioMap
  0,                    // stState
  lblList,              // enLabel
};

class Hooks : public GssHooks {
 public:
  GssHooksRes reduceList(DfaLabel lbl,SharedListVal lv) override {
    if(lbl!=lblList) BugMe<<"Reducing on strange "<<lbl;
    // elt-comma-elt.
    if(lv->size!=3) BugMe<<"Got list of size "<<lv->size<<" != 3";
    if(dynamic_cast<const EmptyVal*>(lv->at(1).get())==nullptr)
      BugMe<<"Commas should be empty. Position 1 is not empty: "
           <<typeid(*lv->at(1)).name();
    return lv;
  }
  GssHooksRes reduceString(DfaLabel lbl,SharedStringVal sv) override {
    if(lbl==lblComma)
      return make_shared<EmptyVal>(sv->stPos,sv->enPos);
    else return GssHooks::reduceString(lbl,std::move(sv));
  }
  GssMergeChoice merge(DfaState en,
                       SharedListVal lv1,SharedListVal lv2) override {
    if(lv1->size!=lv2->size)
      BugMe<<"Can't merge on mismatching sizes: "<<lv1->size<<" != "<<lv2->size;
    if(lv1->size==1) {
      auto sv1=dynamic_cast<const StringVal*>(lv1->last.get());
      auto sv2=dynamic_cast<const StringVal*>(lv2->last.get());
      if(sv1&&sv2)
        return GssMergeChoice::pickFirst;  // Doesn't matter what we return.
      if(en!=DfaState{4})
        BugMe<<"Merging singleton, but "<<en<<" != DfaState{4}";
      auto lu1=dynamic_pointer_cast<const ListVal>(lv1->last);
      auto lu2=dynamic_pointer_cast<const ListVal>(lv2->last);
      if(!lu1||!lu2)
        BugMe<<"Caught null: "<<typeid(*lv1->last).name()
             <<", "<<typeid(*lv2->last).name();
      return merge(en,lu1,lu2);
    }
    if(lv1->size!=3)
      BugMe<<"Expecting elt-comma-elt. Got size "<<lv1->size<<" != 3 at "<<en;
    // TODO check if en is the expected state.
    return lv1->at(0)->enPos>lv2->at(0)->enPos?GssMergeChoice::pickFirst
                                              :GssMergeChoice::pickSecond;
  }
};

const string& unwrapToString(const SemVal* v) {
  if(const auto* sv=dynamic_cast<const StringVal*>(v))
    return sv->s;
  if(const auto* lv=dynamic_cast<const ListVal*>(v)) {
    if(lv->size!=1) BugMe<<"This can't be a string. Size "<<lv->size<<" != 1";
    return unwrapToString(lv->last.get());
  }
  BugMe<<"unwrapToString got weird type: "<<typeid(*v).name();
}

vector<string> gather(SharedVal v) {
  if(dynamic_cast<const EmptyVal*>(v.get())) return {};
  vector<string> rv;
  while(v) {
    if(auto* lv=dynamic_cast<const ListVal*>(v.get())) {
      rv.push_back(unwrapToString(lv->last.get()));
      v=lv->at(0);
    }else{
      auto* sv=dynamic_cast<const StringVal*>(v.get());
      rv.push_back(sv->s);
      break;
    }
  }
  reverse(rv.begin(),rv.end());
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

  for(size_t i=0;i<n;++i) {
    vector<pair<SharedVal,SharedDiagSet>> res
      =glrParse(dfa,hooks,GetFromString(inputs[i]));
    if(res.empty()) BugMe<<"No valid parse on input["<<i<<']';
    for(size_t j=1;j<res.size();++j) {
      GssMergeChoice pick=hooks.merge(DfaState{6},
          dynamic_pointer_cast<const ListVal>(res[0].first),
          dynamic_pointer_cast<const ListVal>(res[j].first));
      if(pick==GssMergeChoice::pickSecond) res[0].first=res[j].first;
    }

    vector<string> resg=gather(res[0].first);
    if(resg!=outputs[i])
      BugMe<<"input["<<i<<"] parsed into "<<resg;
  }

  string invalid_inputs[]={",,","a b","a, , b","FOO"};
  for(size_t i=0;i<sizeof(invalid_inputs)/sizeof(*invalid_inputs);++i) {
    vector<pair<SharedVal,SharedDiagSet>> res
      =glrParse(dfa,hooks,GetFromString(invalid_inputs[i]));
    if(!glrParseFailed(res))
      BugMe<<"Was expecting parse failure, got "
           <<res.size()<<" successful parses";
  }
}

}  // namespace slowListParse


namespace shiftShiftConflict {
const Dfa dfa = {
  { // adjList
    {PushEdge{1},PushEdge{2}},
    {CharRangeEdge{'a','j',1}},
    {StringEdge{"foo",3}},
    {},
  },
  {{},{DfaLabel{0}},{},{DfaLabel{0}}}, // labelsMap
  {0,1,2,3},  // statePrioMap
  0,          // stState
  0,          // enLabel
};

using Hooks=singleStringParse::Hooks;

void test() {
  dieIfBad(dfa);
  Hooks hooks;
  SharedVal v=glrParseUnique(dfa,hooks,GetFromString("foo")).first;
  const StringVal& sv1=dynamic_cast<const StringVal&>(*v);
  if(sv1.s!="foo") BugMe<<"Parsed '"<<sv1.s<<"' != 'foo'";

  v=glrParseUnique(dfa,hooks,GetFromString("fad")).first;
  const StringVal& sv2=dynamic_cast<const StringVal&>(*v);
  if(sv2.s!="fad") BugMe<<"Parsed '"<<sv2.s<<"' != 'fad'";

}

}  // namespace shiftShiftConflict

namespace parseReturnsDiags {

using listParse::dfa;

struct Hooks : listParse::Hooks {
  GssHooksRes reduceString(DfaLabel lbl,SharedStringVal sv) override {
    GssHooksRes res=listParse::Hooks::reduceString(lbl,sv);  // upcall
    if(auto* sv=dynamic_cast<const StringVal*>(res.v.get())) {
      res.diags.push_back(make_shared<Diag>(sv->stPos,sv->enPos,"Got "+sv->s));
    }
    return res;
  }
};

void test() {
  dieIfBad(dfa);
  Hooks hooks;
  auto [v,diags]=glrParseUnique(dfa,hooks,GetFromString("foo,bar,baz"));

  vector<string> expected_diags={"Got bar","Got baz","Got foo"};
  vector<string> observed_diags=diagSetMessages(diags);
  if(observed_diags!=expected_diags)
    BugMe<<"Diag gathering from glrParseUnique is unexpected: "
         <<debug(observed_diags)<<" != "<<expected_diags;
}

}  // namespace parseReturnsDiags

namespace emptyStringParsing {

using Hooks=singleStringParse::Hooks;

void test() {
  Hooks hooks;
  Dfa dfa1 = {{{}}, {{}}, {0}, 0, 0};
  dieIfBad(dfa1);
  auto [v,diags]=glrParseUnique(dfa1,hooks,GetFromString(""));
  if(v)
    BugMe<<"Empty string should have returned nullptr. Got typeid == "
         <<typeid(*v).name();
  vector<string> observed_diags=diagSetMessages(diags);
  vector<string> expected_diags={"No input provided"};
  if(observed_diags!=expected_diags)
    BugMe<<"Diag gathering from glrParseUnique is unexpected: "
         <<debug(observed_diags)<<" != "<<expected_diags;

  // Now let's make empty string a valid input.
  Dfa dfa2=dfa1;
  dfa2.labelsMap[0]={DfaLabel{0}};
  dieIfBad(dfa2);
  tie(v,diags)=glrParseUnique(dfa2,hooks,GetFromString(""));
  if(typeid(*v)!=typeid(EmptyVal))
    BugMe<<"Valid empty string returned strange value: "<<typeid(*v).name()
         <<" != typeid(EmptyVal)";
  if(diags)
    BugMe<<"Was not expecting diagnostics on empty input. Got "
         <<debug(diagSetMessages(diags));
}

}  // namespace emptyStringParsing

namespace unexpectedChar {

void test() {
  singleStringParse::Hooks hooks;
  Dfa dfa=singleStringParse::dfa;  // Parses "foo"
  auto [v,diags]=glrParseUnique(dfa,hooks,GetFromString("foosball"));
  vector<string> observed_diags=diagSetMessages(diags);
  vector<string> expected_diags={"Unexpected character s"};
  if(observed_diags!=expected_diags)
    BugMe<<"Unexpected input characters not being properly reported: "
         <<debug(observed_diags)<<" != "<<debug(expected_diags);
}

}  // namespace unexpectedChar

namespace lastKnownDiags {

struct Hooks : public GssHooks {
  GssHooksRes reduceString(DfaLabel,SharedStringVal sv) override {
    return abandonReduce(sv->stPos,sv->enPos,"Abandoning string "+sv->s);
  }
  GssHooksRes reduceList(DfaLabel,SharedListVal) override {
    BugMe<<"There shouldn't be any list without strings being reduced";
  }
};

const Dfa dfa {
  { // adjList
    {PushEdge{1},PushEdge{2}},
    {CharRangeEdge{'a','z',1}},
    {LabelEdge{DfaLabel{0},DfaState{2}}},
  },
  {{},{DfaLabel{0}},{}},   // labelsMap
  {0,1,2},                 // statePrioMap
  0,                       // stState
  0,                       // enLabel
};

void test() {
  Hooks hooks;
  dieIfBad(dfa);
  auto [v,diags]=glrParseUnique(dfa,hooks,GetFromString("foo"));
  if(v) BugMe<<"Was expecting nullptr value on non-reduction";
  vector<string> observed_diags=diagSetMessages(diags);
  vector<string> expected_diags={"Abandoning string foo"};
  if(observed_diags!=expected_diags)
    BugMe<<"lastKnownDiags_ returning something unexpected: "
         <<debug(observed_diags)<<" != "<<debug(expected_diags);
}

}  // namespace lastKnownDiags

namespace incompleteInput {

const Dfa dfa {
  { // adjList
    {PushEdge{1}},
    {CharRangeEdge{'a','z',1}},
  },
  {{},{}},   // labelsMap
  {0,1},     // statePrioMap
  0,         // stState
  0,         // enLabel
};

void test() {
  singleStringParse::Hooks hooks;
  dieIfBad(dfa);
  auto [v,diags]=glrParseUnique(dfa,hooks,GetFromString("foo"));
  if(v) BugMe<<"Was expecting nullptr value on incomplete input";
  vector<string> observed_diags=diagSetMessages(diags);
  vector<string> expected_diags={"Incomplete input"};
  if(observed_diags!=expected_diags)
    BugMe<<"lastKnownDiags_ returning something unexpected: "
         <<debug(observed_diags)<<" != "<<debug(expected_diags);

}

}  // namespace incompleteInput

}  // namespace

int main() {
  testDiagSetGather();
  testNullDiagsIgnored();
  testDiagNullConcat();
  checkCheckError::test();
  singleShifts::test();
  singleStringParse::test();
  stringSequenceParse::test();
  nullIgnored::test();
  listParse::test();
  slowListParse::test();
  shiftShiftConflict::test();
  parseReturnsDiags::test();
  emptyStringParsing::test();
  unexpectedChar::test();
  lastKnownDiags::test();
  incompleteInput::test();
}
