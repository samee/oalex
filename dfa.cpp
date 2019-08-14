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

#include<algorithm>
#include<limits>
#include<vector>

#include"util.h"
using namespace oalex;
using namespace oalex::internal;
using namespace std;

namespace oalex {

// TODO it would be nice to have a version with a source state.
string edgeDebug(const DfaEdge& e) noexcept {
  if(auto* ce=get_if<CharRangeEdge>(&e))
    return Str()<<'['<<ce->st<<'-'<<ce->en<<"] to "<<ce->dest.toInt;
  if(auto* le=get_if<LabelEdge>(&e))
    return Str()<<"Label "<<le->lbl.toInt<<" to "<<le->dest.toInt;
  if(auto* pe=get_if<PushEdge>(&e))
    return Str()<<"Push to "<<pe->dest.toInt;
  if(auto* se=get_if<StringEdge>(&e))
    return Str()<<"String '"<<se->s<<"' to "<<se->dest.toInt;
  BugDie()<<"Unknown edge type "<<e.index();
}

bool operator!=(const DfaEdge& e1,const DfaEdge& e2) {
  if(e1.index()!=e2.index()) return true;
  if(auto* ce1=get_if<CharRangeEdge>(&e1)) {
    auto* ce2=get_if<CharRangeEdge>(&e2);
    return ce1->st!=ce2->st||ce1->en!=ce2->en||ce1->dest!=ce2->dest;
  }
  if(auto* le1=get_if<LabelEdge>(&e1)) {
    auto* le2=get_if<LabelEdge>(&e2);
    return le1->lbl!=le2->lbl||le1->dest!=le2->dest;
  }
  if(auto* pe1=get_if<PushEdge>(&e1)) {
    auto* pe2=get_if<PushEdge>(&e2);
    return pe1->dest!=pe2->dest;
  }
  if(auto* se1=get_if<StringEdge>(&e1)) {
    auto* se2=get_if<StringEdge>(&e2);
    return se1->s!=se2->s||se1->dest!=se2->dest;
  }
  BugDie()<<"Comparing unknown edge type "<<e1.index();
}

bool operator==(const DfaEdge& e1,const DfaEdge& e2) { return !(e1!=e2); }

namespace {

// This is only used internally, user hooks never see this.
struct InputViewVal : SemVal {
  input_view s;
  InputViewVal(size_t st,size_t en,input_view ss)
    : SemVal(st,en),s(std::move(ss)) {}
};

DfaState dest(const DfaEdge* edge) noexcept {
  if(auto e=get_if<CharRangeEdge>(edge)) return e->dest;
  if(auto e=get_if<LabelEdge>(edge)) return e->dest;
  if(auto e=get_if<PushEdge>(edge)) return e->dest;
  if(auto e=get_if<StringEdge>(edge)) return e->dest;
  BugDie()<<"dest(edge) found edge with unknown index "<<edge->index();
}

bool charInRange(char ch,const CharRangeEdge& e)
  { return e.st<=ch && ch<=e.en; }

bool charCanStart(char ch,const DfaEdge* edge) noexcept {
  if(auto e=get_if<CharRangeEdge>(edge)) return charInRange(ch,*e);
  if(auto e=get_if<StringEdge>(edge)) return e->s[0]==ch;
  if(holds_alternative<PushEdge>(*edge)||holds_alternative<LabelEdge>(*edge))
    return false;
  BugDie()<<"charCanStart(edge) found edge with unknown index "<<edge->index();
}

bool stringOrCharEdge(const DfaEdge& e) {
  return holds_alternative<StringEdge>(e)||holds_alternative<CharRangeEdge>(e);
}

bool isPrefixEdge(const DfaEdge& e1,const DfaEdge& e2) {
  if(auto* se1=get_if<StringEdge>(&e1)) {
    if(se1->s.empty()) return true;
    else if(auto* se2=get_if<StringEdge>(&e2))
      return se1->s==se2->s.substr(0,se1->s.size());
    else if(auto* ce2=get_if<CharRangeEdge>(&e2))
      return se1->s.size()==1&&charInRange(se1->s[0],*ce2);
    else BugDie()<<"isPrefixEdge called on wrong edge type";
  }else if(auto* ce1=get_if<CharRangeEdge>(&e1)) {
    if(auto* se2=get_if<StringEdge>(&e2))
      return se2->s.size()==1&&charInRange(se2->s[0],*ce1);
    else if(auto* ce2=get_if<CharRangeEdge>(&e2))
      return ce1->st<=ce2->en&&ce2->st<=ce1->en;
    else BugDie()<<"isPrefixEdge called on wrong edge type";
  }else BugDie()<<"isPrefixEdge called on wrong edge type";
}

// Pops earlier than everything else.
const GssPendingReduce len0={0,-1,GssHead{},nullptr,nullptr,true};

// Convention: numerically lower statePrio gets reduced earlier.
bool gssReduceLater(const GssPendingReduce& a,
                    const GssPendingReduce& b) {
  if(a.length!=b.length) return a.length>b.length;
  else if(a.statePrio!=b.statePrio) return a.statePrio>b.statePrio;
  else return a.pushAgain<b.pushAgain;
}

bool canMerge(const optional<GssHead>& a,
              const GssPendingReduce& b) {
  if(!a.has_value()) return true;  // merging with nullopt is an identity op.

  // This method is a bit weird, in that we are using the whole
  // GssPendingReduce to decide merging just the GssHead.
  // I may switch to comparing labeledEdge, both here and gssReduceLater.
  return a->stPos()==b.h.stPos() &&
         std::get<DfaState>(a->enState)==b.labeledEdge->dest;
}

shared_ptr<GssEdge> closeHead(GssHead head,size_t breakPos) {
  // This is typically taken care of in enqueueLabeledHeads, pre-merging.
  if(auto iv=dynamic_cast<const InputViewVal*>(head.v.get()))
    BugDie()<<"Unexpectedly pushing from unreduced string "<<string_view(iv->s);

  auto ge=make_shared<GssEdge>();
  ge->v=std::move(head.v);
  ge->prev=std::move(head.prev);
  ge->enPos=breakPos;
  if(DfaState* a=get_if<DfaState>(&head.enState)) ge->enState=*a;
  else BugDie()<<"Can't push back from a state in "<<head.enState.index();
  return ge;
}

GssHead openNew(shared_ptr<const GssEdge> ge,
                const DfaEdge& de,SharedVal newv) {
  GssHead rv;
  rv.prev={std::move(ge)};
  rv.v=std::move(newv);
  if(auto se=get_if<StringEdge>(&de)) {
    if(se->s.size()>1) rv.enState=MidString{se,rv.prev[0]->enPos};
    else rv.enState=se->dest;
  }else rv.enState=dest(&de);
  return rv;
}

SharedVal reduceStringOrList(GssHooks& hk,
    SharedListVal prev,DfaLabel lbl,SharedVal v) {
  if(auto lv=dynamic_pointer_cast<const ListVal>(v)) {
    SharedVal v2=hk.reduceList(lbl,std::move(lv));
    return Append(prev,std::move(v2));
  }else if(auto sv=dynamic_pointer_cast<const StringVal>(v)) {
    SharedVal v2=hk.reduceString(lbl,std::move(sv));
    return Append(prev,std::move(v2));
  }else {
    BugDie()<<"GssHooks should reduce from String or List. Got "
            <<typeid(*v).name()<<" instead, on label "<<lbl.toInt;
  }
}


}  // namespace

string Dfa::checkError() const {
  try {
    using state_int=DfaState::int_type;

    // Basic range and size checks.
    if(adjList.size()>numeric_limits<state_int>::max())
      return Str()<<"Too many nodes. n = "<<adjList.size();
    if(adjList.size()!=labelsMap.size())
      return Str()<<"adjList and labelsMap has different sizes. "
                  <<adjList.size()<<" != "<<labelsMap.size();
    if(adjList.size()!=statePrioMap.size())
      return Str()<<"adjList and statePrioMap has different sizes. "
                  <<adjList.size()<<" != "<<statePrioMap.size();
    state_int a,n=adjList.size();

    /* Check that:
       - StringEdge::s are not be empty.
       - Self-loops of PushEdge don't exist. */
    for(a=0;a<n;++a) {
      for(const DfaEdge& e:adjList[a]) {
        if(auto se=get_if<StringEdge>(&e)) {
          if(se->s.empty()) return Str()<<"State "<<a<<" has empty string edge";
        }else if(auto pe=get_if<PushEdge>(&e)) {
          if(pe->dest==DfaState{a})
            return Str()<<"We have a PushEdge self-loop at state "<<a;
        }
      }
    }

    /* Check that:
       - All StringEdge and CharRangeEdge out of a node are prefix-free.
       - All outgoing LabelEdges are distinct.
    */
    for(a=0;a<n;++a) {
      auto outs=outOf(DfaState{a});
      for(const DfaEdge& ei:outs) for(const DfaEdge& ej:outs) if(&ei!=&ej) {
        if(stringOrCharEdge(ei)&&stringOrCharEdge(ej)&&
           isPrefixEdge(ei,ej))
          return Str()<<"Conflict out of state "<<a<<": "
                      <<edgeDebug(ei)<<" is a prefix of"<<edgeDebug(ej);
        if(auto lei=get_if<LabelEdge>(&ei))
          if(auto lej=get_if<LabelEdge>(&ej))
            if(lei->lbl==lej->lbl)
              return Str()<<"Duplicate labels out of state "<<a<<": "
                          <<edgeDebug(ej);
      }
    }
    /* A DFA component with LabelEdges are separated from other components
       with PushEdges. Also, states in terminal components shouldn't have
       outgoing PushEdges. */
    vector<bool> inLabel(n,false),inNonLabel(n,false);
    for(a=0;a<n;++a) for(const DfaEdge& e:adjList[a]) {
      if(holds_alternative<LabelEdge>(e))
        inLabel[a]=inLabel[dest(&e).toInt]=true;
      else if(!holds_alternative<PushEdge>(e))
        inNonLabel[a]=inNonLabel[dest(&e).toInt]=true;
    }
    for(a=0;a<n;++a) if(inLabel[a]&&inNonLabel[a])
      return Str()<<"Components mix in state "<<a;
    for(a=0;a<n;++a) if(inNonLabel[a]) for(const DfaEdge& e:adjList[a]) {
      if(holds_alternative<PushEdge>(e))
        return Str()<<"State "<<a
                    <<" in terminal component has outgoing PushEdge";
    }

    // All out-edges out of stState must be PushEdges.
    for(const DfaEdge& e:outOf(stState))
      if(!holds_alternative<PushEdge>(e))
        return Str()<<"Start state must have only PusheEdges";

    // statePrioMap elements need to be all unique.
    vector<state_int> pmap=statePrioMap;
    sort(pmap.begin(),pmap.end());
    for(size_t i=1;i<pmap.size();++i) if(pmap[i]==pmap[i-1])
      return Str()<<"Dfa::statePrioMap has duplicates";
  }catch(invalid_argument& ex) { return ex.what(); }

  return "";
}

namespace internal {

SharedVal GlrCtx::valFromString(const SemVal* sv) const {
  const InputViewVal* iv=dynamic_cast<const InputViewVal*>(sv);
  return iv&&iv->s.size()!=0
    ?make_shared<StringVal>(iv->stPos,pos(),string(iv->s))
    :nullptr;
}

// TODO define ostream& operator<< for DfaState and DfaLabel.
optional<GssHead> GlrCtx::extendValue(const GssEdge& prev,SharedVal v,
                                      const LabelEdge& edge) {
  SharedListVal prevlv=dynamic_pointer_cast<const ListVal>(prev.v);
  if(!prevlv)
    BugDie()<<"GssHooks should always extend from a ListVal. Got "
            <<typeid(*prev.v).name()<<" instead, on edge "<<prev.enState.toInt
            <<" ---DfaLabel{"<<edge.lbl.toInt<<"}--> "<<edge.dest.toInt;
  SharedVal newv=reduceStringOrList(*hooks_,std::move(prevlv),
                                    edge.lbl,std::move(v));
  if(!newv) return nullopt;
  return GssHead{newv,edge.dest,prev.prev};
}

// Same as extendValue, but using hooks_->useVal instead of hooks_->extend.
optional<GssHead> GlrCtx::changeValue(
    shared_ptr<const GssEdge> prev,SharedVal v,const LabelEdge& edge) {
  if(dynamic_cast<const InputViewVal*>(v.get()))
    BugDie()<<"We shouldn't expose objects of internal type InputViewVal to "
              "GssHook::useVal()";
  SharedVal newv=static_cast<GssAggregator*>(hooks_)
    ->useVal(edge.lbl,std::move(v));
  if(!newv) return nullopt;
  if(prev->enPos>pos())
    BugDie()<<"Problem in changeValue: prev->enPos too large: "<<prev->enPos;
  return GssHead{newv,edge.dest,{std::move(prev)}};
}

optional<GssHead>
GlrCtx::mergeHeads(optional<GssHead> h1,optional<GssHead> h2) {
  if(!h1) return h2;
  if(!h2) return h1;
  else return mergeHeads(std::move(*h1),std::move(*h2));
}

optional<GssHead> GlrCtx::mergeHeads(GssHead h1,GssHead h2) {
  DfaState s1=std::get<DfaState>(h1.enState);
  DfaState s2=std::get<DfaState>(h2.enState);
  if(s1!=s2||h1.stPos()!=h2.stPos())
    BugDie()<<"Merging incompatible heads. States "<<s1.toInt<<','<<s2.toInt
            <<" stPos "<<h1.stPos()<<','<<h2.stPos();
  SharedVal newv=static_cast<GssAggregator*>(hooks_)
    ->merge(s1,std::move(h1.v),std::move(h2.v));
  if(!newv) return nullopt;
  // There shouldn't be any duplicate prevs, since they should already
  // have been merged.
  vector<shared_ptr<const GssEdge>> mergedPrev=std::move(h1.prev);
  for(const auto& p:h2.prev) mergedPrev.push_back(std::move(p));
  return GssHead{newv,s1,std::move(mergedPrev)};
}

// Can move from *it.
void GlrCtx::shiftAllPushEdges(DfaState s,char ch,list<GssHead>::iterator it) {
  shared_ptr<GssEdge> ge;
  shared_ptr<InputViewVal> iv;
  for(const DfaEdge& e1:dfa_->outOf(s)) if(auto pe=get_if<PushEdge>(&e1)) {
    for(const DfaEdge& e2:dfa_->outOf(pe->dest)) if(charCanStart(ch,&e2)) {
      if(!ge) {
        iv=make_shared<InputViewVal>(pos(),string::npos,grab_tail(buf_));
        ge=closeHead(std::move(*it),pos());
      }
      heads_.insert(it,openNew(ge,e2,iv));
    }
  }
}

// Shift ch from enState, asserts enState is a DfaState.
bool GlrCtx::shiftTerminalEdge(
    char ch,variant<DfaState,MidString>& enState) const {
  for(const DfaEdge& e:dfa_->outOf(std::get<DfaState>(enState))) {
    if(holds_alternative<LabelEdge>(e)) continue;
    else if(const auto *se=get_if<StringEdge>(&e)) {
      if(se->s[0]!=ch) continue;
      if(se->s.size()==1) enState=se->dest;
      else enState=MidString{se,pos()};
      return true;
    }else if(const auto *cre=get_if<CharRangeEdge>(&e)) {
      if(cre->st<=ch&&ch<=cre->en) {
        enState=cre->dest;
        return true;
      }
    }else if(!holds_alternative<PushEdge>(e))
      BugDie()<<"Shift edge found with unknown index "<<e.index();
  }
  return false;
}

// We should have a GssHead::shift that is easier to test in isolation. TODO
void GlrCtx::shift(char ch) {
  const size_t pos=this->pos();
  auto it=heads_.begin();
  while(it!=heads_.end()) {
    GssHead& head=*it;

    // Convention: we don't keep zero-length heads. A new head is only allocated
    // when the previous one got pushed back to a GssEdge, and a non-null
    // string was available for the new GssHead.
    // The only exception is the first shift() call, when we do start with a
    // single zero-length head.
    if(pos<=head.stPos()&&
        !(pos==0&&head.stPos()==0)) BugDie()<<"GSS corrupted. Head backwards.";

    if(const MidString* ms=get_if<MidString>(&head.enState)) {
      const StringEdge& se=*ms->se;  // Die if ms->se is null.
      size_t i=pos-ms->edgeStart;
      if(i==0) BugDie()<<"Who started this edge? "<<se.s;
      else if(i>=se.s.size()) BugDie()<<"Overflowing StringEdge "<<se.s;
      else if(se.s[i]!=ch) it=heads_.erase(it);
      else {
        if(i+1==se.s.size()) head.enState=se.dest;
        ++it;
      }
    }else if(DfaState* a=get_if<DfaState>(&head.enState)) {
      if(shiftTerminalEdge(ch,head.enState)) ++it;
      else {
        shiftAllPushEdges(*a,ch,it);
        it=heads_.erase(it);
      }
    }
  }
  buf_.push_back(ch);
}

const LabelEdge* labelOutOf(const Dfa& dfa,DfaState s,DfaLabel l) {
  // Assumes no repeated edge.
  for(const DfaEdge& e:dfa.outOf(s))
    if(const LabelEdge* le=get_if<LabelEdge>(&e))
      if(le->lbl==l) return le;
  return nullptr;
}

vector<DfaState> pushDestsOutOf(const Dfa& dfa,DfaState s) {
  vector<DfaState> rv;
  for(const DfaEdge& e:dfa.outOf(s))
    if(const PushEdge* pe=get_if<PushEdge>(&e)) rv.push_back(pe->dest);
  return rv;
}

void GlrCtx::enqueueAllLabelsInHead(const GssHead& h,GssPendingQueue& q,
   const GssPendingReduce& curReduce) const {
  const auto* hendp=get_if<DfaState>(&h.enState);
  if(!hendp) return;  // Nothing to push from MidString.

  for(DfaLabel l:dfa_->labels(*hendp)) {
    if(h.prev.empty())
      BugDie()<<"prev is empty. This should not be possible if start state "
                "only has PushEdges going out of it.";
    for(const shared_ptr<const GssEdge>& prev:h.prev) {
      auto qpush=[&](const LabelEdge* le,size_t st,bool pushAgain) {
        if(!le) return;
        auto prio=dfa_->statePrio(le->dest);
        GssPendingReduce pr{pos()-st,prio,h,prev,le,pushAgain};
        if(gssReduceLater(pr,curReduce)) q.push(pr);
      };
      DfaState b=prev->enState;
      qpush(labelOutOf(*dfa_,b,l),prev->stPos(),false);
      for(DfaState pd:pushDestsOutOf(*dfa_,b))
        qpush(labelOutOf(*dfa_,pd,l),h.stPos(),true);
    }
  }
}

void GlrCtx::enqueueLabeledHeads(GssPendingQueue& q) const {
  for(const GssHead& h:heads_) {
    const auto* hendp=get_if<DfaState>(&h.enState);
    if(!hendp) continue;  // Not pushing from MidString.
    if(auto iv=dynamic_cast<const InputViewVal*>(h.v.get()))
      enqueueAllLabelsInHead(GssHead{valFromString(iv),
                             h.enState,h.prev},q,len0);
    else enqueueAllLabelsInHead(h,q,len0);
  }
}

// This EmptyVal never gets extended or merged in.
// But it is returned on empty inputs.
GssHead GlrCtx::startingHeadAt(DfaState s) {
  return {make_shared<EmptyVal>(0,0),s,{}};
}

vector<SharedVal> GlrCtx::parse(function<int16_t()> getch) {
  heads_.clear();
  heads_.push_back(startingHeadAt(dfa_->stState));
  char ch;
  while((ch=getch())>=0) {
    shift(ch);
    GssPendingQueue q(gssReduceLater);
    enqueueLabeledHeads(q);
    while(!q.empty()) {
      GssPendingReduce cur;
      optional<GssHead> prevHead,newHead;
      do {
        cur=q.top(); q.pop();
        if(cur.pushAgain)
          newHead=changeValue(cur.oldPrev,cur.h.v,*cur.labeledEdge);
        else newHead=extendValue(*cur.oldPrev,cur.h.v,*cur.labeledEdge);
        prevHead=mergeHeads(prevHead,newHead);
        // Keep popping as many as I can merge in.
      } while(!q.empty()&&canMerge(prevHead,q.top()));
      // cur is guaranteed not to be garbage at this point.
      // TODO BugDie on a proper empty check.
      if(prevHead) {
        heads_.push_back(*prevHead);
        enqueueAllLabelsInHead(*prevHead,q,cur);
      }
    }
  }

  vector<SharedVal> rv;
  for(GssHead& h:heads_) {
    if(h.v==nullptr)
      BugDie()<<"nullptr values should already have been dropped";
    if(h.stPos()!=0) continue;
    if(dynamic_cast<const InputViewVal*>(h.v.get())) continue;
    if(dfa_->isEnState(std::get<DfaState>(h.enState))) rv.push_back(h.v);
  }
  return std::move(rv);
}

}  // namespace internal

SharedListVal GssHooks::merge(DfaState,
                              SharedListVal lv1,SharedListVal lv2) {
  BugDie()<<"Unexpectedly encountered ambiguous parsing: ["
          <<lv1->stPos<<','<<lv2->enPos<<')';
}

SharedVal GssHooks::extend(DfaState,const LabelEdge&,
                           const SharedVal&,const SharedVal&) {
  BugDie()<<"Unused. Nobody should call "<<__func__;
}

SharedVal GssHooks::reduceString(DfaLabel,shared_ptr<const StringVal> sv) {
  return sv;
}

SharedVal GssHooks::useVal(DfaLabel lbl,SharedVal val) {
  return reduceStringOrList(*this,nullptr,lbl,std::move(val));
}

SharedVal GssHooks::merge(DfaState en,SharedVal v1,SharedVal v2) {
  SharedListVal lv1=dynamic_pointer_cast<const ListVal>(v1);
  SharedListVal lv2=dynamic_pointer_cast<const ListVal>(v2);
  if(lv1==nullptr||lv2==nullptr)
    BugDie()<<"GssHooks can only merge lists. Found "
            <<typeid(*lv1).name()<<" and "<<typeid(*lv2).name()<<" at DfaState{"
            <<en.toInt<<"}";
  return merge(en,std::move(lv1),std::move(lv2));
}


vector<SharedVal> glrParse(
    const Dfa& dfa,GssHooks& hk,function<int16_t()> getch) {
  GlrCtx glr(dfa,hk);
  return glr.parse(getch);
}

}  // namespace oalex
