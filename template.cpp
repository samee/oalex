/*  Copyright 2020 Google LLC

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. */

#include "template.h"
#include <algorithm>
#include <iterator>
#include <map>
#include <type_traits>
#include "fmt/format.h"
#include "runtime/util.h"
using fmt::format;
using std::get_if;
using std::holds_alternative;
using std::is_same_v;
using std::make_pair;
using std::map;
using std::max;
using std::min;
using std::move_iterator;
using std::nullopt;
using std::optional;
using std::pair;
using std::sort;
using std::string;
using std::string_view;
using std::unique_ptr;
using std::vector;
using std::visit;
using oalex::Error;
using oalex::lex::NewlineChar;
using oalex::lex::QuotedString;
using oalex::lex::UnquotedToken;

namespace oalex {

static string debug(const QuotedString& qs) { return qs; }
static string debug(const DelimPair& dp) {
  return format("{} ... {}", string(dp.st), string(dp.en));
}
static string debug(const PartPattern& pp) {
  if(auto* p = get_if<QuotedString>(&pp)) return debug(*p);
  if(auto* p = get_if<DelimPair>(&pp)) return debug(*p);
  Bug("Unknown PartPattern with index {}", pp.index());
}

static auto matchAllParts(const QuotedString& spatt, const QuotedString& s)
  -> optional<vector<pair<size_t, size_t>>> {
  if(spatt.empty())
    return Error(spatt, 0, spatt.size(), "Placeholder pattern cannot be empty");
  vector<pair<size_t, size_t>> rv;
  for(size_t i=0; i+spatt.size() <= s.size(); ++i)
    if(s.substr(i, spatt.size()) == spatt) {
      if(!rv.empty() && i<rv.back().second)
        return Error(s, rv.back().first, i + spatt.size(),
                     format("Pattern '{}' matches overlapping segments",
                            debug(spatt)));
      else rv.push_back(make_pair(i, i+spatt.size()));
    }
  return rv;
}

static auto matchAllParts(const DelimPair& dpatt, const QuotedString& s)
  -> optional<vector<pair<size_t,size_t>>> {
  if(dpatt.st.empty() || dpatt.en.empty()) {
    const QuotedString& qs = (dpatt.st.empty() ? dpatt.st : dpatt.en);
    return Error(qs, 0, qs.size(), "Placeholder pattern cannot be empty");
  }
  if(dpatt.st.find(dpatt.en, 1) != string::npos)
    return Error(dpatt.en, 0, dpatt.en.size(),
                 "End pattern is a substring of the start pattern");
  vector<pair<size_t, size_t>> rv;
  for(size_t i=0; i+dpatt.st.size() <= s.size(); ++i)
    if(s.substr(i, dpatt.st.size()) == dpatt.st) {
      if(!rv.empty() && i<rv.back().second) {
        return Error(s, rv.back().first,
                     max(rv.back().second, i + dpatt.st.size()),
                     format("Pattern '{}' matches overlapping segments",
                            debug(dpatt)));
      }
      size_t j = s.find(dpatt.en, i+1);
      if(j == string::npos) {
        Error(s, i, i+dpatt.st.size(), "Unterminated segment");
        continue;  // recover
      }
      rv.push_back(make_pair(i, j+dpatt.en.size()));
    }
  return rv;
}

// TODO: Produce error message for every `return nullopt` here.
auto matchAllParts(const PartPattern& patt, const QuotedString& s)
  -> optional<vector<pair<size_t, size_t>>> {
  if(auto* spatt = get_if<QuotedString>(&patt)) return matchAllParts(*spatt, s);
  if(auto* dpatt = get_if<DelimPair>(&patt)) return matchAllParts(*dpatt, s);
  Bug("matchAllParts() called with unknown variant: index {}", patt.index());
}

// A map of non-empty, non-overlapping intervals. Key is the start position,
// while mapped_type.first is end position (end position being exclusive).
using IntervalMap = map<size_t, pair<size_t, Ident>>;

static bool
overlaps(const IntervalMap::value_type& a, const IntervalMap::value_type& b) {
  return b.first < a.second.first && a.first < b.second.first;
}

// Inserts x into the m. If the insert is successful, it returns nullptr.
// If the insert is unsuccessful, it returns a pointer to an overlapping
// interval already in the array.
static auto
insert(IntervalMap& m, const IntervalMap::value_type& x)
  -> const IntervalMap::value_type* {
  if(x.first >= x.second.first)
    Bug("matchAllParts() produced invalid interval: {}, {}",
        x.first, x.second.first);

  IntervalMap::iterator next = m.lower_bound(x.first);
  if(next != m.end() && overlaps(x, *next)) return &*next;
  if(next != m.begin()) {
    auto prev = next; --prev;
    if(overlaps(x, *prev)) return &*prev;
  }
  m.insert(next, x);
  return nullptr;
}

static size_t stPattPos(const PartPattern& patt) {
  if(auto* q = get_if<QuotedString>(&patt)) return stPos(*q);
  if(auto* dp = get_if<DelimPair>(&patt)) return stPos(dp->st);
  Bug("stPattPos() called with unknown index {}", patt.index());
}

static size_t enPattPos(const PartPattern& patt) {
  if(auto* q = get_if<QuotedString>(&patt)) return enPos(*q);
  if(auto* dp = get_if<DelimPair>(&patt)) return enPos(dp->st);
  Bug("enPattPos() called with unknown index {}", patt.index());
}

static const QuotedString& pattStart(const PartPattern& patt) {
  if(auto* q = get_if<QuotedString>(&patt)) return *q;
  if(auto* dp = get_if<DelimPair>(&patt)) return dp->st;
  Bug("pattStart() called with unknown index {}", patt.index());
}

auto labelParts(const QuotedString& s,
                const map<Ident,PartPattern>& partPatterns)
    -> vector<LabelOrPart> {
  bool matchError = false;
  // Find patterns, make IntervalMap.
  IntervalMap m;
  for(auto& [id,patt] : partPatterns) {
    auto matches = matchAllParts(patt, s);
    if(!matches) { matchError = true; continue; }
    if(matches->empty())
      Warning(pattStart(patt), stPattPos(patt), enPattPos(patt),
              format("No match found for pattern '{}'", debug(patt)));
    for(const auto& [st,en] : *matches) {
      const IntervalMap::value_type interval{st, {en, id}};
      const IntervalMap::value_type* ovlap = insert(m, interval);
      if(ovlap) {
        Error(s, st, en,
              format("Part '{}' overlaps with '{}' at {}", debug(patt),
                     debug(partPatterns.at(ovlap->second.second)),
                     locationString(s, ovlap->first, ovlap->second.first)));
        matchError = true;
      }
    }
  }
  if(matchError) return {};

  // Split up string.
  vector<LabelOrPart> rv;
  size_t lastEn = 0;
  for(const auto& [st, enid] : m) {
    auto& [en, id] = enid;
    if(st < lastEn)
      Bug("Overlapping intervals should have been caught earlier: {}"
          " starts before ending another interval at {}",
          locationString(s, st, en),
          locationString(s, lastEn, lastEn+1));
    if(st > lastEn) rv.push_back(s.subqstr(lastEn, st-lastEn));
    rv.push_back(id);
    lastEn = en;
  }
  if(lastEn < s.size() || rv.empty())
    rv.push_back(s.subqstr(lastEn, s.size()-lastEn));
  return rv;
}

// This function assumes we are starting with ctx.input.sizeGt(i).
static TokenOrPart lexTemplateToken(const QuotedString& s, size_t& i,
                             const LexDirective& opts) {
  const size_t st = i;
  const bool isword = matchesCharSet(s[i], opts.wordChars);
  while(s.sizeGt(i) && !opts.skip.canStart(s, i)) {
    if(isword != matchesCharSet(s[i], opts.wordChars)) break;
    ++i;
  }
  return isword ? TokenOrPart(WordToken(s.subqstr(st, i-st)))
                : TokenOrPart(OperToken(s.subqstr(st, i-st)));
}

auto tokenizeTemplateWithoutLabels(const QuotedString& s,
                                   const LexDirective& opts,
                                   string_view comment_end_error)
                                   -> vector<TokenOrPart> {
  size_t i=0;
  vector<TokenOrPart> rv;
  while(true) {
    i = opts.skip.acrossLines(s, i);
    if(!s.sizeGt(i)) break;
    else if(s[i] == '\n') rv.push_back(NewlineChar(s, i++));
    else rv.push_back(lexTemplateToken(s, i, opts));
  }
  if(i == Input::npos)
    Error(s, s.size(), s.size()+1, string(comment_end_error));
  return rv;
}

static auto tokenizeTemplateKeepNewlines(const QuotedString& s,
    const LexDirective& opts, string_view comment_end_error)
    -> vector<TokenOrPart> {
  size_t i=0, lastBol=0;
  vector<TokenOrPart> rv;
  while(true) {
    i = opts.skip.withinLine(s, i);
    if(s.bol(i) != lastBol) {
      rv.push_back(NewlineChar(s, i-1));
      lastBol = i;
    }else if(!s.sizeGt(i)) break;
    else rv.push_back(lexTemplateToken(s, i, opts));
  }
  if(i == Input::npos)
    Error(s, s.size(), s.size()+1, string(comment_end_error));
  return rv;
}

auto tokenizeTemplate(
    const vector<LabelOrPart>& lblParts,
    const LexDirective& lexopts) -> vector<TokenOrPart> {
  vector<TokenOrPart> rv;
  auto tokenizer = (lexopts.keepAllNewlines ? tokenizeTemplateKeepNewlines
                                            : tokenizeTemplateWithoutLabels);
  if(lexopts.keepAllNewlines && lexopts.skip.indicateBlankLines)
    Bug("skip.indicateBlankLines and keepAllNewlines cannot both be set");
  for(const LabelOrPart& lorp : lblParts) {
    if(auto* id = get_if<Ident>(&lorp)) rv.push_back(*id);
    else if(auto* qs = get_if<QuotedString>(&lorp)) {
      const char* err = &lorp == &lblParts.back() ?
        "Comment never ends" : "Placeholders not allowed in comments";
      vector<TokenOrPart> toks
        = tokenizer(*qs, lexopts, err);
      move(toks.begin(), toks.end(), back_inserter(rv));
    }else Bug("{}: Unknown LabelOrPart alternative, index {}",
              __func__, lorp.index());
  }
  return rv;
}

static bool isStrictSubstr(string_view s, string_view t) {
  return isSubstr(s, t) && s != t;
}

bool hasFusedTemplateOpers(InputDiags& ctx, const vector<TokenOrPart>& tops) {
  static string_view tmplOpers[] = {"[", "]", "...", "|"};
  bool rv = false;
  for(auto& top : tops) {
    const UnquotedToken* tok = get_if<WordToken>(&top);
    if(!tok) tok = get_if<OperToken>(&top);
    if(!tok) continue;
    for(auto& op : tmplOpers) if(isStrictSubstr(op, **tok)) {
      Error(ctx, tok->stPos, tok->enPos,
            format("Token '{}' incorporates '{}'. They must be surrounded by "
                   "whitespace outside of marked regions.", **tok, op));
      rv = true;
    }
  }
  return rv;
}

static auto getIfMetaToken(const TokenOrPart& top)
  -> pair<string_view, size_t> {
  string_view s;
  size_t start;
  if(auto* p = get_if<WordToken>(&top)) { s = **p; start = p->stPos; }
  if(auto* p = get_if<OperToken>(&top)) { s = **p; start = p->stPos; }
  for(auto& meta : {"[", "]", "|", "..."}) if(s == meta) return {s, start};
  return {};
}

template <class T> static
Template gatherInto(vector<Template> parts) {
  static_assert(is_same_v<T,TemplateConcat> || is_same_v<T,TemplateOrList>);
  if(parts.size() == 1) return std::move(parts[0]);
  return move_to_unique(T{std::move(parts)});
}

static const UnquotedToken* getIfUnquotedToken(const Template* t) {
  if(auto* p = get_if_unique<WordToken>(t)) return p;
  if(auto* p = get_if_unique<OperToken>(t)) return p;
  return nullptr;
}

static
auto findEllipsis(const vector<Template>& parts, size_t pos)
  -> pair<size_t, const UnquotedToken*> {
  for(size_t i=pos; i<parts.size(); ++i) {
    auto* p = getIfUnquotedToken(&parts[i]);
    if(p && **p == "...") return {i, p};
  }
  return {};
}

static
bool isAtomicToken(const Template& t) {
  return holds_one_of_unique<WordToken,OperToken,NewlineChar,Ident>(t);
}

static
size_t atomicSuffixStart(const vector<Template>& tv, size_t st, size_t en) {
  for(size_t i=en; i>st; --i) if(!isAtomicToken(tv[i-1])) return i;
  return st;
}

static
size_t atomicPrefixEnd(const vector<Template>& tv, size_t st, size_t en) {
  for(size_t i=st; i<en; ++i) if(!isAtomicToken(tv[i])) return i;
  return en;
}

static
bool areTokensAndEqual(const Template& t1, const Template& t2) {
  if(t1.index() != t2.index()) return false;

  if(auto* w = get_if_unique<WordToken>(&t1)) {
    if(**w != *get_unique<WordToken>(t2)) return false;
  }
  else if(auto* o = get_if_unique<OperToken>(&t1)) {
    if(**o != *get_unique<OperToken>(t2)) return false;
  }
  else if(holds_alternative<unique_ptr<NewlineChar>>(t1))
    return true;
  else if(auto* id = get_if_unique<Ident>(&t1)) {
    if(*id != get_unique<Ident>(t2)) return false;
  }
  else return false;
  return true;
}

static
auto spliceInCat(vector<Template> tv, size_t st, size_t en, Template elt)
  -> vector<Template> {
  if(st == en) Unimplemented("spliceInCat() for empty ranges");
  tv.erase(tv.begin()+st+1, tv.begin()+en);
  tv[st] = std::move(elt);
  return tv;
}

template <class Equal, class Iter> static
Iter rolloutPrefix(Iter prefixBegin, Iter prefixEnd,
                   ssize_t period, Equal eq) {
  if(prefixEnd-prefixBegin <= period) return prefixBegin;
  Iter cur = prefixEnd-period;
  while(cur != prefixBegin) {
    Iter prev = cur; --prev;
    if(!eq(*prev, *(prev+period))) break;
    cur = prev;
  }
  return cur;
}

template <class Equal, class Iter> static
Iter rolloutSuffix(Iter suffixBegin, Iter suffixEnd,
                   ssize_t period, Equal eq) {
  if(suffixEnd-suffixBegin <= period) return suffixEnd;
  Iter cur = suffixBegin+period;
  while(cur != suffixEnd) {
    if(!eq(*(cur-period), *cur)) break;
    ++cur;
  }
  return cur;
}

// Takes two halves and tries gluing them together. Return value is glueLen,
// or -1 if they can't be glued together.
// Gluing always fails if total length is less than two periods.
// Always returns the smallest valid glue.
// Assumes period > 0, and exprBegin < ellipsis < exprEnd.
template <class Equal, class Iter> static
ssize_t tryGluing(Iter exprBegin, Iter ellipsis, Iter exprEnd,
                  ssize_t period, Equal eq) {
  auto equalsOuter = [&](Iter begin, Iter end, ssize_t maxLen) {
    ssize_t t = end-ellipsis-1;
    if(t < maxLen) return std::equal(ellipsis+1, end, begin+maxLen-t, eq);
    else return std::equal(begin, min(begin+maxLen, ellipsis), end-maxLen, eq);
  };
  const ssize_t headLen = ellipsis-exprBegin;
  const ssize_t tailLen = exprEnd-ellipsis-1;
  if(headLen+tailLen < 2*period) return -1;
  for(ssize_t glueLen=0; glueLen<period; ++glueLen) {
    const ssize_t partLen = period-glueLen;
    if(!equalsOuter(exprBegin, exprEnd, partLen)) continue;
    if(headLen <= partLen || tailLen <= partLen) return glueLen;
    if(!equalsOuter(exprBegin+partLen, exprEnd-partLen, glueLen)) continue;
    return glueLen;
  }
  return -1;
}

template <class Iter>
struct RolloutEllipsisResult {
  Iter exprBegin, exprEnd;  // The start and end of the ellipsis expression.

  // [periodBegin, foldPoint) == part, this will never be empty.
  // [foldPoint, periodEnd) == glue, this can be empty.
  // All of these will lie within [exprBegin, exprEnd]
  Iter periodBegin, foldPoint, periodEnd;
  string err;  // All other fields are invalid if !err.empty()
};

// Convenience helper
struct EllipsisError{
  string err;
  template <class Iter> operator RolloutEllipsisResult<Iter>() && {
    return {{}, {}, {}, {}, {}, .err{std::move(this->err)}};
  }
};

template <class Iter>
struct RolloutResultCandidate {
  Iter exprBegin, exprEnd;
  ssize_t period, glueLen;
};

template <class Iter> static
bool subsumes(const RolloutResultCandidate<Iter>& a,
              const RolloutResultCandidate<Iter>& b) {
  return a.exprBegin <= b.exprBegin && a.exprEnd >= b.exprEnd;
}

template <class Iter> static
bool subsumes(const RolloutResultCandidate<Iter>& a,
              const vector<RolloutResultCandidate<Iter>>& bv) {
  for(const auto& b : bv) if(subsumes(b, a)) return true;
  return false;
}

template <class Iter> static
void pushIfUseful(vector<RolloutResultCandidate<Iter>>& v,
                  const RolloutResultCandidate<Iter>& c) {
  if(c.exprEnd-c.exprBegin-1 >= 2*c.period && !subsumes(c, v)) v.push_back(c);
}

template <class Iter> static
void sortCandidates(vector<RolloutResultCandidate<Iter>>& v) {
  using ctype = RolloutResultCandidate<Iter>;
  auto cmp = [](const ctype& a, const ctype& b) {
    ssize_t lena = a.exprEnd - a.exprBegin;
    ssize_t lenb = b.exprEnd - b.exprBegin;
    if(lena != lenb) return lena > lenb;
    else return a.period < b.period;
  };
  sort(v.begin(), v.end(), cmp);
}

// Removes everything subsumed by the first element.
// Assumes !v.empty()
template <class Iter> static
void subsumeIntervals(vector<RolloutResultCandidate<Iter>>& v) {
  auto redundant = [&v](const auto& a) { return subsumes(v[0], a); };
  v.erase(std::remove_if(v.begin()+1, v.end(), redundant), v.end());
}

template <class Equal, class Iter> static
auto rolloutEllipsis(Iter begin, Iter ellipsis, Iter end, Equal eq)
  -> RolloutEllipsisResult<Iter> {
  if(ellipsis == end) Bug("{}: Out of bound error", __func__);
  ssize_t maxp = max(ellipsis-begin, end-ellipsis-1);
  vector<RolloutResultCandidate<Iter>> candidates;
  for(ssize_t p=1; p<=maxp; ++p) {
    RolloutResultCandidate<Iter> cand;
    // TODO limit max exprLen to something like 9*period.
    cand.exprBegin = rolloutPrefix(begin, ellipsis, p, eq);
    cand.exprEnd   = rolloutSuffix(ellipsis+1, end, p, eq);
    cand.period = p;
    if(cand.exprEnd - cand.exprBegin - 1 < 2 * p) continue;
    if(subsumes(cand, candidates)) continue;
    ssize_t glueLen = tryGluing(cand.exprBegin, ellipsis, cand.exprEnd,
                                cand.period, eq);
    if(glueLen == -1) {
      pushIfUseful(candidates, RolloutResultCandidate<Iter>{
          cand.exprBegin, ellipsis+1, cand.period, 0});
      pushIfUseful(candidates, RolloutResultCandidate<Iter>{
          ellipsis, cand.exprEnd, cand.period, 0});
    }else {
      cand.glueLen = glueLen;
      candidates.push_back(cand);
    }
  }
  if(candidates.empty()) {
    if(maxp == 0)
      return EllipsisError{"Nothing to repeat around ellipsis"};
    else
      return EllipsisError{"Every repeating part must appear at least twice"};
  }
  sortCandidates(candidates);
  subsumeIntervals(candidates);
  if(candidates.size() > 1)
    return EllipsisError{"Ambiguous ellipsis"};  // TODO return more details

  // Either we return the only remaining element, or nothing at all.
  const auto& cand = candidates[0];
  if(ellipsis-cand.exprBegin < cand.period)
    return EllipsisError{"Repeating parts need to appear before ellipsis "
                         "as well"};
  if(cand.glueLen && cand.exprEnd-ellipsis-1 < cand.period)
    return EllipsisError{
      "Repeating parts of an infix expression need to appear after "
      "ellipsis as well"
    };
  return {cand.exprBegin, cand.exprEnd, cand.exprBegin,
          cand.exprBegin + cand.period - cand.glueLen,
          cand.exprBegin + cand.period, {}};
}

RolloutEllipsisForTestResult rolloutEllipsisForTest(string s) {
  size_t e = s.find("...");
  if(e == s.npos) Bug("{}: No dash found", __func__);
  s.replace(e, 3, 1, '-');  // This needs to be a single element.
  RolloutEllipsisResult<string::const_iterator> res =
    rolloutEllipsis(s.cbegin(), s.cbegin()+e, s.cend(), std::equal_to<char>());
  string newExpr(res.exprBegin, res.exprEnd);
  e = newExpr.find("-");
  if(e != newExpr.npos) newExpr.replace(e, 1, "...");
  return {.expr{std::move(newExpr)},
          .part{res.periodBegin, res.foldPoint},
          .glue{res.foldPoint, res.periodEnd}, .err{res.err}};
}

static
auto repeatFoldOnEllipsis(InputDiags& ctx, vector<Template> tv)
  -> optional<vector<Template>> {
  auto [idx, tokp] = findEllipsis(tv, 0);
  if(!tokp) return tv;
  auto [idx2, tok2p] = findEllipsis(tv, idx+1);
  if(tok2p)
    return Error(ctx, tok2p->stPos, "Multiple ellipsis are strung together");

  auto at = [&](size_t i) { return tv.begin()+i; };
  size_t lo = atomicSuffixStart(tv, 0, idx);
  size_t hi = atomicPrefixEnd(tv, idx+1, tv.size());
  RolloutEllipsisResult<vector<Template>::iterator> rollout =
    rolloutEllipsis(at(lo), at(idx), at(hi), areTokensAndEqual);
  if(!rollout.err.empty()) return Error(ctx, tokp->stPos, rollout.err);
  auto movetovec = [](auto a, auto b) {
    return vector(move_iterator(a), move_iterator(b));
  };
  size_t cuti = rollout.exprBegin - tv.begin();
  size_t cutj = rollout.exprEnd - tv.begin();
  if(rollout.foldPoint < rollout.periodEnd) {
    TemplateFold tf;
    tf.part = gatherInto<TemplateConcat>(movetovec(rollout.periodBegin,
                                                   rollout.foldPoint));
    tf.glue = gatherInto<TemplateConcat>(movetovec(rollout.foldPoint,
                                                   rollout.periodEnd));
    return spliceInCat(std::move(tv), cuti, cutj, move_to_unique(tf));
  }else {
    auto part = gatherInto<TemplateConcat>(movetovec(rollout.periodBegin,
                                                     rollout.periodEnd));
    return spliceInCat(std::move(tv), cuti, cutj,
                       move_to_unique(TemplateRepeat{.part{std::move(part)}}));
  }
}

auto templatize(InputDiags& ctx, vector<TokenOrPart> tops)
  -> optional<Template> {
  if(tops.empty()) Bug("{} was not expecting an empty template", __func__);

  // Parsing stack: first entry is to construct the root node, while the
  // rest (if any) are for pending '[' brackets.
  // At every nesting level, we have some possibly empty list of
  // '|'-connected ORs, with concatenation still ongoing on the final branch.
  vector<pair<TemplateOrList,TemplateConcat>> openopts(1);

  // Helpers
  auto prevbranches = [&]()->auto& { return openopts.back().first.parts; };
  auto curbranch    = [&]()->auto& { return openopts.back().second.parts; };
  auto close_curbranch = [&](size_t closepos) {
    const char outerOptHint[] =
      "Empty '|' branch is not supported, "
      "make this pattern optional in parent rules instead.";
    const char innerOptHint[] =
      "Empty '|' branch is unnecessary, the group is already optional";
    if(curbranch().empty()) {
      const char* errmsg = openopts.size() > 1 ? innerOptHint : outerOptHint;
      Error(ctx, closepos, errmsg);
      return false;
    }else {
      optional<vector<Template>> branch
        = repeatFoldOnEllipsis(ctx, std::move(curbranch()));
      if(!branch.has_value()) return false;
      prevbranches().push_back(gatherInto<TemplateConcat>(std::move(*branch)));
      curbranch().clear();
      return true;
    }
  };

  size_t lastPush = 0;  // Only used for error-reporting.
  size_t lasttok = 0;
  for(auto& part : tops) {
    auto [meta, tokstart] = getIfMetaToken(part);
    lasttok = tokstart;
    if(meta.empty() || meta == "...") {
      visit([&](auto& x){ curbranch().push_back(move_to_unique(x)); },
            part);
    }else if(meta == "[") {
      openopts.emplace_back();
      lastPush = tokstart;
    }else if(meta == "|") {
      if(!close_curbranch(tokstart)) return nullopt;
    }else if(meta == "]") {
      if(curbranch().empty() && prevbranches().empty())
        return Error(ctx, tokstart, "Empty '[]' not allowed");
      if(openopts.size() == 1) return Error(ctx, tokstart, "Unmatched ']'");
      if(!close_curbranch(tokstart)) return nullopt;
      Template tmpl = gatherInto<TemplateOrList>(std::move(prevbranches()));
      openopts.pop_back();
      curbranch().push_back(
          move_to_unique(TemplateOptional{std::move(tmpl)})
      );
    }else Bug("Found unknown metacharacter '{}'", meta);
  }
  if(openopts.size() > 1) return Error(ctx, lastPush, "Unmatched '['");
  if(!close_curbranch(lasttok)) return nullopt;
  else return gatherInto<TemplateOrList>(std::move(prevbranches()));
}

}  // namespace oalex
