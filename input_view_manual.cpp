#include "input_view_manual.h"
#include "util.h"

namespace oalex {

void Input::peekMore() {
  if(endSeen()) return;
  int16_t ch=getch_();
  if(ch < 0) size_=buf_.size()+pos_;
  else buf_+=ch;
}
// operator[](x) will no longer be valid for x < begin.
void Input::forgetBefore(size_t begin) {
  while(!endSeen() && begin >= pos_+buf_.size()) peekMore();
  // No point saving memory anymore.
  if(endSeen()) return;
  // Check if we've already forgotten this part.
  if(begin <= pos_) return;
  buf_.erase(buf_.begin(),buf_.begin()+(begin-pos_));
  pos_=begin;
}

char Input::operator[](size_t i) {
  if(i < pos_) BugDie()<<"Out of bound error, already forgotten "<<i<<'.';
  while(!endSeen() && i+1-pos_<buf_.size()) peekMore();
  if(i-pos_ < buf_.size()) return buf_[i-pos_];
  BugDie()<<"Out of bound error. "<<i<<" is beyond the end of input.";
}

}  // namespace oalex
