#pragma once
#include <functional>
#include <limits>
#include <string>

namespace oalex {

// input_view.h seemed too complicated for a simple recursive descent parser.
// We try an alternative abstraction here, where we access it like a normal
// string, but it only keeps a smallish substring in memory. Any time we use
// operator[], it loads new characters into buf_ if necessary. It only forgets
// parts of it when explicitly asked to.
//
// It always tries to keep one extra character beyond what was already read with
// operator[], so that the following loop will work:
//
//   for(size_t i=0;i<input.size();++i) { process(input[i]); }
//
// input.size() will change from npos to the real size as soon as the last
// character is accessed.
class Input {
 public:
  static constexpr auto npos = std::numeric_limits<size_t>::max();
  explicit Input(std::function<int16_t()> getch)
    : getch_(getch), pos_(0), size_(npos) { peekMore(); }
  void forgetBefore(size_t begin);
  char operator[](size_t sz);

 private:
  std::string buf_;
  std::function<int16_t()> getch_;
  size_t pos_,size_;

  void peekMore();
  bool endSeen() const { return size_ < npos; }
};

}  // namespace oalex
