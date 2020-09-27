#pragma once

namespace oalex {

inline bool is_in(char ch, const char s[]) {
  const char* c = strchr(s, ch);
  return c && *c;
}

}
