#include "strings.h"

Strings& Strings::instance() {
  static Strings inst;
  return inst;
}

Strings::Strings() {}

Strings::~Strings() {}

const char* Strings::Add(const char* str) { return Add(str, strlen(str)); }

const char* Strings::Add(const char* str, size_t length) {
  for (auto& s : registry_) {
    if (s.length() == length) {
      if (memcmp(s.c_str(), str, length) == 0) {
        return s.c_str();
      }
    }
  }

  std::string new_str(str, length);
  registry_.push_back(new_str);

  return registry_.back().c_str();
}
