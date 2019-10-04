#pragma once

#include <string.h>
#include <string>
#include <vector>

class Strings {
 public:
  static const char* Add(const char* str);
  static const char* Add(const char* str, size_t length);

 private:
  static std::vector<std::string> registry_;
};
