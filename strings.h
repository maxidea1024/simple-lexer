#pragma once

#include <string.h>
#include <string>
#include <vector>

class Strings {
 public:
  static Strings& instance();

  Strings();
  ~Strings();

  const char* Add(const char* str);
  const char* Add(const char* str, size_t length);

 private:
  std::vector<std::string> registry_;
};

