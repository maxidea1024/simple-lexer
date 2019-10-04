#include "value.h"
#include "strings.h"

const Value Value::NONE;

Value::Value(const char* str) : type(VALUE_STRING) {
    val.string_ = Strings::instance().Add(str);
}

Value::Value(const char* str, size_t length) : type(VALUE_STRING) {
    val.string_ = Strings::instance().Add(str, length);
}

std::string Value::ToString() const {
  switch (type) {
    case VALUE_NONE:
      return std::string("none");

    case VALUE_INTEGER: {
      char buf[64];
      snprintf(buf, 64, "%lld", val.integer_);
      return std::string(buf);
    }

    case VALUE_FLOAT: {
      char buf[64];
      snprintf(buf, 64, "%g", val.float_);
      return std::string(buf);
    }

    case VALUE_STRING:
      return std::string(val.string_);
  }

  return std::string("");
}
