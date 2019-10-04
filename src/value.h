#pragma once

#include <stdint.h>
#include <string>

enum ValueType {
  VALUE_NONE,
  VALUE_INTEGER,
  VALUE_FLOAT,
  VALUE_STRING,
};

struct Value {
  ValueType type;
  union {
    int64_t integer_;
    double float_;
    const char* string_;
    int64_t largest_;
  } val;

  Value() : type(VALUE_NONE) { val.largest_ = 0; }

  Value(const Value& rhs) : type(rhs.type) { val.largest_ = rhs.val.largest_; }

  Value(int64_t i) : type(VALUE_INTEGER) { val.integer_ = i; }

  Value(float f) : type(VALUE_FLOAT) { val.float_ = f; }

  Value(double d) : type(VALUE_FLOAT) { val.float_ = d; }

  Value(const char* str);
  Value(const char* str, size_t length);

  Value& operator=(const Value& rhs) {
    type = rhs.type;
    val.largest_ = rhs.val.largest_;
    return *this;
  }

  bool operator==(const Value& rhs) const {
    return type == rhs.type && val.largest_ == rhs.val.largest_;
  }

  bool operator!=(const Value& rhs) const { return !(*this == rhs); }

  std::string ToString() const;

  static const Value NONE;
};
