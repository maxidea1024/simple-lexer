#include "value.h"
#include "strings.h"

const Value Value::NONE;

Value::Value(const char* str) : type(VALUE_STRING) {
    val.string_ = Strings::instance().Add(str);
}

Value::Value(const char* str, size_t length) : type(VALUE_STRING) {
    val.string_ = Strings::instance().Add(str, length);
}
