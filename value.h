#pragma once

#include <stdint.h>

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
        void* primary_;
    } val;

    Value() : type(VALUE_NONE) {
        val.primary_ = 0;
    }

    Value(const Value& rhs) : type(rhs.type) {
        val.primary_ = rhs.val.primary_;
    }

    Value(int64_t i) : type(VALUE_INTEGER) {
        val.integer_ = i;
    }

    Value(float f) : type(VALUE_FLOAT) {
        val.float_ = f;
    }

    Value(double d) : type(VALUE_FLOAT) {
        val.float_ = d;
    }

    Value(const char* str);
    Value(const char* str, size_t length);

    Value& operator = (const Value& rhs) {
        type = rhs.type;
        val.primary_ = rhs.val.primary_;
        return *this;
    }

    bool operator == (const Value& rhs) const {
        return type == rhs.type && val.primary_ == rhs.val.primary_;
    }

    bool operator != (const Value& rhs) const {
        return !(*this == rhs);
    }

    static const Value NONE;
};

#define IS_NONE(v)     ((v).type == VALUE_NONE)
#define IS_INTEGER(v)  ((v).type == VALUE_INTEGER)
#define IS_FLOAT(v)    ((v).type == VALUE_FLOAT)
#define IS_STRING(v)   ((v).type == VALUE_STRING)

#define AS_INTEGER(v)  ((v).val.integer_)
#define AS_FLOAT(v)    ((v).val.float_)
#define AS_STRING(v)   ((v).val.string_)
#define AS_BOOL(v)     (AS_INTEGER(v) != 0)
