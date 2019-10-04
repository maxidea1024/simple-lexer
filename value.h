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
    } val;
};

#define IS_NONE(v)     ((v).type == VALUE_NONE)
#define IS_INTEGER(v)  ((v).type == VALUE_INTEGER)
#define IS_FLOAT(v)    ((v).type == VALUE_FLOAT)
#define IS_STRING(v)   ((v).type == VALUE_STRING)

#define AS_INTEGER(v)  ((v).val.integer_)
#define AS_FLOAT(v)    ((v).val.float_)
#define AS_STRING(v)   ((v).val.string_)

//#define VALUE_NONE(v)  
