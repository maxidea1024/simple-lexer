#pragma once

#include <stdint.h>

int Utf8EncodeNumBytes(int value);
int Utf8Encode(int value, uint8_t* bytes);
int Utf8Decode(const uint8_t* bytes, uint32_t length);
int Utf8DecodeNumBytes(uint8_t byte);
