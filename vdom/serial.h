#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "prelude.h"

// Parsing numbers from u8*

static inline uint32_t parse_u32_le(const uint8_t *p) {
  return (uint32_t)p[0]
      | ((uint32_t)p[1] << 8) 
      | ((uint32_t)p[2] << 16) 
      | ((uint32_t)p[3] << 24);
}
static inline uint16_t parse_u16_le(const uint8_t *p) {
  return (uint16_t)p[0] 
      | ((uint16_t)p[1] << 8);
}
static inline int32_t parse_i32_le(const uint8_t *p) {
  return (int32_t)parse_u32_le(p);
}
static inline int16_t parse_i16_le(const uint8_t *p) {
  return (int16_t)parse_u16_le(p);
}

// Reading numbers from Serial

static inline uint8_t read_u8() {
  while (!Serial.available()) {}
  return (uint8_t)Serial.read();
}
static inline uint32_t read_le(int nbytes) {
  uint32_t v = 0;
  for (int i = 0; i < nbytes; ++i) {
    v |= ((uint32_t)read_u8()) << (i * 8);
  }
  return v;
}
static inline uint32_t read_u32_le() { return read_le(4); }
static inline uint16_t read_u16_le() { return (uint16_t)read_le(2); }
static inline int32_t read_i32_le() { return (int32_t)read_u32_le(); }
static inline int16_t read_i16_le() { return (int16_t)read_u16_le(); }

// Writing numbers to Serial

static inline void write_u8(uint8_t v) {
  Serial.write(v);
}
static inline void write_u16_le(uint16_t v) {
  Serial.write((uint8_t)(v & 0xFF));
  Serial.write((uint8_t)((v >> 8) & 0xFF));
}
static inline void write_u32_le(uint32_t v) {
  Serial.write((uint8_t)(v & 0xFF));
  Serial.write((uint8_t)((v >> 8) & 0xFF));
  Serial.write((uint8_t)((v >> 16) & 0xFF));
  Serial.write((uint8_t)((v >> 24) & 0xFF));
}
static inline void write_i32_le(int32_t v) {
  write_u32_le((uint32_t)v);
}

static inline void write_sized_string(const char* s) {
  uint16_t len = (uint16_t)strlen(s);
  write_u16_le(len);
  Serial.write((const uint8_t*)s, len);
}

// Sized list of uint8: length (u16 LE) then that many bytes.
static inline void write_sized_u8_list(const uint8_t* data, uint16_t count) {
  write_u16_le(count);
  Serial.write(data, count);
}

// Length-prefixed (u16 LE) string: reads length, allocates len+1, reads bytes, NUL-terminates. Caller must free.
static inline char* read_sized_string(void) {
  uint16_t len = read_u16_le();
  if (len == 0) return NULL;
  if (len < 0) {
    complain("read_sized_string: negative string length");
    return NULL;
  }
  char* buf = (char*)malloc((size_t)len + 1);
  if (!buf) {
    complain("read_sized_string: malloc failed");
    return NULL;
  }
  for (uint16_t i = 0; i < len; ++i)
    buf[i] = (char)read_u8();
  buf[len] = '\0';
  return buf;
}