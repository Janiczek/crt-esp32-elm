#pragma once

#include <stddef.h>
#include <stdint.h>

// Incremental 32-bit FNV-1a helpers (same algorithm as hash()).
static uint32_t hash_init = 2166136261UL;
static inline uint32_t hash_step(uint32_t h, uint8_t b) { return (h ^ b) * 16777619UL; }

static inline uint32_t hash_update(uint32_t h, const void* data_any, size_t size) {
  const uint8_t* data = (const uint8_t*)data_any;
  for (size_t i = 0; i < size; i++) h = hash_step(h, data[i]);
  return h;
}

static inline uint32_t hash_update_u8(uint32_t h, uint8_t v) {
  return hash_step(h, v);
}

static inline uint32_t hash_update_u32(uint32_t h, uint32_t v) {
  h = hash_step(h, (uint8_t)(v));
  h = hash_step(h, (uint8_t)(v >> 8));
  h = hash_step(h, (uint8_t)(v >> 16));
  h = hash_step(h, (uint8_t)(v >> 24));
  return h;
}

static inline uint32_t hash_update_cstr(uint32_t h, const char* s) {
  for (size_t i = 0; s[i]; i++) h = hash_step(h, (uint8_t)s[i]);
  return h;
}
