#pragma once

#include <stdbool.h>
#include <stdint.h>

typedef struct FontMono1B {
  const char* name;
  uint16_t ascii_first;
  uint16_t ascii_last;
  uint16_t num_glyphs;
  uint8_t glyph_w;  // <= 8, so as to fit in a byte
  uint8_t glyph_h;
  uint8_t extra_line_height;  // extra rows at top of each line when drawing (0 or 1)
  const unsigned char *bits;
} FontMono1B;

#define NUM_FONTS 4

#include "font/cg_pixel_4x5_mono_5.h"
#include "font/f5x7_7.h"
#include "font/spleen_5x8_8.h"
#include "font/limey_10.h"

const FontMono1B* const fonts[NUM_FONTS] = {
  &font_cg_pixel_4x5_mono_5,
  &font_f5x7_7,
  &font_spleen_5x8_8,
  &font_limey_10,
};

static inline uint16_t font_bits_byte_len(const FontMono1B* font) {
  return (uint16_t)(font->num_glyphs * font->glyph_h * ((font->glyph_w + 7) / 8));
}

static inline bool font_hasChar(int font_index, char c) {
  const FontMono1B* font = fonts[font_index];
  unsigned char u = (unsigned char)c;
  return u >= font->ascii_first && u <= font->ascii_last;
}

static inline void font_textMultilineSize(const char* s, int font_index, int* out_w, int* out_h) {
  const FontMono1B* font = fonts[font_index];
  int max_w = 0;
  int line_w = 0;
  int line_count = 1;
  for (const char* p = s; *p; ++p) {
    if (*p == '\n') {
      if (line_w > max_w) max_w = line_w;
      line_w = 0;
      line_count++;
    } else {
      if (font_hasChar(font_index, *p))
        line_w += font->glyph_w;
    }
  }
  if (line_w > max_w) max_w = line_w;
  *out_w = max_w;
  *out_h = line_count * (font->glyph_h + font->extra_line_height);
}