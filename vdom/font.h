#pragma once

#include <stdbool.h>

typedef struct FontMono1B {
  const char* name;
  unsigned char ascii_first;
  unsigned char ascii_last;
  int num_glyphs;
  int glyph_w; // <= 8, so as to fit in a byte
  int glyph_h;
  const unsigned char *bits;
} FontMono1B;

static inline bool font_hasChar(const FontMono1B* font, char c) {
  unsigned char u = (unsigned char)c;
  return u >= font->ascii_first && u <= font->ascii_last;
}

static inline int font_textWidth(const char* s, const FontMono1B* font) {
  int n = 0;
  while (s[n]) n++;
  return n * font->glyph_w;
}

static inline void font_textMultilineSize(const char* s, const FontMono1B* font, int* out_w, int* out_h) {
  int max_w = 0;
  int line_w = 0;
  int line_count = 1;
  for (const char* p = s; *p; ++p) {
    if (*p == '\n') {
      if (line_w > max_w) max_w = line_w;
      line_w = 0;
      line_count++;
    } else {
      if (font_hasChar(font, *p))
        line_w += font->glyph_w;
    }
  }
  if (line_w > max_w) max_w = line_w;
  *out_w = max_w;
  *out_h = line_count * font->glyph_h;
}
