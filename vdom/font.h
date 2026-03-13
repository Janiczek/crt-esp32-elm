#pragma once

typedef struct FontMono1B {
  const char* name;
  unsigned char ascii_first;
  unsigned char ascii_last;
  int num_glyphs;
  int glyph_w;
  int glyph_h;
  int stride;
  const unsigned char *bits;
} FontMono1B;

static inline int font_textWidth(const char* s, const FontMono1B* font) {
  int n = 0;
  while (s[n]) n++;
  return n * font->glyph_w;
}
