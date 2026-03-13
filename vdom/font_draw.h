#ifndef FONT_DRAW_H
#define FONT_DRAW_H

#include <stdint.h>
#include "font.h"
#include "globals.h"

static inline void drawChar(const FontMono1B *font, int x0, int y0, char c, uint8_t color) {
  if (!font_hasChar(font, c))
    return;
  int idx = (unsigned char)c - font->ascii_first;
  const unsigned char *glyph = font->bits + (unsigned long)idx * font->stride * font->glyph_h;

  for (int row = 0; row < font->glyph_h; row++) {
    for (int col = 0; col < font->glyph_w; col++) {
      int byteIdx = row * font->stride + col / 8;
      int bitIdx  = 7 - (col % 8);
      if ((glyph[byteIdx] >> bitIdx) & 1)
        video.dotFast(x0 + col, y0 + row, color);
    }
  }
}

static inline int charAdvance(const FontMono1B *font, char c) {
  if (c == '\n' || !font_hasChar(font, c))
    return 0;
  return font->glyph_w;
}

#endif
