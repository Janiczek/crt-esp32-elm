#ifndef FONT_DRAW_H
#define FONT_DRAW_H

#include <stdint.h>
#include "font.h"
#include "globals.h"

static inline void drawChar(const FontMono1B *font, int x0, int y0, char c, uint8_t color) {
  if (!font_hasChar(font, c))
    return;
  int idx = (unsigned char)c - font->ascii_first;
  const unsigned char *glyph = font->bits + (unsigned long)idx * font->glyph_h;

  int y_top = y0 + font->extra_line_height;
  for (int row = 0; row < font->glyph_h; row++) {
    for (int col = 0; col < font->glyph_w; col++) {
      int px = x0 + col;
      int py = y_top + row;
      if (px >= 0 && px < video.xres && py >= 0 && py < video.yres) {
        int byteIdx = row;
        int bitIdx  = 7 - (col % 8);
        if ((glyph[byteIdx] >> bitIdx) & 1)
          video.dotFast(px, py, color);
      }
    }
  }
}

static inline int charAdvance(const FontMono1B *font, char c) {
  if (c == '\n' || !font_hasChar(font, c))
    return 0;
  return font->glyph_w;
}

#endif
