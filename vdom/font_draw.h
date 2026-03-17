#ifndef FONT_DRAW_H
#define FONT_DRAW_H

#include <stdint.h>
#include "font.h"
#include "globals.h"

/* Draw glyph only inside the rectangle [rx0,ry0]..[rx1,ry1] (inclusive). */
static inline void drawCharInRect(int font_index, int x0, int y0, char c, uint8_t color,
    int rx0, int ry0, int rx1, int ry1) {
  if (!font_hasChar(font_index, c))
    return;
  const FontMono1B* font = fonts[font_index];
  int idx = (unsigned char)c - font->ascii_first;
  const unsigned char *glyph = font->bits + (unsigned long)idx * font->glyph_h;

  int y_top = y0 + font->extra_line_height;
  for (int row = 0; row < font->glyph_h; row++) {
    for (int col = 0; col < font->glyph_w; col++) {
      int px = x0 + col;
      int py = y_top + row;
      if (px >= rx0 && px <= rx1 && py >= ry0 && py <= ry1 &&
          px >= 0 && px < video.xres && py >= 0 && py < video.yres) {
        int byteIdx = row;
        int bitIdx  = 7 - (col % 8);
        if ((glyph[byteIdx] >> bitIdx) & 1)
          video.dotFast(px, py, color);
      }
    }
  }
}

static inline int charAdvance(int font_index, char c) {
  if (c == '\n' || !font_hasChar(font_index, c)) return 0;
  return fonts[font_index]->glyph_w;
}

#endif
