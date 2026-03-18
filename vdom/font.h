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

#define NUM_FONTS 73

#include "font/06_cg_pixel_3x5_mono.h"
#include "font/06_cg_pixel_4x5_mono.h"
#include "font/07_bitocra.h"
#include "font/07_f4x6.h"
#include "font/07_tom_thumb.h"
#include "font/08_ecran_monochrome.h"
#include "font/08_f5x7.h"
#include "font/08_f5x8.h"
#include "font/09_f6x9.h"
#include "font/09_monogram_extended.h"
#include "font/09_phil_ui_tiny.h"
#include "font/09_spleen_5x8.h"
#include "font/09_unscii_alt.h"
#include "font/09_unscii_thin.h"
#include "font/09_unscii.h"
#include "font/10_cherry_400.h"
#include "font/10_cherry_700.h"
#include "font/10_dina_regular_400.h"
#include "font/10_f6x10.h"
#include "font/10_limey.h"
#include "font/10_monospaced_serif.h"
#include "font/10_semifraktur_monospace.h"
#include "font/10_six_twelve_mono.h"
#include "font/11_berry_400.h"
#include "font/11_berry_700.h"
#include "font/11_bitbuntu.h"
#include "font/11_bitocra.h"
#include "font/11_haxormedium.h"
#include "font/12_clr6x12.h"
#include "font/12_creep.h"
#include "font/12_f6x12.h"
#include "font/12_lemon_j.h"
#include "font/12_lemon.h"
#include "font/12_lemon2.h"
#include "font/12_monocle_fixed.h"
#include "font/12_scientifica_italic_400.h"
#include "font/12_scientifica_regular_400.h"
#include "font/12_scientifica_regular_700.h"
#include "font/12_terminus_400.h"
#include "font/12_terminus_700.h"
#include "font/13_dina_italic_400.h"
#include "font/13_dina_italic_700.h"
#include "font/13_dina_regular_400.h"
#include "font/13_dina_regular_700.h"
#include "font/13_f6x13.h"
#include "font/13_f6x13b.h"
#include "font/13_f6x13o.h"
#include "font/13_f7x13.h"
#include "font/13_f7x13b.h"
#include "font/13_f7x13o.h"
#include "font/13_f8x13.h"
#include "font/13_f8x13b.h"
#include "font/13_f8x13o.h"
#include "font/13_haxormedium.h"
#include "font/14_f7x14.h"
#include "font/14_f7x14b.h"
#include "font/14_haxormedium.h"
#include "font/14_terminus_400.h"
#include "font/14_terminus_700.h"
#include "font/14_terminus_v.h"
#include "font/15_dina_italic_400.h"
#include "font/15_dina_italic_700.h"
#include "font/15_dina_regular_400.h"
#include "font/15_dina_regular_700.h"
#include "font/16_dina_italic_400.h"
#include "font/16_dina_italic_700.h"
#include "font/16_dina_regular_400.h"
#include "font/16_dina_regular_700.h"
#include "font/16_spleen_8x16.h"
#include "font/17_haxornarrow.h"
#include "font/18_f9x18.h"
#include "font/18_f9x18b.h"
#include "font/21_haxornarrow.h"

const FontMono1B* const fonts[NUM_FONTS] = {
  &font_06_cg_pixel_3x5_mono,
  &font_06_cg_pixel_4x5_mono,
  &font_07_bitocra,
  &font_07_f4x6,
  &font_07_tom_thumb,
  &font_08_ecran_monochrome,
  &font_08_f5x7,
  &font_08_f5x8,
  &font_09_f6x9,
  &font_09_monogram_extended,
  &font_09_phil_ui_tiny,
  &font_09_spleen_5x8,
  &font_09_unscii_alt,
  &font_09_unscii_thin,
  &font_09_unscii,
  &font_10_cherry_400,
  &font_10_cherry_700,
  &font_10_dina_regular_400,
  &font_10_f6x10,
  &font_10_limey,
  &font_10_monospaced_serif,
  &font_10_semifraktur_monospace,
  &font_10_six_twelve_mono,
  &font_11_berry_400,
  &font_11_berry_700,
  &font_11_bitbuntu,
  &font_11_bitocra,
  &font_11_haxormedium,
  &font_12_clr6x12,
  &font_12_creep,
  &font_12_f6x12,
  &font_12_lemon_j,
  &font_12_lemon,
  &font_12_lemon2,
  &font_12_monocle_fixed,
  &font_12_scientifica_italic_400,
  &font_12_scientifica_regular_400,
  &font_12_scientifica_regular_700,
  &font_12_terminus_400,
  &font_12_terminus_700,
  &font_13_dina_italic_400,
  &font_13_dina_italic_700,
  &font_13_dina_regular_400,
  &font_13_dina_regular_700,
  &font_13_f6x13,
  &font_13_f6x13b,
  &font_13_f6x13o,
  &font_13_f7x13,
  &font_13_f7x13b,
  &font_13_f7x13o,
  &font_13_f8x13,
  &font_13_f8x13b,
  &font_13_f8x13o,
  &font_13_haxormedium,
  &font_14_f7x14,
  &font_14_f7x14b,
  &font_14_haxormedium,
  &font_14_terminus_400,
  &font_14_terminus_700,
  &font_14_terminus_v,
  &font_15_dina_italic_400,
  &font_15_dina_italic_700,
  &font_15_dina_regular_400,
  &font_15_dina_regular_700,
  &font_16_dina_italic_400,
  &font_16_dina_italic_700,
  &font_16_dina_regular_400,
  &font_16_dina_regular_700,
  &font_16_spleen_8x16,
  &font_17_haxornarrow,
  &font_18_f9x18,
  &font_18_f9x18b,
  &font_21_haxornarrow,
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
