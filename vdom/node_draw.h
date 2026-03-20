#pragma once

#include "node.h"
#include "globals.h"
#include "graphics_extra.h"
#include "prelude.h"
#include "font_draw.h"

void node_draw_tileXLine_inner(int x0, int x1, int y, uint8_t color, int tx0, int ty0) {
  int tx1 = tx0 + TILE_SIZE;
  int ty1 = ty0 + TILE_SIZE;
  // only draw if tile is not to the...
  if (tx1 >= x0 && tx0 <= x1 && ty1 >= y && ty0 <= y) {
    // right        left         top         bottom
    // of the line
    int cx0 = MAX(x0, tx0); // clamped x0
    int cx1 = MIN(x1, tx1); // clamped x1 (inclusive; esp32lib xLine uses half-open x2)
    int x2 = cx1 + 1;
    if (x2 > video.xres)
      x2 = video.xres;
    video.xLine(cx0, x2, y, color);
  }
}

void node_draw_tileYLine_inner(int y0, int y1, int x, uint8_t color, int tx0, int ty0) {
  int tx1 = tx0 + TILE_SIZE;
  int ty1 = ty0 + TILE_SIZE;
  // only draw if tile is not to the...
  if (tx1 >= x && tx0 <= x && ty1 >= y0 && ty0 <= y1) {
    // right       left        top          bottom
    // of the line
    int cy0 = MAX(y0, ty0); // clamped y0
    int cy1 = MIN(y1, ty1); // clamped y1
    yLine(cy0,cy1,x,color);
  }
}

void node_draw_tileRect(Node* node, int tx0, int ty0) {
  uint8_t color = node->u.rect.color;
  int rx0 = node->u.rect.x;
  int ry0 = node->u.rect.y;
  int rx1 = rx0 + node->u.rect.w - 1;
  int ry1 = ry0 + node->u.rect.h - 1;
  node_draw_tileXLine_inner(rx0,rx1,ry0,color,tx0,ty0);
  node_draw_tileXLine_inner(rx0,rx1,ry1,color,tx0,ty0);
  node_draw_tileYLine_inner(ry0,ry1,rx0,color,tx0,ty0);
  node_draw_tileYLine_inner(ry0,ry1,rx1,color,tx0,ty0);
}

void node_draw_tileRectFill(Node* node, int tx0, int ty0) {
  int tx1 = tx0 + TILE_SIZE;
  int ty1 = ty0 + TILE_SIZE;
  int rx0 = MAX(node->u.rect.x, tx0);
  int ry0 = MAX(node->u.rect.y, ty0);
  int rx1 = MIN(node->u.rect.x + node->u.rect.w, tx1);
  int ry1 = MIN(node->u.rect.y + node->u.rect.h, ty1);
  if (rx1 > rx0 && ry1 > ry0) 
    video.fillRect(rx0, ry0, rx1 - rx0, ry1 - ry0, node->u.rect.color);
}

void node_draw_tileXLine(Node* node, int tx0, int ty0) {
  int y = node->u.xline.y;
  int x0 = node->u.xline.x;
  int x1 = x0 + node->u.xline.len - 1;
  uint8_t color = node->u.xline.color;
  node_draw_tileXLine_inner(x0, x1, y, color, tx0, ty0);
}

void node_draw_tileYLine(Node* node, int tx0, int ty0) {
  int x = node->u.yline.x;
  int y0 = node->u.yline.y;
  int y1 = y0 + node->u.yline.len - 1;
  uint8_t color = node->u.yline.color;
  node_draw_tileYLine_inner(y0, y1, x, color, tx0, ty0);
}

void node_draw_tileText(Node* node, int tx0, int ty0) {
  if (node->u.text.text == nullptr)
    return;
  const char* text = node->u.text.text;
  int fi = node->u.text.font_index;
  uint8_t color = node->u.text.color;
  int x = node->u.text.x;
  int y = node->u.text.y;
  int w = node->bbox.w;
  int h = node->bbox.h;

  int tx1 = tx0 + TILE_SIZE - 1;
  int ty1 = ty0 + TILE_SIZE - 1;

  // Fast reject: if the text bbox doesn't intersect the tile at all, skip drawing
  if (x + w < tx0 || x > tx1 || y + h < ty0 || y > ty1)
    return;

  int px = x;
  int line_y = y;
  for (const char* p = text; *p; ++p) {
    if (*p == '\n') {
      line_y += fonts[fi]->glyph_h + fonts[fi]->extra_line_height;
      px = x;
      continue;
    }
    int adv = charAdvance(fi, *p);
    int cx0 = px;
    int cy0 = line_y;
    int draw_y0 = cy0 + fonts[fi]->extra_line_height;
    int cx1 = cx0 + fonts[fi]->glyph_w - 1;
    int cy1 = draw_y0 + fonts[fi]->glyph_h - 1;
    if (!(cx1 < tx0 || cx0 > tx1 || cy1 < ty0 || draw_y0 > ty1)) {
      drawCharInRect(fi, cx0, cy0, *p, color, tx0, ty0, tx1, ty1);
    }
    px += adv;
  }
}

static inline uint8_t bitmap_tile_pixel_gray_1(const uint8_t* data, size_t byte_len, int pixel_index) {
  if (!data || pixel_index < 0) {
    return 0;
  }
  size_t byte_index = (size_t)pixel_index >> 3;
  if (byte_index >= byte_len) {
    return 0;
  }
  int shift = 7 - (pixel_index & 7);
  uint8_t sample = (uint8_t)((data[byte_index] >> shift) & 0x01);
  return sample ? 255 : 0;
}

static inline uint8_t bitmap_tile_pixel_gray_2(const uint8_t* data, size_t byte_len, int pixel_index) {
  if (!data || pixel_index < 0) {
    return 0;
  }
  size_t byte_index = (size_t)pixel_index >> 2;
  if (byte_index >= byte_len) {
    return 0;
  }
  int shift = 6 - ((pixel_index & 3) * 2);
  uint8_t sample = (uint8_t)((data[byte_index] >> shift) & 0x03);
  return (uint8_t)(sample * 85);
}

static inline uint8_t bitmap_tile_pixel_gray_4(const uint8_t* data, size_t byte_len, int pixel_index) {
  if (!data || pixel_index < 0) {
    return 0;
  }
  size_t byte_index = (size_t)pixel_index >> 1;
  if (byte_index >= byte_len) {
    return 0;
  }
  int shift = (pixel_index & 1) == 0 ? 4 : 0;
  uint8_t sample = (uint8_t)((data[byte_index] >> shift) & 0x0F);
  return (uint8_t)(sample * 17);
}

static inline uint8_t bitmap_tile_pixel_gray_8(const uint8_t* data, size_t byte_len, int pixel_index) {
  if (!data || pixel_index < 0) {
    return 0;
  }
  size_t byte_index = (size_t)pixel_index;
  if (byte_index >= byte_len) {
    return 0;
  }
  return data[byte_index];
}

void node_draw_tileBitmap(Node* node, int tx0, int ty0) {
  const uint8_t* data = node->u.bitmap.data;
  if (!data && node->u.bitmap.byte_len > 0) {
    return;
  }

  int tx1 = tx0 + TILE_SIZE;
  int ty1 = ty0 + TILE_SIZE;
  int bx0 = MAX(node->u.bitmap.x, tx0);
  int by0 = MAX(node->u.bitmap.y, ty0);
  int bx1 = MIN(node->u.bitmap.x + node->u.bitmap.w, tx1);
  int by1 = MIN(node->u.bitmap.y + node->u.bitmap.h, ty1);
  if (bx1 <= bx0 || by1 <= by0) {
    return;
  }

  const size_t byte_len = node->u.bitmap.byte_len;
  const int bmp_x = node->u.bitmap.x;
  const int bmp_y = node->u.bitmap.y;
  const int bmp_w = node->u.bitmap.w;

  switch (node->u.bitmap.bit_depth) {
    case 1:
      for (int py = by0; py < by1; ++py) {
        int src_y = py - bmp_y;
        for (int px = bx0; px < bx1; ++px) {
          int pixel_index = src_y * bmp_w + (px - bmp_x);
          video.dotFast(px, py, bitmap_tile_pixel_gray_1(data, byte_len, pixel_index));
        }
      }
      break;
    case 2:
      for (int py = by0; py < by1; ++py) {
        int src_y = py - bmp_y;
        for (int px = bx0; px < bx1; ++px) {
          int pixel_index = src_y * bmp_w + (px - bmp_x);
          video.dotFast(px, py, bitmap_tile_pixel_gray_2(data, byte_len, pixel_index));
        }
      }
      break;
    case 4:
      for (int py = by0; py < by1; ++py) {
        int src_y = py - bmp_y;
        for (int px = bx0; px < bx1; ++px) {
          int pixel_index = src_y * bmp_w + (px - bmp_x);
          video.dotFast(px, py, bitmap_tile_pixel_gray_4(data, byte_len, pixel_index));
        }
      }
      break;
    case 8:
      for (int py = by0; py < by1; ++py) {
        int src_y = py - bmp_y;
        for (int px = bx0; px < bx1; ++px) {
          int pixel_index = src_y * bmp_w + (px - bmp_x);
          video.dotFast(px, py, bitmap_tile_pixel_gray_8(data, byte_len, pixel_index));
        }
      }
      break;
    default:
      complain("node_draw_tileBitmap: unsupported bit depth");
      break;
  }
}
