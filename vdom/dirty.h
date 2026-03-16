#pragma once

#include <string.h>
#include <stdint.h>

#include "constants.h"
#include "bbox.h"

#define TILE_SIZE 8
#define TILE_COLS (DISPLAY_W / TILE_SIZE)
#define TILE_ROWS (DISPLAY_H / TILE_SIZE)
#define TILE_COUNT (TILE_COLS * TILE_ROWS)

#define DIRTY_WORDS ((TILE_COUNT + 31) / 32)

static uint32_t dirty[DIRTY_WORDS];

inline void dirty_clear(void) {
  memset(dirty, 0, sizeof(dirty));
}

inline bool dirty_any(void) {
  for (int i = 0; i < DIRTY_WORDS; i++)
    if (dirty[i])
      return true;
  return false;
}

inline void dirty_mark(int tx, int ty) {
  int idx = ty * TILE_COLS + tx;
  dirty[idx >> 5] |= 1u << (idx & 31);
}

// Marks ALL tiles overlapping a bounding box as dirty
// Note this might be suboptimal eg. for diagonal lines
inline void dirty_mark_bbox(BoundingBox bbox) {
  int tx0 = bbox.x / TILE_SIZE;
  int ty0 = bbox.y / TILE_SIZE;
  int tx1 = (bbox.x + bbox.w - 1) / TILE_SIZE;
  int ty1 = (bbox.y + bbox.h - 1) / TILE_SIZE;

  // If completely off-screen, nothing to do
  if (tx1 < 0 || ty1 < 0 || tx0 >= TILE_COLS || ty0 >= TILE_ROWS)
    return;

  // Clamp to valid tile range
  if (tx0 < 0) tx0 = 0;
  if (ty0 < 0) ty0 = 0;
  if (tx1 >= TILE_COLS) tx1 = TILE_COLS - 1;
  if (ty1 >= TILE_ROWS) ty1 = TILE_ROWS - 1;

  for (int ty = ty0; ty <= ty1; ty++)
    for (int tx = tx0; tx <= tx1; tx++)
      dirty_mark(tx, ty);
}

inline bool dirty_get(int tx, int ty) {
  int idx = ty * TILE_COLS + tx;
  return (dirty[idx >> 5] >> (idx & 31)) & 1u;
}

inline void dirty_foreach(void (*cb)(int tx, int ty)) {
    for(int w=0; w<DIRTY_WORDS; w++) {
        uint32_t word = dirty[w];
        while(word) {
            int bit = __builtin_ctz(word);
            word &= word-1;
            int idx = w*32 + bit;
            cb(idx%TILE_COLS, idx/TILE_COLS);
        }
    }
}

// Marks tiles for one character cell. Same layout as node_draw_tileText.
// font, gw, lineHeight must be precomputed from the text node (e.g. at start of dirty_mark_text_diff).
static inline void dirty_mark_char_cell(int x, int y, int row, int col, int gw, int lineHeight) {
  BoundingBox bbox = { x + col * gw, y + row * lineHeight, gw, lineHeight };
  dirty_mark_bbox(bbox);
}

// Text-vs-text: only mark tiles for character cells that actually changed.
// oldStr/newStr may be null (treated as empty).
static void dirty_mark_text_diff(const char* oldStr, const char* newStr, int x, int y, int gw, int lineHeight) {
  const char* o = oldStr ? oldStr : "";
  const char* n = newStr ? newStr : "";

  int iOld = 0, iNew = 0, row = 0, col = 0;
  for (;;) {
    char cOld = o[iOld];
    char cNew = n[iNew];

    if (cOld == '\0') {
      while (n[iNew] != '\0') {
        dirty_mark_char_cell(x, y, row, col, gw, lineHeight);
        if (n[iNew] == '\n') { iNew++; row++; col = 0; } else { iNew++; col++; }
      }
      return;
    }
    if (cNew == '\0') {
      while (o[iOld] != '\0') {
        dirty_mark_char_cell(x, y, row, col, gw, lineHeight);
        if (o[iOld] == '\n') { iOld++; row++; col = 0; } else { iOld++; col++; }
      }
      return;
    }
    if (cOld == '\n' && cNew == '\n') {
      iOld++; iNew++; row++; col = 0;
      continue;
    }
    if (cOld == '\n') {
      while (cNew != '\0' && cNew != '\n') {
        dirty_mark_char_cell(x, y, row, col, gw, lineHeight);
        col++; iNew++; cNew = n[iNew];
      }
      if (cNew == '\n') iNew++;
      iOld++; row++; col = 0;
      continue;
    }
    if (cNew == '\n') {
      while (cOld != '\0' && cOld != '\n') {
        dirty_mark_char_cell(x, y, row, col, gw, lineHeight);
        col++; iOld++; cOld = o[iOld];
      }
      if (cOld == '\n') iOld++;
      iNew++; row++; col = 0;
      continue;
    }
    if (cOld != cNew)
      dirty_mark_char_cell(x, y, row, col, gw, lineHeight);
    col++; iOld++; iNew++;
  }
}