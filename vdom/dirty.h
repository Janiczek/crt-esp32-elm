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