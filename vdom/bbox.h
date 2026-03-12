#pragma once

typedef struct BoundingBox {
  int x, y, w, h;
} BoundingBox;

static inline bool bboxIntersects(const BoundingBox* a, const BoundingBox* b) {
  if (!a || !b) return false;
  if (a->w <= 0 || a->h <= 0 || b->w <= 0 || b->h <= 0) return false;
  if (a->x + a->w <= b->x) return false;
  if (b->x + b->w <= a->x) return false;
  if (a->y + a->h <= b->y) return false;
  if (b->y + b->h <= a->y) return false;
  return true;
}