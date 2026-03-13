#pragma once

#include <stddef.h>
#include <stdint.h>

#include "hash.h"
#include "bbox.h"
#include "font.h"

#define NODE_GROUP_MAX_CHILDREN 32

typedef enum {
  NODE_RECT,
  NODE_RECTFILL,
  NODE_XLINE,
  NODE_YLINE,
  NODE_TEXT,
  NODE_GROUP
} NodeType;

// needed for the self-reference below
struct Node;
typedef struct Node Node;

// actual definition
typedef struct Node {
  NodeType type;
  uint32_t key;
  BoundingBox bbox;
  uint32_t hash;
  union {
    struct { int x, y, w, h; uint8_t color; } rect;
    struct { int x, y, len; uint8_t color; } xline;
    struct { int x, y, len; uint8_t color; } yline;
    struct { int x, y; const char* text; const struct FontMono1B* font; uint8_t color; } text;
    struct { Node** children; int child_count; } group;
  } u;
} Node;

static inline Node nodeGroup(uint32_t key, Node** children, int child_count) {
  Node n = {};
  n.type = NODE_GROUP;
  n.key = key;

  // CONTENT
  n.u.group.children = children;
  n.u.group.child_count = child_count;

  // BBOX + HASH (looping over children)
  if (child_count == 0) {
    // BBOX
    n.bbox.x = 0;
    n.bbox.y = 0;
    n.bbox.w = 0;
    n.bbox.h = 0;

    // HASH
    uint32_t h = hash_init;
    h = hash_update_u8(h, n.type);
    n.hash = h;

    return n;
  }

  int x0 = children[0]->bbox.x;
  int y0 = children[0]->bbox.y;
  int x1 = children[0]->bbox.x + children[0]->bbox.w;
  int y1 = children[0]->bbox.y + children[0]->bbox.h;

  uint32_t h = hash_init;
  h = hash_update_u8(h, n.type);

  for (int i = 0; i < child_count; i++) {
    Node* child = children[i];

    // BBOX
    if (child->bbox.x < x0) x0 = child->bbox.x;
    if (child->bbox.y < y0) y0 = child->bbox.y;
    if (child->bbox.x + child->bbox.w > x1) x1 = child->bbox.x + child->bbox.w;
    if (child->bbox.y + child->bbox.h > y1) y1 = child->bbox.y + child->bbox.h;

    // HASH
    h = hash_update_u32(h, child->hash);
  }

  n.bbox.x = x0; 
  n.bbox.y = y0; 
  n.bbox.w = x1 - x0; 
  n.bbox.h = y1 - y0;

  n.hash = h;

  return n;
}

static inline Node nodeEmpty(uint32_t key) {
  return nodeGroup(key, nullptr, 0);
}

static inline Node nodeRect(uint32_t key, int x, int y, int w, int h, uint8_t color) {
  Node n = {};
  n.type = NODE_RECT;
  n.key = key;

  // BBOX
  n.bbox.x = x; 
  n.bbox.y = y; 
  n.bbox.w = w; 
  n.bbox.h = h;

  // CONTENT
  n.u.rect.x = x; 
  n.u.rect.y = y; 
  n.u.rect.w = w; 
  n.u.rect.h = h; 
  n.u.rect.color = color;

  // HASH
  uint32_t hv = hash_init;
  hv = hash_update_u8(hv, n.type);
  hv = hash_update_u32(hv, (uint32_t)x);
  hv = hash_update_u32(hv, (uint32_t)y);
  hv = hash_update_u32(hv, (uint32_t)w);
  hv = hash_update_u32(hv, (uint32_t)h);
  hv = hash_update_u8(hv, color);
  n.hash = hv;

  return n;
}

static inline Node nodeRectFill(uint32_t key, int x, int y, int w, int h, uint8_t color) {
  Node n = {};
  n.type = NODE_RECTFILL;
  n.key = key;

  // BBOX
  n.bbox.x = x; 
  n.bbox.y = y; 
  n.bbox.w = w; 
  n.bbox.h = h;

  // CONTENT
  n.u.rect.x = x; 
  n.u.rect.y = y; 
  n.u.rect.w = w; 
  n.u.rect.h = h; 
  n.u.rect.color = color;

  // HASH
  uint32_t hv = hash_init;
  hv = hash_update_u8(hv, n.type);
  hv = hash_update_u32(hv, (uint32_t)x);
  hv = hash_update_u32(hv, (uint32_t)y);
  hv = hash_update_u32(hv, (uint32_t)w);
  hv = hash_update_u32(hv, (uint32_t)h);
  hv = hash_update_u8(hv, color);
  n.hash = hv;

  return n;
}

static inline Node nodeXLine(uint32_t key, int x, int y, int len, uint8_t color) {
  Node n = {};
  n.type = NODE_XLINE;
  n.key = key;

  // BBOX
  n.bbox.x = x; 
  n.bbox.y = y; 
  n.bbox.w = len; 
  n.bbox.h = 1;

  // CONTENT
  n.u.xline.x = x;
  n.u.xline.y = y; 
  n.u.xline.len = len; 
  n.u.xline.color = color;

  // HASH
  uint32_t h = hash_init;
  h = hash_update_u8(h, n.type);
  h = hash_update_u32(h, (uint32_t)x);
  h = hash_update_u32(h, (uint32_t)y);
  h = hash_update_u32(h, (uint32_t)len);
  h = hash_update_u8(h, color);
  n.hash = h;

  return n;
}

static inline Node nodeYLine(uint32_t key, int x, int y, int len, uint8_t color) {
  Node n = {};
  n.type = NODE_YLINE;
  n.key = key;

  // BBOX
  n.bbox.x = x;
  n.bbox.y = y;
  n.bbox.w = 1;
  n.bbox.h = len;

  // CONTENT
  n.u.yline.x = x;
  n.u.yline.y = y;
  n.u.yline.len = len;
  n.u.yline.color = color;

  // HASH
  uint32_t h = hash_init;
  h = hash_update_u8(h, n.type);
  h = hash_update_u32(h, (uint32_t)x);
  h = hash_update_u32(h, (uint32_t)y);
  h = hash_update_u32(h, (uint32_t)len);
  h = hash_update_u8(h, color);
  n.hash = h;

  return n;
}

static inline Node nodeText(uint32_t key, int x, int y, const char* text, const FontMono1B* font, uint8_t color) {
  Node n = {};
  n.type = NODE_TEXT;
  n.key = key;

  // BBOX
  n.bbox.x = x;
  n.bbox.y = y;
  n.bbox.w = font_textWidth(text, font);
  n.bbox.h = font->glyph_h;

  // CONTENT
  n.u.text.x = x;
  n.u.text.y = y;
  n.u.text.text = text;
  n.u.text.font = font;
  n.u.text.color = color;

  // HASH
  uint32_t h = hash_init;
  h = hash_update_u8(h, n.type);
  h = hash_update_u32(h, (uint32_t)x);
  h = hash_update_u32(h, (uint32_t)y);
  h = hash_update_u8(h, color);
  h = hash_update_cstr(h, text);
  h = hash_update_cstr(h, font->name);
  n.hash = h;

  return n;
}

// The callback can short-circuit the walk (eg. when drawing, if the node's bbox
// doesn't intersect with the dirty tile).
template<typename F>
static inline void nodeWalkPreOrderDFS(Node* root, F&& callback) {
  if (!root) return;

  bool shouldContinue = callback(root);
  if (!shouldContinue) return;

  // Then recur on children if it's a group
  if (root->type == NODE_GROUP)
    for (int i = 0; i < root->u.group.child_count; i++)
      nodeWalkPreOrderDFS(root->u.group.children[i], callback);
}
