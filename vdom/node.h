#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include "prelude.h"
#include "bbox.h"
#include "font.h"
#include "serial.h"

#define MAX_TOTAL_NODES 64
#define NODE_GROUP_MAX_CHILDREN 32

typedef enum {
  NODE_RECT,
  NODE_RECTFILL,
  NODE_XLINE,
  NODE_YLINE,
  NODE_TEXT,
  NODE_GROUP,
  NODE_BITMAP
} NodeType;

// needed for the self-reference below
struct Node;
typedef struct Node Node;

// actual definition
typedef struct Node {
  NodeType type;
  BoundingBox bbox;
  union {
    struct { int x, y, w, h; uint8_t color; } rect;
    struct { int x, y, len; uint8_t color; } xline;
    struct { int x, y, len; uint8_t color; } yline;
    struct { int x, y; const char* text; int font_index; uint8_t color; } text;
    struct { Node** children; int child_count; } group;
    struct { int x, y, w, h; uint8_t bit_depth; uint8_t* data; size_t byte_len; } bitmap;
  } u;
} Node;

static inline bool bitmapBitDepthSupported(uint8_t bit_depth) {
  return bit_depth == 1 || bit_depth == 2 || bit_depth == 4 || bit_depth == 8;
}

static inline size_t bitmapPackedByteLength(int w, int h, uint8_t bit_depth) {
  uint64_t total_bits = (uint64_t)w * (uint64_t)h * (uint64_t)bit_depth;
  return (size_t)((total_bits + 7u) / 8u);
}

static inline Node nodeGroup(Node** children, int child_count) {
  Node n = {};
  n.type = NODE_GROUP;

  // CONTENT
  n.u.group.children = children;
  n.u.group.child_count = child_count;

  // BBOX (looping over children)
  if (child_count == 0) {
    // BBOX
    n.bbox.x = 0;
    n.bbox.y = 0;
    n.bbox.w = 0;
    n.bbox.h = 0;

    return n;
  }

  int x0 = children[0]->bbox.x;
  int y0 = children[0]->bbox.y;
  int x1 = children[0]->bbox.x + children[0]->bbox.w;
  int y1 = children[0]->bbox.y + children[0]->bbox.h;

  for (int i = 0; i < child_count; i++) {
    Node* child = children[i];

    // BBOX
    if (child->bbox.x < x0) x0 = child->bbox.x;
    if (child->bbox.y < y0) y0 = child->bbox.y;
    if (child->bbox.x + child->bbox.w > x1) x1 = child->bbox.x + child->bbox.w;
    if (child->bbox.y + child->bbox.h > y1) y1 = child->bbox.y + child->bbox.h;
  }

  n.bbox.x = x0; 
  n.bbox.y = y0; 
  n.bbox.w = x1 - x0; 
  n.bbox.h = y1 - y0;

  return n;
}

static inline Node nodeEmpty() {
  return nodeGroup(nullptr, 0);
}

static inline Node nodeRect(int x, int y, int w, int h, uint8_t color) {
  Node n = {};
  n.type = NODE_RECT;

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

  return n;
}

static inline Node nodeRectFill(int x, int y, int w, int h, uint8_t color) {
  Node n = {};
  n.type = NODE_RECTFILL;

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

  return n;
}

static inline Node nodeXLine(int x, int y, int len, uint8_t color) {
  Node n = {};
  n.type = NODE_XLINE;

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

  return n;
}

static inline Node nodeYLine(int x, int y, int len, uint8_t color) {
  Node n = {};
  n.type = NODE_YLINE;

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

  return n;
}

static inline Node nodeText(int x, int y, const char* text, int font_index, uint8_t color) {
  Node n = {};
  n.type = NODE_TEXT;

  // BBOX
  n.bbox.x = x;
  n.bbox.y = y;
  if (text != nullptr) {
    font_textMultilineSize(text, font_index, &n.bbox.w, &n.bbox.h);
  } else {
    n.bbox.w = 0;
    n.bbox.h = 0;
  }

  // CONTENT
  n.u.text.x = x;
  n.u.text.y = y;
  n.u.text.text = text;
  n.u.text.font_index = font_index;
  n.u.text.color = color;

  return n;
}

static inline Node nodeBitmap(int x, int y, int w, int h, uint8_t bit_depth, uint8_t* data, size_t byte_len) {
  Node n = {};
  n.type = NODE_BITMAP;

  // BBOX
  n.bbox.x = x;
  n.bbox.y = y;
  n.bbox.w = w;
  n.bbox.h = h;

  // CONTENT
  n.u.bitmap.x = x;
  n.u.bitmap.y = y;
  n.u.bitmap.w = w;
  n.u.bitmap.h = h;
  n.u.bitmap.bit_depth = bit_depth;
  n.u.bitmap.data = data;
  n.u.bitmap.byte_len = byte_len;

  size_t expected_len = bitmapPackedByteLength(w, h, bit_depth);
  if (expected_len != byte_len) {
    complain("nodeBitmap: byte length mismatch");
  }

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

// --- Flattened node pool for deserialization ---
// All nodes from a single tree live in one contiguous Node array.
// Group children pointers live in one contiguous Node* array.
// This allows arbitrary nesting depth with a fixed total node limit.

struct NodePool {
  Node nodes[MAX_TOTAL_NODES];
  Node* ptrs[MAX_TOTAL_NODES];
  int node_cursor;
  int ptr_cursor;
};

static inline Node* node_pool_alloc(NodePool* pool) {
  if (pool->node_cursor >= MAX_TOTAL_NODES) {
    complain("node_pool_alloc: Node pool full");
    return nullptr;
  }
  return &pool->nodes[pool->node_cursor++];
}

static inline Node** node_pool_alloc_ptrs(NodePool* pool, int count) {
  if (pool->ptr_cursor + count > MAX_TOTAL_NODES) {
    complain("node_pool_alloc_ptrs: Ptr pool full");
    return nullptr;
  }
  Node** base = &pool->ptrs[pool->ptr_cursor];
  pool->ptr_cursor += count;
  return base;
}

// Free all heap memory owned by nodes in a pool
static inline void node_pool_free(NodePool* pool) {
  for (int i = 0; i < pool->node_cursor; i++) {
    // NODE_TEXT strings
    if (pool->nodes[i].type == NODE_TEXT && pool->nodes[i].u.text.text) {
      free((void*)pool->nodes[i].u.text.text);
      pool->nodes[i].u.text.text = nullptr;
    }
    // NODE_BITMAP data
    if (pool->nodes[i].type == NODE_BITMAP && pool->nodes[i].u.bitmap.data) {
      free(pool->nodes[i].u.bitmap.data);
      pool->nodes[i].u.bitmap.data = nullptr;
      pool->nodes[i].u.bitmap.byte_len = 0;
    }
  }
}

static inline void node_pool_reset(NodePool* pool) {
  node_pool_free(pool);
  pool->node_cursor = 0;
  pool->ptr_cursor = 0;
}

// --- Deserialization ---

static inline Node* node_read(NodePool* pool);

static inline Node* node_read(NodePool* pool) {
  uint8_t type = read_u8();

  Node* slot = node_pool_alloc(pool);
  if (!slot) {
    complain("node_read() didn't get a slot from the pool");
    return nullptr;
  }

  switch (type) {
    case 0: // NODE_RECT
      *slot = nodeRect(
        read_i32_le(), // x
        read_i32_le(), // y
        read_i32_le(), // w
        read_i32_le(), // h
        read_u8()); // color
      return slot;
    case 1: // NODE_RECTFILL
      *slot = nodeRectFill(
        read_i32_le(), // x
        read_i32_le(), // y
        read_i32_le(), // w
        read_i32_le(), // h
        read_u8()); // color
      return slot;
    case 2: // NODE_XLINE
      *slot = nodeXLine(
        read_i32_le(), // x
        read_i32_le(), // y
        read_i32_le(), // len
        read_u8()); // color
      return slot;
    case 3: // NODE_YLINE
      *slot = nodeYLine(
        read_i32_le(), // x
        read_i32_le(), // y
        read_i32_le(), // len
        read_u8()); // color
      return slot;
    case 4: { // NODE_TEXT
      int x = read_i32_le();
      int y = read_i32_le();
      int font_index = (int)read_i32_le();
      uint8_t color = read_u8();
      char* str = read_sized_compressed_string();
      *slot = nodeText(x, y, str, font_index, color);
      return slot;
    }
    case 5: { // NODE_GROUP
      uint16_t count = read_u16_le();
      Node** children = (count > 0) ? node_pool_alloc_ptrs(pool, count) : nullptr;
      if (count > 0 && !children) return nullptr;
      if (count == 0) {
        *slot = nodeGroup(children, count);
        return slot;
      }
      int child_count = 0;
      for (uint16_t i = 0; i < count; i++) {
        Node* child = node_read(pool);
        if (child) {
          children[child_count++] = child;
        }
      }
      *slot = nodeGroup(children, child_count);
      return slot;
    }
    case 6: { // NODE_BITMAP
      int x = read_i32_le();
      int y = read_i32_le();
      int w = read_i32_le();
      int h = read_i32_le();
      uint8_t bit_depth = read_u8();
      size_t byte_len = bitmapPackedByteLength(w, h, bit_depth);
      uint16_t declared_byte_len = 0;
      uint16_t compressed_len = 0;
      if (!read_sized_compressed_bytes_header(&declared_byte_len, &compressed_len)) {
        return nullptr;
      }
      if ((size_t)declared_byte_len != byte_len) {
        complain("node_read: bitmap byte length mismatch");
        activeReadBuffer->ok = false;
        return nullptr;
      }
      uint8_t* data = read_zlib_bytes(compressed_len, byte_len, false);
      if (!activeReadBuffer->ok) {
        free(data);
        return nullptr;
      }
      *slot = nodeBitmap(x, y, w, h, bit_depth, data, byte_len);
      return slot;
    }
    default: {
      complain("node_read: Unknown node type");
      return nullptr;
    }
  }
}