#include "constants.h"
#include "globals.h"
#include "node.h"
#include "node_draw.h"
#include "dirty.h"
#include "prelude.h"
#include "font/dina_regular_400_6.h"
#include "font/dina_regular_400_8.h"
#include "font/dina_regular_400_9.h"
#include "font/dina_regular_400_10.h"
#include "font/dina_regular_700_8.h"
#include "font/dina_regular_700_9.h"
#include "font/dina_regular_700_10.h"
#include "font/dina_italic_400_8.h"
#include "font/dina_italic_400_9.h"
#include "font/dina_italic_400_10.h"
#include "font/dina_italic_700_8.h"
#include "font/dina_italic_700_9.h"
#include "font/dina_italic_700_10.h"

//----------------------------------------------
// VDOM-specific
Node* rootNodeOld;
Node* rootNodeNew;

// Node pools. Roots live at ROOT_SLOT.
#define NODE_POOL_SIZE 32
#define ROOT_SLOT (NODE_POOL_SIZE - 1)
struct ViewPool {
  Node nodes[NODE_POOL_SIZE];
  Node* ptrs[ROOT_SLOT];  // children only; root at nodes[ROOT_SLOT]
};
static ViewPool viewPool[2];
static int viewPoolIndex = 0;

//----------------------------------------------
// Scene-specific
const int boxW = 16;
const int boxH = 16;
int boxX, boxY, boxDX, boxDY;
int8_t boxColor; // intentionally -127..127 and overflowing


Node* view() {
  ViewPool& pool = viewPool[viewPoolIndex];
  viewPoolIndex = 1 - viewPoolIndex;

  int nodeCount = 0;
  pool.nodes[nodeCount++] = nodeRectFill(nodeCount, boxX,      boxY,      boxW, boxH,         abs(boxColor)); // bouncing box
  // TEXT START
  int gap = 2;
  int y = Y_MIN + gap;
  pool.nodes[nodeCount++] = nodeText(nodeCount, y - Y_MIN + X_MIN, y, "r400_6  Hello world!", &font_dina_regular_400_6,  COLOR_WHITE); y += font_dina_regular_400_6.glyph_h + gap;
  pool.nodes[nodeCount++] = nodeText(nodeCount, y - Y_MIN + X_MIN, y, "r400_8  Hello world!", &font_dina_regular_400_8,  COLOR_WHITE); y += font_dina_regular_400_8.glyph_h + gap;
  pool.nodes[nodeCount++] = nodeText(nodeCount, y - Y_MIN + X_MIN, y, "r400_9  Hello world!", &font_dina_regular_400_9,  COLOR_WHITE); y += font_dina_regular_400_9.glyph_h + gap;
  pool.nodes[nodeCount++] = nodeText(nodeCount, y - Y_MIN + X_MIN, y, "r400_10 Hello world!", &font_dina_regular_400_10, COLOR_WHITE); y += font_dina_regular_400_10.glyph_h + gap;
  pool.nodes[nodeCount++] = nodeText(nodeCount, y - Y_MIN + X_MIN, y, "r700_8  Hello world!", &font_dina_regular_700_8,  COLOR_WHITE); y += font_dina_regular_700_8.glyph_h + gap;
  pool.nodes[nodeCount++] = nodeText(nodeCount, y - Y_MIN + X_MIN, y, "r700_9  Hello world!", &font_dina_regular_700_9,  COLOR_WHITE); y += font_dina_regular_700_9.glyph_h + gap;
  pool.nodes[nodeCount++] = nodeText(nodeCount, y - Y_MIN + X_MIN, y, "r700_10 Hello world!", &font_dina_regular_700_10, COLOR_WHITE); y += font_dina_regular_700_10.glyph_h + gap;
  pool.nodes[nodeCount++] = nodeText(nodeCount, y - Y_MIN + X_MIN, y, "i400_8  Hello world!", &font_dina_italic_400_8,   COLOR_WHITE); y += font_dina_italic_400_8.glyph_h + gap;
  pool.nodes[nodeCount++] = nodeText(nodeCount, y - Y_MIN + X_MIN, y, "i400_9  Hello world!", &font_dina_italic_400_9,   COLOR_WHITE); y += font_dina_italic_400_9.glyph_h + gap;
  pool.nodes[nodeCount++] = nodeText(nodeCount, y - Y_MIN + X_MIN, y, "i400_10 Hello world!", &font_dina_italic_400_10,  COLOR_WHITE); y += font_dina_italic_400_10.glyph_h + gap;
  pool.nodes[nodeCount++] = nodeText(nodeCount, y - Y_MIN + X_MIN, y, "i700_8  Hello world!", &font_dina_italic_700_8,   COLOR_WHITE); y += font_dina_italic_700_8.glyph_h + gap;
  pool.nodes[nodeCount++] = nodeText(nodeCount, y - Y_MIN + X_MIN, y, "i700_9  Hello world!", &font_dina_italic_700_9,   COLOR_WHITE); y += font_dina_italic_700_9.glyph_h + gap;
  pool.nodes[nodeCount++] = nodeText(nodeCount, y - Y_MIN + X_MIN, y, "i700_10 Hello world!", &font_dina_italic_700_10,  COLOR_WHITE);
  // TEXT END
  pool.nodes[nodeCount++] = nodeXLine(   nodeCount, X_MIN,     Y_CENTER,  USABLE_W,           COLOR_GRAY);    // x-cross
  pool.nodes[nodeCount++] = nodeYLine(   nodeCount, X_CENTER,  Y_MIN,     USABLE_H,           COLOR_GRAY);    // y-cross
  pool.nodes[nodeCount++] = nodeRect(    nodeCount, X_MIN,     Y_MIN,     USABLE_W, USABLE_H, COLOR_WHITE);   // border

  for (int i = 0; i < nodeCount; i++) {
    pool.ptrs[i] = &pool.nodes[i];
  }

  pool.nodes[ROOT_SLOT] = nodeGroup(0, pool.ptrs, nodeCount);
  return &pool.nodes[ROOT_SLOT];
}

// DIFFING

// TODO PERF: make dirty_mark_bbox specialized for each node type, eg. for
// diagonal lines we don't want to mark the whole bbox since many tiles will be
// unaffected.

// implicitly returns the dirty tiles into the global `dirty`
void diffNode(Node* oldRoot, Node* newRoot) {
  if (oldRoot->hash == newRoot->hash) return; // Nothing changed!

  if (oldRoot->type != NODE_GROUP) dirty_mark_bbox(oldRoot->bbox);
  if (newRoot->type != NODE_GROUP) dirty_mark_bbox(newRoot->bbox);

  if (oldRoot->type == NODE_GROUP && newRoot->type == NODE_GROUP)
    diffChildren(oldRoot, newRoot);
}

void diffChildren(Node* oldGroup, Node* newGroup) {
  bool matched[NODE_GROUP_MAX_CHILDREN] = { false };

  for (int i = 0; i < oldGroup->u.group.child_count; i++) {
    Node* oldChild = oldGroup->u.group.children[i];
    Node* newChild = NULL;
    int newIndex = -1;

    for (int j = 0; j < newGroup->u.group.child_count; j++) {
      if (oldChild->key == newGroup->u.group.children[j]->key) {
        newChild = newGroup->u.group.children[j];
        newIndex = j;
        break;
      }
    }

    if (newChild == NULL) {
      // DELETION: no corresponding new child found
      dirty_mark_bbox(oldChild->bbox);
    } else {
      // UPDATE: child found but it's yet unclear if it changed or not.
      // We'll have to diff it.
      matched[newIndex] = true;
      diffNode(oldChild, newChild);
    }
  }

  for (int j = 0; j < newGroup->u.group.child_count; j++) {
    // INSERTION: unmatched new children must be new
    if (!matched[j]) {
      dirty_mark_bbox(newGroup->u.group.children[j]->bbox);
    }
  }
}


// implicitly uses `nodeNew` and will walk it back-to-front and draw nodes
void drawTile(int tx, int ty) {
  int tx0 = tx * TILE_SIZE;
  int ty0 = ty * TILE_SIZE;
  BoundingBox tileBbox = { tx0, ty0, TILE_SIZE, TILE_SIZE };

  // Walk the new root and draw all nodes whose bbox intersects the tile
  nodeWalkPreOrderDFS(rootNodeNew, [&](Node* node) -> bool {
    if (!bboxIntersects(&node->bbox, &tileBbox))
      return false; // Short-circuit if node not relevant to the tile

    switch (node->type) {
      case NODE_RECT:     node_draw_tileRect(node,tx0,ty0);     break;
      case NODE_RECTFILL: node_draw_tileRectFill(node,tx0,ty0); break;
      case NODE_XLINE:    node_draw_tileXLine(node,tx0,ty0);    break;
      case NODE_YLINE:    node_draw_tileYLine(node,tx0,ty0);    break;
      case NODE_TEXT:     node_draw_tileText(node,tx0,ty0);     break;
      case NODE_GROUP:    break; // Nothing to do
      default:            break;
    }

    return true;
  });
}

void redrawDirtyTiles() {
  dirty_foreach([](int tx, int ty) {
    int x0 = tx * TILE_SIZE;
    int y0 = ty * TILE_SIZE;

    // Clear the tile
    video.fillRect(x0, y0, TILE_SIZE, TILE_SIZE, COLOR_BLACK);

    // Draw the scene inside this tile
    drawTile(tx, ty);
  });
}

void updateBoxOnTick() {
  boxX += boxDX;
  boxY += boxDY;
  boxColor += 4; // overflow will handle the rest
}

void checkBoxBounce() {
  if      (boxX <= X_MIN)        { boxDX = -boxDX; boxX = X_MIN; }
  else if (boxX >= X_MAX - boxW) { boxDX = -boxDX; boxX = X_MAX - boxW; }
  if      (boxY <= Y_MIN)        { boxDY = -boxDY; boxY = Y_MIN; }
  else if (boxY >= Y_MAX - boxH) { boxDY = -boxDY; boxY = Y_MAX - boxH; }
}

void setup()
{
  // esp32lib-specific
  video.init(CompMode::MODENTSC240P, DAC_PIN, HAVE_VOLTAGE_DIVIDER);

  // VDOM-specific
  // both pools get an empty root so old/new point at different buffers
  viewPool[0].nodes[ROOT_SLOT] = nodeEmpty(0);
  viewPool[1].nodes[ROOT_SLOT] = nodeEmpty(0);
  rootNodeOld = &viewPool[0].nodes[ROOT_SLOT];
  rootNodeNew = &viewPool[1].nodes[ROOT_SLOT];

  // Scene-specific
  boxX = X_CENTER - boxW/2;
  boxY = Y_CENTER - boxH/2;
  boxDX = 1;
  boxDY = 1;
}

void loop()
{
  enforce_fps();

  // Look ma, no clearing the whole buffer before drawing! (It flickered too
  // much and we don't have enough memory for double buffering...)
  // video.clear(0);

  // Diff VDOM trees and mark dirty tiles
  dirty_clear();
  diffNode(rootNodeOld, rootNodeNew);

  redrawDirtyTiles();

  // Update any game logic
  updateBoxOnTick();
  checkBoxBounce();

  // Generate new VDOM trees
  rootNodeOld = rootNodeNew;
  rootNodeNew = view();
}
