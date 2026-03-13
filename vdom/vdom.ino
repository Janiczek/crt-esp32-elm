#include <ESP32Video.h>

#include "constants.h"
#include "globals.h"
#include "node.h"
#include "node_draw.h"
#include "dirty.h"
#include "prelude.h"

//----------------------------------------------
// VDOM-specific
Node* rootNodeOld;
Node* rootNodeNew;

// Node pools. Roots live at ROOT_SLOT.
#define NODE_POOL_SIZE 8
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

Node* view() {
  ViewPool& pool = viewPool[viewPoolIndex];
  viewPoolIndex = 1 - viewPoolIndex;

  int nodeCount = 0;
  pool.nodes[nodeCount++] = nodeRect(    nodeCount, X_MIN,      Y_MIN,      USABLE_W, USABLE_H, COLOR_WHITE); // border
  pool.nodes[nodeCount++] = nodeXLine(   nodeCount, X_MIN,      Y_CENTER,   USABLE_W,           COLOR_GRAY);  // x-cross
  pool.nodes[nodeCount++] = nodeYLine(   nodeCount, X_CENTER,   Y_MIN,      USABLE_H,           COLOR_GRAY);  // y-cross
  pool.nodes[nodeCount++] = nodeRectFill(nodeCount, boxX,       boxY,       boxW, boxH,         COLOR_WHITE); // bouncing box
  pool.nodes[nodeCount++] = nodeText(    nodeCount, X_MIN + 2,  Y_MIN + 2,  "VDOM",             COLOR_WHITE); // text

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
      case NODE_RECT:     break; // TODO
      case NODE_RECTFILL: node_draw_tileRectFill(node,tx0,ty0); break;
      case NODE_XLINE:    node_draw_tileXLine(node,tx0,ty0);    break;
      case NODE_YLINE:    node_draw_tileYLine(node,tx0,ty0);    break;
      case NODE_TEXT:     break; // TODO
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

void updateBoxPosition() {
  boxX += boxDX;
  boxY += boxDY;
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

  // Diff the old and new views (mark dirty tiles)
  dirty_clear();
  diffNode(rootNodeOld, rootNodeNew);

  redrawDirtyTiles();

  // Now update any game logic
  updateBoxPosition();
  checkBoxBounce();

  // Generate new VDOM
  rootNodeOld = rootNodeNew;
  rootNodeNew = view();
}
