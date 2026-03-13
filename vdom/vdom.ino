#include <Arduino.h>
#include <string.h>

#include "constants.h"
#include "globals.h"
#include "node.h"
#include "node_draw.h"
#include "dirty.h"
#include "prelude.h"
#include "font.h"
#include "font/bitbuntu_11.h"
#include "font/bitocra_11.h"
#include "font/bitocra_7.h"
#include "font/cherry_400_10.h"
#include "font/cherry_700_10.h"
#include "font/clr6x12_12.h"
#include "font/creep_12.h"
#include "font/dina_italic_400_10.h"
#include "font/dina_italic_400_8.h"
#include "font/dina_italic_400_9.h"
#include "font/dina_italic_700_10.h"
#include "font/dina_italic_700_8.h"
#include "font/dina_italic_700_9.h"
#include "font/dina_regular_400_10.h"
#include "font/dina_regular_400_6.h"
#include "font/dina_regular_400_8.h"
#include "font/dina_regular_400_9.h"
#include "font/dina_regular_700_10.h"
#include "font/dina_regular_700_8.h"
#include "font/dina_regular_700_9.h"
#include "font/ecran_monochrome_7.h"
#include "font/f4x6_6.h"
#include "font/f5x7_7.h"
#include "font/f5x8_8.h"
#include "font/f6x10_10.h"
#include "font/f6x12_12.h"
#include "font/f6x13b_13.h"
#include "font/f6x13o_13.h"
#include "font/f6x13_13.h"
#include "font/f6x9_9.h"
#include "font/f7x13b_13.h"
#include "font/f7x13o_13.h"
#include "font/f7x13_13.h"
#include "font/f7x14b_14.h"
#include "font/f7x14_14.h"
#include "font/f8x13b_13.h"
#include "font/f8x13o_13.h"
#include "font/f8x13_13.h"
#include "font/f9x18b_18.h"
#include "font/f9x18_18.h"
#include "font/haxormedium_11.h"
#include "font/haxormedium_13.h"
#include "font/haxormedium_14.h"
#include "font/haxornarrow_17.h"
#include "font/haxornarrow_21.h"
#include "font/lemon_12.h"
#include "font/lemon_j_12.h"
#include "font/monocle_fixed_12.h"
#include "font/monogram_extended_9.h"
#include "font/monospaced_serif_10.h"
#include "font/phil_ui_tiny_8.h"
#include "font/scientifica_italic_400_12.h"
#include "font/scientifica_regular_400_12.h"
#include "font/scientifica_regular_700_12.h"
#include "font/semifraktur_monospace_10.h"
#include "font/six_twelve_mono_10.h"
#include "font/spleen_5x8_8.h"
#include "font/spleen_8x16_16.h"
#include "font/three_x_six_pixel_monospace_7.h"
#include "font/tom_thumb_6.h"
#include "font/unscii_8.h"
#include "font/unscii_alt_8.h"
#include "font/unscii_thin_8.h"

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

#define GPIO0_BTN        0
#define FONT_LINE_WIDTH  60
#define NUM_FONTS        63

static const FontMono1B* const fonts[NUM_FONTS] = {
  &font_bitbuntu_11,
  &font_bitocra_11,
  &font_bitocra_7,
  &font_cherry_400_10,
  &font_cherry_700_10,
  &font_clr6x12_12,
  &font_creep_12,
  &font_dina_italic_400_10,
  &font_dina_italic_400_8,
  &font_dina_italic_400_9,
  &font_dina_italic_700_10,
  &font_dina_italic_700_8,
  &font_dina_italic_700_9,
  &font_dina_regular_400_10,
  &font_dina_regular_400_6,
  &font_dina_regular_400_8,
  &font_dina_regular_400_9,
  &font_dina_regular_700_10,
  &font_dina_regular_700_8,
  &font_dina_regular_700_9,
  &font_ecran_monochrome_7,
  &font_f4x6_6,
  &font_f5x7_7,
  &font_f5x8_8,
  &font_f6x10_10,
  &font_f6x12_12,
  &font_f6x13b_13,
  &font_f6x13o_13,
  &font_f6x13_13,
  &font_f6x9_9,
  &font_f7x13b_13,
  &font_f7x13o_13,
  &font_f7x13_13,
  &font_f7x14b_14,
  &font_f7x14_14,
  &font_f8x13b_13,
  &font_f8x13o_13,
  &font_f8x13_13,
  &font_f9x18b_18,
  &font_f9x18_18,
  &font_haxormedium_11,
  &font_haxormedium_13,
  &font_haxormedium_14,
  &font_haxornarrow_17,
  &font_haxornarrow_21,
  &font_lemon_12,
  &font_lemon_j_12,
  &font_monocle_fixed_12,
  &font_monogram_extended_9,
  &font_monospaced_serif_10,
  &font_phil_ui_tiny_8,
  &font_scientifica_italic_400_12,
  &font_scientifica_regular_400_12,
  &font_scientifica_regular_700_12,
  &font_semifraktur_monospace_10,
  &font_six_twelve_mono_10,
  &font_spleen_5x8_8,
  &font_spleen_8x16_16,
  &font_three_x_six_pixel_monospace_7,
  &font_tom_thumb_6,
  &font_unscii_8,
  &font_unscii_alt_8,
  &font_unscii_thin_8,
};

static int fontIndex = 0;

static const char TEXT_REST[] =
  "\n"
  "\n"
  "!\"#$%&'()*+,-./0123456789:;<=>?@\n"
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`\n"
  "abcdefghijklmnopqrstuvwxyz{|}~\n"
  "\n"
  "\n"
  "def greet(name):\n"
  "    print(f\"Hello, {name}!\")\n"
  "\n"
  "greet(\"world\")"
  "\n"
  "\n"
  "int main() {\n"
  "    cout << \"OHAI\" << endl;\n"
  "    return 0;\n"
  "}\n";

static char firstLine[FONT_LINE_WIDTH + 3];
static char textBuffer[sizeof(firstLine) + sizeof(TEXT_REST)];
static const char* text = textBuffer;

static void buildFirstLineAndText() {
  int n = 0;
  firstLine[n++] = 'F';
  firstLine[n++] = 'O';
  firstLine[n++] = 'N';
  firstLine[n++] = 'T';
  firstLine[n++] = ':';
  firstLine[n++] = ' ';
  const char* name = fonts[fontIndex]->name;
  while (*name && n < FONT_LINE_WIDTH) firstLine[n++] = *name++;
  while (n < FONT_LINE_WIDTH) firstLine[n++] = ' ';
  firstLine[n++] = '\n';
  firstLine[n] = '\0';
  strcpy(textBuffer, firstLine);
  strcat(textBuffer, TEXT_REST);
}

static void handleFontButton() {
  static bool lastBtnHigh = true;
  bool btnHigh = (digitalRead(GPIO0_BTN) == HIGH);
  if (lastBtnHigh && !btnHigh) {
    fontIndex = (fontIndex + 1) % NUM_FONTS;
    buildFirstLineAndText();
  }
  lastBtnHigh = btnHigh;
}

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
  //pool.nodes[nodeCount++] = nodeRectFill(nodeCount, boxX,      boxY,       boxW, boxH,         abs(boxColor)); // bouncing box
  pool.nodes[nodeCount++] = nodeText(    nodeCount, X_MIN + 5, Y_MIN + 5,  text, fonts[fontIndex], COLOR_WHITE);   // text
  //pool.nodes[nodeCount++] = nodeXLine(   nodeCount, X_MIN,     Y_CENTER,   USABLE_W,           COLOR_GRAY);    // x-cross
  //pool.nodes[nodeCount++] = nodeYLine(   nodeCount, X_CENTER,  Y_MIN,      USABLE_H,           COLOR_GRAY);    // y-cross
  //pool.nodes[nodeCount++] = nodeRect(    nodeCount, X_MIN,     Y_MIN,      USABLE_W, USABLE_H, COLOR_WHITE);   // border

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

  pinMode(GPIO0_BTN, INPUT_PULLUP);
  buildFirstLineAndText();
}

void loop()
{
  enforce_fps();
  handleFontButton();

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
