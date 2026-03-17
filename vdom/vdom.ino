#include <Arduino.h>
#include <string.h>

#include "constants.h"
#include "globals.h"
#include "font.h"
#include "node.h"
#include "node_draw.h"
#include "dirty.h"
#include "prelude.h"

//----------------------------------------------
// VDOM-specific
Node* rootNodeOld;
Node* rootNodeNew;

// Double-buffered node pools for deserialized trees.
// Pool that owns rootNodeOld is serialPool[1-serialPoolIndex],
// Pool that owns rootNodeNew is serialPool[serialPoolIndex].
static NodePool serialPool[2];
static int serialPoolIndex = 0;

bool lastBtnPressed = false;
void handleButton() {
  bool btnPressed = digitalRead(BTN_PIN) == LOW;
  if (!lastBtnPressed && btnPressed) {
    // some onClick handler here
  }
  lastBtnPressed = btnPressed;
}

void onNewRootNode() {
  // Assumption: something has done this already:
  // rootNodeOld = rootNodeNew;
  // rootNodeNew = ... whatever ...;
  dirty_clear();
  markDirtyTiles();
  redrawDirtyTiles();
}

// Needs to correspond to elm/Command.elm
enum Cmd {
  CMD_GET_ESP32_DATA = 0,
  CMD_SET_ROOT_NODE = 1,
};

static inline void writeAck() {
  write_u8(0xFF);
  write_u8(0xEE);
  write_u8(0xFF);
  write_u8(0xEE);
}

static inline bool commandNeedsAck(uint8_t cmd) {
  switch(cmd) {
    case CMD_SET_ROOT_NODE:
      return true;
    case CMD_GET_ESP32_DATA:
      return false;
    default: {
      Serial.print("> commandNeedsAck: ");
      Serial.println(cmd, DEC);
      complain("commandNeedsAck: Unknown command");
      return false;
    }
  }
}

void handleSerial() {
  if (Serial.available()) {
    uint8_t cmd = Serial.read();
    bool ok = false;
    switch (cmd) {
      case CMD_GET_ESP32_DATA: {
        // Compute the length of the following data to send as an uint16LE
        uint16_t payload_length = 15;
        for (int i = 0; i < NUM_FONTS; i++) {
          const FontMono1B* font = fonts[i];
          // name as sized string: 1 byte length + strlen(name)
          uint16_t name_len = (uint16_t)strlen(font->name);
          payload_length += 13 + name_len + font_bits_byte_len(font);
        }

        // separator
        write_u8(0xFF);
        write_u8(0x00);
        write_u8(0xFF);
        write_u8(0x00);

        write_u16_le(payload_length); // length of the following data
        write_u16_le(DISPLAY_W);
        write_u16_le(DISPLAY_H);
        write_u8(MY_CRT_PADDING_L);
        write_u8(MY_CRT_PADDING_R);
        write_u8(MY_CRT_PADDING_T);
        write_u8(MY_CRT_PADDING_B);
        write_u16_le((uint16_t)MAX_TOTAL_NODES);
        write_u16_le((uint16_t)NODE_GROUP_MAX_CHILDREN);
        write_u8((uint8_t)TILE_SIZE);
        write_u16_le(NUM_FONTS);
        for (int i = 0; i < NUM_FONTS; i++) {
          const FontMono1B* font = fonts[i];
          write_sized_string(font->name);
          write_u16_le(font->ascii_first);
          write_u16_le(font->ascii_last);
          write_u16_le(font->num_glyphs);
          write_u8(font->glyph_w);
          write_u8(font->glyph_h);
          write_u8(font->extra_line_height);
          write_sized_u8_list(font->bits, font_bits_byte_len(font));
        }
        ok = true;
        break;
      }
      case CMD_SET_ROOT_NODE: {
        int newPoolIdx = 1 - serialPoolIndex;
        NodePool* pool = &serialPool[newPoolIdx];
        node_pool_reset(pool);
        Node* parsedNode = node_read(pool);
        if (!parsedNode) {
          if (!hasComplained) complain("Failed to parse root node");
          break;
        }

        rootNodeOld = rootNodeNew;
        rootNodeNew = parsedNode;
        serialPoolIndex = newPoolIdx;
        onNewRootNode();
        ok = true;
        break;
      }
      default: {
        complain("Unknown command");
        break;
      }
    }
    if (ok && commandNeedsAck(cmd)) writeAck();
  }
}

// DIFFING

// TODO PERF: make dirty_mark_bbox specialized for each node type, eg. for
// diagonal lines we don't want to mark the whole bbox since many tiles will be
// unaffected.

// implicitly returns the dirty tiles into the global `dirty`
void diffNode(Node* oldRoot, Node* newRoot) {
  if (oldRoot->hash == newRoot->hash) return; // Nothing changed!

  if (oldRoot->type == NODE_GROUP && newRoot->type == NODE_GROUP) {
    diffChildren(oldRoot, newRoot);
  } else if (// Text-vs-Text when only the text changed
             oldRoot->type == NODE_TEXT && newRoot->type == NODE_TEXT &&
             oldRoot->u.text.x == newRoot->u.text.x &&
             oldRoot->u.text.y == newRoot->u.text.y &&
             oldRoot->u.text.font_index == newRoot->u.text.font_index &&
             oldRoot->u.text.color == newRoot->u.text.color) {
    const FontMono1B* font = fonts[oldRoot->u.text.font_index];
    int gw = (int)font->glyph_w;
    int lineHeight = (int)font->glyph_h + (int)font->extra_line_height;
    dirty_mark_text_diff(oldRoot->u.text.text, newRoot->u.text.text,
                         oldRoot->u.text.x, oldRoot->u.text.y, gw, lineHeight);
  } else {
    // eg. going from TEXT -> GROUP: let's mark both.
    //
    // Only in GROUP -> GROUP do we not mark both groups blindly and instead
    // diff their contents.
    dirty_mark_bbox(oldRoot->bbox);
    dirty_mark_bbox(newRoot->bbox);
  }
}

void diffChildren(Node* oldGroup, Node* newGroup) {
  bool matched[NODE_GROUP_MAX_CHILDREN] = { false };

  for (int i = 0; i < oldGroup->u.group.child_count; i++) {
    Node* oldChild = oldGroup->u.group.children[i];
    Node* newChild = NULL;
    int newIndex = -1;

    // Find by key so that we don't DELETE + INSERT when the node has just moved.
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

inline void markDirtyTiles() {
  diffNode(rootNodeOld, rootNodeNew);

  // TODO workaround - hopefully not needed
  // // If diff marked no tiles (e.g. identical tree or initial default scene), force draw of new root bbox
  // if (!dirty_any() && rootNodeNew)
  //   dirty_mark_bbox(rootNodeNew->bbox);
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

void setup()
{
  // ESP32-specific
  pinMode(BTN_PIN, INPUT_PULLUP);
  pinMode(LED_PIN, OUTPUT);
  Serial.begin(115200);

  // esp32lib-specific
  video.init(CompMode::MODENTSC240P, DAC_PIN, HAVE_VOLTAGE_DIVIDER);

  // VDOM-specific

  // Old root: empty node
  serialPool[0].node_cursor = 0;
  serialPool[0].ptr_cursor = 0;
  Node* emptyNode = node_pool_alloc(&serialPool[0]);
  *emptyNode = nodeEmpty(0);

  // New root: initial scene
  serialPool[1].node_cursor = 0;
  serialPool[1].ptr_cursor = 0;
  Node* textNode = node_pool_alloc(&serialPool[1]);
  char* msg = (char*)malloc(strlen("Waiting for commands from the Command Centre...") + 1);
  if (!msg) {
    complain("Failed to allocate memory for default message");
    *textNode = nodeEmpty(0);
  } else {
    strcpy(msg, "Waiting for commands from the Command Centre...");
    *textNode = nodeText(0, X_MIN, Y_MIN, msg, 0, COLOR_WHITE);
  }

  rootNodeOld = &serialPool[0].nodes[0];
  rootNodeNew = &serialPool[1].nodes[0];
  onNewRootNode();

  serialPoolIndex = 1;
}

void loop()
{
  // We don't need to react to events etc. faster than 60FPS.
  // If yes, move this enforceFps() to onNewRootNode().
  enforceFps();

  handleButton();
  handleSerial();

  // Look ma, no clearing the whole buffer before drawing! (It flickered too
  // much and we don't have enough memory for double buffering...)
  // video.clear(0);
}
