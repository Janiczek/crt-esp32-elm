#include <Arduino.h>
#include <string.h>

#include "constants.h"
#include "globals.h"
#include "font.h"
#include "node.h"
#include "node_draw.h"
#include "prelude.h"

//----------------------------------------------
// VDOM-specific

// Double-buffered node pools for deserialized trees.
// The last known node is in serialPool[serialPoolIndex].
// The new node will be written to serialPool[1-serialPoolIndex].
static NodePool serialPool[2];
static int serialPoolIndex = 0;
Node* rootNode = &serialPool[serialPoolIndex].nodes[0];

static uint16_t dirtyTileCount = 0; // number of {tx,ty} pairs
static uint8_t dirtyTiles[TILE_COUNT * 2];

static inline void dirtyTileBufClear() {
  dirtyTileCount = 0;
}

static inline void dirtyTileBufPush(uint8_t tx, uint8_t ty) {
  if (dirtyTileCount >= TILE_COUNT) return;
  uint16_t ii = (dirtyTileCount++) * 2;
  dirtyTiles[ii] = tx;
  dirtyTiles[ii + 1] = ty;
}

bool lastBtnPressed = false;
void handleButton() {
  bool btnPressed = digitalRead(BTN_PIN) == LOW;
  if (!lastBtnPressed && btnPressed) {
    // some onClick handler here
  }
  lastBtnPressed = btnPressed;
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

// implicitly uses `nodeNew` and will walk it back-to-front and draw nodes
void drawTile(int tx0, int ty0) {
  BoundingBox tileBbox = { tx0, ty0, TILE_SIZE, TILE_SIZE };

  // Walk the new root and draw all nodes whose bbox intersects the tile
  nodeWalkPreOrderDFS(rootNode, [&](Node* node) -> bool {
    if (!bboxIntersects(&node->bbox, &tileBbox))
      return false; // Short-circuit if node not relevant to the tile

    switch (node->type) {
      case NODE_RECT:     node_draw_tileRect(node,tx0,ty0);     break;
      case NODE_RECTFILL: node_draw_tileRectFill(node,tx0,ty0); break;
      case NODE_XLINE:    node_draw_tileXLine(node,tx0,ty0);    break;
      case NODE_YLINE:    node_draw_tileYLine(node,tx0,ty0);    break;
      case NODE_TEXT:     node_draw_tileText(node,tx0,ty0);     break;
      case NODE_GROUP:    break; // Nothing to do. We automatically traverse the group contents in the outside loop.
      default:            break;
    }

    return true;
  });
}

static inline void redrawTile(int tx, int ty) {
  int x0 = tx * TILE_SIZE;
  int y0 = ty * TILE_SIZE;

  // Clear and draw
  video.fillRect(x0, y0, TILE_SIZE, TILE_SIZE, COLOR_BLACK);
  drawTile(x0, y0);
}

static inline void redrawBufferedDirtyTiles() {
  for (uint16_t ii = 0; ii < dirtyTileCount * 2; ii += 2) {
    uint8_t tx = dirtyTiles[ii];
    uint8_t ty = dirtyTiles[ii + 1];
    redrawTile((int)tx, (int)ty);
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

        serialPoolIndex = newPoolIdx;
        rootNode = parsedNode;

        // After the node, Elm appends dirty tiles: u16 count, then count*(u8 tx, u8 ty).
        dirtyTileBufClear();
        uint16_t count = read_u16_le();
        if (count > TILE_COUNT) complain("Dirty tile overflow: count > TILE_COUNT");
        for (uint16_t i = 0; i < count; i++) {
          uint8_t tx = read_u8();
          uint8_t ty = read_u8();
          if (i < TILE_COUNT) {
            dirtyTileBufPush(tx, ty);
          }
          // else: drain/discard extra pairs to keep stream aligned
        }

        ok = true;
        break;
      }
      default: {
        complain("Unknown command");
        break;
      }
    }
    if (ok && commandNeedsAck(cmd)) {
      writeAck();
      if (cmd == CMD_SET_ROOT_NODE) {
        redrawBufferedDirtyTiles();
      }
    }
  }
}

void setup()
{
  // ESP32-specific
  pinMode(BTN_PIN, INPUT_PULLUP);
  pinMode(LED_PIN, OUTPUT);
  Serial.begin(115200);

  // esp32lib-specific
  video.init(CompMode::MODENTSC240P, DAC_PIN, HAVE_VOLTAGE_DIVIDER);

  // Start with empty scenes.
  for (int i = 0; i < 2; ++i) {
    node_pool_reset(&serialPool[i]);
    Node* root = node_pool_alloc(&serialPool[i]);
    if (root) {
      *root = nodeEmpty();
    }
  }
  rootNode = &serialPool[serialPoolIndex].nodes[0];
}

void loop()
{
  // TODO: disabled for now - how does the system behave?
  //// We don't need to react to events etc. faster than 60FPS.
  //// If yes, move this enforceFps() to onNewRootNode().
  //enforceFps();

  handleButton();
  handleSerial();

  // Look ma, no clearing the whole buffer before drawing! (It flickered too
  // much and we don't have enough memory for double buffering...)
  // video.clear(0);
}
