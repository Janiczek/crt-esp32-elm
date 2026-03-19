#include <Arduino.h>
#include <esp_heap_caps.h>
#include <stdlib.h>
#include <string.h>

#include "constants.h"
#include "globals.h"
#include "font.h"
#include "node.h"
#include "node_draw.h"
#include "prelude.h"
#include "serial.h"

//----------------------------------------------
// Serial transport configuration

static const uint8_t PROTOCOL_VERSION = 1;
static const size_t SERIAL_FRAME_BUFFER_LIMIT = 65539;

enum TransportOpcode : uint8_t {
  OPCODE_BINARY = 0x2,
  OPCODE_CLOSE = 0x8,
  OPCODE_PING = 0x9,
  OPCODE_PONG = 0xA,
};

enum FrameType : uint8_t {
  MSG_GET_ESP32_DATA = 0x01,
  MSG_ESP32_DATA_BEGIN = 0x02,
  MSG_SET_ROOT_NODE = 0x03,
  MSG_ACK = 0x04,
  MSG_ERROR = 0x06,
  MSG_ESP32_DATA_CHUNK = 0x07,
  MSG_ESP32_DATA_END = 0x08,
};

//----------------------------------------------
// VDOM-specific

static NodePool rootNodePool;
Node* rootNode = &rootNodePool.nodes[0];

static const size_t ESP32_DATA_CHUNK_SIZE = 4096;

typedef struct {
  uint8_t buffer[ESP32_DATA_CHUNK_SIZE];
  size_t len;
} Esp32DataChunkWriter;

static uint16_t dirtyTileCount = 0; // number of {tx,ty} pairs
static uint8_t dirtyTiles[TILE_COUNT * 2];

static uint8_t* serialRxBuffer = nullptr;
static size_t serialRxBufferLen = 0;
static size_t serialRxBufferCapacity = 0;

static inline void dirtyTileBufClear() {
  dirtyTileCount = 0;
}

static inline void dirtyTileBufPush(uint8_t tx, uint8_t ty) {
  if (dirtyTileCount >= TILE_COUNT) return;
  uint16_t ii = (dirtyTileCount++) * 2;
  dirtyTiles[ii] = tx;
  dirtyTiles[ii + 1] = ty;
}

static void initEmptyRootNode() {
  node_pool_reset(&rootNodePool);
  Node* root = node_pool_alloc(&rootNodePool);
  if (root) {
    *root = nodeEmpty();
    rootNode = root;
    return;
  }

  rootNode = nullptr;
}

static void logHeapCaps(const char* label) {
  Serial.print(label);
  Serial.print(" dma_free=");
  Serial.print(heap_caps_get_free_size(MALLOC_CAP_DMA));
  Serial.print(" dma_largest=");
  Serial.print(heap_caps_get_largest_free_block(MALLOC_CAP_DMA));
  Serial.print(" internal_free=");
  Serial.print(heap_caps_get_free_size(MALLOC_CAP_INTERNAL));
  Serial.print(" internal_largest=");
  Serial.println(heap_caps_get_largest_free_block(MALLOC_CAP_INTERNAL));
}

static bool ensureSerialRxBufferCapacity(size_t size) {
  if (size <= serialRxBufferCapacity) {
    return true;
  }

  uint8_t* next = (uint8_t*)realloc(serialRxBuffer, size);
  if (!next) {
    complain("serialRxBuffer: realloc failed");
    return false;
  }

  serialRxBuffer = next;
  serialRxBufferCapacity = size;
  return true;
}

static void discardSerialRxPrefix(size_t count) {
  if (count >= serialRxBufferLen) {
    serialRxBufferLen = 0;
    return;
  }
  memmove(serialRxBuffer, serialRxBuffer + count, serialRxBufferLen - count);
  serialRxBufferLen -= count;
}

static bool serialWriteAll(const uint8_t* data, size_t len) {
  if (len == 0) {
    return true;
  }
  return Serial.write(data, len) == len;
}

static bool sendProtocolFrame(uint8_t frameType, const uint8_t* payload, size_t payloadLen);

static bool sendSerialFrameRaw(uint8_t opcode, const uint8_t* payload, size_t payloadLen) {
  uint8_t header[8];
  size_t headerLen = 0;
  if (!write_ws_like_frame_header(header, sizeof(header), opcode, payloadLen, &headerLen)) {
    complain("sendSerialFrameRaw: payload too large");
    return false;
  }
  if (!serialWriteAll(header, headerLen)) {
    complain("sendSerialFrameRaw: failed to write header");
    return false;
  }
  if (!serialWriteAll(payload, payloadLen)) {
    complain("sendSerialFrameRaw: failed to write payload");
    return false;
  }
  Serial.flush();
  return true;
}

// implicitly uses `nodeNew` and will walk it back-to-front and draw nodes
void drawTile(int tx0, int ty0) {
  BoundingBox tileBbox = { tx0, ty0, TILE_SIZE, TILE_SIZE };

  // Walk the new root and draw all nodes whose bbox intersects the tile
  nodeWalkPreOrderDFS(rootNode, [&](Node* node) -> bool {
    if (!bboxIntersects(&node->bbox, &tileBbox))
      return false; // Short-circuit if node not relevant to the tile

    switch (node->type) {
      case NODE_RECT:     node_draw_tileRect(node, tx0, ty0);     break;
      case NODE_RECTFILL: node_draw_tileRectFill(node, tx0, ty0); break;
      case NODE_XLINE:    node_draw_tileXLine(node, tx0, ty0);    break;
      case NODE_YLINE:    node_draw_tileYLine(node, tx0, ty0);    break;
      case NODE_TEXT:     node_draw_tileText(node, tx0, ty0);     break;
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

static size_t esp32DataPayloadLength() {
  size_t payloadLen = 15;
  for (int i = 0; i < NUM_FONTS; i++) {
    const FontMono1B* font = fonts[i];
    uint16_t nameLen = (uint16_t)strlen(font->name);
    payloadLen += 13 + nameLen + font_bits_byte_len(font);
  }
  return payloadLen;
}

static bool flushEsp32DataChunk(Esp32DataChunkWriter* writer) {
  if (writer->len == 0) {
    return true;
  }

  if (!sendProtocolFrame(MSG_ESP32_DATA_CHUNK, writer->buffer, writer->len)) {
    complain("flushEsp32DataChunk: failed to send chunk");
    return false;
  }

  writer->len = 0;
  return true;
}

static bool writeEsp32DataChunkBytes(Esp32DataChunkWriter* writer, const uint8_t* data, size_t len) {
  while (len > 0) {
    if (writer->len == ESP32_DATA_CHUNK_SIZE && !flushEsp32DataChunk(writer)) {
      return false;
    }

    size_t available = ESP32_DATA_CHUNK_SIZE - writer->len;
    size_t copyLen = len < available ? len : available;
    memcpy(writer->buffer + writer->len, data, copyLen);
    writer->len += copyLen;
    data += copyLen;
    len -= copyLen;
  }

  return true;
}

static bool writeEsp32DataChunkU8(Esp32DataChunkWriter* writer, uint8_t value) {
  return writeEsp32DataChunkBytes(writer, &value, sizeof(value));
}

static bool writeEsp32DataChunkU16Le(Esp32DataChunkWriter* writer, uint16_t value) {
  uint8_t bytes[2] = {
    (uint8_t)(value & 0xff),
    (uint8_t)((value >> 8) & 0xff),
  };
  return writeEsp32DataChunkBytes(writer, bytes, sizeof(bytes));
}

static bool writeEsp32DataChunkSizedString(Esp32DataChunkWriter* writer, const char* value) {
  size_t len = strlen(value);
  if (len > 0xffff) {
    complain("writeEsp32DataChunkSizedString: string too long");
    return false;
  }

  return writeEsp32DataChunkU16Le(writer, (uint16_t)len)
      && writeEsp32DataChunkBytes(writer, (const uint8_t*)value, len);
}

static bool writeEsp32DataChunkSizedU8List(Esp32DataChunkWriter* writer, const uint8_t* data, uint16_t count) {
  return writeEsp32DataChunkU16Le(writer, count)
      && writeEsp32DataChunkBytes(writer, data, count);
}

static bool sendEsp32DataFrame() {
  size_t payloadLen = esp32DataPayloadLength();
  if (payloadLen > 0xffffffffu) {
    complain("sendEsp32DataFrame: payload length exceeds protocol limit");
    return false;
  }

  size_t chunkCount = (payloadLen + ESP32_DATA_CHUNK_SIZE - 1) / ESP32_DATA_CHUNK_SIZE;
  if (chunkCount > 0xffff) {
    complain("sendEsp32DataFrame: chunk count exceeds protocol limit");
    return false;
  }

  uint8_t beginPayload[6] = {
    (uint8_t)(payloadLen & 0xff),
    (uint8_t)((payloadLen >> 8) & 0xff),
    (uint8_t)((payloadLen >> 16) & 0xff),
    (uint8_t)((payloadLen >> 24) & 0xff),
    (uint8_t)(chunkCount & 0xff),
    (uint8_t)((chunkCount >> 8) & 0xff),
  };
  Esp32DataChunkWriter writer = {};

  if (!sendProtocolFrame(MSG_ESP32_DATA_BEGIN, beginPayload, sizeof(beginPayload))) {
    complain("sendEsp32DataFrame: failed to write begin frame");
    return false;
  }

  if (!writeEsp32DataChunkU16Le(&writer, DISPLAY_W)
      || !writeEsp32DataChunkU16Le(&writer, DISPLAY_H)
      || !writeEsp32DataChunkU8(&writer, MY_CRT_PADDING_L)
      || !writeEsp32DataChunkU8(&writer, MY_CRT_PADDING_R)
      || !writeEsp32DataChunkU8(&writer, MY_CRT_PADDING_T)
      || !writeEsp32DataChunkU8(&writer, MY_CRT_PADDING_B)
      || !writeEsp32DataChunkU16Le(&writer, (uint16_t)MAX_TOTAL_NODES)
      || !writeEsp32DataChunkU16Le(&writer, (uint16_t)NODE_GROUP_MAX_CHILDREN)
      || !writeEsp32DataChunkU8(&writer, (uint8_t)TILE_SIZE)
      || !writeEsp32DataChunkU16Le(&writer, NUM_FONTS)) {
    complain("sendEsp32DataFrame: failed to write metadata payload");
    return false;
  }

  for (int i = 0; i < NUM_FONTS; i++) {
    const FontMono1B* font = fonts[i];
    if (!writeEsp32DataChunkSizedString(&writer, font->name)
        || !writeEsp32DataChunkU16Le(&writer, font->ascii_first)
        || !writeEsp32DataChunkU16Le(&writer, font->ascii_last)
        || !writeEsp32DataChunkU16Le(&writer, font->num_glyphs)
        || !writeEsp32DataChunkU8(&writer, font->glyph_w)
        || !writeEsp32DataChunkU8(&writer, font->glyph_h)
        || !writeEsp32DataChunkU8(&writer, font->extra_line_height)
        || !writeEsp32DataChunkSizedU8List(&writer, font->bits, font_bits_byte_len(font))) {
      complain("sendEsp32DataFrame: failed to write font payload");
      return false;
    }
  }

  if (!flushEsp32DataChunk(&writer)) {
    complain("sendEsp32DataFrame: failed to flush final chunk");
    return false;
  }

  if (!sendProtocolFrame(MSG_ESP32_DATA_END, nullptr, 0)) {
    complain("sendEsp32DataFrame: failed to write end frame");
    return false;
  }

  return true;
}

static bool sendProtocolFrame(uint8_t frameType, const uint8_t* payload, size_t payloadLen) {
  uint8_t prefix[2] = { frameType, PROTOCOL_VERSION };
  uint8_t header[8];
  size_t headerLen = 0;
  size_t frameLen = payloadLen + sizeof(prefix);

  if (!write_ws_like_frame_header(header, sizeof(header), OPCODE_BINARY, frameLen, &headerLen)) {
    complain("sendProtocolFrame: payload too large");
    return false;
  }

  if (!serialWriteAll(header, headerLen)) {
    complain("sendProtocolFrame: failed to write header");
    return false;
  }
  if (!serialWriteAll(prefix, sizeof(prefix))) {
    complain("sendProtocolFrame: failed to write frame prefix");
    return false;
  }
  if (!serialWriteAll(payload, payloadLen)) {
    complain("sendProtocolFrame: failed to write frame payload");
    return false;
  }

  Serial.flush();
  return true;
}

static bool sendErrorFrame(const char* msg) {
  return sendProtocolFrame(MSG_ERROR, (const uint8_t*)msg, strlen(msg));
}

static bool installRootNodeFromPayload(const uint8_t* payload, size_t payloadLen) {
  ReadBuffer readBuf;
  read_buffer_begin(&readBuf, payload, payloadLen);

  NodePool* pool = &rootNodePool;
  node_pool_reset(pool);
  Node* parsedNode = node_read(pool);
  if (!parsedNode || !readBuf.ok) {
    read_buffer_end();
    if (!hasComplained) complain("Failed to parse root node");
    return false;
  }

  dirtyTileBufClear();
  uint16_t count = read_u16_le();
  if (!readBuf.ok) {
    read_buffer_end();
    complain("Failed to read dirty tile count");
    return false;
  }
  if (count > TILE_COUNT) {
    complain("Dirty tile overflow: count > TILE_COUNT");
  }
  for (uint16_t i = 0; i < count; i++) {
    uint8_t tx = read_u8();
    uint8_t ty = read_u8();
    if (i < TILE_COUNT) {
      dirtyTileBufPush(tx, ty);
    }
  }

  bool ok = readBuf.ok;
  read_buffer_end();
  if (!ok) {
    complain("Failed to read full root node command");
    return false;
  }

  rootNode = parsedNode;
  redrawBufferedDirtyTiles();
  return true;
}

static bool handleBinaryFrame(const uint8_t* payload, size_t payloadLen) {
  if (payloadLen < 2) {
    complain("handleBinaryFrame: short frame");
    return sendErrorFrame("short frame");
  }

  uint8_t frameType = payload[0];
  uint8_t version = payload[1];
  const uint8_t* framePayload = payload + 2;
  size_t framePayloadLen = payloadLen - 2;

  if (version != PROTOCOL_VERSION) {
    complain("handleBinaryFrame: unsupported protocol version");
    return sendErrorFrame("unsupported protocol version");
  }

  switch (frameType) {
    case MSG_GET_ESP32_DATA: {
      initEmptyRootNode();
      return sendEsp32DataFrame();
    }

    case MSG_SET_ROOT_NODE: {
      if (!installRootNodeFromPayload(framePayload, framePayloadLen)) {
        return sendErrorFrame("failed to install root node");
      }
      return sendProtocolFrame(MSG_ACK, nullptr, 0);
    }

    default:
      complain("handleBinaryFrame: unknown frame type");
      return sendErrorFrame("unknown frame type");
  }
}

static bool handleTransportFrame(uint8_t opcode, uint8_t* payload, size_t payloadLen) {
  switch (opcode) {
    case OPCODE_BINARY:
      return handleBinaryFrame(payload, payloadLen);

    case OPCODE_CLOSE:
      return sendSerialFrameRaw(OPCODE_CLOSE, nullptr, 0);

    case OPCODE_PING:
      return sendSerialFrameRaw(OPCODE_PONG, payload, payloadLen);

    case OPCODE_PONG:
      return true;

    default:
      complain("Serial frame: unsupported opcode");
      return sendErrorFrame("unsupported serial opcode");
  }
}

static void processSerialRxBuffer() {
  while (serialRxBufferLen > 0) {
    WsLikeFrame frame;
    WsLikeParseStatus status = try_parse_ws_like_frame(serialRxBuffer, serialRxBufferLen, &frame);
    if (status == WS_LIKE_PARSE_NEED_MORE) {
      return;
    }

    if (status == WS_LIKE_PARSE_INVALID) {
      complain("Serial frame: invalid header");
      discardSerialRxPrefix(1);
      continue;
    }

    uint8_t* payload = serialRxBuffer + frame.header_len;
    if (frame.masked) {
      unmask_ws_like_payload(payload, frame.payload_len, frame.mask);
    }

    if (!handleTransportFrame(frame.opcode, payload, frame.payload_len)) {
      complain("Serial frame: handler failed");
    }

    discardSerialRxPrefix(frame.header_len + frame.payload_len);
  }
}

static void pollSerialTransport() {
  while (Serial.available() > 0) {
    int avail = Serial.available();
    if (avail <= 0) {
      return;
    }

    size_t nextLen = serialRxBufferLen + (size_t)avail;
    if (nextLen > SERIAL_FRAME_BUFFER_LIMIT) {
      complain("Serial frame: rx buffer overflow");
      serialRxBufferLen = 0;
      while (Serial.available() > 0) {
        Serial.read();
      }
      return;
    }

    if (!ensureSerialRxBufferCapacity(nextLen)) {
      while (Serial.available() > 0) {
        Serial.read();
      }
      return;
    }

    int readCount = Serial.readBytes((char*)(serialRxBuffer + serialRxBufferLen), (size_t)avail);
    if (readCount <= 0) {
      return;
    }

    serialRxBufferLen += (size_t)readCount;
    processSerialRxBuffer();
  }
}

void setup()
{
  // ESP32-specific
  pinMode(LED_PIN, OUTPUT);
  Serial.begin(115200);
  delay(50);
  logHeapCaps("setup: before video.init");

  // esp32lib-specific
  video.init(CompMode::MODENTSC240P, DAC_PIN, HAVE_VOLTAGE_DIVIDER);
  logHeapCaps("setup: after video.init");

  // Start with empty scenes.
  initEmptyRootNode();
  logHeapCaps("setup: serial transport ready");
  logSerial("Serial transport ready");
}

void loop()
{
  pollSerialTransport();
}
