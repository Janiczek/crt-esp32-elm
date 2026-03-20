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

static const size_t SERIAL_FRAME_BUFFER_LIMIT = 65539;

typedef enum : uint8_t {
  APP_FRAME_GET_ESP32_DATA = 0x01,
  APP_FRAME_ESP32_DATA_BEGIN = 0x02,
  APP_FRAME_SET_ROOT_NODE = 0x03,
  APP_FRAME_ACK = 0x04,
  APP_FRAME_LOG = 0x05,
  APP_FRAME_ERROR = 0x06,
  APP_FRAME_ESP32_DATA_CHUNK = 0x07,
  APP_FRAME_ESP32_DATA_END = 0x08,
} AppFrameType;

//----------------------------------------------
// VDOM-specific

static NodePool rootNodePool;
Node* rootNode = &rootNodePool.nodes[0];

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

// Transport writes stay here because they depend on Arduino `Serial`.
static bool sendAppFrame(uint8_t frameType, const uint8_t* payload, size_t payloadLen);

static bool sendTransportFrame(uint8_t opcode, const uint8_t* payload, size_t payloadLen) {
  uint8_t header[8];
  size_t headerLen = 0;
  if (!write_ws_like_frame_header(header, sizeof(header), opcode, payloadLen, &headerLen)) {
    complain("sendTransportFrame: payload too large");
    return false;
  }
  if (!serialWriteAll(header, headerLen)) {
    complain("sendTransportFrame: failed to write header");
    return false;
  }
  if (!serialWriteAll(payload, payloadLen)) {
    complain("sendTransportFrame: failed to write payload");
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
      case NODE_BITMAP:   node_draw_tileBitmap(node, tx0, ty0);   break;
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

static bool flushEsp32DataChunkFrame(void*, const uint8_t* data, size_t len) {
  if (!sendAppFrame(APP_FRAME_ESP32_DATA_CHUNK, data, len)) {
    complain("flushEsp32DataChunkFrame: failed to send chunk");
    return false;
  }
  return true;
}

// The payload schema is VDOM-specific; `serial.h` only provides byte packing helpers.
static bool writeEsp32DataPayload(BufferedByteWriter* writer) {
  if (!buffered_byte_writer_write_u16_le(writer, DISPLAY_W)
      || !buffered_byte_writer_write_u16_le(writer, DISPLAY_H)
      || !buffered_byte_writer_write_u8(writer, MY_CRT_PADDING_L)
      || !buffered_byte_writer_write_u8(writer, MY_CRT_PADDING_R)
      || !buffered_byte_writer_write_u8(writer, MY_CRT_PADDING_T)
      || !buffered_byte_writer_write_u8(writer, MY_CRT_PADDING_B)
      || !buffered_byte_writer_write_u16_le(writer, (uint16_t)MAX_TOTAL_NODES)
      || !buffered_byte_writer_write_u16_le(writer, (uint16_t)NODE_GROUP_MAX_CHILDREN)
      || !buffered_byte_writer_write_u8(writer, (uint8_t)TILE_SIZE)
      || !buffered_byte_writer_write_u16_le(writer, NUM_FONTS)) {
    complain("writeEsp32DataPayload: failed to write metadata payload");
    return false;
  }

  for (int i = 0; i < NUM_FONTS; i++) {
    const FontMono1B* font = fonts[i];
    if (!buffered_byte_writer_write_u16_sized_string(writer, font->name)
        || !buffered_byte_writer_write_u16_le(writer, font->ascii_first)
        || !buffered_byte_writer_write_u16_le(writer, font->ascii_last)
        || !buffered_byte_writer_write_u16_le(writer, font->num_glyphs)
        || !buffered_byte_writer_write_u8(writer, font->glyph_w)
        || !buffered_byte_writer_write_u8(writer, font->glyph_h)
        || !buffered_byte_writer_write_u8(writer, font->extra_line_height)
        || !buffered_byte_writer_write_u16_sized_bytes(writer, font->bits, font_bits_byte_len(font))) {
      complain("writeEsp32DataPayload: failed to write font payload");
      return false;
    }
  }

  return true;
}

static bool streamEsp32DataChunks(
    uint8_t* rawChunkBuffer,
    size_t rawChunkBufferCapacity) {
  BufferedByteWriter writer;
  buffered_byte_writer_begin(
      &writer,
      rawChunkBuffer,
      rawChunkBufferCapacity,
      flushEsp32DataChunkFrame,
      nullptr);
  return writeEsp32DataPayload(&writer)
      && buffered_byte_writer_flush(&writer);
}

static bool sendEsp32DataFrame() {
  size_t rawPayloadLen = esp32DataPayloadLength();
  if (rawPayloadLen > 0xffffffffu) {
    complain("sendEsp32DataFrame: payload length exceeds protocol limit");
    return false;
  }

  if (ESP32_DATA_CHUNK_SIZE == 0) {
    complain("sendEsp32DataFrame: chunk size must be non-zero");
    return false;
  }

  uint8_t* rawChunkBuffer = (uint8_t*)malloc(ESP32_DATA_CHUNK_SIZE);
  if (!rawChunkBuffer) {
    complain("sendEsp32DataFrame: chunk buffer allocation failed");
    return false;
  }

  size_t chunkCount = (rawPayloadLen + ESP32_DATA_CHUNK_SIZE - 1) / ESP32_DATA_CHUNK_SIZE;
  if (chunkCount > 0xffff) {
    free(rawChunkBuffer);
    complain("sendEsp32DataFrame: chunk count exceeds protocol limit");
    return false;
  }

  uint8_t beginPayload[6] = {
    (uint8_t)(rawPayloadLen & 0xff),
    (uint8_t)((rawPayloadLen >> 8) & 0xff),
    (uint8_t)((rawPayloadLen >> 16) & 0xff),
    (uint8_t)((rawPayloadLen >> 24) & 0xff),
    (uint8_t)(chunkCount & 0xff),
    (uint8_t)((chunkCount >> 8) & 0xff),
  };

  if (!sendAppFrame(APP_FRAME_ESP32_DATA_BEGIN, beginPayload, sizeof(beginPayload))) {
    free(rawChunkBuffer);
    complain("sendEsp32DataFrame: failed to write begin frame");
    return false;
  }

  if (!streamEsp32DataChunks(rawChunkBuffer, ESP32_DATA_CHUNK_SIZE)) {
    free(rawChunkBuffer);
    complain("sendEsp32DataFrame: failed to stream raw chunks");
    return false;
  }

  if (!sendAppFrame(APP_FRAME_ESP32_DATA_END, nullptr, 0)) {
    free(rawChunkBuffer);
    complain("sendEsp32DataFrame: failed to write end frame");
    return false;
  }

  free(rawChunkBuffer);
  return true;
}

static bool sendAppFrame(uint8_t frameType, const uint8_t* payload, size_t payloadLen) {
  uint8_t prefix[1] = { frameType };
  uint8_t header[8];
  size_t headerLen = 0;
  size_t frameLen = payloadLen + sizeof(prefix);

  if (!write_ws_like_frame_header(header, sizeof(header), TRANSPORT_OPCODE_BINARY, frameLen, &headerLen)) {
    complain("sendAppFrame: payload too large");
    return false;
  }

  if (!serialWriteAll(header, headerLen)) {
    complain("sendAppFrame: failed to write header");
    return false;
  }
  if (!serialWriteAll(prefix, sizeof(prefix))) {
    complain("sendAppFrame: failed to write frame prefix");
    return false;
  }
  if (!serialWriteAll(payload, payloadLen)) {
    complain("sendAppFrame: failed to write frame payload");
    return false;
  }

  Serial.flush();
  return true;
}

static bool sendErrorFrame(const char* msg) {
  return sendAppFrame(APP_FRAME_ERROR, (const uint8_t*)msg, strlen(msg));
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
  if (payloadLen < 1) {
    complain("handleBinaryFrame: short frame");
    return sendErrorFrame("short frame");
  }

  uint8_t frameType = payload[0];
  const uint8_t* framePayload = payload + 1;
  size_t framePayloadLen = payloadLen - 1;

  switch (frameType) {
    case APP_FRAME_GET_ESP32_DATA: {
      initEmptyRootNode();
      return sendEsp32DataFrame();
    }

    case APP_FRAME_SET_ROOT_NODE: {
      if (!installRootNodeFromPayload(framePayload, framePayloadLen)) {
        return sendErrorFrame("failed to install root node");
      }
      return sendAppFrame(APP_FRAME_ACK, nullptr, 0);
    }

    default:
      complain("handleBinaryFrame: unknown frame type");
      return sendErrorFrame("unknown frame type");
  }
}

static bool handleTransportFrame(uint8_t opcode, uint8_t* payload, size_t payloadLen) {
  switch (opcode) {
    case TRANSPORT_OPCODE_BINARY:
      return handleBinaryFrame(payload, payloadLen);

    case TRANSPORT_OPCODE_CLOSE:
      return sendTransportFrame(TRANSPORT_OPCODE_CLOSE, nullptr, 0);

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
