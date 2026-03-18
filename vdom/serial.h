#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "miniz.h"

#include "prelude.h"

// Parsing numbers from u8*

static inline uint32_t parse_u32_le(const uint8_t *p) {
  return (uint32_t)p[0]
      | ((uint32_t)p[1] << 8) 
      | ((uint32_t)p[2] << 16) 
      | ((uint32_t)p[3] << 24);
}
static inline uint16_t parse_u16_le(const uint8_t *p) {
  return (uint16_t)p[0] 
      | ((uint16_t)p[1] << 8);
}
static inline int32_t parse_i32_le(const uint8_t *p) {
  return (int32_t)parse_u32_le(p);
}
static inline int16_t parse_i16_le(const uint8_t *p) {
  return (int16_t)parse_u16_le(p);
}

typedef struct {
  const uint8_t* data;
  size_t len;
  size_t offset;
  bool ok;
} ReadBuffer;

typedef struct {
  uint8_t* data;
  size_t capacity;
  size_t len;
  bool ok;
} WriteBuffer;

typedef struct {
  uint8_t opcode;
  size_t header_len;
  size_t payload_len;
  bool masked;
  uint8_t mask[4];
} WsLikeFrame;

typedef enum {
  WS_LIKE_PARSE_INVALID = 0,
  WS_LIKE_PARSE_NEED_MORE = 1,
  WS_LIKE_PARSE_OK = 2,
} WsLikeParseStatus;

static ReadBuffer* activeReadBuffer = nullptr;
static WriteBuffer* activeWriteBuffer = nullptr;

static inline bool write_ws_like_frame_header(
    uint8_t* out,
    size_t out_capacity,
    uint8_t opcode,
    size_t payload_len,
    size_t* out_len) {
  if (!out || !out_len) {
    return false;
  }

  if (payload_len > 0xffff) {
    return false;
  }

  size_t header_len = payload_len <= 125 ? 2 : 4;
  if (out_capacity < header_len) {
    return false;
  }

  out[0] = (uint8_t)(0x80 | (opcode & 0x0f));
  if (payload_len <= 125) {
    out[1] = (uint8_t)payload_len;
  } else {
    out[1] = 126;
    out[2] = (uint8_t)((payload_len >> 8) & 0xff);
    out[3] = (uint8_t)(payload_len & 0xff);
  }

  *out_len = header_len;
  return true;
}

static inline WsLikeParseStatus try_parse_ws_like_frame(
    const uint8_t* data,
    size_t len,
    WsLikeFrame* out) {
  if (!data || !out) {
    return WS_LIKE_PARSE_INVALID;
  }

  if (len < 2) {
    return WS_LIKE_PARSE_NEED_MORE;
  }

  if ((data[0] & 0x80) == 0) {
    return WS_LIKE_PARSE_INVALID;
  }

  size_t header_len = 2;
  size_t payload_len = (size_t)(data[1] & 0x7f);
  bool masked = (data[1] & 0x80) != 0;

  if (payload_len == 126) {
    if (len < 4) {
      return WS_LIKE_PARSE_NEED_MORE;
    }
    header_len = 4;
    payload_len = ((size_t)data[2] << 8) | (size_t)data[3];
  } else if (payload_len == 127) {
    return WS_LIKE_PARSE_INVALID;
  }

  if (masked) {
    if (len < header_len + 4) {
      return WS_LIKE_PARSE_NEED_MORE;
    }
    memcpy(out->mask, data + header_len, 4);
    header_len += 4;
  } else {
    memset(out->mask, 0, sizeof(out->mask));
  }

  if (len < header_len + payload_len) {
    return WS_LIKE_PARSE_NEED_MORE;
  }

  out->opcode = (uint8_t)(data[0] & 0x0f);
  out->header_len = header_len;
  out->payload_len = payload_len;
  out->masked = masked;
  return WS_LIKE_PARSE_OK;
}

static inline void unmask_ws_like_payload(uint8_t* payload, size_t payload_len, const uint8_t mask[4]) {
  if (!payload || !mask) {
    return;
  }
  for (size_t i = 0; i < payload_len; ++i) {
    payload[i] ^= mask[i & 3];
  }
}

static inline void read_buffer_begin(ReadBuffer* buf, const uint8_t* data, size_t len) {
  buf->data = data;
  buf->len = len;
  buf->offset = 0;
  buf->ok = true;
  activeReadBuffer = buf;
}

static inline void read_buffer_end() {
  activeReadBuffer = nullptr;
}

static inline size_t read_buffer_remaining() {
  if (!activeReadBuffer || activeReadBuffer->offset > activeReadBuffer->len) {
    return 0;
  }
  return activeReadBuffer->len - activeReadBuffer->offset;
}

static inline void write_buffer_begin(WriteBuffer* buf, uint8_t* data, size_t capacity) {
  buf->data = data;
  buf->capacity = capacity;
  buf->len = 0;
  buf->ok = true;
  activeWriteBuffer = buf;
}

static inline void write_buffer_end() {
  activeWriteBuffer = nullptr;
}

static inline size_t write_buffer_len(const WriteBuffer* buf) {
  return buf ? buf->len : 0;
}

static inline bool write_buffer_ok(const WriteBuffer* buf) {
  return buf && buf->ok;
}

static inline uint8_t read_u8() {
  if (!activeReadBuffer) {
    complain("read_u8: no active read buffer");
    return 0;
  }
  if (activeReadBuffer->offset >= activeReadBuffer->len) {
    activeReadBuffer->ok = false;
    complain("read_u8: buffer underrun");
    return 0;
  }
  return activeReadBuffer->data[activeReadBuffer->offset++];
}

static inline uint32_t read_le(int nbytes) {
  uint32_t v = 0;
  for (int i = 0; i < nbytes; ++i) {
    v |= ((uint32_t)read_u8()) << (i * 8);
  }
  return v;
}
static inline uint32_t read_u32_le() { return read_le(4); }
static inline uint16_t read_u16_le() { return (uint16_t)read_le(2); }
static inline int32_t read_i32_le() { return (int32_t)read_u32_le(); }
static inline int16_t read_i16_le() { return (int16_t)read_u16_le(); }

static inline bool write_bytes(const uint8_t* data, size_t count) {
  if (!activeWriteBuffer) {
    complain("write_bytes: no active write buffer");
    return false;
  }
  if (activeWriteBuffer->len + count > activeWriteBuffer->capacity) {
    activeWriteBuffer->ok = false;
    complain("write_bytes: buffer overflow");
    return false;
  }
  memcpy(activeWriteBuffer->data + activeWriteBuffer->len, data, count);
  activeWriteBuffer->len += count;
  return true;
}

static inline void write_u8(uint8_t v) {
  write_bytes(&v, 1);
}

static inline void write_u16_le(uint16_t v) {
  // PERF TODO: On ESP32, this could be replaced with
  // `write_bytes((const uint8_t*)&v, sizeof(v))` because the target is
  // little-endian. Upside: less byte packing code, and possibly slightly less
  // work. Downside: it makes the wire encoding depend on target endianness and
  // is less self-documenting/portable than spelling out the LE layout here.
  uint8_t bytes[2] = {
    (uint8_t)(v & 0xFF),
    (uint8_t)((v >> 8) & 0xFF),
  };
  write_bytes(bytes, sizeof(bytes));
}

static inline void write_u32_le(uint32_t v) {
  // PERF TODO: Same tradeoff as `write_u16_le` above. A raw
  // `write_bytes((const uint8_t*)&v, sizeof(v))` write is fine on ESP32 if we
  // intentionally tie the protocol helper to little-endian memory layout.
  uint8_t bytes[4] = {
    (uint8_t)(v & 0xFF),
    (uint8_t)((v >> 8) & 0xFF),
    (uint8_t)((v >> 16) & 0xFF),
    (uint8_t)((v >> 24) & 0xFF),
  };
  write_bytes(bytes, sizeof(bytes));
}

static inline void write_i32_le(int32_t v) {
  write_u32_le((uint32_t)v);
}

static inline void write_sized_string(const char* s) {
  uint16_t len = (uint16_t)strlen(s);
  write_u16_le(len);
  write_bytes((const uint8_t*)s, len);
}

// Sized list of uint8: length (u16 LE) then that many bytes.
static inline void write_sized_u8_list(const uint8_t* data, uint16_t count) {
  write_u16_le(count);
  write_bytes(data, count);
}

// Length-prefixed (u16 LE) string: reads length, allocates len+1, reads bytes, NUL-terminates. Caller must free.
static inline char* read_sized_string(void) {
  uint16_t len = read_u16_le();
  if (len == 0) {
    // Represent empty strings as a valid allocated "" so downstream code
    // (e.g. font_textMultilineSize) never sees a NULL pointer.
    char* buf = (char*)malloc(1);
    if (!buf) {
      complain("read_sized_string: malloc failed for empty string");
      return nullptr;
    }
    buf[0] = '\0';
    return buf;
  }
  char* buf = (char*)malloc((size_t)len + 1);
  if (!buf) {
    complain("read_sized_string: malloc failed");
    return NULL;
  }
  if (read_buffer_remaining() < len) {
    complain("read_sized_string: buffer underrun");
    activeReadBuffer->ok = false;
    free(buf);
    return nullptr;
  }
  memcpy(buf, activeReadBuffer->data + activeReadBuffer->offset, len);
  activeReadBuffer->offset += len;
  buf[len] = '\0';
  return buf;
}

// Zlib-compressed length-prefixed string: u16 decompressed_len, u16 compressed_len, then
// compressed_len bytes of zlib data. Allocates decompressed_len+1, NUL-terminates. Caller must free.
static inline char* read_sized_compressed_string(void) {
  uint16_t decompressed_len = read_u16_le();
  uint16_t compressed_len = read_u16_le();
  if (decompressed_len == 0) {
    char* buf = (char*)malloc(1);
    if (!buf) {
      complain("read_sized_compressed_string: malloc failed for empty string");
      return nullptr;
    }
    buf[0] = '\0';
    return buf;
  }
  uint8_t* inbuf = (uint8_t*)malloc(compressed_len);
  if (!inbuf) {
    complain("read_sized_compressed_string: malloc inbuf failed");
    return nullptr;
  }
  if (read_buffer_remaining() < compressed_len) {
    complain("read_sized_compressed_string: buffer underrun");
    activeReadBuffer->ok = false;
    free(inbuf);
    return nullptr;
  }
  memcpy(inbuf, activeReadBuffer->data + activeReadBuffer->offset, compressed_len);
  activeReadBuffer->offset += compressed_len;
  char* outbuf = (char*)malloc((size_t)decompressed_len + 1);
  if (!outbuf) {
    complain("read_sized_compressed_string: malloc outbuf failed");
    free(inbuf);
    return nullptr;
  }
  // tinfl_decompressor is ~32KB; ESP32 loop task stack is small — keep it in BSS.
  static tinfl_decompressor decomp;
  tinfl_init(&decomp);
  size_t inpos = 0, outpos = 0;
  const int flags = TINFL_FLAG_PARSE_ZLIB_HEADER | TINFL_FLAG_USING_NON_WRAPPING_OUTPUT_BUF;
  for (;;) {
    size_t inbytes = (size_t)compressed_len - inpos;
    size_t outbytes = (size_t)decompressed_len - outpos;
    tinfl_status st = tinfl_decompress(&decomp, &inbuf[inpos], &inbytes,
        (mz_uint8*)outbuf, (mz_uint8*)&outbuf[outpos], &outbytes, (mz_uint32)flags);
    inpos += inbytes;
    outpos += outbytes;
    if (st == TINFL_STATUS_DONE)
      break;
    if (st < TINFL_STATUS_DONE) {
      complain("read_sized_compressed_string: tinfl_decompress failed");
      free(inbuf);
      free(outbuf);
      return nullptr;
    }
  }
  free(inbuf);
  outbuf[decompressed_len] = '\0';
  return outbuf;
}