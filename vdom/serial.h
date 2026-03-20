#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "miniz.h"

#include "prelude.h"

typedef struct {
  const uint8_t* data;
  size_t len;
  size_t offset;
  bool ok;
} ReadBuffer;

typedef struct {
  uint8_t opcode;
  size_t header_len;
  size_t payload_len;
  bool masked;
  uint8_t mask[4];
} WsLikeFrame;

typedef enum : uint8_t {
  TRANSPORT_OPCODE_BINARY = 0x2,
  TRANSPORT_OPCODE_CLOSE = 0x8,
} TransportOpcode;

typedef enum {
  WS_LIKE_PARSE_INVALID = 0,
  WS_LIKE_PARSE_NEED_MORE = 1,
  WS_LIKE_PARSE_OK = 2,
} WsLikeParseStatus;

typedef bool (*BufferedByteWriterFlush)(void* ctx, const uint8_t* data, size_t len);

typedef struct {
  uint8_t* buffer;
  size_t len;
  size_t capacity;
  BufferedByteWriterFlush flush;
  void* flush_ctx;
} BufferedByteWriter;

static ReadBuffer* activeReadBuffer = nullptr;

static inline void buffered_byte_writer_begin(
    BufferedByteWriter* writer,
    uint8_t* buffer,
    size_t capacity,
    BufferedByteWriterFlush flush,
    void* flush_ctx) {
  writer->buffer = buffer;
  writer->len = 0;
  writer->capacity = capacity;
  writer->flush = flush;
  writer->flush_ctx = flush_ctx;
}

static inline bool buffered_byte_writer_flush(BufferedByteWriter* writer) {
  if (!writer || writer->len == 0) {
    return true;
  }
  if (!writer->flush || !writer->flush(writer->flush_ctx, writer->buffer, writer->len)) {
    return false;
  }
  writer->len = 0;
  return true;
}

static inline bool buffered_byte_writer_write_bytes(
    BufferedByteWriter* writer,
    const uint8_t* data,
    size_t len) {
  if (!writer) {
    return false;
  }
  if (len == 0) {
    return true;
  }
  if (!data || !writer->buffer || writer->capacity == 0) {
    return false;
  }

  while (len > 0) {
    if (writer->len == writer->capacity && !buffered_byte_writer_flush(writer)) {
      return false;
    }

    size_t available = writer->capacity - writer->len;
    size_t copy_len = len < available ? len : available;
    memcpy(writer->buffer + writer->len, data, copy_len);
    writer->len += copy_len;
    data += copy_len;
    len -= copy_len;
  }

  return true;
}

static inline bool buffered_byte_writer_write_u8(BufferedByteWriter* writer, uint8_t value) {
  return buffered_byte_writer_write_bytes(writer, &value, sizeof(value));
}

static inline bool buffered_byte_writer_write_u16_le(BufferedByteWriter* writer, uint16_t value) {
  uint8_t bytes[2] = {
    (uint8_t)(value & 0xff),
    (uint8_t)((value >> 8) & 0xff),
  };
  return buffered_byte_writer_write_bytes(writer, bytes, sizeof(bytes));
}

static inline bool buffered_byte_writer_write_u16_sized_bytes(
    BufferedByteWriter* writer,
    const uint8_t* data,
    size_t len) {
  if (len > 0xffff) {
    return false;
  }
  return buffered_byte_writer_write_u16_le(writer, (uint16_t)len)
      && buffered_byte_writer_write_bytes(writer, data, len);
}

static inline bool buffered_byte_writer_write_u16_sized_string(
    BufferedByteWriter* writer,
    const char* value) {
  if (!value) {
    return false;
  }
  return buffered_byte_writer_write_u16_sized_bytes(
      writer,
      (const uint8_t*)value,
      strlen(value));
}

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

static inline uint8_t* read_zlib_bytes(uint16_t compressed_len, size_t decompressed_len, bool nul_terminate) {
  if (decompressed_len == 0) {
    if (compressed_len != 0) {
      complain("read_zlib_bytes: non-zero compressed length for empty payload");
      activeReadBuffer->ok = false;
    }
    if (!nul_terminate) {
      return nullptr;
    }
    uint8_t* buf = (uint8_t*)malloc(1);
    if (!buf) {
      complain("read_zlib_bytes: malloc failed for empty payload");
      return nullptr;
    }
    buf[0] = '\0';
    return buf;
  }
  if (compressed_len == 0) {
    complain("read_zlib_bytes: compressed length is zero for non-empty payload");
    activeReadBuffer->ok = false;
    return nullptr;
  }
  if (read_buffer_remaining() < compressed_len) {
    complain("read_zlib_bytes: buffer underrun");
    activeReadBuffer->ok = false;
    return nullptr;
  }
  uint8_t* inbuf = (uint8_t*)malloc(compressed_len);
  if (!inbuf) {
    complain("read_zlib_bytes: malloc inbuf failed");
    return nullptr;
  }
  memcpy(inbuf, activeReadBuffer->data + activeReadBuffer->offset, compressed_len);
  activeReadBuffer->offset += compressed_len;
  size_t outcap = decompressed_len + (nul_terminate ? 1 : 0);
  uint8_t* outbuf = (uint8_t*)malloc(outcap);
  if (!outbuf) {
    complain("read_zlib_bytes: malloc outbuf failed");
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
      complain("read_zlib_bytes: tinfl_decompress failed");
      free(inbuf);
      free(outbuf);
      return nullptr;
    }
  }
  free(inbuf);
  if (outpos != decompressed_len) {
    complain("read_zlib_bytes: decompressed length mismatch");
    free(outbuf);
    return nullptr;
  }
  if (nul_terminate) {
    outbuf[decompressed_len] = '\0';
  }
  return outbuf;
}

// Zlib-compressed length-prefixed byte buffer: u16 decompressed_len, u16 compressed_len,
// then compressed_len bytes of zlib data.
static inline bool read_sized_compressed_bytes_header(uint16_t* decompressed_len_out,
                                                      uint16_t* compressed_len_out) {
  *decompressed_len_out = read_u16_le();
  *compressed_len_out = read_u16_le();
  return activeReadBuffer->ok;
}

// Zlib-compressed length-prefixed string: u16 decompressed_len, u16 compressed_len, then
// compressed_len bytes of zlib data. Allocates decompressed_len+1, NUL-terminates. Caller must free.
static inline char* read_sized_compressed_string(void) {
  uint16_t decompressed_len = read_u16_le();
  uint16_t compressed_len = read_u16_le();
  return (char*)read_zlib_bytes(compressed_len, decompressed_len, true);
}