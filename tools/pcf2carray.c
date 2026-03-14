// pcf2carray: convert PCF font to a single .h file with C arrays (ASCII 32-126).

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ASCII_FIRST  32
#define ASCII_LAST  126
#define NUM_GLYPHS  95

#define PCF_PROPERTIES     (1<<0)
#define PCF_ACCELERATORS   (1<<1)
#define PCF_METRICS        (1<<2)
#define PCF_BITMAPS        (1<<3)
#define PCF_BDF_ENCODINGS  (1<<5)

#define PCF_COMPRESSED_METRICS 0x00000100
#define PCF_GLYPH_PAD_MASK     3
#define PCF_BYTE_MASK          (1<<2)
#define PCF_BIT_MASK           (1<<3)

#define PCF_MAGIC "\1fcp"

static void usage(const char *prog) {
  fprintf(stderr, "Usage: %s [-r] [input.pcf] [output.h]\n", prog);
  fprintf(stderr, "  -r  reverse bit order (for PCFs that declare one order but store the other)\n");
}

static unsigned int read_u32_le(FILE *f) {
  unsigned char b[4];
  if (fread(b, 1, 4, f) != 4) return 0;
  return (unsigned int)b[0] | ((unsigned int)b[1]<<8) | ((unsigned int)b[2]<<16) | ((unsigned int)b[3]<<24);
}

static unsigned int read_u32_be(FILE *f) {
  unsigned char b[4];
  if (fread(b, 1, 4, f) != 4) return 0;
  return ((unsigned int)b[0]<<24) | ((unsigned int)b[1]<<16) | ((unsigned int)b[2]<<8) | (unsigned int)b[3];
}

static unsigned short read_u16_le(FILE *f) {
  unsigned char b[2];
  if (fread(b, 1, 2, f) != 2) return 0;
  return (unsigned short)(b[0] | (b[1]<<8));
}

static unsigned short read_u16_be(FILE *f) {
  unsigned char b[2];
  if (fread(b, 1, 2, f) != 2) return 0;
  return (unsigned short)((b[0]<<8) | b[1]);
}

/* Return font identifier from output path */
static void output_path_to_id(const char *path, char *id_buf, size_t id_size) {
  const char *base = strrchr(path, '/');
  base = base ? base + 1 : path;
  size_t j = 0;
  for (; j < id_size - 1 && base[j] && base[j] != '.'; j++) {
    char c = base[j];
    if (c == '-') c = '_';
    else if (c >= 'A' && c <= 'Z') c = (char)(c - 'A' + 'a');
    id_buf[j] = c;
  }
  id_buf[j] = '\0';
}

typedef struct {
  int left_bearing;
  int right_bearing;
  int width;
  int ascent;
  int descent;
} PCFMetric;

typedef struct {
  unsigned int type;
  unsigned int format;
  unsigned int size;
  unsigned int offset;
} TOCEntry;

static int process_pcf(const char *input_path, const char *output_path, int reverse_bits) {
  FILE *f = fopen(input_path, "rb");
  if (!f) {
    fprintf(stderr, "pcf2carray: cannot open %s\n", input_path);
    return 1;
  }

  char magic[4];
  if (fread(magic, 1, 4, f) != 4 || memcmp(magic, PCF_MAGIC, 4) != 0) {
    fprintf(stderr, "pcf2carray: not a PCF file (bad magic)\n");
    fclose(f);
    return 1;
  }

  unsigned int table_count = read_u32_le(f);
  if (table_count == 0 || table_count > 64) {
    fprintf(stderr, "pcf2carray: invalid table count\n");
    fclose(f);
    return 1;
  }

  TOCEntry *toc = (TOCEntry *)malloc(table_count * sizeof(TOCEntry));
  if (!toc) {
    fclose(f);
    return 1;
  }
  for (unsigned int i = 0; i < table_count; i++) {
    toc[i].type   = read_u32_le(f);
    toc[i].format = read_u32_le(f);
    toc[i].size   = read_u32_le(f);
    toc[i].offset = read_u32_le(f);
  }

  unsigned int metrics_off = 0, metrics_fmt = 0;
  unsigned int bitmaps_off = 0, bitmaps_fmt = 0;
  unsigned int enc_off = 0;
  for (unsigned int i = 0; i < table_count; i++) {
    if (toc[i].type == PCF_METRICS) {
      metrics_off = toc[i].offset;
      metrics_fmt = toc[i].format;
    } else if (toc[i].type == PCF_BITMAPS) {
      bitmaps_off = toc[i].offset;
      bitmaps_fmt = toc[i].format;
    } else if (toc[i].type == PCF_BDF_ENCODINGS) {
      enc_off = toc[i].offset;
    }
  }
  if (!metrics_off || !bitmaps_off || !enc_off) {
    fprintf(stderr, "pcf2carray: missing METRICS, BITMAPS, or BDF_ENCODINGS table\n");
    free(toc);
    fclose(f);
    return 1;
  }

  /* --- BDF encodings --- */
  if (fseek(f, (long)enc_off, SEEK_SET) != 0) {
    fprintf(stderr, "pcf2carray: seek to encoding table failed\n");
    free(toc);
    fclose(f);
    return 1;
  }
  unsigned int enc_fmt = read_u32_le(f); /* format (always LSB); table body uses enc_fmt byte order */
  unsigned short (*read_u16_enc)(FILE*) = (enc_fmt & PCF_BYTE_MASK) ? read_u16_be : read_u16_le;
  int min_char = (int)read_u16_enc(f);
  int max_char = (int)read_u16_enc(f);
  int min_byte1 = (int)read_u16_enc(f);
  int max_byte1 = (int)read_u16_enc(f);
  (void)read_u16_enc(f); /* default_char */
  /* Single-byte: min_byte1==max_byte1==0. Two-byte: we use byte1=0 for ASCII. */
  if (min_byte1 > 0 || max_byte1 < 0) {
    fprintf(stderr, "pcf2carray: encoding does not include byte1=0 (ASCII)\n");
    free(toc);
    fclose(f);
    return 1;
  }
  int enc_cols = max_char - min_char + 1;
  int enc_rows = max_byte1 - min_byte1 + 1;
  long enc_count_long = (long)enc_cols * enc_rows;
  if (enc_cols <= 0 || enc_rows <= 0 || enc_count_long <= 0 || enc_count_long > 65536) {
    fprintf(stderr, "pcf2carray: invalid encoding range\n");
    free(toc);
    fclose(f);
    return 1;
  }
  int enc_count = (int)enc_count_long;
  unsigned short *enc_to_glyph = (unsigned short *)malloc((size_t)enc_count * sizeof(unsigned short));
  if (!enc_to_glyph) {
    free(toc);
    fclose(f);
    return 1;
  }
  for (int i = 0; i < enc_count; i++) {
    enc_to_glyph[i] = read_u16_enc(f);
  }
  int enc_min = min_char;
  int enc_max = max_char;
  /* Index for (byte1, byte2): (byte1 - min_byte1) * enc_cols + (byte2 - min_char). For ASCII, byte1=0. */
  int enc_row0 = 0 - min_byte1;

  /* --- Metrics --- */
  if (fseek(f, (long)metrics_off, SEEK_SET) != 0) {
    fprintf(stderr, "pcf2carray: seek to metrics failed\n");
    free(enc_to_glyph);
    free(toc);
    fclose(f);
    return 1;
  }
  (void)read_u32_le(f); /* format again in table */
  unsigned short (*read_u16_m)(FILE*) = (metrics_fmt & PCF_BYTE_MASK) ? read_u16_be : read_u16_le;
  unsigned int (*read_u32_m)(FILE*) = (metrics_fmt & PCF_BYTE_MASK) ? read_u32_be : read_u32_le;
  int metrics_count;
  int compressed = (metrics_fmt & PCF_COMPRESSED_METRICS) ? 1 : 0;
  if (compressed) {
    metrics_count = (int)read_u16_m(f);
    if (metrics_count < 0) metrics_count = 0;
  } else {
    metrics_count = (int)read_u32_m(f);
  }
  if (metrics_count <= 0) {
    fprintf(stderr, "pcf2carray: no metrics\n");
    free(enc_to_glyph);
    free(toc);
    fclose(f);
    return 1;
  }
  PCFMetric *metrics = (PCFMetric *)malloc((size_t)metrics_count * sizeof(PCFMetric));
  if (!metrics) {
    free(enc_to_glyph);
    free(toc);
    fclose(f);
    return 1;
  }
  for (int i = 0; i < metrics_count; i++) {
    if (compressed) {
      int lb = (int)(unsigned char)fgetc(f) - 0x80;
      int rb = (int)(unsigned char)fgetc(f) - 0x80;
      int w  = (int)(unsigned char)fgetc(f) - 0x80;
      int a  = (int)(unsigned char)fgetc(f) - 0x80;
      int d  = (int)(unsigned char)fgetc(f) - 0x80;
      metrics[i].left_bearing = lb;
      metrics[i].right_bearing = rb;
      metrics[i].width = w;
      metrics[i].ascent = a;
      metrics[i].descent = d;
    } else {
      metrics[i].left_bearing = (int)(short)read_u16_m(f);
      metrics[i].right_bearing = (int)(short)read_u16_m(f);
      metrics[i].width = (int)read_u16_m(f);
      metrics[i].ascent = (int)(short)read_u16_m(f);
      metrics[i].descent = (int)(short)read_u16_m(f);
      (void)read_u16_m(f); /* attributes */
    }
  }

  /* --- Bitmap table: format, count, offsets[], bitmapSizes[4] --- */
  if (fseek(f, (long)bitmaps_off, SEEK_SET) != 0) {
    fprintf(stderr, "pcf2carray: seek to bitmaps failed\n");
    free(metrics);
    free(enc_to_glyph);
    free(toc);
    fclose(f);
    return 1;
  }
  (void)read_u32_le(f); /* format */
  unsigned int (*read_u32_bm)(FILE*) = (bitmaps_fmt & PCF_BYTE_MASK) ? read_u32_be : read_u32_le;
  unsigned int glyph_count = read_u32_bm(f);
  if (glyph_count != (unsigned int)metrics_count || glyph_count == 0) {
    fprintf(stderr, "pcf2carray: metrics/bitmap count mismatch\n");
    free(metrics);
    free(enc_to_glyph);
    free(toc);
    fclose(f);
    return 1;
  }
  unsigned int *bitmap_offsets = (unsigned int *)malloc((size_t)glyph_count * sizeof(unsigned int));
  if (!bitmap_offsets) {
    free(metrics);
    free(enc_to_glyph);
    free(toc);
    fclose(f);
    return 1;
  }
  for (unsigned int i = 0; i < glyph_count; i++) {
    bitmap_offsets[i] = read_u32_bm(f);
  }
  unsigned int bitmap_sizes[4];
  for (int i = 0; i < 4; i++) {
    bitmap_sizes[i] = read_u32_bm(f);
  }
  unsigned int bitmap_data_start = bitmaps_off + 8 + 4 * glyph_count + 16;
  unsigned int glyph_pad = bitmaps_fmt & PCF_GLYPH_PAD_MASK;
  /* format&8 => LSBit first in file; else MSB first. -r flips interpretation. */
  int msb_first = (bitmaps_fmt & PCF_BIT_MASK) ? 0 : 1;
  if (reverse_bits)
    msb_first = !msb_first;

  /* Determine font dimensions from first encoded printable glyph */
  int font_w = 8, font_h = 8;
  int first_width = -1, first_ascent = -1, first_descent = -1;
  for (int c = ASCII_FIRST; c <= ASCII_LAST; c++) {
    if (c < enc_min || c > enc_max) continue;
    unsigned int g = enc_to_glyph[enc_row0 * enc_cols + (c - enc_min)];
    if (g == 0xFFFF) continue;
    if (g >= (unsigned int)metrics_count) continue;
    if (first_width < 0) {
      first_width = metrics[g].width;
      first_ascent = metrics[g].ascent;
      first_descent = metrics[g].descent;
      break;
    }
  }
  if (first_width < 0) {
    fprintf(stderr, "pcf2carray: no printable glyphs in ASCII 32-126\n");
    free(bitmap_offsets);
    free(metrics);
    free(enc_to_glyph);
    free(toc);
    fclose(f);
    return 1;
  }
  font_w = first_width;
  font_h = first_ascent + first_descent;
  if (font_h <= 0) font_h = 8;

  /* Check monospace: all glyphs 32-126 must have same width and height */
  for (int c = ASCII_FIRST; c <= ASCII_LAST; c++) {
    if (c < enc_min || c > enc_max) continue;
    unsigned int g = enc_to_glyph[enc_row0 * enc_cols + (c - enc_min)];
    if (g == 0xFFFF) continue;
    if (g >= (unsigned int)metrics_count) continue;
    if (metrics[g].width != first_width || metrics[g].ascent + metrics[g].descent != font_h) {
      fprintf(stderr, "pcf2carray: font is not monospace (e.g. glyph for 0x%02x has w=%d h=%d)\n",
              c, metrics[g].width, metrics[g].ascent + metrics[g].descent);
      free(bitmap_offsets);
      free(metrics);
      free(enc_to_glyph);
      free(toc);
      fclose(f);
      return 1;
    }
  }

  if (font_w > 8) {
    fprintf(stderr, "pcf2carray: warning: glyph width %d > 8; trimming to leftmost 8 pixels\n", font_w);
    font_w = 8;
  }

  /* Row stride in bitmap table */
  int row_stride_bytes = (font_w + 7) / 8;
  if (row_stride_bytes <= 0) row_stride_bytes = 1;
  if (glyph_pad == 1 && row_stride_bytes < 2) row_stride_bytes = 2;
  else if (glyph_pad == 2 && row_stride_bytes < 4) row_stride_bytes = 4;

  size_t out_glyph_bytes = (size_t)font_h;
  size_t out_len = (size_t)NUM_GLYPHS * out_glyph_bytes;
  unsigned char *out_bits = (unsigned char *)calloc(1, out_len);
  if (!out_bits) {
    free(bitmap_offsets);
    free(metrics);
    free(enc_to_glyph);
    free(toc);
    fclose(f);
    return 1;
  }

  unsigned char *row_buf = (unsigned char *)malloc((size_t)(row_stride_bytes > 8 ? row_stride_bytes : 8));
  if (!row_buf) {
    free(out_bits);
    free(bitmap_offsets);
    free(metrics);
    free(enc_to_glyph);
    free(toc);
    fclose(f);
    return 1;
  }

  for (int ch = ASCII_FIRST; ch <= ASCII_LAST; ch++) {
    int idx = ch - ASCII_FIRST;
    if (ch < enc_min || ch > enc_max) {
      /* leave out_bits for this glyph zeroed */
      continue;
    }
    unsigned int g = enc_to_glyph[enc_row0 * enc_cols + (ch - enc_min)];
    if (g == 0xFFFF || g >= (unsigned int)metrics_count) {
      continue;
    }
    unsigned int off = bitmap_offsets[g];
    long data_pos = (long)bitmap_data_start + (long)off;
    if (fseek(f, data_pos, SEEK_SET) != 0) {
      continue;
    }
    size_t dst_off = (size_t)idx * out_glyph_bytes;
    for (int row = 0; row < font_h; row++) {
      if (fread(row_buf, 1, (size_t)row_stride_bytes, f) != (size_t)row_stride_bytes) {
        break;
      }
      unsigned char out_byte = 0;
      for (int col = 0; col < font_w && col < 8; col++) {
        int byte_idx = col / 8;
        int bit_idx = col % 8;
        if (msb_first) {
          bit_idx = 7 - bit_idx;
        }
        unsigned char b = byte_idx < row_stride_bytes ? row_buf[byte_idx] : 0;
        int set = (b >> bit_idx) & 1;
        if (set) {
          out_byte |= (unsigned char)(1u << (7 - col));
        }
      }
      out_bits[dst_off + (size_t)row] = out_byte;
    }
  }

  free(row_buf);
  free(bitmap_offsets);
  free(metrics);
  free(enc_to_glyph);
  free(toc);
  fclose(f);

  char font_id[128];
  output_path_to_id(output_path, font_id, sizeof(font_id));

  FILE *out = fopen(output_path, "w");
  if (!out) {
    fprintf(stderr, "pcf2carray: cannot write %s\n", output_path);
    free(out_bits);
    return 1;
  }

  fprintf(out, "#pragma once\n\n");
  fprintf(out, "#include \"../font.h\"\n\n");
  fprintf(out, "static const unsigned char font_%s_bits[] = {\n", font_id);
  size_t glyph_bytes = out_glyph_bytes;
  for (int g = 0; g < NUM_GLYPHS; g++) {
    int c = ASCII_FIRST + g;
    char ch = (c >= 32 && c <= 126) ? (char)c : '?';
    size_t off = (size_t)g * glyph_bytes;
    for (size_t b = 0; b < glyph_bytes; b++) {
      fprintf(out, "0x%02x", (unsigned)out_bits[off + b]);
      fputs(g < NUM_GLYPHS - 1 || b < glyph_bytes - 1 ? "," : " ", out);
    }
    fprintf(out, "  // 0x%02x ", c);
    if (c == 32) fputs(" \n", out);
    else if (c == '\\') fputs("backslash\n", out);
    else fprintf(out, "%c\n", ch);
  }
  fprintf(out, "};\n\n");
  fprintf(out, "/* 1bpp row-major per glyph. MSB first. Glyph index: (unsigned char)c - ascii_first. */\n\n");
  fprintf(out, "static const FontMono1B font_%s = {\n", font_id);
  fprintf(out, "  .name        = \"%s\",\n", font_id);
  fprintf(out, "  .ascii_first = 32,\n");
  fprintf(out, "  .ascii_last  = 126,\n");
  fprintf(out, "  .num_glyphs  = 95,\n");
  fprintf(out, "  .glyph_w           = %d,\n", font_w);
  fprintf(out, "  .glyph_h           = %d,\n", font_h);
  fprintf(out, "  .extra_line_height = 0,\n");
  fprintf(out, "  .bits              = font_%s_bits,\n", font_id);
  fprintf(out, "};\n");

  fclose(out);
  free(out_bits);
  fprintf(stderr, "pcf2carray: %s -> %s (%dx%d)\n", input_path, output_path, font_w, font_h);
  return 0;
}

int main(int argc, char **argv) {
  int reverse_bits = 0;
  int opt = 1;
  if (argc >= 2 && (strcmp(argv[1], "-r") == 0 || strcmp(argv[1], "--reverse-bits") == 0)) {
    reverse_bits = 1;
    opt = 2;
  }
  if (argc < opt + 2) {
    usage(argv[0]);
    return 1;
  }
  return process_pcf(argv[opt], argv[opt + 1], reverse_bits);
}
