// bfm2carray: convert BitFontMaker2 JSON font to a .h file with C arrays (ASCII 32-126).

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define ASCII_FIRST  32
#define ASCII_LAST  126
#define NUM_GLYPHS  (ASCII_LAST - ASCII_FIRST + 1)
#define GLYPH_ROWS  16

static void usage(const char *prog) {
  fprintf(stderr, "Usage: %s [input.json] [output.h]\n", prog);
}

/* Return font identifier from output path (e.g. foo/dina_italic_400_10.h -> dina_italic_400_10) */
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

static int highest_set_bit(unsigned int v) {
  if (!v) return -1;
  int idx = 0;
  while (v >>= 1) idx++;
  return idx;
}

static int process_bfm(const char *input_path, const char *output_path) {
  FILE *f = fopen(input_path, "rb");
  if (!f) {
    fprintf(stderr, "bfm2carray: cannot open %s\n", input_path);
    return 1;
  }

  if (fseek(f, 0, SEEK_END) != 0) {
    fprintf(stderr, "bfm2carray: failed to seek %s\n", input_path);
    fclose(f);
    return 1;
  }
  long len = ftell(f);
  if (len < 0) {
    fprintf(stderr, "bfm2carray: failed to get size of %s\n", input_path);
    fclose(f);
    return 1;
  }
  if (fseek(f, 0, SEEK_SET) != 0) {
    fprintf(stderr, "bfm2carray: failed to rewind %s\n", input_path);
    fclose(f);
    return 1;
  }

  char *buf = (char *)malloc((size_t)len + 1);
  if (!buf) {
    fprintf(stderr, "bfm2carray: out of memory reading %s\n", input_path);
    fclose(f);
    return 1;
  }
  size_t nread = fread(buf, 1, (size_t)len, f);
  fclose(f);
  buf[nread] = '\0';

  int glyph_rows[NUM_GLYPHS][GLYPH_ROWS];
  unsigned char glyph_present[NUM_GLYPHS];
  memset(glyph_rows, 0, sizeof(glyph_rows));
  memset(glyph_present, 0, sizeof(glyph_present));

  const char *p = buf;
  while (*p) {
    while (*p && *p != '"') p++;
    if (!*p) break;
    const char *key_start = ++p;
    int is_digit_key = 1;
    while (*p && *p != '"') {
      if (!isdigit((unsigned char)*p)) is_digit_key = 0;
      p++;
    }
    if (!*p) break;
    const char *key_end = p;
    p++;

    while (*p && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) p++;
    if (*p != ':') {
      while (*p && *p != '\n' && *p != ',') p++;
      continue;
    }
    p++;
    while (*p && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) p++;

    if (!is_digit_key) {
      /* Skip value for non-numeric keys. For our use, these are simple values (string/number). */
      if (*p == '"') {
        p++;
        while (*p && *p != '"') {
          if (*p == '\\' && p[1]) p += 2;
          else p++;
        }
        if (*p == '"') p++;
      } else if (*p == '[') {
        int depth = 1;
        p++;
        while (*p && depth > 0) {
          if (*p == '[') depth++;
          else if (*p == ']') depth--;
          p++;
        }
      } else {
        while (*p && *p != ',' && *p != '\n' && *p != '\r' && *p != '}') p++;
      }
      continue;
    }

    int code = 0;
    for (const char *q = key_start; q < key_end; q++) {
      code = code * 10 + (*q - '0');
    }

    if (code < ASCII_FIRST || code > ASCII_LAST) {
      /* Skip glyphs outside ASCII 32-126 */
      if (*p == '[') {
        int depth = 1;
        p++;
        while (*p && depth > 0) {
          if (*p == '[') depth++;
          else if (*p == ']') depth--;
          p++;
        }
      } else {
        while (*p && *p != ',' && *p != '\n' && *p != '\r' && *p != '}') p++;
      }
      continue;
    }

    if (*p != '[') {
      fprintf(stderr, "bfm2carray: expected '[' for glyph %d\n", code);
      free(buf);
      return 1;
    }
    p++;

    int rows[GLYPH_ROWS];
    int row_count = 0;
    while (*p) {
      while (*p && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r' || *p == ',')) p++;
      if (*p == ']') {
        p++;
        break;
      }
      char *endptr = NULL;
      long v = strtol(p, &endptr, 10);
      if (endptr == p) {
        fprintf(stderr, "bfm2carray: invalid number in glyph %d\n", code);
        free(buf);
        return 1;
      }
      if (row_count < GLYPH_ROWS) {
        rows[row_count++] = (int)v;
      } else {
        row_count++;
      }
      p = endptr;
    }
    if (row_count != GLYPH_ROWS) {
      fprintf(stderr, "bfm2carray: glyph %d has %d rows, expected %d\n", code, row_count, GLYPH_ROWS);
      free(buf);
      return 1;
    }

    int idx = code - ASCII_FIRST;
    for (int r = 0; r < GLYPH_ROWS; r++) {
      glyph_rows[idx][r] = rows[r];
    }
    glyph_present[idx] = 1;
  }

  free(buf);

  int glyph_w = 0;
  for (int g = 0; g < NUM_GLYPHS; g++) {
    int width = 0;
    for (int r = 0; r < GLYPH_ROWS; r++) {
      unsigned int v = (unsigned int)glyph_rows[g][r];
      int hb = highest_set_bit(v);
      if (hb + 1 > width) width = hb + 1;
    }
    if (width == 0) {
      if (!glyph_present[g]) {
        for (int r = 0; r < GLYPH_ROWS; r++) glyph_rows[g][r] = 0;
      }
      continue;
    }
    if (width > glyph_w) glyph_w = width;
  }

  if (glyph_w <= 0) {
    fprintf(stderr, "bfm2carray: no non-empty glyphs in ASCII 32-126\n");
    return 1;
  }
  if (glyph_w > 8) {
    fprintf(stderr, "bfm2carray: warning: glyph width %d > 8; trimming to leftmost 8 pixels\n", glyph_w);
    glyph_w = 8;
  }

  /* Vertical extent of "on" pixels across all glyphs (ink height). */
  int row_min = GLYPH_ROWS;
  int row_max = -1;
  for (int g = 0; g < NUM_GLYPHS; g++) {
    if (!glyph_present[g]) continue;
    for (int r = 0; r < GLYPH_ROWS; r++) {
      if ((unsigned int)glyph_rows[g][r] != 0) {
        if (r < row_min) row_min = r;
        if (r > row_max) row_max = r;
      }
    }
  }
  int glyph_h = (row_max >= row_min) ? (row_max - row_min + 1) : GLYPH_ROWS;
  if (row_max < row_min) {
    row_min = 0;
    row_max = GLYPH_ROWS - 1;
  }

  char font_id[128];
  output_path_to_id(output_path, font_id, sizeof(font_id));

  size_t glyph_bytes = (size_t)glyph_h;
  size_t out_len = (size_t)NUM_GLYPHS * glyph_bytes;
  unsigned char *out_bits = (unsigned char *)calloc(1, out_len);
  if (!out_bits) {
    fprintf(stderr, "bfm2carray: out of memory for bits\n");
    return 1;
  }

  for (int g = 0; g < NUM_GLYPHS; g++) {
    size_t dst_glyph_off = (size_t)g * glyph_bytes;
    for (int r = 0; r < glyph_h; r++) {
      int src_row = row_min + r;
      unsigned int v = (unsigned int)glyph_rows[g][src_row];
      unsigned char row_byte = 0;
      for (int col = 0; col < glyph_w; col++) {
        if ((v >> col) & 1u) {
          int dst_bit = 7 - col;
          if (dst_bit >= 0 && dst_bit < 8) {
            row_byte |= (unsigned char)(1u << dst_bit);
          }
        }
      }
      out_bits[dst_glyph_off + (size_t)r] = row_byte;
    }
  }

  FILE *out = fopen(output_path, "w");
  if (!out) {
    fprintf(stderr, "bfm2carray: cannot write %s\n", output_path);
    free(out_bits);
    return 1;
  }

  fprintf(out, "#pragma once\n\n");
  fprintf(out, "#include \"../font.h\"\n\n");
  fprintf(out, "static const unsigned char font_%s_bits[] = {\n", font_id);

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
  fprintf(out, "  .ascii_first = %d,\n", ASCII_FIRST);
  fprintf(out, "  .ascii_last  = %d,\n", ASCII_LAST);
  fprintf(out, "  .num_glyphs  = %d,\n", NUM_GLYPHS);
  fprintf(out, "  .glyph_w           = %d,\n", glyph_w);
  fprintf(out, "  .glyph_h           = %d,\n", glyph_h);
  fprintf(out, "  .extra_line_height = 0,\n");
  fprintf(out, "  .bits              = font_%s_bits,\n", font_id);
  fprintf(out, "};\n");

  fclose(out);
  free(out_bits);

  fprintf(stderr, "bfm2carray: %s -> %s (w=%d, h=%d)\n", input_path, output_path, glyph_w, glyph_h);
  return 0;
}

int main(int argc, char **argv) {
  if (argc < 3) {
    usage(argv[0]);
    return 1;
  }
  return process_bfm(argv[1], argv[2]);
}

