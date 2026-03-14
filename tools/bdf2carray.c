// bdf2carray: convert BDF font to a single .h file with C arrays (ASCII 32-126).

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#define ASCII_FIRST  32
#define ASCII_LAST  126
#define NUM_GLYPHS  95

static void usage(const char *prog) {
  fprintf(stderr, "Usage: %s [input.bdf] [output.h]\n", prog);
}

static int parse_hex_row(const char *line, unsigned char *out, int bytes_per_row) {
  while (*line == ' ' || *line == '\t') line++;
  int i = 0;
  while (i < bytes_per_row && line[0] && line[1]) {
    unsigned int v = 0;
    if (isxdigit((unsigned char)line[0]) && isxdigit((unsigned char)line[1])) {
      v = (line[0] <= '9' ? line[0] - '0' : (line[0] & 15) + 9) * 16u
        + (line[1] <= '9' ? line[1] - '0' : (line[1] & 15) + 9);
    }
    out[i++] = (unsigned char)v;
    line += 2;
    while (*line == ' ' || *line == '\t') line++;
  }
  return i;
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

static int process_bdf(const char *input_path, const char *output_path) {
  FILE *f = fopen(input_path, "r");
  if (!f) {
    fprintf(stderr, "bdf2carray: cannot open %s\n", input_path);
    return 1;
  }

  int font_w = 0, font_h = 0;
  int font_xo = 0, font_yo = 0;  /* FONTBOUNDINGBOX lower-left offset (for vertical alignment) */
  int advance[NUM_GLYPHS];
  unsigned char *bits = NULL;
  int stride_bytes = 0;
  size_t bits_len = 0;
  int got_bbox = 0;
  int in_bitmap = 0;
  int current_enc = -1;
  int bbx_w = 0, bbx_h = 0, bbx_x = 0, bbx_y = 0;
  int dwx = 0;
  int bitmap_row = 0;
  char line[512];
  int line_num = 0;

  memset(advance, 0, sizeof(advance));
  for (int i = 0; i < NUM_GLYPHS; i++) advance[i] = 8;

  while (fgets(line, sizeof(line), f)) {
    line_num++;
    /* trim trailing newline and CR (BDF files often use CRLF) */
    size_t len = strlen(line);
    while (len > 0 && (line[len - 1] == '\n' || line[len - 1] == '\r')) {
      line[--len] = '\0';
    }

    if (strncmp(line, "FONTBOUNDINGBOX ", 16) == 0) {
      if (sscanf(line + 16, "%d %d %d %d", &font_w, &font_h, &font_xo, &font_yo) >= 2 && font_w > 0 && font_h > 0) {
        got_bbox = 1;
        stride_bytes = (font_w + 7) / 8;
        bits_len = (size_t)NUM_GLYPHS * (size_t)stride_bytes * (size_t)font_h;
        bits = (unsigned char *)calloc(1, bits_len);
        if (!bits) { fclose(f); return 1; }
      }
      continue;
    }

    if (!got_bbox) continue;

    if (strncmp(line, "ENCODING ", 9) == 0) {
      int enc = atoi(line + 9);
      in_bitmap = 0;
      current_enc = (enc >= ASCII_FIRST && enc <= ASCII_LAST) ? enc : -1;
      continue;
    }

    if (strncmp(line, "DWIDTH ", 7) == 0) {
      int dy;
      if (sscanf(line + 7, "%d %d", &dwx, &dy) >= 1 && current_enc >= 0)
        advance[current_enc - ASCII_FIRST] = dwx;
      continue;
    }

    if (strncmp(line, "BBX ", 4) == 0) {
      sscanf(line + 4, "%d %d %d %d", &bbx_w, &bbx_h, &bbx_x, &bbx_y);
      continue;
    }

    if (strncmp(line, "BITMAP", 6) == 0 && (line[6] == '\0' || line[6] == '\r')) {
      in_bitmap = (current_enc >= 0);
      bitmap_row = 0;
      continue;
    }

    if (strncmp(line, "ENDCHAR", 7) == 0 && (line[7] == '\0' || line[7] == '\r')) {
      in_bitmap = 0;
      current_enc = -1;
      continue;
    }

    if (in_bitmap && current_enc >= 0 && bits) {
      int idx = current_enc - ASCII_FIRST;
      int bytes_per_row = (bbx_w + 7) / 8;
      if (bytes_per_row <= 0) bytes_per_row = 1;
      /* BDF: first bitmap row = top of glyph. Cell row 0 = top of cell = font_yo+font_h-1.
       * Glyph top = bbx_y+bbx_h-1 => cell_row = font_h + font_yo - bbx_y - bbx_h + bitmap_row. */
      int dst_row = font_h + font_yo - bbx_y - bbx_h + bitmap_row;
      if (dst_row >= 0 && dst_row < font_h) {
        size_t dst_off = (size_t)idx * (size_t)stride_bytes * (size_t)font_h + (size_t)dst_row * (size_t)stride_bytes;
        if (dst_off + stride_bytes <= bits_len) {
          unsigned char row_buf[64];
          if (bytes_per_row <= (int)sizeof(row_buf)) {
            int n = parse_hex_row(line, row_buf, bytes_per_row);
            /* Cell bit 0 = font bbox left (BDF x=font_xo). Glyph left = bbx_x => cell bit (bbx_x - font_xo). */
            int x_bits = bbx_x - font_xo;
            if (x_bits < 0) {
              fprintf(stderr, "bdf2carray: glyph 0x%02x has bbx_x %d < font_xo %d (would place left of cell)\n",
                      ASCII_FIRST + idx, bbx_x, font_xo);
              fclose(f);
              free(bits);
              return 1;
            }
            int shift = x_bits & 7;
            int dst_byte_start = x_bits / 8;
            for (int i = 0; i < n; i++) {
              int di = dst_byte_start + i;
              if (di < stride_bytes)
                bits[dst_off + di] |= (unsigned char)(row_buf[i] >> shift);
              if (shift != 0 && di + 1 < stride_bytes)
                bits[dst_off + di + 1] |= (unsigned char)(row_buf[i] << (8 - shift));
            }
          }
        }
      }
      bitmap_row++;
    }
  }

  fclose(f);

  if (!bits || !font_w || !font_h) {
    fprintf(stderr, "bdf2carray: no FONTBOUNDINGBOX or invalid BDF %s\n", input_path);
    free(bits);
    return 1;
  }

  int single_advance = advance[0];
  for (int i = 1; i < NUM_GLYPHS; i++) {
    if (advance[i] != single_advance) {
      fprintf(stderr, "bdf2carray: font has varying advance (e.g. glyph %d advance %d != %d); single advance required\n",
              i + ASCII_FIRST, advance[i], single_advance);
      free(bits);
      return 1;
    }
  }

  if (single_advance <= 0) {
    fprintf(stderr, "bdf2carray: glyph advance %d out of supported range (>=1)\n", single_advance);
    free(bits);
    return 1;
  }
  if (single_advance > 8) {
    fprintf(stderr, "bdf2carray: warning: glyph advance %d > 8; trimming to leftmost 8 pixels\n", single_advance);
    single_advance = 8;
  }

  char font_id[128];
  output_path_to_id(output_path, font_id, sizeof(font_id));

  /* Pack from internal stride_bytes buffer into 1-byte-per-row output.
   * We treat DWIDTH (single_advance) as the visible cell width in pixels.
   * Cell x-coordinates run from 0..font_w-1, where FONTBOUNDINGBOX gives
   * font_xo as the lower-left x of that cell. The glyph's advance cell
   * occupies x in [cell_origin, cell_origin + single_advance), where
   * cell_origin = -font_xo. We pack those columns into bits 7..(7-single_advance+1). */
  size_t out_glyph_bytes = (size_t)font_h;
  size_t out_len = (size_t)NUM_GLYPHS * out_glyph_bytes;
  unsigned char *out_bits = (unsigned char *)calloc(1, out_len);
  if (!out_bits) {
    free(bits);
    return 1;
  }

  int cell_origin = -font_xo;

  for (int g = 0; g < NUM_GLYPHS; g++) {
    size_t src_glyph_off = (size_t)g * (size_t)stride_bytes * (size_t)font_h;
    size_t dst_glyph_off = (size_t)g * out_glyph_bytes;
    for (int row = 0; row < font_h; row++) {
      unsigned char row_byte = 0;
      size_t src_row_off = src_glyph_off + (size_t)row * (size_t)stride_bytes;
      for (int col = 0; col < single_advance; col++) {
        int cell_x = cell_origin + col;
        if (cell_x < 0 || cell_x >= font_w)
          continue;
        size_t src_byte_idx = src_row_off + (size_t)(cell_x / 8);
        int src_bit_idx = 7 - (cell_x % 8);
        if (src_byte_idx < src_row_off + (size_t)stride_bytes) {
          unsigned char src_byte = bits[src_byte_idx];
          if ((src_byte >> src_bit_idx) & 1u) {
            int dst_bit_idx = 7 - col;
            row_byte |= (unsigned char)(1u << dst_bit_idx);
          }
        }
      }
      out_bits[dst_glyph_off + (size_t)row] = row_byte;
    }
  }

  FILE *out = fopen(output_path, "w");
  if (!out) {
    fprintf(stderr, "bdf2carray: cannot write %s\n", output_path);
    free(bits);
    free(out_bits);
    return 1;
  }

  fprintf(out, "#pragma once\n\n");
  fprintf(out, "#include \"../font.h\"\n\n");
  fprintf(out, "static const unsigned char font_%s_bits[] = {\n", font_id);
  size_t glyph_bytes = (size_t)font_h;
  for (int g = 0; g < NUM_GLYPHS; g++) {
    int c = ASCII_FIRST + g;
    char ch = (c >= 32 && c <= 126) ? (char)c : '?';
    size_t off = (size_t)g * glyph_bytes;
    for (size_t b = 0; b < glyph_bytes; b++) {
      fprintf(out, "0x%02x", (unsigned)out_bits[off + b]);
      fputs(g < NUM_GLYPHS - 1 || b < glyph_bytes - 1 ? "," : " ",out);
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
  fprintf(out, "  .glyph_w           = %d,\n", single_advance);
  fprintf(out, "  .glyph_h           = %d,\n", font_h);
  fprintf(out, "  .extra_line_height = 0,\n");
  fprintf(out, "  .bits              = font_%s_bits,\n", font_id);
  fprintf(out, "};\n");

  fclose(out);
  free(bits);
  free(out_bits);
  fprintf(stderr, "bdf2carray: %s -> %s (%dx%d, packed stride 1, advance %d)\n", input_path, output_path, font_w, font_h, single_advance);
  return 0;
}

int main(int argc, char **argv) {
  if (argc < 3) {
    usage(argv[0]);
    return 1;
  }
  return process_bdf(argv[1], argv[2]);
}
