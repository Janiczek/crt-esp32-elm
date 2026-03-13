// bdf2carray: convert BDF font to a single .h file with C arrays (ASCII 32-127).

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#define ASCII_FIRST  32
#define ASCII_LAST  127
#define NUM_GLYPHS  96

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
  int advance[NUM_GLYPHS];
  unsigned char *bits = NULL;
  int stride = 0;
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
      int xo, yo;
      if (sscanf(line + 16, "%d %d %d %d", &font_w, &font_h, &xo, &yo) >= 2 && font_w > 0 && font_h > 0) {
        got_bbox = 1;
        stride = (font_w + 7) / 8;
        bits_len = (size_t)NUM_GLYPHS * (size_t)stride * (size_t)font_h;
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
      size_t dst_off = (size_t)idx * (size_t)stride * (size_t)font_h + (size_t)bitmap_row * (size_t)stride;
      if (bitmap_row < font_h && dst_off + stride <= bits_len) {
        parse_hex_row(line, bits + dst_off, stride < bytes_per_row ? stride : bytes_per_row);
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

  char font_id[128];
  output_path_to_id(output_path, font_id, sizeof(font_id));

  FILE *out = fopen(output_path, "w");
  if (!out) {
    fprintf(stderr, "bdf2carray: cannot write %s\n", output_path);
    free(bits);
    return 1;
  }

  fprintf(out, "#pragma once\n\n");
  fprintf(out, "#include \"../font.h\"\n\n");
  fprintf(out, "static const unsigned char font_%s_bits[] = {\n", font_id);
  size_t glyph_bytes = (size_t)stride * (size_t)font_h;
  for (int g = 0; g < NUM_GLYPHS; g++) {
    int c = ASCII_FIRST + g;
    char ch = (c >= 32 && c <= 126) ? (char)c : (c == 127 ? '?' : '.');
    size_t off = (size_t)g * glyph_bytes;
    for (size_t b = 0; b < glyph_bytes; b++)
      fprintf(out, "0x%02x,", (unsigned)bits[off + b]);
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
  fprintf(out, "  .ascii_last  = 127,\n");
  fprintf(out, "  .num_glyphs  = 96,\n");
  fprintf(out, "  .glyph_w     = %d,\n", single_advance);
  fprintf(out, "  .glyph_h     = %d,\n", font_h);
  fprintf(out, "  .stride      = %d,\n", stride);
  fprintf(out, "  .bits        = font_%s_bits,\n", font_id);
  fprintf(out, "};\n");

  fclose(out);
  free(bits);
  fprintf(stderr, "bdf2carray: %s -> %s (%dx%d, stride %d, advance %d)\n", input_path, output_path, font_w, font_h, stride, single_advance);
  return 0;
}

int main(int argc, char **argv) {
  if (argc < 3) {
    usage(argv[0]);
    return 1;
  }
  return process_bdf(argv[1], argv[2]);
}
