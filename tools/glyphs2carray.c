// glyphs2carray: convert FontStruct Glyphs app .glyphs format to a single .h file
// with C arrays (ASCII 32-126). Build: cc -o glyphs2carray glyphs2carray.c

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define ASCII_FIRST  32
#define ASCII_LAST   126
#define NUM_GLYPHS   95
#define MAX_GLYPH_H  32
#define MAX_LINE     512

static void usage(const char *prog) {
  fprintf(stderr, "Usage: %s [input.glyphs] [output.h]\n", prog);
}

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

static int count_chr(const char *line, char c) {
  int n = 0;
  for (const char *p = line; *p; p++)
    if (*p == c) n++;
  return n;
}

static int process_glyphs(const char *input_path, const char *output_path) {
  FILE *f = fopen(input_path, "r");
  if (!f) {
    fprintf(stderr, "glyphs2carray: cannot open %s\n", input_path);
    return 1;
  }

  int grid_length = 0;
  int width_value = 0;
  int cap_height = 0;
  int glyph_w = 0;
  int glyph_h = 0;

  /* Parse global metadata (before glyphs) */
  char line[MAX_LINE];
  while (fgets(line, sizeof(line), f)) {
    if (strstr(line, "glyphs = ("))
      break;
    if (strstr(line, "gridLength = ")) {
      const char *p = strstr(line, "gridLength = ") + 13;
      grid_length = atoi(p);
    }
    if (strstr(line, "widthValue = ")) {
      const char *p = strstr(line, "widthValue = ") + 13;
      width_value = atoi(p);
    }
    if (strstr(line, "capHeight = ")) {
      const char *p = strstr(line, "capHeight = ") + 12;
      cap_height = atoi(p);
    }
  }

  if (grid_length <= 0 || cap_height <= 0) {
    fprintf(stderr, "glyphs2carray: missing or invalid gridLength/capHeight in %s\n", input_path);
    fclose(f);
    return 1;
  }
  if (width_value <= 0) {
    fprintf(stderr, "glyphs2carray: missing or invalid widthValue in fontMaster\n");
    fclose(f);
    return 1;
  }

  glyph_w = width_value;
  glyph_h = cap_height / grid_length;
  if (glyph_h <= 0 || (size_t)glyph_h > MAX_GLYPH_H || glyph_h * grid_length != cap_height) {
    fprintf(stderr, "glyphs2carray: capHeight %d not divisible by gridLength %d\n", cap_height, grid_length);
    fclose(f);
    return 1;
  }
  if (glyph_w > 8) {
    fprintf(stderr, "glyphs2carray: glyph_w %d > 8 not supported\n", glyph_w);
    fclose(f);
    return 1;
  }

  /* out_bits[slot][row] = one byte per row, MSB = left */
  size_t glyph_bytes = (size_t)glyph_h;
  size_t out_len = (size_t)NUM_GLYPHS * glyph_bytes;
  unsigned char *out_bits = (unsigned char *)calloc(1, out_len);
  if (!out_bits) {
    fclose(f);
    return 1;
  }

  int depth = 0;
  int in_glyphs = 0;
  int in_components = 0;
  int components_depth = 0;
  int current_slot = -1;

  /* Rewind and find "glyphs = (" again */
  rewind(f);
  while (fgets(line, sizeof(line), f)) {
    if (!in_glyphs && strstr(line, "glyphs = (")) {
      in_glyphs = 1;
      depth += count_chr(line, '(') - count_chr(line, ')');
      continue;
    }
    if (!in_glyphs)
      continue;

    depth += count_chr(line, '(') - count_chr(line, ')');

    /* unicode = XXXX; (4 hex digits) */
    {
      const char *u = strstr(line, "unicode = ");
      if (u) {
        u += 9;
        while (*u == ' ' || *u == '\t') u++;
        if (isxdigit((unsigned char)u[0]) && isxdigit((unsigned char)u[1]) &&
            isxdigit((unsigned char)u[2]) && isxdigit((unsigned char)u[3])) {
          int code = 0;
          for (int i = 0; i < 4; i++) {
            int d = (u[i] <= '9') ? (u[i] - '0') : ((u[i] & 15) + 9);
            code = code * 16 + d;
          }
          if (code >= ASCII_FIRST && code <= ASCII_LAST)
            current_slot = code - ASCII_FIRST;
        }
      }
    }

    /* paths = ( means this glyph is the brick definition, not a character */
    if (strstr(line, "paths = ("))
      current_slot = -1;

    /* components = ( and we have a current character glyph */
    if (strstr(line, "components = (") && current_slot >= 0) {
      in_components = 1;
      components_depth = depth; /* depth after this line's ( is depth+1, we want to stay in until we close back to depth */
      continue;
    }

    if (in_components && depth < components_depth) {
      in_components = 0;
    }

    /* transform = "{a,b,c,d,tx,ty}"; */
    if (in_components && current_slot >= 0) {
      const char *t = strstr(line, "transform = ");
      if (t) {
        t = strchr(t, '"');
        if (t) t = strchr(t + 1, '{');
        if (t) {
          double a, b, c, d, tx, ty;
          if (sscanf(t, "{%lf,%lf,%lf,%lf,%lf,%lf}", &a, &b, &c, &d, &tx, &ty) >= 6) {
            /* col = round(tx / gridLength - 0.5), row = round((capHeight - ty) / gridLength - 0.5) */
            int col = (int)(tx / (double)grid_length - 0.5 + 0.5);
            int row = (int)((cap_height - ty) / (double)grid_length - 0.5 + 0.5);
            if (col < 0) col = 0;
            if (col >= glyph_w) col = glyph_w - 1;
            if (row < 0) row = 0;
            if (row >= glyph_h) row = glyph_h - 1;
            size_t off = (size_t)current_slot * glyph_bytes + (size_t)row;
            if (off < out_len)
              out_bits[off] |= (unsigned char)(1u << (7 - col));
          }
        }
      }
    }

    if (depth <= 0)
      in_glyphs = 0;
  }

  fclose(f);

  char font_id[128];
  output_path_to_id(output_path, font_id, sizeof(font_id));

  FILE *out = fopen(output_path, "w");
  if (!out) {
    fprintf(stderr, "glyphs2carray: cannot write %s\n", output_path);
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
  fprintf(out, "  .ascii_first = 32,\n");
  fprintf(out, "  .ascii_last  = 126,\n");
  fprintf(out, "  .num_glyphs  = 95,\n");
  fprintf(out, "  .glyph_w           = %d,\n", glyph_w);
  fprintf(out, "  .glyph_h           = %d,\n", glyph_h);
  fprintf(out, "  .extra_line_height = 0,\n");
  fprintf(out, "  .bits              = font_%s_bits,\n", font_id);
  fprintf(out, "};\n");

  fclose(out);
  free(out_bits);
  fprintf(stderr, "glyphs2carray: %s -> %s (%dx%d)\n", input_path, output_path, glyph_w, glyph_h);
  return 0;
}

int main(int argc, char **argv) {
  if (argc < 3) {
    usage(argv[0]);
    return 1;
  }
  return process_glyphs(argv[1], argv[2]);
}
