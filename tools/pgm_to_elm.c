#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void usage(const char *program) {
  fprintf(stderr, "Usage: %s input.pgm --bit-depth N\n", program);
}

typedef struct {
  int width;
  int height;
  unsigned char *pixels;
} PgmImage;

static void pgm_free(PgmImage *image) {
  if (image != NULL) {
    free(image->pixels);
    image->pixels = NULL;
    image->width = 0;
    image->height = 0;
  }
}

static int read_token(FILE *stream, char *buffer, size_t buffer_size, char *err, size_t err_size) {
  int ch = 0;
  size_t length = 0;

  if (buffer_size == 0) {
    snprintf(err, err_size, "Internal token buffer is too small");
    return 0;
  }

  while ((ch = fgetc(stream)) != EOF) {
    if (ch == '#') {
      while ((ch = fgetc(stream)) != EOF && ch != '\n') {
      }
      continue;
    }

    if (!isspace((unsigned char) ch)) {
      break;
    }
  }

  if (ch == EOF) {
    snprintf(err, err_size, "Unexpected EOF while reading PGM header");
    return 0;
  }

  do {
    if (ch == '#') {
      while ((ch = fgetc(stream)) != EOF && ch != '\n') {
      }
      break;
    }

    if (isspace((unsigned char) ch)) {
      break;
    }

    if (length + 1 >= buffer_size) {
      snprintf(err, err_size, "PGM header token is too long");
      return 0;
    }

    buffer[length++] = (char) ch;
    ch = fgetc(stream);
  } while (ch != EOF);

  buffer[length] = '\0';
  return 1;
}

static int scale_to_255(int value, int max_value) {
  if (max_value == 255) {
    return value;
  }
  return (value * 255 + (max_value / 2)) / max_value;
}

static int pgm_read(const char *path, PgmImage *out, char *err, size_t err_size) {
  FILE *stream = NULL;
  char token[64];
  int is_ascii = 0;
  int max_value = 0;
  size_t pixel_count = 0;
  size_t index = 0;

  out->width = 0;
  out->height = 0;
  out->pixels = NULL;

  stream = fopen(path, "rb");
  if (stream == NULL) {
    snprintf(err, err_size, "Could not open PGM: %s", path);
    return 0;
  }

  if (!read_token(stream, token, sizeof(token), err, err_size)) {
    fclose(stream);
    return 0;
  }

  if (strcmp(token, "P5") == 0) {
    is_ascii = 0;
  } else if (strcmp(token, "P2") == 0) {
    is_ascii = 1;
  } else {
    snprintf(err, err_size, "Unsupported PGM magic, expected P2 or P5");
    fclose(stream);
    return 0;
  }

  if (!read_token(stream, token, sizeof(token), err, err_size)) {
    fclose(stream);
    return 0;
  }
  out->width = atoi(token);

  if (!read_token(stream, token, sizeof(token), err, err_size)) {
    fclose(stream);
    return 0;
  }
  out->height = atoi(token);

  if (!read_token(stream, token, sizeof(token), err, err_size)) {
    fclose(stream);
    return 0;
  }
  max_value = atoi(token);

  if (out->width <= 0 || out->height <= 0) {
    snprintf(err, err_size, "PGM dimensions must be positive");
    fclose(stream);
    return 0;
  }

  if (max_value <= 0 || max_value > 255) {
    snprintf(err, err_size, "Only 8-bit-or-less PGM files are supported");
    fclose(stream);
    return 0;
  }

  pixel_count = (size_t) out->width * (size_t) out->height;
  out->pixels = (unsigned char *) malloc(pixel_count > 0 ? pixel_count : 1U);
  if (out->pixels == NULL) {
    snprintf(err, err_size, "Out of memory while reading PGM");
    fclose(stream);
    return 0;
  }

  if (!is_ascii) {
    size_t bytes_read = fread(out->pixels, 1, pixel_count, stream);
    if (bytes_read != pixel_count) {
      snprintf(err, err_size, "PGM pixel data is truncated");
      pgm_free(out);
      fclose(stream);
      return 0;
    }

    if (max_value != 255) {
      for (index = 0; index < pixel_count; ++index) {
        out->pixels[index] = (unsigned char) scale_to_255(out->pixels[index], max_value);
      }
    }
  } else {
    for (index = 0; index < pixel_count; ++index) {
      int value = 0;

      if (!read_token(stream, token, sizeof(token), err, err_size)) {
        pgm_free(out);
        fclose(stream);
        return 0;
      }

      value = atoi(token);
      if (value < 0 || value > max_value) {
        snprintf(err, err_size, "PGM ASCII pixel is outside the declared range");
        pgm_free(out);
        fclose(stream);
        return 0;
      }

      out->pixels[index] = (unsigned char) scale_to_255(value, max_value);
    }
  }

  fclose(stream);
  return 1;
}

static void extract_stem(const char *path, char *out, size_t out_size) {
  const char *base = strrchr(path, '/');
  const char *dot = NULL;
  size_t length = 0;

  if (out_size == 0) {
    return;
  }

  if (base == NULL) {
    base = path;
  } else {
    base += 1;
  }

  dot = strrchr(base, '.');
  if (dot != NULL && dot > base) {
    length = (size_t) (dot - base);
  } else {
    length = strlen(base);
  }

  if (length >= out_size) {
    length = out_size - 1;
  }

  memcpy(out, base, length);
  out[length] = '\0';
}

static void normalize_snake(const char *input, char *out, size_t out_size) {
  size_t out_index = 0;
  int previous_was_underscore = 1;

  if (out_size == 0) {
    return;
  }

  while (*input != '\0' && out_index + 1 < out_size) {
    unsigned char ch = (unsigned char) *input;

    if (isalnum(ch)) {
      out[out_index++] = (char) tolower(ch);
      previous_was_underscore = 0;
    } else if (!previous_was_underscore) {
      out[out_index++] = '_';
      previous_was_underscore = 1;
    }

    input += 1;
  }

  if (out_index > 0 && out[out_index - 1] == '_') {
    out_index -= 1;
  }

  out[out_index] = '\0';
}

static void capitalize_snake(const char *input, char *out, size_t out_size) {
  size_t out_index = 0;
  int capitalize_next = 1;

  if (out_size == 0) {
    return;
  }

  while (*input != '\0' && out_index + 1 < out_size) {
    char ch = *input;

    if (ch == '_') {
      out[out_index++] = '_';
      capitalize_next = 1;
    } else if (capitalize_next) {
      out[out_index++] = (char) toupper((unsigned char) ch);
      capitalize_next = 0;
    } else {
      out[out_index++] = ch;
    }

    input += 1;
  }

  out[out_index] = '\0';
}

static void strip_matching_prefix(char *name, const char *prefix) {
  size_t prefix_len = strlen(prefix);

  if (strncmp(name, prefix, prefix_len) == 0) {
    memmove(name, name + prefix_len, strlen(name + prefix_len) + 1);
  }
}

static void build_names(const char *path, int bit_depth, int width, int height, char *module_name, size_t module_name_size, char *value_name, size_t value_name_size) {
  char stem[512];
  char snake[512];
  char capitalized[512];
  char prefix_with_sep[128];

  extract_stem(path, stem, sizeof(stem));
  normalize_snake(stem, snake, sizeof(snake));

  snprintf(prefix_with_sep, sizeof(prefix_with_sep), "bd%d_%d_%d_", bit_depth, width, height);
  strip_matching_prefix(snake, prefix_with_sep);

  if (snake[0] == '\0') {
    snprintf(value_name, value_name_size, "bd%d_%d_%d", bit_depth, width, height);
    snprintf(module_name, module_name_size, "Bitmap.Bd%d_%d_%d", bit_depth, width, height);
    return;
  }

  capitalize_snake(snake, capitalized, sizeof(capitalized));
  snprintf(value_name, value_name_size, "bd%d_%d_%d_%s", bit_depth, width, height, snake);
  snprintf(module_name, module_name_size, "Bitmap.Bd%d_%d_%d_%s", bit_depth, width, height, capitalized);
}

static int gray_to_sample(int gray, int bit_depth) {
  int scale = 0;

  if (gray < 0) {
    gray = 0;
  } else if (gray > 255) {
    gray = 255;
  }

  if (bit_depth == 8) {
    return gray;
  }

  scale = (1 << bit_depth) - 1;
  return (gray * scale + 127) / 255;
}

static size_t packed_length(size_t pixel_count, int bit_depth) {
  if (bit_depth == 8) {
    return pixel_count;
  }
  return (pixel_count * (size_t) bit_depth + 7U) / 8U;
}

static void pack_pixels(const unsigned char *pixels, size_t pixel_count, int bit_depth, unsigned char *out) {
  size_t index = 0;
  size_t out_index = 0;

  if (bit_depth == 8) {
    memcpy(out, pixels, pixel_count);
    return;
  }

  if (bit_depth == 4) {
    for (index = 0; index < pixel_count; index += 2) {
      int high = gray_to_sample(pixels[index], 4);
      int low = (index + 1 < pixel_count) ? gray_to_sample(pixels[index + 1], 4) : 0;
      out[out_index++] = (unsigned char) ((high << 4) | low);
    }
    return;
  }

  if (bit_depth == 2) {
    for (index = 0; index < pixel_count; index += 4) {
      unsigned char byte = 0;
      size_t offset = 0;
      for (offset = 0; offset < 4; ++offset) {
        int sample = (index + offset < pixel_count) ? gray_to_sample(pixels[index + offset], 2) : 0;
        byte |= (unsigned char) (sample << (6 - ((int) offset * 2)));
      }
      out[out_index++] = byte;
    }
    return;
  }

  if (bit_depth == 1) {
    for (index = 0; index < pixel_count; index += 8) {
      unsigned char byte = 0;
      size_t offset = 0;
      for (offset = 0; offset < 8; ++offset) {
        int sample = (index + offset < pixel_count) ? gray_to_sample(pixels[index + offset], 1) : 0;
        byte |= (unsigned char) (sample << (7 - (int) offset));
      }
      out[out_index++] = byte;
    }
  }
}

static void print_bytes(const unsigned char *packed, size_t packed_len) {
  size_t index = 0;

  if (packed_len == 0) {
    printf("    , []\n");
    return;
  }

  printf("    , [ ");
  while (index < packed_len) {
    if (index > 0) {
      printf(", ");
    }
    printf("0x%02X", packed[index]);
    index += 1;
  }
  printf(" ]\n");
}

int main(int argc, char **argv) {
  PgmImage image;
  char err[256];
  char module_name[1024];
  char value_name[1024];
  const char *input = NULL;
  int bit_depth = 0;
  size_t pixel_count = 0;
  size_t packed_len = 0;
  unsigned char *packed = NULL;
  int argi = 0;

  if (argc < 4) {
    usage(argv[0]);
    return 1;
  }

  input = argv[1];

  for (argi = 2; argi < argc; ++argi) {
    if (strcmp(argv[argi], "--bit-depth") == 0 && argi + 1 < argc) {
      bit_depth = atoi(argv[++argi]);
    } else {
      usage(argv[0]);
      return 1;
    }
  }

  if (bit_depth != 1 && bit_depth != 2 && bit_depth != 4 && bit_depth != 8) {
    fprintf(stderr, "Bit depth must be one of: 1, 2, 4, 8\n");
    return 1;
  }

  if (!pgm_read(input, &image, err, sizeof(err))) {
    fprintf(stderr, "%s\n", err);
    return 1;
  }

  pixel_count = (size_t) image.width * (size_t) image.height;
  packed_len = packed_length(pixel_count, bit_depth);
  packed = (unsigned char *) malloc(packed_len > 0 ? packed_len : 1U);
  if (packed == NULL) {
    fprintf(stderr, "Out of memory while packing bitmap bytes\n");
    pgm_free(&image);
    return 1;
  }

  pack_pixels(image.pixels, pixel_count, bit_depth, packed);
  build_names(input, bit_depth, image.width, image.height, module_name, sizeof(module_name), value_name, sizeof(value_name));

  printf("module %s exposing (%s)\n\n", module_name, value_name);
  printf("import Bitmap exposing (BitDepth)\n\n\n");
  printf("%s : ( ( Int, Int ), BitDepth, List Int )\n", value_name);
  printf("%s =\n", value_name);
  printf("    ( ( %d, %d )\n", image.width, image.height);
  printf("    , Bitmap.BitDepth%d\n", bit_depth);
  print_bytes(packed, packed_len);
  printf("    )\n");

  free(packed);
  pgm_free(&image);
  return 0;
}
