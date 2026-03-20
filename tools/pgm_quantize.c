#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  int width;
  int height;
  unsigned char *pixels;
} PgmImage;

static void usage(const char *program) {
  fprintf(stderr, "Usage: %s input.pgm bit-depth output.pgm\n", program);
}

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

static int pgm_write(const char *path, const PgmImage *image, char *err, size_t err_size) {
  FILE *stream = NULL;
  size_t pixel_count = 0;

  if (image == NULL || image->pixels == NULL) {
    snprintf(err, err_size, "PGM image is empty");
    return 0;
  }

  stream = fopen(path, "wb");
  if (stream == NULL) {
    snprintf(err, err_size, "Could not write PGM: %s", path);
    return 0;
  }

  if (fprintf(stream, "P5\n%d %d\n255\n", image->width, image->height) < 0) {
    snprintf(err, err_size, "Could not write PGM header: %s", path);
    fclose(stream);
    return 0;
  }

  pixel_count = (size_t) image->width * (size_t) image->height;
  if (fwrite(image->pixels, 1, pixel_count, stream) != pixel_count) {
    snprintf(err, err_size, "Could not write full PGM payload: %s", path);
    fclose(stream);
    return 0;
  }

  fclose(stream);
  return 1;
}

static int quantize_gray(int gray, int bit_depth) {
  int levels_minus_one = 0;
  int step = 0;
  int sample = 0;

  if (gray < 0) {
    gray = 0;
  } else if (gray > 255) {
    gray = 255;
  }

  if (bit_depth == 8) {
    return gray;
  }

  levels_minus_one = (1 << bit_depth) - 1;
  step = 255 / levels_minus_one;
  sample = (gray * levels_minus_one + 127) / 255;
  return sample * step;
}

int main(int argc, char **argv) {
  PgmImage image;
  char err[256];
  int bit_depth = 0;
  size_t pixel_count = 0;
  size_t index = 0;

  if (argc != 4) {
    usage(argv[0]);
    return 1;
  }

  bit_depth = atoi(argv[2]);
  if (bit_depth != 1 && bit_depth != 2 && bit_depth != 4 && bit_depth != 8) {
    fprintf(stderr, "Bit depth must be one of: 1, 2, 4, 8\n");
    return 1;
  }

  if (!pgm_read(argv[1], &image, err, sizeof(err))) {
    fprintf(stderr, "%s\n", err);
    return 1;
  }

  pixel_count = (size_t) image.width * (size_t) image.height;
  for (index = 0; index < pixel_count; ++index) {
    image.pixels[index] = (unsigned char) quantize_gray(image.pixels[index], bit_depth);
  }

  if (!pgm_write(argv[3], &image, err, sizeof(err))) {
    fprintf(stderr, "%s\n", err);
    pgm_free(&image);
    return 1;
  }

  printf("%s\n", argv[3]);
  pgm_free(&image);
  return 0;
}
