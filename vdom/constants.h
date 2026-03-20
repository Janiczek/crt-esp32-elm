#pragma once

#define LED_PIN 2
#define DAC_PIN 25
#define HAVE_VOLTAGE_DIVIDER false

#define DISPLAY_W 400 // These are true for CompMode::MODENTSC240P
#define DISPLAY_H 240

#define TILE_SIZE 8
#define TILE_COLS (DISPLAY_W / TILE_SIZE)
#define TILE_ROWS (DISPLAY_H / TILE_SIZE)
#define TILE_COUNT (TILE_COLS * TILE_ROWS)

#define MY_CRT_PADDING_L 25
#define MY_CRT_PADDING_R 35
#define MY_CRT_PADDING_T 10
#define MY_CRT_PADDING_B 10

#define COLOR_BLACK 0

#define ESP32_DATA_CHUNK_SIZE 4096