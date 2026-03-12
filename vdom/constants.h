#pragma once

#define DAC_PIN 25
#define HAVE_VOLTAGE_DIVIDER false

#define DISPLAY_W 400 // TODO make sure this is it (print video.xres)
#define DISPLAY_H 240 // TODO make sure this is it (print video.yres)

#define MY_CRT_PADDING_L 25
#define MY_CRT_PADDING_R 35
#define MY_CRT_PADDING_T 10
#define MY_CRT_PADDING_B 10
#define X_MIN MY_CRT_PADDING_L
#define X_MAX (DISPLAY_W - MY_CRT_PADDING_R)
#define Y_MIN MY_CRT_PADDING_T
#define Y_MAX (DISPLAY_H - MY_CRT_PADDING_B)
#define USABLE_W (X_MAX - X_MIN + 1)
#define USABLE_H (Y_MAX - Y_MIN + 1)
#define X_CENTER (USABLE_W/2 + X_MIN)
#define Y_CENTER (USABLE_H/2 + Y_MIN)

#define COLOR_BLACK 0
#define COLOR_GRAY 127
#define COLOR_WHITE 255