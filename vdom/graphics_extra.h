#pragma once

#include <stddef.h>

#include "globals.h"

// missing from esp32lib Graphics.h
inline void yLine(int y0, int y1, int x, uint8_t color)
	{
		if (x < 0 || x >= video.xres)
			return;
		if (y0 > y1)
		{
			int yb = y0;
			y0 = y1;
			y1 = yb;
		}
		if (y0 < 0)
			y0 = 0;
		if (y1 >= video.yres)
			y1 = video.yres - 1;
		if (y0 > y1)
			return;
		for (int y = y0; y <= y1; y++)
			video.dotFast(x, y, color);
	}
