#include <ESP32Video.h>

const int outputPin = 25;
const bool voltageDivider = false;
CompositeGrayDAC videodisplay;

const int BOX_SIZE = 16;
int xmin, xmax, ymin, ymax;
int px, py;   // position (inside border)
int dx, dy;   // velocity (diagonal)
int w, h;
int innerW, innerH, innerR, innerB;

void setup()
{
  videodisplay.init(CompMode::MODENTSC240P, outputPin, voltageDivider);

  w = videodisplay.xres;
  h = videodisplay.yres;

  xmin = 25; xmax = w - 35;
  ymin = 10; ymax = h - 10;
  innerW = xmax - xmin + 1;
  innerH = ymax - ymin + 1;
  innerR = xmax - BOX_SIZE;
  innerB = ymax - BOX_SIZE;

  px = w/2; py = h/2;
  dx = dy = 1;
}

void loop()
{
  //videodisplay.clear(0);

  // borders
  videodisplay.fillRect(xmin, ymin, innerW, 1,      255);
  videodisplay.fillRect(xmin, ymax, innerW, 1,      255);
  videodisplay.fillRect(xmin, ymin, 1,      innerH, 255);
  videodisplay.fillRect(xmax, ymin, 1,      innerH, 255);

  // bouncing box
  videodisplay.fillRect(px, py, BOX_SIZE, BOX_SIZE, 127);

  // center lines
  videodisplay.fillRect(w/2,  ymin, 1,      innerH, 0);
  videodisplay.fillRect(xmin, h/2,  innerW, 1,      0);

  // diagonal px crispness tests
  videodisplay.line(xmin,ymin,xmin+30,ymin+30,0);
  videodisplay.fillRect(xmax-40,ymax-40,40,40,0);
  videodisplay.line(xmax,ymax,xmax-30,ymax-30,255);

  videodisplay.show();

  px += dx;
  py += dy;

  // Bounce off inner edges
  if      (px <= xmin)   { px = xmin;   dx = -dx; }
  else if (px >= innerR) { px = innerR; dx = -dx; }
  if      (py <= ymin)   { py = ymin;   dy = -dy; }
  else if (py >= innerB) { py = innerB; dy = -dy; }
}
