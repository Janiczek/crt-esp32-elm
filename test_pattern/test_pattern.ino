#include <ESP32Video.h>
#include <Ressources/Font6x8.h>

const int outputPin = 25;
const bool voltageDivider = false;
CompositeGrayDAC videodisplay;

void setup()
{
  videodisplay.init(CompMode::MODENTSC240P, outputPin, voltageDivider);
  videodisplay.setFont(Font6x8);
  videodisplay.rect(30, 88, 255+5, 40+4, 127);
  for(int x = 0; x < 256; x++)
  {
    videodisplay.fillRect(x + 32, 90, 1, 40, x);
    if(x % 16 == 0)
    {
      videodisplay.fillRect(x + 32, 85, 1, 4, 255);
      videodisplay.setCursor(x + 32 - 3, 78);
      videodisplay.print(x,HEX);
    }
  }
}

void loop()
{
}