#pragma once

#include <stdint.h>

#include "constants.h"

inline uint8_t abs(int8_t a)     { return(a < 0 ? -a : a); }
inline int     MIN(int a, int b) { return(a < b ? a : b); }
inline int     MAX(int a, int b) { return(a > b ? a : b); }

// Ran at every frame. Tries to let other tasks run while it waits.
void enforceFps() {
  static unsigned long lastFrameUs = 0;
  unsigned long now = micros();
  if (lastFrameUs != 0) {
    unsigned long elapsed = now - lastFrameUs;
    if (elapsed < LOOP_FRAME_US) {
      delayMicroseconds(LOOP_FRAME_US - elapsed);
    }
  }
  lastFrameUs = micros();
}

void ledOn() {
  digitalWrite(LED_PIN, HIGH);
}

void complain(const char* msg) {
  ledOn();
  Serial.println(msg);
}