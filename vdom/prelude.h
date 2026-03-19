#pragma once

#include <stdint.h>

#include "constants.h"

inline uint8_t abs(int8_t a)     { return(a < 0 ? -a : a); }
inline int     MIN(int a, int b) { return(a < b ? a : b); }
inline int     MAX(int a, int b) { return(a > b ? a : b); }

void ledOn() {
  digitalWrite(LED_PIN, HIGH);
}

void logSerial(const char* msg) {
  Serial.println(msg);
  Serial.flush();
}

void complain(const char* msg) {
  hasComplained = true;
  ledOn();
  Serial.println(msg);
  Serial.flush();
}