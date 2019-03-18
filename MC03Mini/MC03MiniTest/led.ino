#ifndef _LED_INO
#define _LED_INO

void setColorStatusLEDs(cRGB color) {
  SWITCH_LED.set_crgb_at(LED_STATUS, color);
  SWITCH_LED.sync();
}

void clearLEDs() {
  for (byte i = 0; i < LED_COUNT; i++) {
    SWITCH_LED.set_crgb_at(i, cBlack);
  }
}

void initNeoPixel() {
  dbgOutLn(F("Init neo pixel"));
  SWITCH_LED.setOutput(PIN_RGB_LED);
  SWITCH_LED.setColorOrderRGB();
  clearLEDs();
}

// ----- RGB LEDs -----
void showRGBLed() {
  clearLEDs();
  dbgOutLn(F("show pixel"));
  for (byte i = 0; i < LED_USED; i++) {
    byte intensity = 0;
    cRGB value;
    if (buttonSettings[i]) {
      if (buttonState[i]) {
        intensity = ((buttonColor[i] & 0xC0) >> 6); // + 2;
      } else {
        intensity = 1;
      }
    }
    dbgOut(F("int:"));
    dbgOut(intensity);
    value.r = ((buttonColor[i] & 0x03) << 4) * intensity;
    value.g = ((buttonColor[i] & 0x0C) << 2) * intensity;
    value.b = (buttonColor[i] & 0x30) * intensity;
    for (byte x = 0; x < LED_PER_SWITCH; x++) {
      byte pos = (i * LED_PER_SWITCH) + x;
      dbgOut(F("P:"));
      dbgOut2(pos, DEC);
      dbgOut(F("R:"));
      dbgOut2(value.r, DEC);
      dbgOut(F("G:"));
      dbgOut2(value.g, DEC);
      dbgOut(F("B:"));
      dbgOut2(value.b, DEC);
      SWITCH_LED.set_crgb_at(pos, value);
      dbgOutLn();      
    }
  }
  SWITCH_LED.sync();
}

cRGB getColorValue(byte buttonColor) {
  byte intensity = 0;
  cRGB value;
  intensity = ((buttonColor & 0xC0) >> 6); // + 2;
  value.r = ((buttonColor & 0x03) << 4) * intensity;
  value.g = ((buttonColor & 0x0C) << 2) * intensity;
  value.b = (buttonColor & 0x30) * intensity;
  return value;
}

#endif
