WS2812 LED(LED_COUNT);

void clearLEDs() {
  for (byte i = 0; i < LED_COUNT; i++) {
    LED.set_crgb_at(i, cBlack);
  }
}

void initNeoPixel() {
  LED.setOutput(PIN_RGB_LED);
  LED.setColorOrderRGB();
  clearLEDs();
}

// ----- RGB LEDs -----
void showRGBLed() {
  clearLEDs();
  for (byte i = 0; i < LED_USED; i++) {
    byte intensity = 0;
    cRGB value;
    if (buttonSettings[i]) {
      if (buttonState[i]) {
        intensity = ((buttonColor[i] & 0xC0) >> 6);// + 2;
      } else {
        intensity = 1;
      }
    }
    value.r = ((buttonColor[i] & 0x03) << 4) * intensity;
    value.g = ((buttonColor[i] & 0x0C) << 2) * intensity;
    value.b = (buttonColor[i] & 0x30) * intensity;
    for (byte x = 0; x < LED_PER_SWITCH; x++) {
      LED.set_crgb_at((i * LED_PER_SWITCH) + x, value);
    }
  }
  LED.sync();
}
