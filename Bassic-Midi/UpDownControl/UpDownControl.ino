/*
   Simple program to send program changes over the MIDI protokol, if a switch is pushed.
   For every switch there is a LED to present, that this switch is pressed.

*/
#include "midi.h"
#include "eepromData.h"
#include <TM1637Display.h>
#define DIGIT_CLOCK 3
#define DIGIT_DATA_IO 2

enum SWITCH_STATUS { UP, DOWN, RESET, NN };

const byte SWITCH_UP = 4;
const byte LED_1 = 8;
const byte SWITCH_DOWN = 5;
const byte LED_2 = 9;

const byte SEND_1[] = {0xC1, 0x01};
const byte SEND_2[] = {0xC1, 0x02};
const byte SEND_3[] = {0xC1, 0x03};
const byte SEND_4[] = {0xC1, 0x04};

Midi midi;
TM1637Display display(DIGIT_CLOCK, DIGIT_DATA_IO);
Presets presets;
byte displayValues[4];
byte actualPreset = 0;

void setup() {
  initSwitches();
  initLEDs();
  initMidi();
  initDisplay();

  presets.initPresetData();

  sendPreset();
}

void loop() {
  SWITCH_STATUS switches = getSwitches();
  if (switches > 0) {
    switch (switches) {
      case UP:
        break;
      case DOWN:
        break;
      case RESET:
        break;
    }
  }
  actualPreset++;
  Serial.print(actualPreset, HEX);
  Serial.print(':');
  if (actualPreset == 16) {
    actualPreset = 0;
  }
  sendPreset();
  delay(500);
}

void initSwitches() {
  pinMode(SWITCH_UP, INPUT_PULLUP);
  pinMode(SWITCH_DOWN, INPUT_PULLUP);
}

void initLEDs() {
  pinMode(LED_1, OUTPUT);
  pinMode(LED_2, OUTPUT);
  digitalWrite(LED_1, 1);
  digitalWrite(LED_2, 1);
  delay(500);
  allLEDsOff();
}

void initMidi() {
  midi.initMidi();
}

void initDisplay() {
  display.setBrightness(0x0f);
  for (byte i = 0; i < 4; i++) {
    displayValues[i] = 1;
  }
  display.setSegments(displayValues);
  delay(500);
  resetDisplay();
}

void resetDisplay() {
  for (byte i = 0; i < 4; i++) {
    displayValues[i] = 0;
  }
  display.setSegments(displayValues);
}

SWITCH_STATUS getSwitches() {
  byte portd = PIND && 0xF0;
  if ((portd && (0x01 << SWITCH_UP)) > 0) {
    return UP;
  }
  if ((portd && (0x01 << SWITCH_DOWN)) > 0) {
    return DOWN;
  }
  return NN;
}

void setLEDs(byte led) {
  allLEDsOff();
  digitalWrite(led, 1);
}

void allLEDsOff() {
  digitalWrite(LED_1, 0);
  digitalWrite(LED_2, 0);
}

void sendPreset() {
  presets.copyPresetData(actualPreset);
  byte count = presets.getPresetByteCount(actualPreset);
  for (byte i = 0; i < count; i++) {
    Serial.print(presets.presetData[i], HEX);
    Serial.print(' ');
  }
  Serial.write("\r\n");
  display.showNumberDec(actualPreset, false, 4, 0);
}
<<<<<<< Updated upstream

=======

>>>>>>> Stashed changes
