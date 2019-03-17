
/*
   Simple program to send program changes over the MIDI protokol, if a switch is pushed.
   For every switch there is a LED to present, that this switch is pressed.

*/
//#define debug
#include "debug.h"
#include "midi.h"
#include <TM1637Display.h>
#include <avdweb_Switch.h>

const byte SWITCH_1 = 4;
const byte LED_1 = 5;
const byte SWITCH_2 = 6;
const byte LED_2 = 7;
const byte DISPLAY_CLK = 2;
const byte DISPLAY_DIO = 3;

// Hier stehen die zu sendenden Daten. Erstes Byte gibt jeweils die Länge der Bytes an.
const byte SONG_COUNT = 10;
const byte PROGRAM_COUNT = 20;
const byte MAX_PROGRAM_LENGTH = 16;
const byte SEND_ARRAY[PROGRAM_COUNT][MAX_PROGRAM_LENGTH] = {
  {0x02, 0xC1, 0x01},
  {0x02, 0xC1, 0x02},
  {0x02, 0xC1, 0x03},
  {0x04, 0xC1, 0x04, 0xC2, 0x04},
  {0x04, 0xC1, 0x05, 0xC2, 0x04},
  {0x04, 0xC1, 0x06, 0xC2, 0x04},
  {0x04, 0xC1, 0x07, 0xC2, 0x04},
  {0x04, 0xC1, 0x08, 0xC2, 0x04},
  {0x04, 0xC1, 0x09, 0xC2, 0x04},
  {0x04, 0xC1, 0x0A, 0xC2, 0x04},
  {0x04, 0xC1, 0x0B, 0xC2, 0x04},
  {0x04, 0xC1, 0x0C, 0xC2, 0x04},
  {0x04, 0xC1, 0x0D, 0xC2, 0x04},
  {0x04, 0xC1, 0x0E, 0xC2, 0x04},
  {0x04, 0xC1, 0x0F, 0xC2, 0x04},
  {0x04, 0xC1, 0x10, 0xC2, 0x04},
  {0x04, 0xC1, 0x11, 0xC2, 0x04},
  {0x04, 0xC1, 0x12, 0xC2, 0x04},
  {0x02, 0xC1, 0x13}
};

const byte SONG2SOUND[SONG_COUNT] = {
  0x00, 0x01, 0x02, 0x01, 0x05, 0x06, 0x03, 0x04, 0x01, 0x1a
};

TM1637Display display(DISPLAY_CLK, DISPLAY_DIO);

Midi midi;
Switch switch_1 = Switch(SWITCH_1);
Switch switch_2 = Switch(SWITCH_2);
byte pos = 0;

void setup() {
  initDebug();
  initLEDs();
  midi.initMidi();
  display.setBrightness(0x0f);
  clearDisplay();
  pos = 0;
  pollSwitches();
  sendActualMidiCommand(pos);
}

void loop() {
  pollSwitches();
  if (switch_1.pushed()) {
    dbgOutLn("p1");
    pos += 1;
    if (pos >= SONG_COUNT) {
      pos = 0;
    }
    sendActualMidiCommand(pos);
  }
  if (switch_2.pushed()) {
    dbgOutLn("p2");
    if (pos == 0) {
      pos = SONG_COUNT;
    }
    pos -= 1;
    sendActualMidiCommand(pos);
  }
}

void sendActualMidiCommand(byte position) {
  dbgOut("send");
  dbgOut2(position, DEC);
  dbgOutLn();
  byte soundPosition = SONG2SOUND[position];
  midi.sendMidiCommands(SEND_ARRAY[soundPosition]);
  clearDisplay();
  display.setColon(true);
  display.showNumberDec(position + 1, false, 2, 0);
  display.showNumberDec(soundPosition + 1, false, 2, 2);
}

void initLEDs() {
  pinMode(LED_1, OUTPUT);
  pinMode(LED_2, OUTPUT);
  allLEDsOff();
}

void pollSwitches() {
  switch_1.poll();
  switch_2.poll();
}

void setLEDs(byte led) {
  allLEDsOff();
  digitalWrite(led, 1);
}

void allLEDsOff() {
  digitalWrite(LED_1, 0);
  digitalWrite(LED_2, 0);
}

void clearDisplay() {
  byte data[] = {0, 0, 0, 0};
  display.setSegments(data);
}
