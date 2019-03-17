/*
   Simple program to send program changes over the MIDI protokol, if a switch is pushed.
   For every switch there is a LED to present, that this switch is pressed.

*/
#include "midi.h"
#include <avdweb_Switch.h>

const byte SWITCH_1 = 4;
const byte LED_1 = 5;
const byte SWITCH_2 = 6;
const byte LED_2 = 7;
const byte SWITCH_3 = 8;
const byte LED_3 = 9;
const byte SWITCH_4 = 10;
const byte LED_4 = 11;

const byte SEND_1[] = {0xC1, 0x01};
const byte SEND_2[] = {0xC1, 0x02};
const byte SEND_3[] = {0xC1, 0x03};
const byte SEND_4[] = {0xC1, 0x04};

Midi midi;
Switch switch_1 = Switch(SWITCH_1);
Switch switch_2 = Switch(SWITCH_2);
Switch switch_3 = Switch(SWITCH_3);
Switch switch_4 = Switch(SWITCH_4);

void setup() {
  initLEDs();
  midi.initMidi();
  midi.sendMidiCommands(SEND_1, sizeof(SEND_1));
  setLEDs(LED_1);
}

void loop() {
  pollSwitches();
  if (switch_1.pushed()) {
    midi.sendMidiCommands(SEND_1, sizeof(SEND_1));
    setLEDs(LED_1);
  }
  if (switch_2.pushed()) {
    midi.sendMidiCommands(SEND_2, sizeof(SEND_2));
    setLEDs(LED_2);
  }
  if (switch_3.pushed()) {
    midi.sendMidiCommands(SEND_3, sizeof(SEND_3));
    setLEDs(LED_3);
  }
  if (switch_4.pushed()) {
    midi.sendMidiCommands(SEND_4, sizeof(SEND_4));
    setLEDs(LED_4);
  }
}

void initLEDs() {
  pinMode(LED_1, OUTPUT);
  pinMode(LED_2, OUTPUT);
  pinMode(LED_3, OUTPUT);
  pinMode(LED_4, OUTPUT);
  allLEDsOff();
}

void pollSwitches() {
  switch_1.poll();
  switch_2.poll();
  switch_3.poll();
  switch_4.poll();
}

void setLEDs(byte led) {
  allLEDsOff();
  digitalWrite(led, 1);
}

void allLEDsOff() {
  digitalWrite(LED_1, 0);
  digitalWrite(LED_2, 0);
  digitalWrite(LED_3, 0);
  digitalWrite(LED_4, 0);
}

