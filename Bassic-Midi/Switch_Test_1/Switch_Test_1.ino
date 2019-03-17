#define debug

#include <debug.h>
#include <makros.h>
#include <RCReceive.h>

/*
   Simple program to send program changes over the MIDI protokol, if a switch is pushed.
   For every switch there is a LED to present, that this switch is pressed.

*/

const byte SWITCH_1 = 4;
const byte LED_1 = 5;
const byte SWITCH_2 = 6;
const byte LED_2 = 7;
const byte SWITCH_3 = 8;
const byte LED_3 = 9;
const byte SWITCH_4 = 10;
const byte LED_4 = 11;

void setup() {
  initDebug();
  dbgOutLn("init Switches");
  initSwitches();
  dbgOutLn("init LED");
  initLEDs();
  setLEDs(LED_1);
}

void loop() {
  if (digitalRead(SWITCH_1) == 0) {
    dbgOutLn("Sw 1 pushed");
    setLEDs(LED_1);
  }
  if (digitalRead(SWITCH_2) == 0) {
    dbgOutLn("Sw 2 pushed");
    setLEDs(LED_2);
  }
  if (digitalRead(SWITCH_3) == 0) {
    dbgOutLn("Sw 3 pushed");
    setLEDs(LED_3);
  }
  if (digitalRead(SWITCH_4) == 0) {
    dbgOutLn("Sw 4 pushed");
    setLEDs(LED_4);
  }
}

void initSwitches() {
  pinMode(SWITCH_1, INPUT_PULLUP);
  pinMode(SWITCH_2, INPUT_PULLUP);
  pinMode(SWITCH_3, INPUT_PULLUP);
  pinMode(SWITCH_4, INPUT_PULLUP);  
}

void initLEDs() {
  pinMode(LED_1, OUTPUT);
  pinMode(LED_2, OUTPUT);
  pinMode(LED_3, OUTPUT);
  pinMode(LED_4, OUTPUT);
  allLEDsOff();
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
