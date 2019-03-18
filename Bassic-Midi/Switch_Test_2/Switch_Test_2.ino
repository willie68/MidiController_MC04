<<<<<<< Updated upstream
/*
   Simple program to send program changes over the MIDI protokol, if a switch is pushed.
   For every switch there is a LED to present, that this switch is pressed.

*/
#define debug
#include "debug.h"
#include <avdweb_Switch.h>

const byte SWITCH_1 = 4;
const byte LED_1 = 5;
const byte SWITCH_2 = 6;
const byte LED_2 = 7;
const byte SWITCH_3 = 8;
const byte LED_3 = 9;
const byte SWITCH_4 = 10;
const byte LED_4 = 11;

Switch switch_1 = Switch(SWITCH_1);
Switch switch_2 = Switch(SWITCH_2);
Switch switch_3 = Switch(SWITCH_3);
Switch switch_4 = Switch(SWITCH_4);

void setup() {
  initDebug();
  dbgOutLn("init LED");
  initLEDs();
  setLEDs(LED_1);
}

void loop() {
  pollSwitches();
  if (switch_1.pushed()) {
    dbgOutLn("Sw 1 pushed");
    setLEDs(LED_1);
  }
  if (switch_2.pushed()) {
    dbgOutLn("Sw 2 pushed");
    setLEDs(LED_2);
  }
  if (switch_3.pushed()) {
    dbgOutLn("Sw 3 pushed");
    setLEDs(LED_3);
  }
  if (switch_4.pushed()) {
    dbgOutLn("Sw 4 pushed");
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
  dbgOutLn("poll switches");
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
=======
/*
   Simple program to send program changes over the MIDI protokol, if a switch is pushed.
   For every switch there is a LED to present, that this switch is pressed.

*/
#define debug
#include "debug.h"
#include <avdweb_Switch.h>

const byte SWITCH_1 = 4;
const byte LED_1 = 5;
const byte SWITCH_2 = 6;
const byte LED_2 = 7;
const byte SWITCH_3 = 8;
const byte LED_3 = 9;
const byte SWITCH_4 = 10;
const byte LED_4 = 11;

Switch switch_1 = Switch(SWITCH_1);
Switch switch_2 = Switch(SWITCH_2);
Switch switch_3 = Switch(SWITCH_3);
Switch switch_4 = Switch(SWITCH_4);

void setup() {
  initDebug();
  dbgOutLn("init LED");
  initLEDs();
  setLEDs(LED_1);
}

void loop() {
  pollSwitches();
  if (switch_1.pushed()) {
    dbgOutLn("Sw 1 pushed");
    setLEDs(LED_1);
  }
  if (switch_2.pushed()) {
    dbgOutLn("Sw 2 pushed");
    setLEDs(LED_2);
  }
  if (switch_3.pushed()) {
    dbgOutLn("Sw 3 pushed");
    setLEDs(LED_3);
  }
  if (switch_4.pushed()) {
    dbgOutLn("Sw 4 pushed");
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
  dbgOutLn("poll switches");
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
>>>>>>> Stashed changes
