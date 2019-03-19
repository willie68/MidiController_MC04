#include <debug.h>
#include <Arduino.h>
#include <inttypes.h>
#include "prgstorage.h"
#include "constants.h"
#include <EEPROM.h>
#include <avr/eeprom.h>
//#include "tools.h"

const byte btn_use_0[6] = { SINGLE_CLICK, DOUBLE_CLICK, LONG_CLICK, PUSHED, RELEASED, 0x00 };
const byte btn_use_1[6] = { SINGLE_CLICK | DOUBLE_CLICK, DOUBLE_CLICK | LONG_CLICK | RELEASED, 0x00, 0x00, 0x00, 0x00 };

const uint16_t ADDRESS_GLOBAL_BUTTON_MODE = E2END;
const uint16_t ADDRESS_GLOBAL_EXPRESSION_MODE = E2END - 1;
const uint16_t DATA_VERSION = E2END - 2;
/**
  storage class, first using eeprom for store and retrieve program settings
*/
PrgStorage::PrgStorage() {
}

byte PrgStorage::getDataVersion() {
  return EEPROM.read(DATA_VERSION);
}

void PrgStorage::init() {
}

// getting the button mode for this
byte PrgStorage::getGlobalButtonMode() {
  byte value = EEPROM.read(ADDRESS_GLOBAL_BUTTON_MODE);
  if (value == 0xff) {
    value = GLOBAL_MODE_4_BUTTON;
  }
  return value;
}

// setting a new Button mode
void PrgStorage::setGlobalButtonMode(byte mode) {
  EEPROM.write(ADDRESS_GLOBAL_BUTTON_MODE, mode);
}

// getting the expression pedal mode for this
byte PrgStorage::getGlobalExpressionMode() {
  byte value = EEPROM.read(ADDRESS_GLOBAL_EXPRESSION_MODE);
  if (value == 0xff) {
    value = GLOBAL_MODE_0_EXPRESSION;
  }
  return value;
}

// setting the expression pedal mode for this
void PrgStorage::setGlobalExpressionMode(byte mode) {
  EEPROM.write(ADDRESS_GLOBAL_EXPRESSION_MODE, mode);
}

// getting name of actual program
char* PrgStorage::getName(char* buf) {
  char* p = buf;
  int m = 0;
  byte i = 0;
  char value;
  do {
    value = readByte(m++);
    *p++ = value;
    i++;
  } while ((i < NAME_SIZE) && value > 0);
  return buf;
}

void PrgStorage::setName(char* buf) {
  char* p = buf;
  int m = 0;
  byte i = 0;
  char value;
  do {
    value = *p++;
    writeByte(m++, value);
    i++;
  } while ((i < NAME_SIZE) && value > 0);
}

byte PrgStorage::getInternalMidiChannel() {
  return readByte(13);
}

// this is the midi channel this unit will respond to.
void PrgStorage::setInternalMidiChannel(byte intMidi) {
  writeByte(13, intMidi);
}

byte PrgStorage::getExternalMidiChannel() {
  return readByte(14);
}

// this is the midi channel which this unit will send to
void PrgStorage::setExternalMidiChannel(byte extMidi) {
  writeByte(14, extMidi);
}

byte PrgStorage::getSwitchSettings() {
  return readByte(69);
}

void PrgStorage::setSwitchSettings(byte value) {
  writeByte(69, value);
}

bool PrgStorage::getEvent(byte eventnumber, byte eventData[]) {
  int p = 70;
  byte i = 0;
  do {
    byte event = readByte(p);
    if (event == eventnumber) {
      p++;
      for (byte x = 0; x < (EVENT_SIZE - 1); x++) {
        eventData[x] = readByte(p++);
      }
      return true;
    }
    i++;
    p = p + EVENT_SIZE;
  } while (i < 16);
  return false;
}

byte PrgStorage::getEventByNumber(byte number, byte eventData[]) {
  int p = 70;
  p = p + (EVENT_SIZE * number);
  byte i = 0;
  byte eventnumber = readByte(p++);
  for (byte x = 0; x < (EVENT_SIZE - 1); x++) {
    eventData[x] = readByte(p++);
  }
  return eventnumber;
}

// getting name of button <number>
char* PrgStorage::getButtonName(byte number, char* buf) {
  char* p = buf;
  int m = m + 15 + (number * (BUTTON_NAME_SIZE + 1));
  byte i = 0;
  char value;
  do {
    value = readByte(m++);
    *p++ = value;
    i++;
  } while ((i < BUTTON_NAME_SIZE) && value > 0);
  return buf;
}

void PrgStorage::setButtonName(byte number, char* buf) {
  char* p = buf;
  int m = m + 15 + (number * (BUTTON_NAME_SIZE + 1));
  byte i = 0;
  char value;
  do {
    value = *p++;
    writeByte(m++ , value);
    i++;
  } while ((i < BUTTON_NAME_SIZE) && value > 0);
}

// getting name of button <number>
byte PrgStorage::getButtonType(byte number) {
  byte switchSettings = getSwitchSettings();
  switchSettings = switchSettings & (0x01 << number);
  if (switchSettings > 0) {
    return 1;
  }
  return 0;
}

void PrgStorage::setButtonType(byte number, byte value) {
  Serial.print("set Button ");
  Serial.print(number);
  Serial.print(" type:");
  Serial.print(value);
  Serial.print(",");

  byte switchSettings = getSwitchSettings();

  Serial.print(switchSettings, BIN);
  Serial.print(",");
  if (value == 1) {
    switchSettings = switchSettings | (0x01 << number);
  } else {
    switchSettings = switchSettings ^ (0x01 << number);
  }
  setSwitchSettings(switchSettings);
  Serial.print(switchSettings, BIN);
}

// getting the defined buttons
byte PrgStorage::getButtonColor(byte button) {
  return readByte(1 + 14 + (button * 9) + 8);
}

// getting the defined buttons
void PrgStorage::setButtonColor(byte button, byte color) {
  writeByte(1 + 14 + (button * 9) + 8, color);
}

// getting the defined buttons
byte PrgStorage::getButtonUsage(byte button) {
/*  switch (prgNumber) {
    case 0:
      return btn_use_0[button];
      break;
    case 1:
      return btn_use_1[button];
      break;
  }
  */
  return btn_use_0[button];
}

byte PrgStorage::getPCNumber() {
  byte PC = readByte(12);
  if ((PC & 0x80) > 0) {
    PC = 0x00;
  }
  return PC;
}

void PrgStorage::setPCNumber(byte pcNumber) {
  writeByte(12, pcNumber);
}

int PrgStorage::getProgramSize() {
  return PRG_SIZE;
}

byte PrgStorage::readByte(int addr) {
  return EEPROM.read(addr);
}

void PrgStorage::writeByte(int addr, byte value) {
  return EEPROM.write(addr, value);
}
