/**
 * defining global variables for all source files
 */
#ifndef GLOBALS_H
#define GLOBALS_H

ClickEncoder encoder(PIN_ENCODER_A, PIN_ENCODER_B, PIN_ENCODER_SW, ENCODER_STP_NOTCH);

Midi midi;
PrgStorage storage;
LiquidCrystal_I2C lcd(0x27, 2, 1, 0, 4, 5, 6, 7, 3, POSITIVE);

byte prg = 0;
uint16_t value;
bool settingsDirty = false;

bool buttonSettings[] = { false, false, false, false, false, false };
byte buttonColor[] = { 0xC3, 0xCC, 0xF0, 0xFF, 0x00, 0x00 };
bool buttonState[] = { false, false, false, false, false, false };
byte globalButtonMode = GLOBAL_MODE_4_BUTTON;
byte globalExpresionMode = GLOBAL_MODE_0_EXPRESSION;

#endif
