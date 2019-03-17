/**
 * defining global variables for all source files
 */
#ifndef GLOBALS_H
#define GLOBALS_H

ClickEncoder encoder(PIN_ENCODER_A, PIN_ENCODER_B, PIN_ENCODER_SW, ENCODER_STP_NOTCH);

Midi midi;
PrgStorage storage;
LCD_I2C_AIP31068L lcd(0x3E);
WS2812 SWITCH_LED(LED_COUNT);

byte actualProgram = 0;
uint16_t value;
bool settingsDirty = false;

bool buttonSettings[] = { false, false, false, false, false, false };
byte buttonColor[] = { 0xC3, 0xCC, 0xF0, 0xFF, 0x00, 0x00 };
bool buttonState[] = { false, false, false, false, false, false };
byte globalButtonMode = GLOBAL_MODE_4_BUTTON;
byte globalExpressionMode = GLOBAL_MODE_0_EXPRESSION;

#endif
