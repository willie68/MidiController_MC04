#include <WS2812.h>

#ifndef CONSTANTS_H
#define CONSTANTS_H

#define VERSION F("Mini V0.0.1")

// Hardware mappings
const byte PIN_SWITCH_1 = 5;
const byte PIN_SWITCH_2 = 6;
const byte PIN_SWITCH_3 = 7;

// Hardware pins for LCD (Just for documentation purpose
//const byte PIN_CLK = 13;
//const byte PIN_MISO = 12;
//const byte PIN_MOSI = 11;
//const byte PIN_SS = 10;

const byte PIN_EXPRESSION_1 = 0; // (D 14)
const byte PIN_EXPRESSION_2 = 1; // (D 15)

//const byte PIN_ENCODER_A = 2;
//const byte PIN_ENCODER_B = 3;
//const byte PIN_ENCODER_SW = 4;

const byte PIN_ENCODER_A = 4;
const byte PIN_ENCODER_B = 5;
const byte PIN_ENCODER_SW = 6;

const byte ENCODER_STP_NOTCH = 4;

const byte BUTTON_COUNT = 3;

const byte PIN_RGB_LED = 4;
const byte LED_COUNT = 3;
const byte LED_PER_SWITCH = 1;
const byte LED_USED = 3;
const byte LED_STATUS = (LED_USED * LED_PER_SWITCH) - 1 + 1;

const cRGB cBlack = { 0, 0, 0 };

const byte LCD_WIDTH = 16;
const byte LCD_HEIGHT = 2;
const byte MAX_BUTTON_WIDTH = LCD_WIDTH / BUTTON_COUNT;


// some widely used constants
const byte GLOBAL_MODE_4_BUTTON = 0;
const byte GLOBAL_MODE_6_BUTTON = 1;
const byte GLOBAL_MODE_0_EXPRESSION = 2;
const byte GLOBAL_MODE_1_EXPRESSION = 3;
const byte GLOBAL_MODE_2_EXPRESSION = 4;

const byte MIDI_CHANNEL_IN = 4;

const byte SINGLE_CLICK = 1;
const byte DOUBLE_CLICK = 2;
const byte LONG_CLICK = 4;
const byte PUSHED = 8;
const byte RELEASED = 16;

const byte EVENT_TYPE_INTERNAL = 0x00;
const byte EVENT_TYPE_BUTTON = 0x10;
const byte EVENT_TYPE_EXPRESSION = 0x70;

const byte EVENT_INTERNAL = 0x00;
const byte EVENT_BUTTON_0 = 0x10;
const byte EVENT_BUTTON_1 = 0x20;
const byte EVENT_BUTTON_2 = 0x30;
const byte EVENT_BUTTON_3 = 0x40;
const byte EVENT_BUTTON_4 = 0x50;
const byte EVENT_BUTTON_5 = 0x60;
const byte EVENT_EXPRESSION_1 = 0x70;
const byte EVENT_EXPRESSION_2 = 0x80;
const byte EVENT_START = 0x00;
const byte EVENT_STOP = 0x01;
const byte EVENT_PUSH = 0x02;
const byte EVENT_RELEASE = 0x03;
const byte EVENT_CLICK = 0x04;
const byte EVENT_DOUBLECLICK = 0x05;
const byte EVENT_LONGCLICK = 0x06;
const byte EVENT_VALUECHANGE = 0x07;
const byte EVENT_CC = 0x08;

const byte CC_EVENT_EXECUTE = 0x00;
const byte CC_PROGRAMM_UP = 0x01;
const byte CC_PROGRAM_DOWN = 0x02;
const byte CC_EXPRESSION_1 = 0x04;
const byte CC_EXPRESSION_2 = 0x05;
const byte CC_COLOR_1 = 0x08;
const byte CC_COLOR_2 = 0x09;
const byte CC_COLOR_3 = 0x0A;
const byte CC_COLOR_4 = 0x0B;
const byte CC_COLOR_5 = 0x0C;
const byte CC_COLOR_6 = 0x0D;
const byte CC_FS_1 = 0x10;
const byte CC_FS_2 = 0x11;
const byte CC_FS_3 = 0x12;
const byte CC_FS_4 = 0x13;
const byte CC_FS_5 = 0x14;
const byte CC_FS_6 = 0x15;

const byte CC_FS_RELEASE = 0x00;
const byte CC_FS_SHORT_CLICK = 0x20;
const byte CC_FS_DOUBLE_CLICK = 0x40;
const byte CC_FS_LONG_CLICK = 0x60;
const byte CC_FS_PUSH = 0x7F;

#define PRG_SIZE 806
#define NAME_SIZE 12
#define BUTTON_NAME_SIZE 8
#define SEQUENZES 16
#define MIDI_COMMANDS 16
#define EVENT_SIZE 49

#endif
