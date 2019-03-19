#define debug
#include <debug.h>
#include <makros.h>
#include <avdweb_Switch.h>
#include "midi.h"
#include "constants.h"
#include <Wire.h>
#include "LCD_I2C_AIP31068L.h"
#include <ClickEncoder.h>
#include <TimerOne.h>
#include <WS2812.h>
#include "prgstorage.h"
#include "globals.h"

Switch* switch_1;
Switch* switch_2;
Switch* switch_3;

// timer interrupt routines
void timerIsr() {
  encoder.service();
}

void setup() {
  // put your setup code here, to run once:
  initDebug();
  dbgOutLn(F("init lcd"));
  initLCD();

  dbgOutLn(F("init leds"));
  initLED();

  dbgOutLn(F("init switches"));
  initSwitches();
}

void initLED() {
  initNeoPixel();
}

void initSwitches() {
  switch_1 = new Switch(PIN_SWITCH_1, INPUT_PULLUP, LOW, 50, 500, 400, 10);
  switch_2 = new Switch(PIN_SWITCH_2, INPUT_PULLUP, LOW, 50, 500, 400, 10);
  switch_3 = new Switch(PIN_SWITCH_3, INPUT_PULLUP, LOW, 50, 500, 400, 10);
}

void initLCD() {
  lcd.begin(LCD_WIDTH, LCD_HEIGHT);
  lcd.clear();
  lcd.setBacklight(HIGH);
  lcd.setCursor(4, 0);
  lcd.print(F("WK Music"));
  lcd.setCursor(0, 1);
  lcd.print(F("MC3 "));
  lcd.print(VERSION);
  delay(1000);
}

//------ main program -----

bool updateLED;
byte midiData[48];
char line[16];
byte buttonAction[] = { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };

void loop() {
  updateLED = false;

  pollSwitches();

  checkSwitches();
  checkEncoder();
  if (updateLED) {
    showRGBLed();
  }

  ClickEncoder::Button b = encoder.getButton();
  if (b != ClickEncoder::Open) {
    if (b == ClickEncoder::Clicked) {
      doSerialProgram();
    }
  }
}

void pollSwitches() {
  switch_1->poll();
  switch_2->poll();
  switch_3->poll();
}

int16_t last = -1;
void checkEncoder() {
  value += encoder.getValue();
  value = constrain(value, 0, 127);
  if (value != last) {
    last = value;

//    printPrgName(value);

//    resetTime = millis() + 3000;
//    prgToSet = value;
//    setDirty(true);
//    showDirty();
  }
}

void checkSwitches() {
  bool isEvent = false;
  byte event = 0;

  processButton(switch_1, 0, EVENT_BUTTON_0);
  processButton(switch_2, 1, EVENT_BUTTON_1);
  processButton(switch_3, 2, EVENT_BUTTON_2);
}

bool processButton(Switch* mySwitch, byte number, byte mask) {
  byte event = 0;
  bool isEvent = false;
  if (mySwitch->singleClick()) {
    dbgOut(F("b"));
    dbgOut2(number, DEC);
    dbgOut(F(": sc"));
    if (buttonSettings[number]) {
      buttonState[number] = !buttonState[number];
      if (buttonState[number]) {
        event = mask + EVENT_PUSH;
        isEvent = true;
        dbgOut(F(" x"));
      } else {
        event = mask + EVENT_RELEASE;
        isEvent = true;
      }
    } else {
      event = mask + EVENT_CLICK;
      isEvent = true;
    }
    dbgOutLn();
    updateLED = true;
  }
  if (mySwitch->doubleClick()) {
    event = mask + EVENT_DOUBLECLICK;
    isEvent = true;
    dbgOut(F("b"));
    dbgOut2(number, DEC);
    dbgOutLn(F(": dc"));
  }
  if (mySwitch->longPress()) {
    event = mask + EVENT_LONGCLICK;
    isEvent = true;
    dbgOut(F("b"));
    dbgOut2(number, DEC);
    dbgOutLn(F(": lc"));
  }
  if (isEvent) {
    dbgOut(F("event:"));
    dbgOutLn2(event, HEX);
    isEvent = storage.getEvent(event, midiData);
    if (isEvent) {
#ifdef debug
      dbgOutLn(F("event found"));
      for (byte i = 0; i < 24; i++) {
        if (i > 0) {
          dbgOut(F(","));
        }
        dbgOut2(midiData[i], HEX);
      }
      dbgOutLn();
#endif
      sendMidi(midiData, sizeof(midiData));
    }
  }
  return isEvent;
}

bool processEvent(byte event, byte expValue) {
  bool isEvent = storage.getEvent(event, midiData);
  if (isEvent) {
    for (byte i = 0; i < 48; i = i + 3) {
      if ((midiData[i] & 0xF0) == MIDI_CONTROLLER_CHANGE) {
        if (midiData[i + 2] == 128) {
          midiData[i + 2] = expValue & 0x7F;
        }
      }
    }
#ifdef debug
    dbgOutLn(F("event found"));
    for (byte i = 0; i < 48; i++) {
      if (i > 0) {
        dbgOut(F(","));
      }
      dbgOut2(midiData[i], HEX);
    }
    dbgOutLn();
#endif
    sendMidi(midiData, sizeof(midiData));
  }
  return isEvent;
}

void sendMidi(byte midiData[], byte count) {
  byte i = 0;
  byte cmd, data1, data2, channel;
  do {
    cmd = midiData[i++];
    data1 = midiData[i++];
    data2 = midiData[i++];
    channel = cmd & 0x0F;
    if ((cmd & MIDI_PROGRAM_CHANGE) == MIDI_PROGRAM_CHANGE) {
      if (channel == storage.getInternalMidiChannel()) {
        actualProgram = data1;
        initProgram();
      } else {
        midi.changeProgram(channel, data1);
      }
    }
    if ((cmd & MIDI_CONTROLLER_CHANGE) == MIDI_CONTROLLER_CHANGE) {
      if (channel == storage.getInternalMidiChannel()) {
        changeController(data1, data2);
      } else {
        midi.changeController(channel, data1, data2);
      }
    }
  } while ((i < count) && (cmd != 0));
}

//---- changeController ----
void changeController(byte controller, byte data) {
  dbgOut(F("internal CC"));
  dbgOut2(controller, HEX);
  dbgOut(",");
  dbgOutLn2(data, HEX);
  byte channel = controller & 0x0F;
  byte cmd = controller & 0xF0;
  byte event = 0;
  if (channel == storage.getInternalMidiChannel()) {
    switch (cmd) {
      case CC_EVENT_EXECUTE:
        break;
      case CC_PROGRAMM_UP:
        actualProgram++;
        initProgram();
        break;
      case CC_PROGRAM_DOWN:
        actualProgram--;
        initProgram();
        break;
      case CC_EXPRESSION_1:
        //        expression_1 = data;
        //        event = EVENT_EXPRESSION_1 + EVENT_VALUECHANGE;
        //        processEvent(event, expression_1);
        break;
      case CC_EXPRESSION_2:
        //        expression_2 = data;
        //        event = EVENT_EXPRESSION_2 + EVENT_VALUECHANGE;
        //        processEvent(event, expression_2);
        break;
      case CC_COLOR_1:
        buttonColor[0] = data;
        updateLED = true;
        break;
      case CC_COLOR_2:
        buttonColor[1] = data;
        updateLED = true;
        break;
      case CC_COLOR_3:
        buttonColor[2] = data;
        updateLED = true;
        break;
      case CC_COLOR_4:
        buttonColor[3] = data;
        updateLED = true;
        break;
      case CC_COLOR_5:
        buttonColor[4] = data;
        updateLED = true;
        break;
      case CC_COLOR_6:
        buttonColor[5] = data;
        updateLED = true;
        break;
      case CC_FS_1:
        event = EVENT_BUTTON_0 + convertDataToEvent(data);
        processEvent(event, 0);
        break;
      case CC_FS_2:
        event = EVENT_BUTTON_1 + convertDataToEvent(data);
        processEvent(event, 0);
        break;
      case CC_FS_3:
        event = EVENT_BUTTON_2 + convertDataToEvent(data);
        processEvent(event, 0);
        break;
      case CC_FS_4:
        event = EVENT_BUTTON_3 + convertDataToEvent(data);
        processEvent(event, 0);
        break;
      case CC_FS_5:
        event = EVENT_BUTTON_4 + convertDataToEvent(data);
        processEvent(event, 0);
        break;
      case CC_FS_6:
        event = EVENT_BUTTON_5 + convertDataToEvent(data);
        processEvent(event, 0);
        break;
    }
  }
}

void initProgram() {
  byte event = EVENT_INTERNAL + EVENT_STOP;
  processEvent(event, 0);

  char row[17];
  byte pos = 1;

  itoa(actualProgram, row, 10);
  if (actualProgram < 10) {
    row[1] = ' ';
  }
  row[2] = ':';
  row[3] = 0;
  storage.getName(line);
  strncat(row, line, 12);

  lcd.clear();
  lcd.setCursor(0, 0);
  lcd.print(row);
  dbgOutLn(row);

  byte internalMidiChannel = storage.getInternalMidiChannel();
  byte externalMidiChannel = storage.getExternalMidiChannel();
  dbgOut(F("Midi:"));
  dbgOut(internalMidiChannel);
  dbgOut(F(","));
  dbgOutLn(externalMidiChannel);

  byte switchSettings = storage.getSwitchSettings();
  dbgOut(F("switch:"));
  dbgOut2(switchSettings, BIN);
  dbgOut(F(","));
  byte i = 1;
  for (byte x = 0; x < 6; x++) {
    buttonSettings[x] = (switchSettings & i) > 0;
    i = i << 1;
  }
#ifdef debug
  for (byte x = 6; x > 0; x--) {
    if (buttonSettings[x - 1]) {
      dbgOut(1);
    } else {
      dbgOut(0);
    }
  }
  dbgOutLn();
#endif

  byte PC = storage.getPCNumber();
  dbgOut(F("sPC:"));
  dbgOutLn(PC);
  if (PC >= 0) {
    midi.changeProgram(externalMidiChannel, storage.getPCNumber());
    dbgOut(F("PC:"));
    dbgOutLn(storage.getPCNumber());
  } else {
    dbgOutLn(F("no PC"));
  }

  event = EVENT_INTERNAL + EVENT_START;
  processEvent(event, 0);

  for (byte x = 0; x < BUTTON_COUNT; x++) {
    lcd.setCursor((x * MAX_BUTTON_WIDTH), 1);
    storage.getButtonName(x, line);
    lcd.print(line);
    //    lcd.setCursor(6, 1);
    //    storage.getButtonName(1, line);
    //    lcd.print(line);
    //    dbgOut(line);
    //    dbgOut(F("  "));
    //
    //    lcd.setCursor(12, 1);
    //    storage.getButtonName(2, line);
    //    lcd.print(line);
    //    dbgOutLn(line);
  }
  for (byte i = 0; i < 3; i++) {
    buttonColor[i] = storage.getButtonColor(i);
  }

  for (byte i = 0; i < 6; i++) {
    byte action = storage.getButtonUsage(i);
    buttonAction[i] = action;
    dbgOut(i);
    dbgOut(F(":"));
    dbgOutLn2(action, HEX);
  }

  showRGBLed();
}

byte convertDataToEvent(byte data) {
  if (data == CC_FS_RELEASE) {
    return EVENT_RELEASE;
  }
  if (data == CC_FS_SHORT_CLICK) {
    return EVENT_CLICK;
  }
  if (data == CC_FS_DOUBLE_CLICK) {
    return EVENT_DOUBLECLICK;
  }
  if (data == CC_FS_LONG_CLICK) {
    return EVENT_LONGCLICK;
  }
  if (data == CC_FS_PUSH) {
    return EVENT_PUSH;
  }
}

void printPrgName(byte value) {
  char row[17];
  lcd.setCursor(0, 0);
  lcd.print("                ");
  itoa(value, row, 10);
  if (value < 10) {
    row[1] = ' ';
  }
  row[2] = ':';
  row[3] = 0;
  storage.getNameOfPrg(value, line);
  strncat(row, line, 12);
  lcd.setCursor(0, 0);
  lcd.print(row);
}

//----- starting serial programming -----
void doSerialProgram() {
  lcd.clear();
  lcd.setCursor(0, 0);
  lcd.print(F("serial program"));
  lcd.setCursor(0, 1);
  lcd.print(F("(u)e (r)d (w)t (e)d"));
  serialPrg();

  midi.initMidi();
  initLCD();

  initProgram();
}
