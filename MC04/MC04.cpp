#include <Arduino.h>
#include <Print.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <WString.h>

#define debug

#include <debug.h>
#include <makros.h>
#include <avdweb_Switch.h>
#include "midi.h"
#include "constants.h"
#include "prgstorage.h"
#include <Wire.h>
#include <LCD.h>
#include <LiquidCrystal_I2C.h>
#include <ClickEncoder.h>
#include <TimerOne.h>
#include <WS2812.h>
#include "globals.h"

/**
 Midi Controller MC04, a simple programmable midi controller with 4 switches (plus 2 external).
 */
Switch switch_1 = Switch(PIN_SWITCH_1, INPUT, HIGH);
Switch switch_2 = Switch(PIN_SWITCH_2, INPUT, HIGH);
Switch switch_3 = Switch(PIN_SWITCH_3, INPUT, HIGH);
Switch switch_4 = Switch(PIN_SWITCH_4, INPUT, HIGH);
Switch switch_5 = Switch(PIN_SWITCH_5, INPUT_PULLUP, LOW);
Switch switch_6 = Switch(PIN_SWITCH_6, INPUT_PULLUP, LOW);

void doSerialProgram();

#include "serialprg.ino"
#include "Settings.ino"
#include "LED.ino"

byte buttonAction[] = { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };
int expression_1 = 0;
int expression_2 = 0;

// forward declarations of functions
void initStorage();
void initLCD();
void initNeoPixel();

void checkEncoder();
void setDirty(bool value);
void showDirty();
void changeProgram(byte prg);
void printPrgName(byte value);
void changeController(byte controller, byte data);
void sendMidi(byte midiData[], byte count);
bool processEvent(byte event, byte expValue);
bool processButton(Switch mySwitch, byte number, byte mask);
void pollSwitches();
void checkSwitches();
void checkExpression();
void processMidiIn();
bool checkMidiIn();

// timer interrupt routines
void timerIsr() {
	encoder.service();
}

// arduino setup function
void setup() {
	initDebug()
	;
	dbgOutLn(F("setup"));

	initNeoPixel();

	Timer1.initialize(1000);
	Timer1.attachInterrupt(timerIsr);
	checkEncoder();

	dbgOutLn(F("init MIDI"));
	midi.initMidi();
	initLCD();

	initMenuSystem();

	initStorage();

	globalButtonMode = storage.getGlobalButtonMode();
	globalExpresionMode = storage.getGlobalExpressionMode();

	changeProgram(0);
}

void initStorage() {
	dbgOut(F("init storage, "));
	storage.init();
	dbgOut(F("v: "));
	dbgOut2(storage.getDataVersion(), DEC);
	dbgOut(F(","));
	dbgOut2(storage.getNumberOfPrograms(), DEC);
	dbgOutLn(F(" programs found."));
}

void initLCD() {
	dbgOutLn(F("init LCD"));
	lcd.begin(16, 2);
	lcd.clear();
	lcd.setBacklight(HIGH);
	lcd.setCursor(4, 0);
	lcd.print(F("WK Music"));
	lcd.setCursor(2, 1);
	lcd.print(F("MC 04 "));
	lcd.print(VERSION);
	delay(1000);
#ifndef debug
	delay(2000);
#endif
}

// ----- main loop -----
byte lastPrg = -1;
byte prgToSet = 0;
unsigned long time;
unsigned long resetTime = 0;
unsigned long sec = 0;
bool updateLED;
bool settingsActive = false;
void loop() {
	updateLED = false;
	pollSwitches();

	if (settingsActive) {
		settingsActive = !doMenuWork();
		if (!settingsActive) {
			lcd.clear();
			printPrgName(prg);
			lcd.setCursor(0, 1);
			lcd.print(F("saving data"));
			if (settingsDirty) {
				storage.saveRAM2Prg();
			}
			changeProgram(prg);
		}
	} else {
		if (checkMidiIn()) {
			processMidiIn();
		}
		checkExpression();
		checkSwitches();
		checkEncoder();

		if (prg != lastPrg) {
			lastPrg = prg;
			updateLED = true;
		}
		if (updateLED) {
			showRGBLed();
		}
		time = millis();

		ClickEncoder::Button b = encoder.getButton();
		if (resetTime > 0) {
			if (b != ClickEncoder::Open) {
				if (b == ClickEncoder::Clicked) {
					if (prg != prgToSet) {
						prg = prgToSet;
						changeProgram(prg);
						resetTime = 0;
					}
				}
			}
			if (time > resetTime) {
				if (prgToSet != prg) {
					changeProgram(prg);
					prgToSet = prg;
				}
				resetTime = 0;
			}
		} else {
			if (b != ClickEncoder::Open) {
				if (b == ClickEncoder::Clicked) {
					settingsActive = true;
					showMenu();
				}
			}
		}
	}
}

void pollSwitches() {
	switch_1.poll();
	switch_2.poll();
	switch_3.poll();
	switch_4.poll();
	if (globalButtonMode == GLOBAL_MODE_6_BUTTON) {
		switch_5.poll();
		switch_6.poll();
	}
}

bool checkMidiIn() {
	return midi.poll();
}

void processMidiIn() {
	MidiCommand command;
	if (midi.getMidiCommand(&command)) {
		if ((command.command & 0x0F) == storage.getInternalMidiChannel()) {
			if ((command.command & 0xF0) == MIDI_CONTROLLER_CHANGE) {
				changeController(command.data1, command.data2);
			} else if ((command.command & 0xF0) == MIDI_PROGRAM_CHANGE) {
				changeProgram(command.data1);
			}
		}
	}
}

void checkExpression() {
	if ((globalExpresionMode == GLOBAL_MODE_1_EXPRESSION)
			|| (globalExpresionMode == GLOBAL_MODE_2_EXPRESSION)) {
		int value = analogRead(PIN_EXPRESSION_1);
		byte bValue = map(value, 0, 1023, 0, 127);
		if (!between(bValue, expression_1 - 1, expression_1 + 1)) {
			expression_1 = bValue;
			bValue = EVENT_EXPRESSION_1 + EVENT_VALUECHANGE;
			processEvent(bValue, expression_1);
			dbgOut(F("exp1:"));
			dbgOut2(expression_1, HEX);
			dbgOutLn();
		}
		if (globalExpresionMode == GLOBAL_MODE_2_EXPRESSION) {
			value = analogRead(PIN_EXPRESSION_2);
			bValue = map(value, 0, 1023, 0, 127);
			if (!between(bValue, expression_2 - 1, expression_2 + 1)) {
				expression_2 = bValue;
				bValue = EVENT_EXPRESSION_2 + EVENT_VALUECHANGE;
				processEvent(bValue, expression_2);
				dbgOut(F("exp2:"));
				dbgOut2(expression_2, HEX);
				dbgOutLn();
			}
		}
	}
}

void checkSwitches() {
	bool isEvent = false;
	byte event = 0;

	processButton(switch_1, 0, EVENT_BUTTON_0);
	processButton(switch_2, 1, EVENT_BUTTON_1);
	processButton(switch_3, 2, EVENT_BUTTON_2);
	processButton(switch_4, 3, EVENT_BUTTON_3);
	if (globalButtonMode == GLOBAL_MODE_6_BUTTON) {
		processButton(switch_5, 4, EVENT_BUTTON_4);
		processButton(switch_6, 5, EVENT_BUTTON_5);
	}
}

byte midiData[48];
bool processButton(Switch mySwitch, byte number, byte mask) {
	byte event = 0;
	bool isEvent = false;
	if (mySwitch.singleClick()) {
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
	if (mySwitch.doubleClick()) {
		event = mask + EVENT_DOUBLECLICK;
		isEvent = true;
		dbgOut(F("b"));
		dbgOut2(number, DEC);
		dbgOutLn(F(": dc"));
	}
	if (mySwitch.longPress()) {
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
				changeProgram(data1);
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

char line[16];
int16_t last = -1;
void checkEncoder() {
	value += encoder.getValue();
	value = constrain(value, 0, 127);
	if (value != last) {
		last = value;

		printPrgName(value);

		resetTime = millis() + 3000;
		prgToSet = value;
		setDirty(true);
		showDirty();
		dbgOutLn(F("enc"));
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
			prg++;
			changeProgram(prg);
			break;
		case CC_PROGRAM_DOWN:
			prg--;
			changeProgram(prg);
			break;
		case CC_EXPRESSION_1:
			expression_1 = data;
			event = EVENT_EXPRESSION_1 + EVENT_VALUECHANGE;
			processEvent(event, expression_1);
			break;
		case CC_EXPRESSION_2:
			expression_2 = data;
			event = EVENT_EXPRESSION_2 + EVENT_VALUECHANGE;
			processEvent(event, expression_2);
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

void changeProgram(uint8_t prg) {
	byte event = EVENT_INTERNAL + EVENT_STOP;
	processEvent(event, 0);

	char row[17];
	byte pos = 1;
	storage.setProgram(prg);

	itoa(prg, row, 10);
	if (prg < 10) {
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

	lcd.setCursor(0, 1);
	storage.getButtonName(0, line);
	lcd.print(line);
	dbgOut(line);
	dbgOut(F("  "));

	lcd.setCursor(6, 1);
	storage.getButtonName(1, line);
	lcd.print(line);
	dbgOut(line);
	dbgOut(F("  "));

	lcd.setCursor(12, 1);
	storage.getButtonName(2, line);
	lcd.print(line);
	dbgOutLn(line);

	dbgOut(F("color: "));
	for (byte i = 0; i < 3; i++) {
		buttonColor[i] = storage.getButtonColor(i);
#ifdef debug
		if (i > 0) {
			dbgOut(",");
		}
		dbgOut2(buttonColor[i], HEX);
#endif
	}
	dbgOutLn();

	for (byte i = 0; i < 6; i++) {
		byte action = storage.getButtonUsage(i);
		buttonAction[i] = action;
		dbgOut(i);
		dbgOut(F(":"));
		dbgOutLn2(action, HEX);
	}
	setDirty(false);
	showDirty();
}

bool dirty = false;
void setDirty(bool value) {
	dirty = value;
}

void showDirty() {
	lcd.setCursor(15, 0);
	if (dirty) {
		lcd.print(F("*"));
	} else {
		lcd.print(F(" "));
	}
}

//----- starting serial programming -----
void doSerialProgram() {
	lcd.clear();
	lcd.setCursor(0, 0);
	lcd.print(F("serial program"));
	lcd.setCursor(0, 1);
	lcd.print(F("(r)d (w)t (e)d"));
	serialPrg();

	midi.initMidi();
	initLCD();

	changeProgram(0);
}
