#include <debug.h>
#include <Arduino.h>
#include <inttypes.h>
#include "prgstorage.h"
#include "constants.h"
#include <SPIMemory.h>
#include <EEPROM.h>
#include <avr/eeprom.h>
//#include "tools.h"

const byte btn_use_0[6] = { SINGLE_CLICK, DOUBLE_CLICK, LONG_CLICK, PUSHED, RELEASED, 0x00 };
const byte btn_use_1[6] = { SINGLE_CLICK | DOUBLE_CLICK, DOUBLE_CLICK | LONG_CLICK | RELEASED, 0x00, 0x00, 0x00, 0x00 };

const byte PC_0 = 0x13;
const byte PC_1 = 0xff;

const uint16_t ADDRESS_GLOBAL_BUTTON_MODE = E2END;
const uint16_t ADDRESS_GLOBAL_EXPRESSION_MODE = E2END - 1;

SPIFlash flash;

/**
 storage class, first using eeprom for store and retrieve program settings
 */
PrgStorage::PrgStorage() {
	prgMemory = (byte *) malloc(PRG_SIZE);
}

byte PrgStorage::getDataVersion() {
	return EEPROM.read(0);
}

void PrgStorage::init() {
	scanPrograms();
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

void PrgStorage::scanPrograms() {
	numberOfPrograms = 0;
	char name[16];
	for (int addr = 0; addr <= 128; addr++) {
		byte value = flash.readByte((addr * PAGE_SIZE));
		if ((value > 0) && (value < 0xff)) {
			numberOfPrograms++;
			byte i = 0;
			do {
				value = flash.readByte((addr * PAGE_SIZE) + i);
				name[i] = value;
				i++;
			} while ((i < 16) && value > 0);
		}
	}
}

byte PrgStorage::getNumberOfPrograms() {
	return numberOfPrograms;
}

// setting storage to first program
void PrgStorage::setFirstProgram() {
	prgNumber = 0;
	copyPrg2RAM();
}

char* PrgStorage::getNameOfPrg(byte number, char* buf) {
	char* p = buf;
	int myPrg = ((number % numberOfPrograms) * PAGE_SIZE);
	byte i = 0;
	char value;
	do {
		value = flash.readByte(myPrg++);
		*p++ = value;
		i++;
	} while ((i < NAME_SIZE) && value > 0);
	return buf;
}

// setting storage to program
void PrgStorage::setProgram(byte number) {
	prgNumber = number % numberOfPrograms;
	copyPrg2RAM();
}

byte PrgStorage::getNumber() {
	return prgNumber;
}

// getting name of actual program
char* PrgStorage::getName(char* buf) {
	char* p = buf;
	byte* m = prgMemory;
	byte i = 0;
	char value;
	do {
		value = *m++;
		*p++ = value;
		i++;
	} while ((i < NAME_SIZE) && value > 0);
	return buf;
}

void PrgStorage::setName(char* buf) {
	char* p = buf;
	byte* m = prgMemory;
	byte i = 0;
	char value;
	do {
		value = *p++;
		*m++ = value;
		i++;
	} while ((i < NAME_SIZE) && value > 0);
}

// getting name of button <number>
char* PrgStorage::getButtonName(byte number, char* buf) {
	char* p = buf;
	byte* m = prgMemory;
	m = m + 15 + (number * (BUTTON_NAME_SIZE + 1));
	byte i = 0;
	char value;
	do {
		value = *m++;
		*p++ = value;
		i++;
	} while ((i < BUTTON_NAME_SIZE) && value > 0);
	return buf;
}

void PrgStorage::setButtonName(byte number, char* buf) {
	char* p = buf;
	byte* m = prgMemory;
	m = m + 15 + (number * (BUTTON_NAME_SIZE + 1));
	byte i = 0;
	char value;
	do {
		value = *p++;
		*m++ = value;
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
	return *(prgMemory + 1 + 14 + (button * 9) + 8);
}

// getting the defined buttons
void PrgStorage::setButtonColor(byte button, byte color) {
	*(prgMemory + 1 + 14 + (button * 9) + 8) = color;
}

// getting the defined buttons
byte PrgStorage::getButtonUsage(byte button) {
	switch (prgNumber) {
	case 0:
		return btn_use_0[button];
		break;
	case 1:
		return btn_use_1[button];
		break;
	}
}

byte PrgStorage::getPCNumber() {
	byte PC = *(prgMemory + 12);
	if ((PC & 0x80) > 0) {
		PC = prgNumber;
	}
	return PC;
}

void PrgStorage::setPCNumber(byte pcNumber) {
	*(prgMemory + 12) = pcNumber;
}

byte PrgStorage::getInternalMidiChannel() {
	return *(prgMemory + 13);
}

// this is the midi channel this unit will respond to.
void PrgStorage::setInternalMidiChannel(byte intMidi) {
	*(prgMemory + 13) = intMidi;
}

byte PrgStorage::getExternalMidiChannel() {
	return *(prgMemory + 14);
}

// this is the midi channel which this unit will send to
void PrgStorage::setExternalMidiChannel(byte extMidi) {
	*(prgMemory + 14) = extMidi;
}

byte PrgStorage::getSwitchSettings() {
	return *(prgMemory + 69);
}

void PrgStorage::setSwitchSettings(byte value) {
	*(prgMemory + 69) = value;
}

bool PrgStorage::getEvent(byte eventnumber, byte eventData[]) {
	byte* p = prgMemory + 70;
	byte i = 0;
	do {
		byte event = *p;
		if (event == eventnumber) {
			p++;
			for (byte x = 0; x < (EVENT_SIZE - 1); x++) {
				eventData[x] = *p++;
			}
			return true;
		}
		i++;
		p = p + EVENT_SIZE;
	} while (i < 16);
	return false;
}

byte PrgStorage::getEventByNumber(byte number, byte eventData[]) {
	byte* p = prgMemory + 70;
	p = p + (EVENT_SIZE * number);
	byte i = 0;
	byte eventnumber = *p++;
	for (byte x = 0; x < (EVENT_SIZE - 1); x++) {
		eventData[x] = *p++;
	}
	return eventnumber;
}

void PrgStorage::compressEventData(byte* eventData, byte size) {
	byte i = 0;
	byte value = 0;
	byte subPosition = 0;
	while (i < size) {
		value = eventData[i];
		if (value == 0) {
			subPosition = i + 3;
			while (subPosition < size) {
				if (subPosition < size) {
					byte newValue = eventData[subPosition];
					if (!(newValue == 0)) {
						eventData[i] = eventData[subPosition];
						eventData[i + 1] = eventData[subPosition + 1];
						eventData[i + 2] = eventData[subPosition + 2];
						eventData[subPosition] = 0;
						eventData[subPosition + 1] = 0;
						eventData[subPosition + 2] = 0;
						subPosition = size;
					}
				} else {
					i = size;
				}
				subPosition = subPosition + 3;
			}
		}
		i = i + 3;
	};
}

void PrgStorage::setEventByNumber(byte number, byte eventnumber, byte eventData[]) {
	byte* p = prgMemory + 70;
	p = p + (EVENT_SIZE * number);
	byte i = 0;
	compressEventData(eventData, (byte) sizeof(eventData));
	*p++ = eventnumber;
	for (byte x = 0; x < (EVENT_SIZE - 1); x++) {
		*p++ == eventData[x];
	}
	return;
}

void PrgStorage::copyPrg2RAM() {
	Serial.println(F("reading program from eeprom"));
	byte* p = prgMemory;
	unsigned int i;
	unsigned int address = (prgNumber * PAGE_SIZE);
	flash.readByteArray(address, p, PRG_SIZE, true);
//	for (i = 0; i < PRG_SIZE; i++)
//		*p++ = flash.readByte(address++);
}

void PrgStorage::saveRAM2Prg() {
	Serial.println(F("saving program to eeprom"));
	byte* p = prgMemory;
	unsigned int i;
	unsigned int address = (prgNumber * PAGE_SIZE);
	flash.eraseSector(address);
	flash.writeByteArray(address, p, PRG_SIZE, true);
//	for (i = 0; i < PRG_SIZE; i++)
//		EEPROM.write(address++, *p++);
}
