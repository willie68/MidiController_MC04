#include <arduino.h>
#include <EEPROM.h>
#include <avr/eeprom.h>
#include "tools.h"
#include "globals.h"

//#define serial_debug
#define SERIAL_BAUDRATE 9600

bool endOfFile = false;

// forward declarations of functions
char getNextChar();
byte getHexValue();
uint16_t getAddress();
void printCheckSum(int value);
void programEEprom(uint16_t readAddress, byte data[], byte count);


void inputEEpromData() {
  byte data[32];
  char c;
  byte count;
  uint16_t crc;
  uint16_t readAddress;
  uint16_t readcrc;
  byte type;

  endOfFile = false;

  Serial.println(F("ready"));
  uint16_t addr = 0;
  do {
    for (byte i = 0; i < sizeof(data); i++) {
      data[i] = 0xFF;
    }
    do {
      c = getNextChar();
    } while (!(c == ':'));

    // read counter
    count = getHexValue();

    crc = count;
    // address
    readAddress = getAddress();

    crc += readAddress >> 8;
    crc += readAddress & 0x00FF;

    // reading data type
    type = getHexValue();

#ifdef serial_debug
    Serial.print(".");
    printHex8(count);
    Serial.print(".");
    printHex16(readAddress);
    Serial.print(".");
    printHex8(type);
    Serial.print(".");
#endif

    crc += type;

    if (type == 0x01) {
      endOfFile = true;
    }

    // read data bytes
    for (byte x = 0; x < count; x++) {
      data[x] = getHexValue();
      crc += data[x];

#ifdef serial_debug
      printHex8(data[x]);
      Serial.print(".");
#endif
    }

    // read CRC
    readcrc = getHexValue();

    crc += readcrc;
    // check CRC
    value = crc & 0x00FF;

#ifdef serial_debug
    printHex8(readcrc);
    Serial.print(".");
    printHex8(value);
#endif

    if (value == 0) {
      Serial.print("ok");
      // adding value to EEPROM
      programEEprom(readAddress, data, count);
    } else {
      Serial.println(F(", CRC Error"));
      endOfFile = true;
    }

    Serial.println();
  } while (!(endOfFile));
  Serial.println(F("endOfFile"));

}

void outputEEpromData() {
  Serial.println(F("EEPROM data:"));
  int checksum = 0;
  for (int addr = 0; addr <= E2END; addr++) {
    value = EEPROM.read(addr);
    if ((addr % 8) == 0) {
      printCheckSum(checksum);
      checksum = 0;
      Serial.print(F(":08"));
      checksum += 0x08;
      printHex16(addr);
      checksum += (addr >> 8);
      checksum += (addr & 0x00FF);
      Serial.print(F("00"));
    }
    printHex8(value);
    checksum += value;
  }
  printCheckSum(checksum);
  // ending
  Serial.println(F(":00000001FF"));
}

void programEEprom(uint16_t readAddress, byte data[], byte count) {
  for (byte x = 0; x < count; x++) {
    EEPROM.write(readAddress + x, data[x]);
  }
}

byte getHexValue() {
  char  c = getNextChar();
  uint8_t value = hexToByte(c) << 4;
  c = getNextChar();
  value += hexToByte(c);
  return value;
}

uint16_t getAddress() {
  char c = getNextChar();
  uint16_t readAddress = hexToByte(c) << 12;
  c = getNextChar();
  readAddress += hexToByte(c) << 8;
  c = getNextChar();
  readAddress += hexToByte(c) << 4;
  c = getNextChar();
  readAddress += hexToByte(c);
  return readAddress;
}

char getNextChar() {
  while (!Serial.available()) {
  }
  return  Serial.read();
}

void printCheckSum(int value) {
  int checksum = value & 0xFF;
  checksum = (checksum ^ 0xFF) + 1;
  printHex8(checksum);
  Serial.println();
}

void serialPrg() {
  bool endOfPrg = false;

  ClickEncoder::Button b = encoder.getButton();

  Serial.end();
  Serial.begin(SERIAL_BAUDRATE);
  Serial.println();
  Serial.println(F("waiting for command:"));
  Serial.println(F("w: write HEX file, r: read EPPROM, e: end"));
  while (!endOfPrg) {
    while (Serial.available() > 0) {
      // look for the next valid integer in the incoming serial stream:
      char myChar = Serial.read();
      if (myChar == 'w') {
        // hexfile is comming to programm
        inputEEpromData();
      }
      if (myChar == 'r') {
        // write eeprom as hexfile to receiver
        outputEEpromData();
      }
      if (myChar == 'e') {
        // end of programm
        endOfPrg = true;
      }
    }
    /*    switch_prg.poll();
        if (switch_prg.singleClick()) {
          endOfPrg = true;
        }
    */
    if (b != ClickEncoder::Open) {
      if (b == ClickEncoder::Clicked) {
        endOfPrg = true;
      }
    }

  }
  Serial.println(F("end"));
  Serial.end();
}
