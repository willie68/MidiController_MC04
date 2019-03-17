/*
   simple library for sending midi commads over standard serial.
*/
#include <Arduino.h>
#include <inttypes.h>

#define BAUDRATE 31250
//#define BAUDRATE 57600

class Midi {
  private:
    const byte PROGRAM_CHANGE = 0xC0;
    const byte CONTROLLER_CHANGE = 0xB0;
  public:
    void changeController(byte channel, byte ctrl, byte data) {
      Serial.write(PROGRAM_CHANGE + channel);
      Serial.write(ctrl);
      Serial.write(data);
    }

    void changeProgram(byte channel, byte program) {
      Serial.write(PROGRAM_CHANGE + channel);
      Serial.write(program);
    }

    void initMidi() {
      setMidiBaudrate();
    }

    void setMidiBaudrate() {
      Serial.begin(BAUDRATE);
    }

    void sendMidiCommands(byte commands[], byte lengthOfCommands) {
      for (byte i = 0; i < lengthOfCommands; i++) {
        Serial.write(commands[i]);
      }
    }

    void sendMidiCommands(byte commands[]) {
      byte lengthOfCommands = commands[0];
      for (byte i = 0; i < lengthOfCommands; i++) {
        Serial.write(commands[i+1]);
      }
    }
};
