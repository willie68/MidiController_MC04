/*
 simple library for sending midi commads over standard serial.
 */
#include <Arduino.h>
#include <inttypes.h>

#define BAUDRATE 31250
const byte MIDI_PROGRAM_CHANGE = 0xC0;
const byte MIDI_CONTROLLER_CHANGE = 0xB0;
const byte MIDI_PAUSE = 0xF8;

struct MidiCommand {
  byte command;
  byte data1;
  byte data2;
};

class Midi {
private:
  byte midiData[3];
  byte writePos = 0;
  bool midiThru = false;
  bool midiAvailable = false;
public:
  void changeController(byte channel, byte ctrl, byte data) {
#ifdef debug
    dbgOut(F("md CC:"));
    dbgOut2(MIDI_CONTROLLER_CHANGE + (channel & 0x0f), HEX);
    dbgOut(F(","));
    dbgOut2(ctrl & 0x7F, HEX);
    dbgOut(F(","));
    dbgOut2(data & 0x7F, HEX);
#endif
    MidiCommand command;
    command.command = MIDI_CONTROLLER_CHANGE + (channel & 0x0f);
    command.data1 = ctrl & 0x7F;
    command.data2 = data & 0x7F;
    sendMidiCommand(&command);
    dbgOutLn();
  }

  void changeProgram(uint8_t channel, uint8_t program) {
#ifdef debug
    dbgOut(F("md PC:"));
    dbgOut2(MIDI_PROGRAM_CHANGE + (channel & 0x0f), HEX);
    dbgOut(F(","));
    dbgOut2(program & 0x7F, HEX);
#endif
    MidiCommand command;
    command.command = MIDI_PROGRAM_CHANGE + (channel & 0x0f);
    command.data1 = program & 0x7F;
    command.data2 = 0;
    sendMidiCommand(&command);
    dbgOutLn();
  }

  bool poll() {
    while (Serial1.available() > 0) {
      midiData[writePos++] = Serial1.read();
      if (midiThru) {
        Serial1.write(midiData[writePos - 1]);
      }
      if (writePos == 3) {
        writePos = 0;
        midiAvailable = true;
        break;
      }
    }
    return midiAvailable;
  }

  bool getMidiCommand(MidiCommand* command) {
    if (midiAvailable) {
      command->command = midiData[0];
      command->data1 = midiData[1];
      command->data2 = midiData[2];
      midiAvailable = false;
      return true;
    } else {
      return false;
    }
  }

  void initMidi() {
    setMidiBaudrate();
  }

  void setMidiBaudrate() {
    Serial1.begin(BAUDRATE);
    while(!Serial1) {};
  }

  /*
   send a single midi command
   */
  void sendMidiCommand(MidiCommand* command) {
    Serial1.write(command->command);
    Serial1.write(command->data1);
    if ((command->command & 0xF0) != MIDI_PROGRAM_CHANGE) {
      Serial1.write(command->data2);
    }
  }

  /*
   send array of midi commands
   */
  void sendMidiCommands(byte commands[], byte lengthOfCommands) {
    for (byte i = 0; i < lengthOfCommands; i++) {
      Serial1.write(commands[i]);
    }
  }

  /*
   send array of midi commands, first byte is the size
   */
  void sendMidiCommands(byte commands[]) {
    byte lengthOfCommands = commands[0];
    for (byte i = 0; i < lengthOfCommands; i++) {
      Serial1.write(commands[i + 1]);
    }
  }
};
