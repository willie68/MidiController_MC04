
/**
   storage class, first using eeprom for store and retrieve program settings
*/

#ifndef PRGSTORAGE_H
#define PRGSTORAGE_H

class PrgStorage {
  public:
    PrgStorage();

    void init();

    //----- global settings functions -----
    // getting the button mode for this
    byte getGlobalButtonMode();

    // setting a new Button mode
    void setGlobalButtonMode(byte mode);

    // getting the expression pedal mode for this
    byte getGlobalExpressionMode();

    // setting the expression pedal mode for this
    void setGlobalExpressionMode(byte mode);

    //----- program memory functions -----
    // this will return the program change number
    byte getDataVersion();

    // this will return the program change number
    byte getNumberOfPrograms();

    // getting name of program prg
    char* getNameOfPrg(byte number, char* buf);

    // setting storage to first program
    void setFirstProgram();

    // setting storage to program
    void setProgram(byte number);

    // getting name of actual program
    char* getName(char* buf);

    // this is the midi channel this unit will respond to.
    byte getInternalMidiChannel();

    // this is the midi channel which this unit will send to
    byte getExternalMidiChannel();

    // getting name of button <number>
    char* getButtonName(byte number, char* buf);

    // getting color of button <number>
    byte getButtonColor(byte number);

    // getting the defined buttons
    byte getButtonUsage(byte button);

    // this will return the settings (momentary or switch) of each switch
    byte getSwitchSettings();

    // this will return the program change number
    byte getPCNumber();

    // storage interface for writing program data
    void setPCNumber(byte pcNumber);

    // this is the midi channel this unit will respond to.
    void setInternalMidiChannel(byte intMidi);

    // this is the midi channel which this unit will send to
    void setExternalMidiChannel(byte extMidi);

    // saving cached program to EEPROM
    void saveRAM2Prg();
    
    bool getEvent(byte eventnumber, byte eventData[]);
  protected:
    byte prgNumber;
    byte* prgMemory;
    byte numberOfPrograms;

    // copy the actual program to RAM
    void scanPrograms();

    // copy the actual program to RAM
    void copyPrg2RAM();
};

#endif
