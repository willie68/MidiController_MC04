/** 
 storage class, first using eeprom for store and retrieve program settings 
 */ 
 
#ifndef PRGSTORAGE_H 
#define PRGSTORAGE_H 
 
class PrgStorage { 
public: 
  PrgStorage(); 

  // this will return the data version number 
  byte getDataVersion(); 

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

  // getting name of actual program 
  char* getName(char* buf); 
 
  void setName(char* buf); 
 
  // this is the midi channel this unit will respond to. 
  byte getInternalMidiChannel(); 
 
  // this is the midi channel this unit will respond to. 
  void setInternalMidiChannel(byte intMidi); 
 
  // this is the midi channel which this unit will send to 
  byte getExternalMidiChannel(); 
 
  // this is the midi channel which this unit will send to 
  void setExternalMidiChannel(byte extMidi); 
 
  // this will return the settings (momentary or switch) of each switch 
  byte getSwitchSettings(); 
 
  void setSwitchSettings(byte value); 

  bool getEvent(byte eventnumber, byte eventData[]); 
 
  byte getEventByNumber(byte number, byte eventData[]); 
 
  // getting name of button <number> 
  char* getButtonName(byte number, char* buf); 
 
  void setButtonName(byte number, char* buf); 
 
  byte getButtonType(byte number); 
 
  void setButtonType(byte number, byte value); 
 
  // getting color of button <number> 
  byte getButtonColor(byte button); 
  void setButtonColor(byte button, byte color); 
 
  // getting the defined buttons 
  byte getButtonUsage(byte button); 
 
  // this will return the program change number 
  byte getPCNumber(); 
 
  // storage interface for writing program data 
  void setPCNumber(byte pcNumber); 
 
  int getProgramSize();

  byte readByte(int addr);
  void writeByte(int addr, byte value);
}; 
 
#endif 
