void setup() {
  setMidiBaudrate();
}

void loop() {
  changeProgram(0x0C, 0x38);
}

const byte PROGRAM_CHANGE = 0xC0;

void changeProgram(byte channel, byte program) {
  Serial.write(PROGRAM_CHANGE + channel);
  Serial.write(program);  
}

void setMidiBaudrate() {
    Serial.begin(31250);
}
<<<<<<< Updated upstream

=======

>>>>>>> Stashed changes
