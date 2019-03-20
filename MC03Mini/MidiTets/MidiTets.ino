#define debug
#include <debug.h>
#include <makros.h>

void setup() {
  initDebug();
  while(!Serial) ;

  dbgOutLn("start");
  Serial1.begin(31250);
  while(!Serial1) ;
}

byte prg = 0;
void loop() {
  // put your main code here, to run repeatedly:
  dbgOut(prg);
  dbgOutLn();
  Serial1.write(0xC0);
  Serial1.write(prg++);
  Serial1.flush();
  prg = prg & 0x7F;

  delay(1000);
}
