#define debug
#include <debug.h>
#include <makros.h>
#include <ClickEncoder.h>
#include <TimerOne.h>

const byte PIN_ENCODER_A = 8;
const byte PIN_ENCODER_B = 9;
const byte PIN_ENCODER_SW = 21;

const byte ENCODER_STP_NOTCH = 4;

ClickEncoder encoder(PIN_ENCODER_A, PIN_ENCODER_B, PIN_ENCODER_SW, ENCODER_STP_NOTCH);

// timer interrupt routines
void timerIsr() {
  encoder.service();
}

void setup() {
  // put your setup code here, to run once:
  initDebug();
  while (!Serial);
  dbgOutLn("init debug");
  Timer1.initialize(1000);
  Timer1.attachInterrupt(timerIsr);
}

void loop() {
  dbgOut("test ");
  checkEncoder();
  
  dbgOutLn();
  delay(100);
}
int16_t value = 0;
int16_t last = -1;
void checkEncoder() {
  value += encoder.getValue();
  value = constrain(value, 0, 127);
  if (value != last) {
    last = value;
    dbgOut("Encoder change value:");
    dbgOut(value);
  }
  ClickEncoder::Button b = encoder.getButton();
  if (b != ClickEncoder::Open) {
    dbgOut(", btn open ");
    if (b == ClickEncoder::Clicked) {
      dbgOut(", btn clicked ");
    }
  }
}
