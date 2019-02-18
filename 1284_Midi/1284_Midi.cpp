/*
  Blink

  Turns an LED on for one second, then off for one second, repeatedly.

  Most Arduinos have an on-board LED you can control. On the UNO, MEGA and ZERO
  it is attached to digital pin 13, on MKR1000 on pin 6. LED_BUILTIN is set to
  the correct LED pin independent of which board is used.
  If you want to know what pin the on-board LED is connected to on your Arduino
  model, check the Technical Specs of your board at:
  https://www.arduino.cc/en/Main/Products

  modified 8 May 2014
  by Scott Fitzgerald
  modified 2 Sep 2016
  by Arturo Guadalupi
  modified 8 Sep 2016
  by Colby Newman

  This example code is in the public domain.

  http://www.arduino.cc/en/Tutorial/Blink
*/
#include <Arduino.h>
#include "LCD_I2C_AIP31068L.h"
LCD_I2C_AIP31068L lcd(0x7C);

// the setup function runs once when you press reset or power the board
void setup() {
  // set up the LCD's number of columns and rows:
  lcd.begin(16, 2);

  pinMode(LED_BUILTIN, OUTPUT);
  Serial.begin(9600);
  Serial.println("start");

  lcd.print("Hello, Willie!");
}

// the loop function runs over and over again forever
int i = 0;
void loop() {
  i++;
  Serial.print("counter: ");
  Serial.print(i);
  Serial.println();
  digitalWrite(LED_BUILTIN, HIGH);   // turn the LED on (HIGH is the voltage level)
  delay(500);                       // wait for a second
  digitalWrite(LED_BUILTIN, LOW);    // turn the LED off by making the voltage LOW
  delay(500);                       // wait for a second

  lcd.setCursor(0, 1);
  lcd.print("counter ");
  lcd.print(i);
}
