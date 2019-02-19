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
#include <ClickEncoder.h>
#include <TimerOne.h>
#include<SPIMemory.h>

const byte PIN_ENCODER_A = 4;
const byte PIN_ENCODER_B = 5;
const byte PIN_ENCODER_SW = 6;

const byte ENCODER_STP_NOTCH = 4;

LCD_I2C_AIP31068L lcd(0x3E);
ClickEncoder encoder(PIN_ENCODER_A, PIN_ENCODER_B, PIN_ENCODER_SW, ENCODER_STP_NOTCH);
SPIFlash flash;

void checkEncoder();

void timerIsr() {
	encoder.service();
}

// the setup function runs once when you press reset or power the board
void setup() {
	// set up the LCD's number of columns and rows:
	lcd.begin(16, 2);

	pinMode(LED_BUILTIN, OUTPUT);
	Serial.begin(115200);
	Serial.println("start");

	lcd.print("Hello, Willie!");

	Timer1.initialize(1000);
	Timer1.attachInterrupt(timerIsr);

	flash.begin();
}

// the loop function runs over and over again forever
int16_t value = 0;
int wrtCount = 0;
void loop() {
	checkEncoder();
	Serial.print("counter: ");
	Serial.print(value);
	Serial.print(", fls ");
	Serial.print(flash.readByte(0x0005));
	Serial.print(", wrt: ");
	Serial.print(wrtCount);
	Serial.println();

	if (((millis() / 500) % 2) == 0) {
		digitalWrite(LED_BUILTIN, HIGH);
	} else {
		digitalWrite(LED_BUILTIN, LOW);
	}

	lcd.setCursor(0, 1);
	lcd.print("cnt: ");
	lcd.print(value);
	lcd.print(" ");
	lcd.print(flash.readByte(0x00005));
	ClickEncoder::Button b = encoder.getButton();
	if (b != ClickEncoder::Open) {
		if (b == ClickEncoder::Clicked) {
			wrtCount++;
			flash.eraseSector(0x00005);
			if (!flash.writeByte(0x00005, value)){
				Serial.println("fail");
			}
			value = 0;
		}
	}
	delay(250);
}

int16_t last = -1;
void checkEncoder() {
	value += encoder.getValue();
	value = constrain(value, 0, 127);
	if (value != last) {
		last = value;
	}
}
