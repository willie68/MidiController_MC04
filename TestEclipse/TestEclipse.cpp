#include <Arduino.h>
#include <MenuSystem.h>
#include "TextEditMenuItem.h"
#include "MyRenderer.h"

// Forward declarations
void on_item1_selected(MenuComponent* p_menu_component);
void on_gloBtnMode_selected(MenuComponent* p_menu_component);

char buffer[13] = "HausMeister1";
Menu muGlobal("Global Settings");
NumericMenuItem miGloBtnMode("Buttons", &on_gloBtnMode_selected, 0, 0, 9, 1.0);
TextEditMenuItem miGloExpression("Pedals", &on_item1_selected, buffer, 12);

MyRenderer my_renderer;

// Menu variables
MenuSystem ms(my_renderer);

void display_help() {
	Serial.println("***************");
	Serial.println("w: go to previus item (up)");
	Serial.println("s: go to next item (down)");
	Serial.println("a: go back (right)");
	Serial.println("d: select \"selected\" item");
	Serial.println("?: print this help");
	Serial.println("h: print this help");
	Serial.println("***************");
}

void serial_handler() {
	char inChar;
	if ((inChar = Serial.read()) > 0) {
		Serial.println("\033c");
		switch (inChar) {
		case 'w': // Previus item
			ms.prev(true);
			ms.display();
			Serial.println("");
			break;
		case 's': // Next item
			ms.next(true);
			ms.display();
			Serial.println("");
			break;
		case 'a': // Back pressed
			ms.back();
			ms.display();
			Serial.println("");
			break;
		case 'd': // Select pressed
			ms.select();
			ms.display();
			Serial.println("");
			break;
		case '?':
		case 'h': // Display help
			ms.display();
			Serial.println("");
			break;
		default:
			break;
		}
	}
}

void setup() {
	Serial.begin(115200);
	Serial.println("start");
	pinMode(LED_BUILTIN, OUTPUT);

	ms.get_root_menu().add_menu(&muGlobal);
	muGlobal.add_item(&miGloBtnMode);
	muGlobal.add_item(&miGloExpression);

	display_help();
	ms.display();

}

void loop() {
	serial_handler();
}

// Menu callback function
void on_item1_selected(MenuComponent* p_menu_component) {
	Serial.print(F("s:"));
	Serial.println(miGloExpression.get_value());
}

void on_gloBtnMode_selected(MenuComponent* p_menu_component) {
	byte value = miGloBtnMode.get_value();
	Serial.print("BtnMode selected: ");
	Serial.println(value);
}

