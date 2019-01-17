#include <Arduino.h>
#include <MenuSystem.h>
#include "TextEditMEnuItem.h"

// Forward declarations
void on_item1_selected(MenuComponent* p_menu_component);
void on_gloBtnMode_selected(MenuComponent* p_menu_component);

Menu muGlobal("Global Settings");
NumericMenuItem miGloBtnMode("Buttons", &on_gloBtnMode_selected, 0, 0, 9, 1.0);
NumericMenuItem miGloExpression("Pedals", &on_item1_selected, 0, 0, 2, 1.0);

class MyRenderer: public MenuComponentRenderer {
public:
	void render(Menu const& menu) const {
		Serial.print(menu.get_name());
		Serial.print(": ");
		menu.get_current_component()->render(*this);
	}

	void render_menu_item(MenuItem const& menu_item) const {
		Serial.println(menu_item.get_name());
	}

	void render_back_menu_item(BackMenuItem const& menu_item) const {
		Serial.println(menu_item.get_name());
	}

	void render_numeric_menu_item(NumericMenuItem const& menu_item) const {
		Serial.print(menu_item.get_name());
		if (menu_item.has_focus()) {
			Serial.print(": ");
			byte value = (byte) menu_item.get_value();
			Serial.print(value);
		}
		Serial.println();
	}

	void render_textedit_menu_item(TextEditMenuItem const& menu_item) const {
		Serial.print(menu_item.get_name());
		Serial.print(": ");
		char* value = menu_item.get_value();
		Serial.println(value);
	}

	void render_menu(Menu const& menu) const {
		Serial.print(menu.get_name());
	}
};
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
		case 'a': // Back presed
			ms.back();
			ms.display();
			Serial.println("");
			break;
		case 'd': // Select presed
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
	Serial.println(F("Item1 Selected  "));
	delay(1500); // so we can look the result on the LCD
}

void on_gloBtnMode_selected(MenuComponent* p_menu_component) {
	byte value = miGloBtnMode.get_value();
	Serial.print("BtnMode selected: ");
	Serial.println(value);
}

