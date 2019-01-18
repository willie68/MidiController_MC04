/*
 * MyRenderer.cpp
 *
 *  Created on: 17.01.2019
 *      Author: w.klaas
 */

#include "MyRenderer.h"

void MyMenuRenderer::render(Menu const& menu) const {
	Serial.print(menu.get_name());
	Serial.print(": ");
	menu.get_current_component()->render(*this);
}

void MyMenuRenderer::render_menu_item(MenuItem const& menu_item) const {
	Serial.println(menu_item.get_name());
}

void MyMenuRenderer::render_back_menu_item(BackMenuItem const& menu_item) const {
	Serial.println(menu_item.get_name());
}

void MyMenuRenderer::render_numeric_menu_item(NumericMenuItem const& menu_item) const {
	Serial.print(menu_item.get_name());
	if (menu_item.has_focus()) {
		Serial.print(": ");
		byte value = (byte) menu_item.get_value();
		Serial.print(value);
	}
	Serial.println();
}

void MyMenuRenderer::render_text_edit_menu_item(TextEditMenuItem const& menu_item) const {
	Serial.print(menu_item.get_name());
	Serial.print(": ");
	Serial.print(menu_item.get_pos());
	Serial.print(": ");
	char* value = menu_item.get_value();
	Serial.println(value);
	Serial.print(menu_item.get_pos());
	Serial.print(" ");
	Serial.print(menu_item.has_focus() ? "f" : "x");
	Serial.print(" ");
	switch (menu_item._editing_state) {
	case FOCUSED:
		Serial.print("f");
		break;
	case SELECTION:
		Serial.print("s");
		break;
	case EDITING:
		Serial.print("e");
		break;
	}
	if (menu_item._pos > 0) {
		char c = menu_item._value[menu_item._pos - 1];
		Serial.print(c);
	}
	Serial.println();
}

void MyMenuRenderer::render_menu(Menu const& menu) const {
	Serial.print(menu.get_name());
}
