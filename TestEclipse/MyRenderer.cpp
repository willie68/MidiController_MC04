/*
 * MyRenderer.cpp
 *
 *  Created on: 17.01.2019
 *      Author: w.klaas
 */

#include "MyRenderer.h"

void MyRenderer::render(Menu const& menu) const {
	Serial.print(menu.get_name());
	Serial.print(": ");
	menu.get_current_component()->render(*this);
}

void MyRenderer::render_menu_item(MenuItem const& menu_item) const {
	Serial.println(menu_item.get_name());
}

void MyRenderer::render_back_menu_item(BackMenuItem const& menu_item) const {
	Serial.println(menu_item.get_name());
}

void MyRenderer::render_numeric_menu_item(NumericMenuItem const& menu_item) const {
	Serial.print(menu_item.get_name());
	if (menu_item.has_focus()) {
		Serial.print(": ");
		byte value = (byte) menu_item.get_value();
		Serial.print(value);
	}
	Serial.println();
}

void MyRenderer::render_text_edit_menu_item(TextEditMenuItem const& menu_item) const {
	Serial.print(menu_item.get_name());
	Serial.print(": ");
	Serial.print(menu_item.get_pos());
	Serial.print(": ");
	char* value = menu_item.get_value();
	Serial.println(value);
	Serial.print(menu_item.get_pos());
	Serial.print(" ");
	Serial.print(menu_item.has_focus() ? "f":"x");
	Serial.print(" ");
	Serial.print(menu_item._editing ? "e":"x");
	Serial.println();
}

void MyRenderer::render_menu(Menu const& menu) const {
	Serial.print(menu.get_name());
}
