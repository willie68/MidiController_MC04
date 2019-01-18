/*
 * TextEditMenuItem.cpp
 *
 *  Created on: 17.01.2019
 *      Author: w.klaas
 */

#include "MenuSystem.h"
#include <stdlib.h>
#include "TextEditMenuItem.h"
#include "MyRenderer.h"

// *********************************************************
// TextEditMenuItem
// *********************************************************

TextEditMenuItem::TextEditMenuItem(const char* basename, SelectFnPtr select_fn, char* value, uint8_t size) :
		MenuItem(basename, select_fn), _value(value), _size(size) {
}

Menu* TextEditMenuItem::select() {
	if (!_has_focus) {
		_editing_state = FOCUSED;
		_has_focus = true;
	} else {
		switch (_editing_state) {
		case FOCUSED:
			if (_pos == 0) {
				_has_focus = false;
			} else {
				_editing_state = SELECTION;
			}
			break;
		case SELECTION:
			if (_pos == 0) {
				_has_focus = false;
				_editing_state = FOCUSED;
			} else {
				_editing_state = EDITING;
			}
			break;
		case EDITING:
			_editing_state = FOCUSED;
			break;
		}
	}
	if (!_has_focus && _select_fn != nullptr)
		_select_fn(this);
	return nullptr;
}

char TextEditMenuItem::getNextValidChar(char value) {
	char myValue = value;
	if (myValue < 32) {
		myValue = 32;
	}
	if (myValue >= 126) {
		return 126;
	}
	return myValue + 1;
}

char TextEditMenuItem::getPrevValidChar(char value) {
	char myValue = value;
	if (myValue < 32) {
		myValue = 32;
	}
	if (myValue <= 32) {
		return 32;
	}
	return myValue - 1;
}

bool TextEditMenuItem::next(bool loop) {
	switch (_editing_state) {
	case FOCUSED:
		_editing_state = SELECTION;
	case SELECTION:
		_pos++;
		if (_pos > _size) {
			_pos = _size;
		}
		break;
	case EDITING:
		if (_pos > 0) {
			_value[_pos - 1] = getNextValidChar(_value[_pos - 1]);
		}
		break;
	}
	return true;
}

bool TextEditMenuItem::prev(bool loop) {
	switch (_editing_state) {
	case SELECTION:
		if (_pos == 0) {
		} else {
			_pos--;
		}
		break;
	case EDITING:
		if (_pos > 0) {
			_value[_pos - 1] = getPrevValidChar(_value[_pos - 1]);

		}
		break;
	}
	return true;
}

void TextEditMenuItem::render(MenuComponentRenderer const& renderer) const {
	MyRenderer const& my_renderer = static_cast<MyRenderer const&>(renderer);
	my_renderer.render_text_edit_menu_item(*this);
}

char* TextEditMenuItem::get_value() const {
	return _value;
}

uint8_t TextEditMenuItem::get_size() const {
	return _size;
}

void TextEditMenuItem::set_value(char* value) {
	_value = value;
}

void TextEditMenuItem::set_size(uint8_t value) {
	_size = value;
}

uint8_t TextEditMenuItem::get_pos() const {
	return _pos;
}
