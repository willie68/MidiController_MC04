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
	if (!_editing) {
		if (_pos > 0) {
			_editing = !_editing;
		} else {
			_has_focus = !_has_focus;
			if (_has_focus) {
				// start editing
				_pos = 0;
				_editing = false;
			}
			// Only run _select_fn when the user is done editing the value
			if (!_has_focus && _select_fn != nullptr)
				_select_fn(this);
		}
	} else {
		_editing = !_editing;
	}
	return nullptr;
}

bool TextEditMenuItem::next(bool loop) {
	if (_editing) {
		if (_pos > 0) {
			_value[_pos - 1]++;
		}
	} else {
		if (_has_focus) {
			_pos++;
			if (_pos > _size) {
				_pos = _size;
			}
		}
	}
	return true;
}

bool TextEditMenuItem::prev(bool loop) {
	if (_editing) {
		if (_pos > 0) {
			_value[_pos - 1]--;
		}
	} else {
		if (_has_focus) {
			if (_pos == 0) {
			} else {
				_pos--;
			}
		}
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
