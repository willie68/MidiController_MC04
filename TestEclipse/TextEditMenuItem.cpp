/*
 * TextEditMenuItem.cpp
 *
 *  Created on: 17.01.2019
 *      Author: w.klaas
 */

#include "MenuSystem.h"
#include <stdlib.h>
#include "TextEditMenuItem.h"

// *********************************************************
// TextEditMenuItem
// *********************************************************

TextEditMenuItem::TextEditMenuItem(const char* basename, SelectFnPtr select_fn,
                                 char* value, uint8_t size)
: MenuItem(basename, select_fn),
  _value(value),
  _size(size) {

};

Menu* TextEditMenuItem::select() {
    _has_focus = !_has_focus;

    // Only run _select_fn when the user is done editing the value
    if (!_has_focus && _select_fn != nullptr)
        _select_fn(this);
    return nullptr;
}

void TextEditMenuItem::render(MenuComponentRenderer const& renderer) const {
    renderer.render_menu_item(*this);
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

bool TextEditMenuItem::next(bool loop) {
    return true;
}

bool TextEditMenuItem::prev(bool loop) {
    return true;
}
