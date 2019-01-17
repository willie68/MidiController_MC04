/*
 * TextEditMenuItem.h
 *
 *  Created on: 17.01.2019
 *      Author: w.klaas
 */

#ifndef TEXTEDITMENUITEM_H_
#define TEXTEDITMENUITEM_H_

#include <MenuSystem.h>

//! \brief A MenuItem for editing a short text string
//!
//! TextEditMenuItem is a menu item for automatically
//! edit a short text buffer. It's specialised for use
//! with an rotary encoder. On editing ending the
//! the user-defined Menu::_select_fn callback is called.
class TextEditMenuItem : public MenuItem {
public:
    //! Constructor
    //!
    //! @param name The name of the menu item.
    //! @param select_fn The function to call when this MenuItem is selected.
    //! @param value the buffer with the text to edit.
    //! @param size size of the buffer
	TextEditMenuItem(const char* name, SelectFnPtr select_fn,
                    char* value, uint8_t size);

    char* get_value() const;
    uint8_t get_size() const;

    void set_value(char* value);
    void set_size(byte size);

    virtual void render(MenuComponentRenderer const& renderer) const;

    bool has_focus();

protected:
    virtual bool next(bool loop=false);
    virtual bool prev(bool loop=false);

    virtual Menu* select();

protected:
    char* _value;
    uint8_t _size;
};

#endif /* TEXTEDITMENUITEM_H_ */
