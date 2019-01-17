/*
 * MyRenderer.h
 *
 *  Created on: 17.01.2019
 *      Author: w.klaas
 */

#ifndef MYRENDERER_H_
#define MYRENDERER_H_

#include <MenuSystem.h>
#include "TextEditMenuItem.h"

class CustomNumericMenuItem;

class MyRenderer : public MenuComponentRenderer {
public:
    void render(Menu const& menu) const;
    void render_menu_item(MenuItem const& menu_item) const;
    void render_back_menu_item(BackMenuItem const& menu_item) const;
    void render_numeric_menu_item(NumericMenuItem const& menu_item) const;
    void render_text_edit_menu_item(TextEditMenuItem const& menu_item) const;
    void render_menu(Menu const& menu) const;
};

#endif /* MYRENDERER_H_ */
