#include <MenuSystem.h>
#include <LiquidCrystal_I2C.h>
#include "constants.h"
#include "globals.h"
#include "prgstorage.h"

/**
 menu struktur

 global settings
 button mode: [4 Button, 6 Button]
 Expression: [0..2]
 1..2:
 min value: [0..126] Einstellbar über Schweller
 max value: [1..127] Einstellbar über Schweller
 back
 program settings xx
 Name: <12 Zeichen>
 Prg Number: [n.n., <1..128>]
 int Midi Channel: <#1..16>
 ext Midi Channel: <#1..16>
 1..6 Buttons
 Name: <8 Zeichen>
 Type: [Switch, Momentary]
 Color: [0..64]
 Bright: [0..3]
 back
 1..16: Midi Sequenz
 Trigger: [internal, Switch 1..6, Expression 1..2]
 Event: [Start, Stop, Push, Release, Click, Long Click, DblClick, ValueChange, CC]
 1..16 Midi Commands;
 Type: [CC, PC, Note On, Note Off]
 Data1: [0..127]
 Data2: [0..127]
 back
 back
 back
 serial program
 start...
 about
 Version...
 back
 back
 */

// Forward declarations
void on_item1_selected(MenuComponent* p_menu_component);
void on_global_selected(MenuComponent* p_menu_component);
void on_gloBtnMode_selected(MenuComponent* p_menu_component);
void on_gloExp_selected(MenuComponent* p_menu_component);
void on_prgNumber_selected(MenuComponent* p_menu_component);
void on_version_selected(MenuComponent* p_menu_component);
void on_back_selected(MenuComponent* p_menu_component);
void on_nothing_selected(MenuComponent* p_menu_component);
void on_prgIntMidi_selected(MenuComponent* p_menu_component);
void on_prgExtMidi_selected(MenuComponent* p_menu_component);
void on_prgBtn_selected(MenuComponent* p_menu_component);
void on_prgBtnNumber_selected(MenuComponent* p_menu_component);
void on_prgBtnType_selected(MenuComponent* p_menu_component);
void on_prgBtnColor_selected(MenuComponent* p_menu_component);
void on_prgBtnBright_selected(MenuComponent* p_menu_component);
void on_prgMidNumber_selected(MenuComponent* p_menu_component);
void on_prgMidTrigger_selected(MenuComponent* p_menu_component);
void on_prgMidEvent_selected(MenuComponent* p_menu_component);
void on_prgMidCmdNumber_selected(MenuComponent* p_menu_component);
void on_prgMidCmdType_selected(MenuComponent* p_menu_component);
void on_prgMidCmdData1_selected(MenuComponent* p_menu_component);
void on_prgMidCmdData2_selected(MenuComponent* p_menu_component);
void on_serialPrg_selected(MenuComponent* p_menu_component);
void on_prgSequenz_selected(MenuComponent* p_menu_component);
void on_prgMid_selected(MenuComponent* p_menu_component);

Menu muGlobal("Global Settings", &on_global_selected);
NumericMenuItem miGloBtnMode("Buttons", &on_gloBtnMode_selected, 0, 0, 1, 1.0);
NumericMenuItem miGloExpression("Pedals", &on_gloExp_selected, 0, 0, 2, 1.0);

Menu muProgram("Program Settings");
MenuItem miPrgName("Name", &on_item1_selected);

NumericMenuItem miPrgNumber("Number", &on_prgNumber_selected, 0, 0, 127, 1.0);
NumericMenuItem miPrgInternalMidi("int. Midi", &on_prgIntMidi_selected, 0, 0, 127, 1.0);
NumericMenuItem miPrgExternalMidi("ext. Midi", &on_prgExtMidi_selected, 0, 0, 127, 1.0);

Menu muButtons("Buttons", &on_prgBtn_selected);
NumericMenuItem miBtnNumber("Number", &on_prgBtnNumber_selected, 1, 1, 6, 1.0);
MenuItem miBtnName("Name", &on_item1_selected);
NumericMenuItem miBtnType("Type", &on_prgBtnType_selected, 0, 0, 1, 1.0);
NumericMenuItem miBtnColor("Color", &on_prgBtnColor_selected, 0, 0, 63, 1.0);
NumericMenuItem miBtnBright("Bright", &on_prgBtnBright_selected, 0, 0, 3, 1.0);

Menu muMidiSequences("Sequences", &on_prgSequenz_selected);
NumericMenuItem miMidNumber("Number", &on_prgMidNumber_selected, 1, 1, 16, 1.0);
NumericMenuItem miMidTrigger("Trigger", &on_prgMidTrigger_selected, 0, 0, 8, 1.0);
NumericMenuItem miMidEvent("Event", &on_prgMidEvent_selected, 0, 0, 8, 1.0);

Menu muMidCommands("Commands", &on_prgMid_selected);
NumericMenuItem miMidCmdNumber("Number", &on_prgMidCmdNumber_selected, 1, 1, 16, 1.0);
NumericMenuItem miMidCmdType("Type", &on_prgMidCmdType_selected, 0, 0, 3, 1.0);
NumericMenuItem miMidCmdData1("Data1", &on_prgMidCmdData1_selected, 0, 0, 127, 1.0);
NumericMenuItem miMidCmdData2("Data2", &on_prgMidCmdData2_selected, 0, 0, 127, 1.0);

Menu muSerialProgram("Serial Program");
MenuItem miSerStart("Start", &on_serialPrg_selected);

Menu muAbout("About");
MenuItem miVersion("Version...", on_version_selected);

bool endMenu;
int16_t lastMenu = -1;
int16_t menuValue;
byte buttonActive = 1;
bool buttonDirty = false;
byte seqActive = 0;
bool seqDirty = false;
byte cmdActive = 0;
bool cmdDirty = false;

void setSettingsDirty() {
	settingsDirty = true;
}

class MyRenderer: public MenuComponentRenderer {
public:
	void render(Menu const& menu) const {
		lcd.clear();
		lcd.setCursor(0, 0);
		lcd.print(menu.get_name());
		if (&menu == &muProgram) {
			lcd.setCursor(11, 0);
			lcd.print(".  ");
			if (prg < 10) {
				lcd.print('0');
			}
			lcd.print(prg);
		}
		lcd.setCursor(0, 1);
		menu.get_current_component()->render(*this);
	}

	void render_menu_item(MenuItem const& menu_item) const {
		lcd.print(menu_item.get_name());
	}

	void render_back_menu_item(BackMenuItem const& menu_item) const {
		lcd.print(menu_item.get_name());
	}

	void render_numeric_menu_item(NumericMenuItem const& menu_item) const {
		lcd.print(menu_item.get_name());
		lcd.print(": ");
		byte value = (byte) menu_item.get_value();
		if (&menu_item == &miBtnType) {
			if (value == 0) {
				lcd.print(F("Switch"));
			} else {
				lcd.print(F("Momentary"));
			}
		} else if (&menu_item == &miMidTrigger) {
			switch (value) {
			case 0:
				lcd.print(F("internal"));
				break;
			case 1:
				lcd.print(F("Button 1"));
				break;
			case 2:
				lcd.print(F("Button 2"));
				break;
			case 3:
				lcd.print(F("Button 3"));
				break;
			case 4:
				lcd.print(F("Button 4"));
				break;
			case 5:
				lcd.print(F("Button 5"));
				break;
			case 6:
				lcd.print(F("Button 6"));
				break;
			case 7:
				lcd.print(F("Pedal 1"));
				break;
			case 8:
				lcd.print(F("Pedal 2"));
				break;
			}
		} else if (&menu_item == &miMidEvent) {
			switch (value) {
			case 0:
				lcd.print(F("Start"));
				break;
			case 1:
				lcd.print(F("Stop"));
				break;
			case 2:
				lcd.print(F("Push"));
				break;
			case 3:
				lcd.print(F("Release"));
				break;
			case 4:
				lcd.print(F("Click"));
				break;
			case 5:
				lcd.print(F("LongClk"));
				break;
			case 6:
				lcd.print(F("DblClk"));
				break;
			case 7:
				lcd.print(F("ValueChg"));
				break;
			case 8:
				lcd.print(F("CC"));
				break;
			}
		} else if (&menu_item == &miMidCmdType) {
			switch (value) {
			case 0:
				lcd.print(F("CC"));
				break;
			case 1:
				lcd.print(F("PC"));
				break;
			case 2:
				lcd.print(F("Note On"));
				break;
			case 3:
				lcd.print(F("Note Off"));
				break;
			}
		} else if (&menu_item == &miGloBtnMode) {
			if (value == 0) {
				lcd.print(F("4"));
			} else {
				lcd.print(F("6"));
			}
		} else if (&menu_item == &miGloBtnMode) {
			switch (value) {
			case 0:
				lcd.print(F("none"));
				break;
			default:
				lcd.print(value);
				break;
			}
		} else {
			lcd.print(value);
		}
	}
//	void render_textedit_menu_item(TextEditMenuItem const& menu_item) const {
//		lcd.print(menu_item.get_name());
//		lcd.print(": ");
//		byte value = (byte) menu_item.get_value();
//	}

	void render_menu(Menu const& menu) const {
		lcd.print(menu.get_name());
	}
};
MyRenderer my_renderer;

// Menu variables
MenuSystem ms(my_renderer);

BackMenuItem miBack("Back", on_nothing_selected, &ms);
BackMenuItem miRestart("Back", on_back_selected, &ms);

void initMenuSystem() {
	ms.get_root_menu().add_menu(&muGlobal);

	muGlobal.add_item(&miGloBtnMode);
	muGlobal.add_item(&miGloExpression);
	muGlobal.add_item(&miBack);

	ms.get_root_menu().add_menu(&muProgram);
	muProgram.add_item(&miPrgName);
	muProgram.add_item(&miPrgNumber);
	muProgram.add_item(&miPrgInternalMidi);
	muProgram.add_item(&miPrgExternalMidi);

	muProgram.add_menu(&muButtons);
	muButtons.add_item(&miBtnNumber);
	muButtons.add_item(&miBtnName);
	muButtons.add_item(&miBtnType);
	muButtons.add_item(&miBtnColor);
	muButtons.add_item(&miBtnBright);
	muButtons.add_item(&miBack);

	muProgram.add_menu(&muMidiSequences);
	muMidiSequences.add_item(&miMidNumber);
	muMidiSequences.add_item(&miMidTrigger);
	muMidiSequences.add_item(&miMidEvent);
	muMidiSequences.add_menu(&muMidCommands);

	muMidCommands.add_item(&miMidCmdNumber);
	muMidCommands.add_item(&miMidCmdType);
	muMidCommands.add_item(&miMidCmdData1);
	muMidCommands.add_item(&miMidCmdData2);
	muMidCommands.add_item(&miBack);

	muMidiSequences.add_item(&miBack);

	muProgram.add_item(&miBack);

	ms.get_root_menu().add_menu(&muSerialProgram);
	muSerialProgram.add_item(&miSerStart);
	muSerialProgram.add_item(&miBack);

	ms.get_root_menu().add_menu(&muAbout);
	muAbout.add_item(&miVersion);
	muAbout.add_item(&miBack);

	ms.get_root_menu().add_item(&miRestart);
}

void showMenu() {
	endMenu = false;
	miPrgNumber.set_value((float) storage.getPCNumber());
	miPrgInternalMidi.set_value((float) storage.getInternalMidiChannel());
	miPrgExternalMidi.set_value((float) storage.getExternalMidiChannel());
	ms.reset();
	ms.display();
}

bool doMenuWork() {
	menuValue = encoder.getValue();
	if (menuValue > 0) {
		ms.next();
		ms.display();
	} else if (menuValue < 0) {
		ms.prev();
		ms.display();
	}
	ClickEncoder::Button b = encoder.getButton();
	if (b != ClickEncoder::Open) {
		if (b == ClickEncoder::Clicked) {
			ms.select();
			ms.display();
		}
	}
	delay(10);
	return endMenu;
}

// Menu callback function
void on_item1_selected(MenuComponent* p_menu_component) {
	lcd.setCursor(0, 1);
	lcd.print(F("Item1 Selected  "));
	delay(1500); // so we can look the result on the LCD
}

void on_global_selected(MenuComponent* p_menu_component) {
	byte value = storage.getGlobalButtonMode();
	miGloBtnMode.set_value(value);
	value = storage.getGlobalExpressionMode();
	miGloExpression.set_value(value);
}

void on_gloBtnMode_selected(MenuComponent* p_menu_component) {
	byte value = miGloBtnMode.get_value();
	storage.setGlobalButtonMode(value);
}

void on_gloExp_selected(MenuComponent* p_menu_component) {
	byte value = miGloExpression.get_value();
	storage.setGlobalExpressionMode(value);
}

void on_nothing_selected(MenuComponent* p_menu_component) {
}

void on_version_selected(MenuComponent* p_menu_component) {
	lcd.setCursor(0, 0);
	lcd.clear();
	lcd.print(F("WK Music MC 04"));
	lcd.setCursor(0, 1);
	lcd.print(VERSION);
	delay(3000);
}

void on_back_selected(MenuComponent* p_menu_component) {
	endMenu = true;
}

void on_prgNumber_selected(MenuComponent* p_menu_component) {
	byte value = miPrgNumber.get_value();
	storage.setPCNumber(value);
	setSettingsDirty();
}

void on_prgIntMidi_selected(MenuComponent* p_menu_component) {
	byte value = miPrgInternalMidi.get_value();
	storage.setInternalMidiChannel(value);
	setSettingsDirty();
}

void on_prgExtMidi_selected(MenuComponent* p_menu_component) {
	byte value = miPrgExternalMidi.get_value();
	storage.setExternalMidiChannel(value);
	setSettingsDirty();
}

void setButtonSettings() {
	//TODO Hier müssen noch die anderen Menüpunkte mit den Werten des 1. Buttons gefüllt werden
	// set name to text menu
	byte switchTypes = storage.getSwitchSettings();
	switchTypes = switchTypes & (1 << buttonActive);
}

void on_prgBtn_selected(MenuComponent* p_menu_component) {
	miBtnNumber.set_value(1);
	buttonActive = 1;
	setButtonSettings();
	buttonDirty = false;
}

void on_prgBtnNumber_selected(MenuComponent* p_menu_component) {
	lcd.setCursor(0, 1);
	lcd.print(F("Todo prgBtnNumber"));
	delay(1000);
	byte value = (byte) miBtnNumber.get_value();
	if (value != buttonActive) {
		if (buttonDirty) {
			// alten Button speichern.
		}
		//TODO Hier müssen noch die anderen Menüpunkte mit den Werten des neuen Buttons gefüllt werden
	}
	buttonActive = value;
	buttonDirty = false;
}

void on_prgBtnType_selected(MenuComponent* p_menu_component) {
	lcd.setCursor(0, 1);
	lcd.print(F("Todo prgBtnType"));
	delay(1000);
	buttonDirty = true;
	setSettingsDirty();
}

void on_prgBtnColor_selected(MenuComponent* p_menu_component) {
	lcd.setCursor(0, 1);
	lcd.print(F("Todo prgBtnColor"));
	delay(1000);
	buttonDirty = true;
	setSettingsDirty();
}

void on_prgBtnBright_selected(MenuComponent* p_menu_component) {
	lcd.setCursor(0, 1);
	lcd.print(F("Todo prgBtnBright"));
	delay(1000);
	buttonDirty = true;
	setSettingsDirty();
}

void on_prgMidNumber_selected(MenuComponent* p_menu_component) {
	lcd.setCursor(0, 1);
	lcd.print(F("Todo prgMidNumber"));
	delay(1000);
	byte value = (byte) miMidNumber.get_value();
	if (value != seqActive) {
		if (seqDirty) {
			// alte Midisequenz speichern.
		}
		//TODO Hier müssen noch die anderen Menüpunkte mit den Werten der neuen Midisequenz gefüllt werden
	}
	seqActive = value;
	seqDirty = false;
}
void on_prgSequenz_selected(MenuComponent* p_menu_component) {
	seqActive = 0;
}

void on_prgMidTrigger_selected(MenuComponent* p_menu_component) {

	setSettingsDirty();
}

void on_prgMidEvent_selected(MenuComponent* p_menu_component) {

	setSettingsDirty();
}

void on_prgMid_selected(MenuComponent* p_menu_component) {
	cmdActive = 0;
}

void on_prgMidCmdNumber_selected(MenuComponent* p_menu_component) {
	byte value = (byte) miMidCmdNumber.get_value();
	if (value != cmdActive) {
		if (cmdDirty) {
			// alte Midisequenz speichern.
		}
		//TODO Hier müssen noch die anderen Menüpunkte mit den Werten der neuen Midisequenz gefüllt werden
	}
	cmdActive = value;
	cmdDirty = false;
}

void on_prgMidCmdType_selected(MenuComponent* p_menu_component) {
	setSettingsDirty();
}

void on_prgMidCmdData1_selected(MenuComponent* p_menu_component) {
	setSettingsDirty();
}

void on_prgMidCmdData2_selected(MenuComponent* p_menu_component) {
	setSettingsDirty();
}

void on_serialPrg_selected(MenuComponent* p_menu_component) {
	doSerialProgram();
	settingsDirty = false;
}
