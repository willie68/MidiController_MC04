#include <MenuSystem.h>
#include "LCD_I2C_AIP31068L.h"
#include "constants.h"
#include "globals.h"
#include "prgstorage.h"
#include <WS2812.h>
#include "led.ino"

/**
 menu struktur

 global settings
 button mode: [4 Button, 6 Button]
 Expression: [0..2]
 1..2:
 min value: [0..126] Einstellbar �ber Schweller
 max value: [1..127] Einstellbar �ber Schweller
 back
 program settings xx
 Name: <12 Zeichen>
 Prg Number: [n.n., <1..128>]
 int Midi Channel: <#1..16>
 ext Midi Channel: <#1..16>
 1..6 Buttons
 Name: <8 Zeichen>
 Type: [Switch, Momentary]
 Color: [0..63]
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
void on_global_selected(MenuComponent* p_menu_component);
void on_gloBtnMode_selected(MenuComponent* p_menu_component);
void on_gloExp_selected(MenuComponent* p_menu_component);
void on_prgName_selected(MenuComponent* p_menu_component);
void on_prgPCNumber_selected(MenuComponent* p_menu_component);
void on_version_selected(MenuComponent* p_menu_component);
void on_back_selected(MenuComponent* p_menu_component);
void on_nothing_selected(MenuComponent* p_menu_component);
void on_prgIntMidi_selected(MenuComponent* p_menu_component);
void on_prgExtMidi_selected(MenuComponent* p_menu_component);
void on_prgBtn_selected(MenuComponent* p_menu_component);
void on_prgBtnNumber_selected(MenuComponent* p_menu_component);
void on_prgBtnName_selected(MenuComponent* p_menu_component);
void on_prgBtnType_selected(MenuComponent* p_menu_component);
void on_prgBtnColor_selected(MenuComponent* p_menu_component);
void on_prgBtnBright_selected(MenuComponent* p_menu_component);
void on_prgSeqNumber_selected(MenuComponent* p_menu_component);
void on_prgSeqTrigger_selected(MenuComponent* p_menu_component);
void on_prgSeqEvent_selected(MenuComponent* p_menu_component);
void on_prgSeqMidCmdNumber_selected(MenuComponent* p_menu_component);
void on_prgSeqMidCmdType_selected(MenuComponent* p_menu_component);
void on_prgSeqMidCmdChannel_selected(MenuComponent* p_menu_component);
void on_prgSeqMidCmdData1_selected(MenuComponent* p_menu_component);
void on_prgSeqMidCmdData2_selected(MenuComponent* p_menu_component);
void on_serialPrg_selected(MenuComponent* p_menu_component);
void on_prgSequenz_selected(MenuComponent* p_menu_component);
void on_prgSeqMid_selected(MenuComponent* p_menu_component);
void on_prgSeqMidBack_selected(MenuComponent* p_menu_component);

char prgNameBuffer[13] = "            ";
char btnNameBuffer[5] = "    ";
byte menuMidiData[48];

void resetPrgNameBuffer() {
	strcpy(prgNameBuffer, "            ");
}

void resetBtnNameBuffer() {
	strcpy(btnNameBuffer, "    ");
}

Menu muGlobal("Global Settings", &on_global_selected);
NumericMenuItem miGloBtnMode("Buttons", &on_gloBtnMode_selected, 0, 0, 1, 1.0);
NumericMenuItem miGloExpression("Pedals", &on_gloExp_selected, 0, 0, 2, 1.0);

Menu muProgram("Program Settings");
TextEditMenuItem miPrgName("Name", &on_prgName_selected, prgNameBuffer, 12);

NumericMenuItem miPrgPCNumber("Number", &on_prgPCNumber_selected, 0, 0, 127, 1.0);
NumericMenuItem miPrgInternalMidi("int. Midi", &on_prgIntMidi_selected, 0, 0, 127, 1.0);
NumericMenuItem miPrgExternalMidi("ext. Midi", &on_prgExtMidi_selected, 0, 0, 127, 1.0);

Menu muButtons("Buttons", &on_prgBtn_selected);
NumericMenuItem miBtnNumber("Number", &on_prgBtnNumber_selected, 1, 1, 6, 1.0);
TextEditMenuItem miBtnName("Name", &on_prgBtnName_selected, btnNameBuffer, 12);
NumericMenuItem miBtnType("Type", &on_prgBtnType_selected, 0, 0, 1, 1.0);
NumericMenuItem miBtnColor("Color", &on_prgBtnColor_selected, 0, 0, 63, 1.0);
NumericMenuItem miBtnBright("Bright", &on_prgBtnBright_selected, 0, 0, 3, 1.0);

Menu muMidiSequences("Sequences", &on_prgSequenz_selected);
NumericMenuItem miSeqNumber("Number", &on_prgSeqNumber_selected, 1, 1, 16, 1.0);
NumericMenuItem miSeqTrigger("Trig.", &on_prgSeqTrigger_selected, 0, 0, 9, 1.0);
NumericMenuItem miSeqEvent("Event", &on_prgSeqEvent_selected, 0, 0, 8, 1.0);

Menu muSeqMidCommands("Midi Commands", &on_prgSeqMid_selected);
NumericMenuItem miSeqMidCmdNumber("Number", &on_prgSeqMidCmdNumber_selected, 1, 1, 16, 1.0);
NumericMenuItem miSeqMidCmdType("Type", &on_prgSeqMidCmdType_selected, 0, 0, 4, 1.0);
NumericMenuItem miSeqMidCmdChannel("Channel", &on_prgSeqMidCmdChannel_selected, 1, 16, 3, 1.0);
NumericMenuItem miSeqMidCmdData1("Data1", &on_prgSeqMidCmdData1_selected, 0, 0, 127, 1.0);
NumericMenuItem miSeqMidCmdData2("Data2", &on_prgSeqMidCmdData2_selected, 0, 0, 127, 1.0);

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
		lcd.noCursor();
		lcd.setCursor(0, 0);
		if (menu.get_name() == "") {
			lcd.print("Menu");
		} else {
			lcd.print(menu.get_name());
		}
		if (&menu == &muProgram) {
			lcd.setCursor(11, 0);
			lcd.print(".  ");
			if (actualProgram < 10) {
				lcd.print('0');
			}
			lcd.print(actualProgram);
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
		} else if (&menu_item == &miSeqTrigger) {
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
			case 9:
				lcd.print(F("nn"));
				break;
			}
		} else if (&menu_item == &miSeqEvent) {
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
		} else if (&menu_item == &miSeqMidCmdType) {
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
			case 4:
				lcd.print(F("nn"));
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
		} else if ((&menu_item == &miBtnColor) || (&menu_item == &miBtnBright)) {
			lcd.print(value);
			if (menu_item.has_focus()) {
				byte lenght = strlen(menu_item.get_name()) + 2;
				lcd.setCursor(lenght, 1);
				lcd.cursor();
			} else {
				lcd.noCursor();
			}
			byte buttonColor = ((byte) miBtnBright.get_value()) << 6;
			buttonColor += (byte) miBtnColor.get_value();
			cRGB color = getColorValue(buttonColor);
			setColorStatusLEDs(color);
		} else {
			lcd.print(value);
			if (menu_item.has_focus()) {
				byte lenght = strlen(menu_item.get_name()) + 2;
				lcd.setCursor(lenght, 1);
				lcd.cursor();
			} else {
				lcd.noCursor();
			}
		}
	}

	void render_text_edit_menu_item(TextEditMenuItem const& menu_item) const {
		lcd.setCursor(0, 1);
		byte lenght = sizeof(menu_item.get_name());
		lcd.print(menu_item.get_name());
		lenght++;
		lcd.print(":");
		char* value = menu_item.get_value();
		lcd.print(value);
		if ((menu_item._editing_state == EDITING) || (menu_item._editing_state == SELECTION)) {
			if (menu_item.get_pos() > 0) {
				lcd.setCursor(menu_item.get_pos() + 1 + lenght, 1);
				lcd.cursor();
			} else {
				lcd.noCursor();
			}
		}
	}

	void render_menu(Menu const& menu) const {
		lcd.print(menu.get_name());
	}
};

MyRenderer my_renderer;

// Menu variables
MenuSystem ms(my_renderer);

BackMenuItem miBack("Back", on_nothing_selected, &ms);
BackMenuItem miRestart("Back", on_back_selected, &ms);
BackMenuItem miMidBack("Back", on_prgSeqMidBack_selected, &ms);

void initMenuSystem() {
	ms.get_root_menu().add_menu(&muGlobal);

	muGlobal.add_item(&miGloBtnMode);
	muGlobal.add_item(&miGloExpression);
	muGlobal.add_item(&miBack);

	ms.get_root_menu().add_menu(&muProgram);
	muProgram.add_item(&miPrgName);
	muProgram.add_item(&miPrgPCNumber);
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
	muMidiSequences.add_item(&miSeqNumber);
	muMidiSequences.add_item(&miSeqTrigger);
	muMidiSequences.add_item(&miSeqEvent);
	muMidiSequences.add_menu(&muSeqMidCommands);

	muSeqMidCommands.add_item(&miSeqMidCmdNumber);
	muSeqMidCommands.add_item(&miSeqMidCmdType);
	muSeqMidCommands.add_item(&miSeqMidCmdChannel);
	muSeqMidCommands.add_item(&miSeqMidCmdData1);
	muSeqMidCommands.add_item(&miSeqMidCmdData2);
	muSeqMidCommands.add_item(&miMidBack);

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
	miPrgPCNumber.set_value((float) storage.getPCNumber());
	miPrgInternalMidi.set_value((float) storage.getInternalMidiChannel());
	miPrgExternalMidi.set_value((float) storage.getExternalMidiChannel());
	resetPrgNameBuffer();
	resetBtnNameBuffer();
	storage.getName(prgNameBuffer);
	Serial.print("name: ");
	Serial.println(prgNameBuffer);
	miPrgName.set_value(prgNameBuffer);
	miPrgName.set_size(12);
	ms.reset();
	ms.display();
}

bool doMenuWork() {
	menuValue = encoder.getValue();
	if (menuValue > 0) {
		ms.next(true);
		ms.display();
	} else if (menuValue < 0) {
		ms.prev(true);
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
	lcd.print(F(" WK Music MC 04"));
	lcd.setCursor(0, 1);
	lcd.print(VERSION);
	delay(3000);
}

void on_back_selected(MenuComponent* p_menu_component) {
	endMenu = true;
}

void on_prgName_selected(MenuComponent* p_menu_component) {
	Serial.print("Name: ");
	Serial.println(prgNameBuffer);
	storage.setName(prgNameBuffer);
	setSettingsDirty();
}

void on_prgPCNumber_selected(MenuComponent* p_menu_component) {
	byte value = miPrgPCNumber.get_value();
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
	resetBtnNameBuffer();
	byte storageBtnNumber = buttonActive - 1;
	storage.getButtonName(storageBtnNumber, btnNameBuffer);
	miBtnName.set_value(btnNameBuffer);
	miBtnType.set_value((float) storage.getButtonType(storageBtnNumber));
	byte value = storage.getButtonColor(storageBtnNumber);
	value = value & 0x3f;
	miBtnColor.set_value((float) value);
	value = (storage.getButtonColor(storageBtnNumber) & 0xC0) >> 6;
	miBtnBright.set_value((float) value);
}

void on_prgBtn_selected(MenuComponent* p_menu_component) {
	miBtnNumber.set_value(1);
	buttonActive = 1;
	setButtonSettings();
	buttonDirty = false;
}

void on_prgBtnNumber_selected(MenuComponent* p_menu_component) {
	byte value = (byte) miBtnNumber.get_value();
	if (value != buttonActive) {
		if (buttonDirty) {
			// alten Button speichern.
		}
		//TODO Hier müssen noch die anderen Menüpunkte mit den Werten des neuen Buttons gefüllt werden
		buttonActive = value;
		setButtonSettings();
	}
	buttonActive = value;
	buttonDirty = false;
}

void on_prgBtnName_selected(MenuComponent* p_menu_component) {
	storage.setButtonName(buttonActive - 1, btnNameBuffer);
	buttonDirty = true;
	setSettingsDirty();
}

void on_prgBtnType_selected(MenuComponent* p_menu_component) {
	storage.setButtonType(buttonActive - 1, (byte) miBtnType.get_value());
	buttonDirty = true;
	setSettingsDirty();
}

void on_prgBtnColor_selected(MenuComponent* p_menu_component) {
	byte buttonColor = ((byte) miBtnBright.get_value()) << 6;
	buttonColor += (byte) miBtnColor.get_value();
	storage.setButtonColor(buttonActive - 1, buttonColor);
	buttonDirty = true;
	setSettingsDirty();
}

void on_prgBtnBright_selected(MenuComponent* p_menu_component) {
	byte buttonColor = ((byte) miBtnBright.get_value()) << 6;
	buttonColor += (byte) miBtnColor.get_value();
	storage.setButtonColor(buttonActive - 1, buttonColor);
	buttonDirty = true;
	setSettingsDirty();
}

byte eventnumber;
void initEventData() {
	eventnumber = storage.getEventByNumber(seqActive - 1, menuMidiData);
	miSeqTrigger.set_value((float) (eventnumber & 0x0F));
	miSeqEvent.set_value((float) (eventnumber >> 4));
	Serial.print("event:");
	Serial.print(eventnumber, HEX);
	Serial.print(", mid:");
	for (byte i = 0; i < sizeof(menuMidiData); i++) {
		if (i != 0) {
			Serial.print(",");
		}
		Serial.print(menuMidiData[i], HEX);
	}
	Serial.println();
}

void on_prgSequenz_selected(MenuComponent* p_menu_component) {
	seqActive = 1;
	initEventData();
}

void on_prgSeqNumber_selected(MenuComponent* p_menu_component) {
	byte value = (byte) miSeqNumber.get_value();
	if (value != seqActive) {
		if (seqDirty) {
			// alte Midisequenz speichern.
			eventnumber = ((byte) miSeqEvent.get_value()) << 4;
			eventnumber += ((byte) miSeqTrigger.get_value());
			storage.setEventByNumber(seqActive - 1, eventnumber, menuMidiData);
		}
		//TODO Hier müssen noch die anderen Menüpunkte mit den Werten der neuen Midisequenz gefüllt werden

	}
	seqActive = value;
	seqDirty = false;
	initEventData();
}

void on_prgSeqTrigger_selected(MenuComponent* p_menu_component) {
	setSettingsDirty();
}

void on_prgSeqEvent_selected(MenuComponent* p_menu_component) {
	setSettingsDirty();
}

byte type;
byte channel;

void readSeqMid() {
	byte position = (cmdActive - 1) * 3;
	type = menuMidiData[position] & 0xF0;
	channel = menuMidiData[position++] & 0x0F;
	switch (type) {
	case 0xb0:
		// Control Change
		type = 0;
		break;
	case 0xc0:
		// Program change
		type = 1;
		break;
	case 0x90:
		// note on
		type = 2;
		break;
	case 0x80:
		// note off
		type = 3;
		break;
	default:
		type = 4;
		break;
	}
	miSeqMidCmdType.set_value((float) type);
	miSeqMidCmdChannel.set_value((float) channel + 1);
	miSeqMidCmdData1.set_value((float) menuMidiData[position++]);
	miSeqMidCmdData2.set_value((float) menuMidiData[position++]);
}

void on_prgSeqMid_selected(MenuComponent* p_menu_component) {
	cmdActive = 1;
	readSeqMid();
	cmdDirty = false;
}

void setType() {
	byte value = (byte) miSeqMidCmdNumber.get_value() - 1;
	byte position = value * 3;
	type = (byte) miSeqMidCmdType.get_value();
	switch (type) {
	case 0:
		// Control Change
		value = 0xb0;
		break;
	case 1:
		// Program change
		value = 0xc0;
		break;
	case 2:
		// note on
		value = 0x90;
		break;
	case 3:
		// note off
		value = 0x80;
		break;
	default:
		break;
	}
	value += miSeqMidCmdChannel.get_value() - 1;
	menuMidiData[position++] = value;
}

void on_prgSeqMidCmdNumber_selected(MenuComponent* p_menu_component) {
	byte value = (byte) miSeqMidCmdNumber.get_value();
	cmdActive = value;
	readSeqMid();
}

void on_prgSeqMidCmdType_selected(MenuComponent* p_menu_component) {
	setType();
	setSettingsDirty();
	cmdDirty = true;
}

void on_prgSeqMidCmdChannel_selected(MenuComponent* p_menu_component) {
	setType();
	setSettingsDirty();
	cmdDirty = true;
}

void on_prgSeqMidCmdData1_selected(MenuComponent* p_menu_component) {
	byte value = (byte) miSeqMidCmdNumber.get_value() - 1;
	byte position = (value * 3) + 1;
	menuMidiData[position] = (byte) miSeqMidCmdData1.get_value();
	setSettingsDirty();
	cmdDirty = true;
}

void on_prgSeqMidCmdData2_selected(MenuComponent* p_menu_component) {
	byte value = (byte) miSeqMidCmdNumber.get_value() - 1;
	byte position = (value * 3) + 2;
	menuMidiData[position] = (byte) miSeqMidCmdData2.get_value();
	setSettingsDirty();
	cmdDirty = true;
}

void on_prgSeqMidBack_selected(MenuComponent* p_menu_component) {
	if (cmdDirty) {
		seqDirty = true;
		cmdDirty = false;
	}
}

void on_serialPrg_selected(MenuComponent* p_menu_component) {
	doSerialProgram();
	settingsDirty = false;
}
