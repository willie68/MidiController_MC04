#ifndef _tools_h 
#define _tools_h 

byte hexToByte(char c) {
	if ((c >= '0') && (c <= '9')) {
		return c - '0';
	}
	if ((c >= 'A') && (c <= 'F')) {
		return (c - 'A') + 10;
	}
}

byte nibbleToHex(byte value) {
	byte c = value & 0x0F;
	if ((c >= 0) && (c <= 9)) {
		return c + '0';
	}
	if ((c >= 10) && (c <= 15)) {
		return (c + 'A') - 10;
	}
}

void printHex8(int num) {
	char tmp[3];
	tmp[0] = nibbleToHex(num >> 4);
	tmp[1] = nibbleToHex(num);
	tmp[2] = 0x00;
	Serial.print(tmp);
}

void printHex16(int num) {
	char tmp[5];
	tmp[0] = nibbleToHex(num >> 12);
	tmp[1] = nibbleToHex(num >> 8);
	tmp[2] = nibbleToHex(num >> 4);
	tmp[3] = nibbleToHex(num);
	tmp[4] = 0x00;
	Serial.print(tmp);
}
#endif
