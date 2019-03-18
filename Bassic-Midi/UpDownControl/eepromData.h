#include <EEPROM.h>

class Presets {
  public:
    byte presetData[15];
    void initPresetData() {
      for (word x = 0; x < sizeof(presetData); x++) {
        presetData[x] = 0;
      }
      if (EEPROM.read(0) == 0xFF) {
        Serial.println("init EEPROM");
        for (word x = 0; x < 32; x++) {
          word pos = x * 16;
          byte value = 0x02;
          EEPROM.write(pos, value);
          for (byte i = 1; i < 16; i++) {
            switch (i) {
              case 1:
                value = 0xC0;
                break;
              case 2:
                value = 0x70 + x;
                break;
              default:
                value = 0x00;
                break;
            }
            Serial.print(value, HEX);
            Serial.write(' ');
            EEPROM.write(pos + i, value);
          }
          Serial.write("\r\n");
        }
        Serial.write("\r\n");
        for (word i = 0; i < 32 * 16; i++) {
          Serial.print(EEPROM.read(i), HEX);
          Serial.write(' ');
          if ((i % 16) == 15) {
            Serial.write("\r\n");
          }
        }
      }
    }

    word getStartPos(byte preset) {
      return preset * 16;
    }

    byte getPresetByteCount(byte preset) {
      word pos = getStartPos(preset);
      return EEPROM.read(pos);
    }

    void copyPresetData(byte preset) {
      byte count = getPresetByteCount(preset);
      word pos = getStartPos(preset) + 1;
      for (byte i = 0; i < count; i++) {
        EEPROM.get(pos, presetData);
      }
    }
<<<<<<< Updated upstream
};
=======
};
>>>>>>> Stashed changes
