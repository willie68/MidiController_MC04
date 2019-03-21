/**
 * MCS Media Computer Software
 * <one line to give the program's name and a brief idea of what it does.>
 * Copyright (C) 2018 by Wilfried Klaas
 * Project: SPSEmulator
 * File: ConvertJsonData2Hex.java
 * EMail: W.Klaas@gmx.de
 * Created: 24.12.2018 wklaa
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */
package de.mcs.tools.midicontroller;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;

import de.mcs.tools.midicontroller.data.ButtonData;
import de.mcs.tools.midicontroller.data.ButtonData.TYPE;
import de.mcs.tools.midicontroller.data.DataData;
import de.mcs.tools.midicontroller.data.ProgramData;
import de.mcs.tools.midicontroller.data.Programs;
import de.mcs.tools.midicontroller.data.SequenceData;
import de.mcs.utils.IntelHex;
import de.mcs.utils.JacksonUtils;

/**
 * @author wklaa
 *
 */
public class ConvertJsonData2Hex {

  private static final int NUMBER_OF_SWITCHES = 6;
  private static final int NUMBER_OF_SEQUENZES = 16;
  private static final int NUMBER_OF_MIDICOMMANDS = 16;

  /**
   * @param args
   * @throws Exception
   */
  public static void main(String[] args) throws Exception {
    InputStream source = ClassLoader.getSystemResourceAsStream("programdata.json");
    Programs programs = JacksonUtils.getJsonMapper().readValue(source, Programs.class);
    int saveSize = 0;

    for (ProgramData programData : programs.getPrograms()) {
      try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
        // out.write((byte) programs.getVersion());
        saveSize = out.size();
        System.out.println(programData.toString());
        byte[] name = copyInto(getEmptyByteArray(12), getStringAsByte(programData.getName(), 12));

        out.write(name);

        out.write((byte) programData.getPrgNumber());
        out.write((byte) programData.getInternalMidi());
        out.write((byte) programData.getExternalMidi());

        // bis hier sind es 15 Bytes
        buildSwitches(out, programData);

        // Sequenzes starten bei 70
        buildSequenzes(out, programData);

        // end identifier
        out.write(0xFF);
        out.close();

        File dest = new File(String.format("%s.hex", programData.getName()));
        System.out.printf("preset data written to file: %s\r\n", dest.getName());
        try (BufferedOutputStream output = new BufferedOutputStream(new FileOutputStream(dest))) {
          IntelHex intelHex = new IntelHex();
          intelHex.writeHexStream(output, out.toByteArray());
        }

        System.out.println(String.format("actual size: %d", out.size() - saveSize));
      }
    }
  }

  private static void buildSequenzes(ByteArrayOutputStream out, ProgramData programData) throws Exception, IOException {
    for (int i = 0; i < NUMBER_OF_SEQUENZES; i++) {
      byte[] data = getEmptyByteArray(1 + 3 * NUMBER_OF_MIDICOMMANDS);
      if ((programData.getSequences() != null) && (programData.getSequences().length > i)) {
        int eventTyp = 0;
        SequenceData eventData = programData.getSequences()[i];
        switch (eventData.getType()) {
        case INTERNAL:
          eventTyp = 0;
          break;
        case EXPRESSION:
          eventTyp = 0x70;
          break;
        case BUTTON:
          eventTyp = 0x10 * (eventData.getValue());
          break;
        default:
          break;
        }
        eventTyp = eventTyp | eventData.getEvent().ordinal();
        data[0] = (byte) (eventTyp & 0xFF);
        buildSequenzData(programData, data, eventData);
      }
      out.write(data);
    }
  }

  private static void buildSequenzData(ProgramData programData, byte[] data, SequenceData eventData) throws Exception {
    DataData[] datas = eventData.getDatas();
    int pos = 1;
    if (datas.length > NUMBER_OF_MIDICOMMANDS) {
      throw new Exception("not enough memory.");
    }
    for (DataData dataData : datas) {
      int dataType = dataData.getType().getByte();
      if (DataData.CHANNEL.INTERNAL.equals(dataData.getChannel())) {
        dataType = dataType + programData.getInternalMidi();
      }
      if (DataData.CHANNEL.EXTERNAL.equals(dataData.getChannel())) {
        dataType = dataType + programData.getExternalMidi();
      }
      data[pos++] = (byte) dataType;
      if ((pos + 1) < data.length) { // mindestens 2 noch
                                     // platz
        switch (dataData.getType()) {
        case CC:
        case NOTE_OFF:
        case NOTE_ON:
          data[pos++] = (byte) dataData.getData1();
          data[pos++] = (byte) dataData.getData2();
          break;
        case ALL_NOTE_OFF:
          data[pos++] = 0x78;
          data[pos++] = 0x00;
          break;
        case PC:
          data[pos++] = (byte) dataData.getData1();
          data[pos++] = (byte) 0;
          break;
        default:
          break;
        }
      }
    }
  }

  private static void buildSwitches(ByteArrayOutputStream out, ProgramData programData)
      throws UnsupportedEncodingException, IOException, Exception {
    byte switchSettings = 0x00;
    ButtonData[] buttons = programData.getButtons();
    if ((buttons != null) && (buttons.length > 2)) {
      for (int i = 0; i < NUMBER_OF_SWITCHES; i++) {
        byte[] buttonName = getEmptyByteArray(8);
        ButtonData buttonData;
        if (i < buttons.length) {
          buttonData = buttons[i];
        } else {
          buttonData = new ButtonData();
          buttonData.setName(String.format("btn%d_nn", i));
          buttonData.setColor(0);
          buttonData.setType(TYPE.SWITCH);
        }

        buttonName = copyInto(buttonName, getStringAsByte(buttonData.getName(), 8));

        out.write(buttonName);
        out.write((byte) buttonData.getColor());
        if (ButtonData.TYPE.SWITCH.equals(buttonData.getType())) {
          switchSettings = (byte) (switchSettings | (0x01 << i));
        }
      }
    } else {
      throw new Exception("buttons not correct configured.");
    }
    // 9 pro Button = 54 pos: 54+15 69
    out.write(switchSettings);
  }

  private static byte[] getStringAsByte(String value, int count) throws UnsupportedEncodingException {
    String newValue = value;
    if (newValue.length() > count) {
      newValue = newValue.substring(0, count);
    }
    return newValue.getBytes("US-ASCII");
  }

  private static byte[] getEmptyByteArray(int count) {
    byte[] value = new byte[count];
    for (int i = 0; i < value.length; i++) {
      value[i] = 0;
    }
    return value;
  }

  private static byte[] copyInto(byte[] dest, byte[] source) {
    for (int i = 0; i < source.length; i++) {
      dest[i] = source[i];
    }
    return dest;
  }
}
