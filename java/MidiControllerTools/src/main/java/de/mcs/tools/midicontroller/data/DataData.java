/**
 * MCS Media Computer Software
 * <one line to give the program's name and a brief idea of what it does.>
 * Copyright (C) 2018 by Wilfried Klaas
 * Project: SPSEmulator
 * File: DataData.java
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
package de.mcs.tools.midicontroller.data;

/**
 * @author wklaa
 *
 */
public class DataData {

  public enum CHANNEL {
    INTERNAL, EXTERNAL
  }

  public enum TYPE {
    CC {
      public byte getByte() {
        return (byte) 0xB0;
      }
    },
    PC {
      public byte getByte() {
        return (byte) 0xC0;
      }
    },
    PC_NEXT {
      public byte getByte() {
        return (byte) 0xD0;
      }
    },
    PC_PREV {
      public byte getByte() {
        return (byte) 0xD1;
      }
    },
    NOTE_ON {
      public byte getByte() {
        return (byte) 0x90;
      }
    },
    NOTE_OFF {
      public byte getByte() {
        return (byte) 0x80;
      }
    },
    ALL_NOTE_OFF {
      public byte getByte() {
        return (byte) 0xB0;
      }
    };

    public byte getByte() {
      return 0;
    }
  }

  private CHANNEL channel;
  private TYPE type;
  private int data1;
  private int data2;

  public CHANNEL getChannel() {
    return channel;
  }

  public void setChannel(CHANNEL channel) {
    this.channel = channel;
  }

  public TYPE getType() {
    return type;
  }

  public void setType(TYPE type) {
    this.type = type;
  }

  public int getData1() {
    return data1;
  }

  public void setData1(int data1) {
    this.data1 = data1;
  }

  public int getData2() {
    return data2;
  }

  public void setData2(int data2) {
    this.data2 = data2;
  }

  @Override
  public String toString() {
    return String.format("data[channel: %s, type: %s, data1: %d, data2: %d]", channel.name(), type.name(), data1,
        data2);
  }
}
