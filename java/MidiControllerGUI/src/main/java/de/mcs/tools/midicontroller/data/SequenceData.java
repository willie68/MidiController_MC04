/**
 * MCS Media Computer Software
 * <one line to give the program's name and a brief idea of what it does.>
 * Copyright (C) 2018 by Wilfried Klaas
 * Project: SPSEmulator
 * File: EventData.java
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

import java.util.Arrays;

/**
 * @author wklaa
 *
 */
public class SequenceData {
  public enum EVENTTYPE {
    INTERNAL, BUTTON, EXPRESSION
  };

  public enum EVENT {
    START, STOP, PUSH, RELEASE, CLICK, DOUBLECLICK, LONGCLICK, VALUECHANGE, CC
  }

  private EVENTTYPE type;

  private EVENT event;

  private int value;

  private DataData[] datas;

  @Override
  public String toString() {
    return String.format("Sequence[ type: %s, event: %s, value: %d, datas: %s]", type.name(), event.name(), value,
        Arrays.toString(datas));
  }

  public EVENTTYPE getType() {
    return type;
  }

  public void setType(EVENTTYPE type) {
    this.type = type;
  }

  public EVENT getEvent() {
    return event;
  }

  public void setEvent(EVENT event) {
    this.event = event;
  }

  public DataData[] getDatas() {
    return datas;
  }

  public void setDatas(DataData[] datas) {
    this.datas = datas;
  }

  public int getValue() {
    return value;
  }

  public void setValue(int value) {
    this.value = value;
  }
}
