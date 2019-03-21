/**
 * MCS Media Computer Software
 * <one line to give the program's name and a brief idea of what it does.>
 * Copyright (C) 2018 by Wilfried Klaas
 * Project: SPSEmulator
 * File: ProgramData.java
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
public class ProgramData {
  private String name;
  private int prgNumber;
  private int internalMidi;
  private int externalMidi;
  private ButtonData[] buttons;
  private SequenceData[] sequences;

  @Override
  public String toString() {
    return String.format("PRG[name: %s, prgNumber: %d, buttons: %s, sequences: %s]", name, prgNumber,
        Arrays.toString(buttons), Arrays.toString(sequences));
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public int getPrgNumber() {
    return prgNumber;
  }

  public void setPrgNumber(int prgNumber) {
    this.prgNumber = prgNumber;
  }

  public ButtonData[] getButtons() {
    return buttons;
  }

  public void setButtons(ButtonData[] buttons) {
    this.buttons = buttons;
  }

  public SequenceData[] getSequences() {
    return sequences;
  }

  public void setSequences(SequenceData[] sequences) {
    this.sequences = sequences;
  }

  /**
   * @return the internalMidi
   */
  public int getInternalMidi() {
    return internalMidi;
  }

  /**
   * @param internalMidi
   *          the internalMidi to set
   */
  public void setInternalMidi(int internalMidi) {
    this.internalMidi = internalMidi;
  }

  /**
   * @return the externalMidi
   */
  public int getExternalMidi() {
    return externalMidi;
  }

  /**
   * @param externalMidi
   *          the externalMidi to set
   */
  public void setExternalMidi(int externalMidi) {
    this.externalMidi = externalMidi;
  }

}
