/**
 * MCS Media Computer Software
 * <one line to give the program's name and a brief idea of what it does.>
 * Copyright (C) 2018 by Wilfried Klaas
 * Project: SPSEmulator
 * File: Programs.java
 * EMail: W.Klaas@gmx.de
 * Created: 29.12.2018 wklaa
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
public class Programs {
	private int version;
	private ProgramData[] programs;

	public int getVersion() {
		return version;
	}

	public void setVersion(int version) {
		this.version = version;
	}

	public ProgramData[] getPrograms() {
		return programs;
	}

	public void setPrograms(ProgramData[] programs) {
		this.programs = programs;
	}
}
