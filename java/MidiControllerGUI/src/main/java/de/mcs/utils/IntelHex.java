/**
 * MCS Media Computer Software
 * Copyright 2018 by Wilfried Klaas
 * Project: SPSEmulator
 * File: IntelHex.java
 * EMail: W.Klaas@gmx.de
 * Created: 25.11.2018 wklaa_000
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
package de.mcs.utils;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

/**
 * @author wklaa_000
 *
 */
public class IntelHex {

	private int byteCount;

	public IntelHex() {
		byteCount = 8;
	}

	public void writeHexStream(OutputStream output, byte[] data) {
		int address = 0;
		int crc = 0;
		try (BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(output))) {
			for (int i = 0; i < data.length; i++) {
				if ((i % byteCount) == 0) {
					if (i > 0) {
						crc = writeCRC(crc, writer);
					}
					crc = 0;
					writer.append(':');
					if ((data.length - i) > byteCount) {
						writer.append(String.format("%02X", byteCount));
						crc += byteCount;
					} else {
						writer.append(String.format("%02X", data.length - i));
						crc += (data.length - i);
					}
					writer.append(String.format("%04X", address));
					crc += address >> 8;
					crc += address & 0x00FF;
					writer.append("00");
				}
				writer.append(String.format("%02X", data[i]));
				crc += data[i];
				address++;
			}
			if (data.length > 0) {
				writeCRC(crc, writer);
			}
			crc = 0;
			writer.append(':');
			writer.append(String.format("%02X", 0));
			crc += byteCount;
			writer.append(String.format("%04X", 0));
			crc += address >> 8;
			crc += address & 0x00FF;
			writer.append("01FF");

		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private int writeCRC(int crc, BufferedWriter writer) throws IOException {
		crc = crc & 0x00FF;
		crc = crc ^ 0x00FF;
		crc++;
		crc = crc & 0x00FF;
		writer.append(String.format("%02X", crc));
		writer.append("\r\n");
		return crc;
	}
}
