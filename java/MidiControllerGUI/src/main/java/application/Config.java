/**
 * MCS Media Computer Software
 * Copyright 2016 by Wilfried Klaas
 * Project: WK_Schematics
 * File: Config.java
 * EMail: W.Klaas@gmx.de
 * Created: 03.12.2016 wklaa_000
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
package application;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author wklaa_000
 *
 */
public class Config {

  @JsonProperty(defaultValue = "E:/temp/temp/backup/schematic/")
  private String backupPath;

  /**
   * @return the backPath
   */
  public String getBackupPath() {
    if (backupPath == null) {
      backupPath = "E:/temp/backup/schematic/";
    }
    return backupPath;
  }

  /**
   * @param backupPath
   *          the backPath to set
   */
  public void setBackPath(String backupPath) {
    this.backupPath = backupPath;
  }

}
