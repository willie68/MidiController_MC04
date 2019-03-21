/**
 * MCS Media Computer Software
 * Copyright 2016 by Wilfried Klaas
 * Project: WK_Schematics
 * File: Session.java
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

import java.io.File;
import java.io.IOException;
import java.util.ResourceBundle;

import de.mcs.utils.JacksonUtils;
import de.mcs.utils.Logger;

/**
 * @author wklaa_000
 *
 */
public class Session {
  // private static final String SYSTEM = "TEST";
  // private static final String API_KEY = "3328333824571259182";
  // private static final String BASEURL = "https://dev.mobility.easy.software/yambas/rest/apps/SchematicWorld";
  // private static final String SYSTEM = "LIVE";
  // private static final String API_KEY = "2462072624678912323";
  // private static final String BASEURL = "https://prod.mobility.easy.software/yambas/rest/apps/SchematicWorld";

  private Logger log = Logger.getLogger(this.getClass());
  private ResourceBundle resourceBundle;

  private Main main;
  private Config config;

  public void setResourceBundle(ResourceBundle bundle) {
    this.resourceBundle = bundle;
  }

  public ResourceBundle getResourceBundle() {
    return resourceBundle;
  }

  public void initConfig() {
    File configurationFile = new File(System.getProperty("user.home"), "MCSSchematic/config.yml");
    log.debug("config file: %s", configurationFile.getAbsolutePath());
    configurationFile.getParentFile().mkdirs();
    if (configurationFile.exists()) {
      try {
        config = JacksonUtils.getYmlMapper().readValue(configurationFile, Config.class);
      } catch (IOException e) {
        log.error(e, "error loading configuration");
      }
    } else {
      config = new Config();
    }
  }

  public void saveConfig() {
    File configurationFile = new File(System.getProperty("user.home"), "MCSSchematic/config.yml");
    try {
      JacksonUtils.getYmlMapper().writeValue(configurationFile, config);
    } catch (IOException e) {
      log.error(e, "error loading configuration");
    }
  }

  public Config getConfig() {
    return config;
  }

  public void exit() {
    main.exit();
  }

  public void setMain(Main main) {
    this.main = main;
  }

  public void toMainScene() {
    main.toMainScene();
  }

}
