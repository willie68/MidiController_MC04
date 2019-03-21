/**
 * 
 */
package de.mcs.utils;

/**
 * @author w.klaas
 *
 */
public class YambasHelper {

  private static final String MODULES = "modules/";
  private static final String METAMODELS = "metamodels/";

  /**
   * Stripping the module from the href parameter
   * @param href the href parameter
   * @return the stripped name
   */
  public static String getModulFromHref(String href) {
    String modul = href.substring(href.indexOf(MODULES) + MODULES.length());
    if (modul.indexOf("/") > 0) {
      modul = modul.substring(0, modul.indexOf("/"));
    }
    return modul;
  }

  /**
   * stripping the classid from the href parameter
   * @param href the href parameter
   * @return the stripped class id
   */
  public static String getClassIDFromHref(String href) {
    String modul = href.substring(href.indexOf(METAMODELS) + METAMODELS.length());
    if (modul.indexOf("/") > 0) {
      modul = modul.substring(0, modul.indexOf("/"));
    }
    return modul;
  }

}
