/**
 * 
 */
package de.mcs.utils;

/**
 * @author w.klaas
 *
 */
public class Logger {

  public static Logger getLogger(Class<?> class1) {
    return new Logger(class1);
  }

  private org.apache.log4j.Logger logger;

  public Logger(Class<?> class1) {
    logger = org.apache.log4j.Logger.getLogger(class1);
  }

  public void debug(String string, Object... args) {
    logger.debug(String.format(string, args));
  }

  public void info(String string, Object... args) {
    logger.info(String.format(string, args));
  }

  public void error(String string, Object... args) {
    logger.error(String.format(string, args));
  }

  public void error(Throwable t, String string, Object... args) {
    logger.error(String.format(string, args), t);
  }
}
