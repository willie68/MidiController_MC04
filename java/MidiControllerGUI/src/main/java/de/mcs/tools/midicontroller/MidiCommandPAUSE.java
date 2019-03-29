/**
 * 
 */
package de.mcs.tools.midicontroller;

/**
 * @author w.klaas
 *
 */
public class MidiCommandPAUSE extends MidiCommand {

  private int time;

  @Override
  public String toString() {
    return String.format("PAUSE.%d", getTime());
  }

  /**
   * @return the time
   */
  public int getTime() {
    return time;
  }

  /**
   * @param time
   *          the time to set
   * @return
   */
  public MidiCommandPAUSE setTime(int time) {
    if (time > 10000 || time < 0) {
      throw new IllegalArgumentException(String.format("time should be in 0..10000, actual value: %d", time));
    }
    this.time = time;
    return this;
  }

}
