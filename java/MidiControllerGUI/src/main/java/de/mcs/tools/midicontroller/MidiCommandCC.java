/**
 * 
 */
package de.mcs.tools.midicontroller;

/**
 * @author w.klaas
 *
 */
public class MidiCommandCC extends MidiCommand {

  private int controller;
  private int value;

  @Override
  public String toString() {
    return String.format("CC@%d.%d.%d", getChannel(), getController(), getValue());
  }

  public MidiCommandCC setValue(int value) {
    if (value > 127 || value < 0) {
      throw new IllegalArgumentException(String.format("value should be in 0..127, actual value: %d", value));
    }
    this.value = value;
    return this;
  }

  public int getValue() {
    return value;
  }

  /**
   * @return the controller
   */
  public int getController() {
    return controller;
  }

  /**
   * @param controller
   *          the controller to set
   * @return
   */
  public MidiCommandCC setController(int controller) {
    if (controller > 127 || controller < 0) {
      throw new IllegalArgumentException(String.format("controller should be in 0..127, actual value: %d", controller));
    }
    this.controller = controller;
    return this;
  }

}
