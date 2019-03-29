/**
 * 
 */
package de.mcs.tools.midicontroller;

/**
 * @author w.klaas
 *
 */
public class MidiCommandPC extends MidiCommand {

  private int program;

  @Override
  public String toString() {
    return String.format("PC@%d.%d", getChannel(), getProgram());
  }

  public MidiCommandPC setProgram(int program) {
    if (program > 127 || program < 0) {
      throw new IllegalArgumentException(String.format("program should be in 0..127, actual value: %d", program));
    }
    this.program = program;
    return this;
  }

  public int getProgram() {
    return program;
  }
}
