/**
 * 
 */
package de.mcs.tools.midicontroller;

/**
 * @author w.klaas
 * @param <T>
 *
 */
public class MidiCommand {

  int channel;

  public <T extends MidiCommand> T setChannel(int channel) {
    this.channel = channel;
    return (T) this;
  }

  public int getChannel() {
    return channel;
  }

  public static MidiCommandPC createPCCommand(int channel) {
    checkChannel(channel);
    return new MidiCommandPC().setChannel(channel);
  }

  private static void checkChannel(int channel) {
    if (channel > 16 || channel < 1) {
      throw new IllegalArgumentException(String.format("channel should be in 1..16, actual value: %d", channel));
    }
  }

  public static MidiCommandCC createCCCommand(int channel) {
    checkChannel(channel);
    return new MidiCommandCC().setChannel(channel);
  }

  public static MidiCommandPAUSE createPAUSECommand() {
    return new MidiCommandPAUSE();
  }

  public static MidiCommand parse(String string) {
    // TODO Auto-generated method stub
    return null;
  }

}
