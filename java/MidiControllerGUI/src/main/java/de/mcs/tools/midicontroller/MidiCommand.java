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

  public static MidiCommand parse(String value) throws Exception {
    MidiCommand midiCommand = null;
    int channel = -1;
    String[] split = value.split("\\.");
    if ((split == null) || (split.length == 0)) {
      throw new IllegalArgumentException(String.format("can't find any command, actual value: %s", value));
    }
    String command = split[0];
    if (command.indexOf("@") > 0) {
      String chnStr = command.substring(command.indexOf("@") + 1);
      channel = Integer.parseInt(chnStr);
    }
    if (command.startsWith("CC")) {
      midiCommand = createCCCommand(channel);
      if (split.length != 3) {
        throw new IllegalArgumentException(String.format("wrong parameter count for CC, actual value: %s", value));
      }
      ((MidiCommandCC) midiCommand).setController(Integer.parseInt(split[1])).setValue(Integer.parseInt(split[2]));
    }
    if (command.startsWith("PC")) {
      midiCommand = createPCCommand(channel);
      if (split.length != 2) {
        throw new IllegalArgumentException(String.format("wrong parameter count for PC, actual value: %s", value));
      }
      ((MidiCommandPC) midiCommand).setProgram(Integer.parseInt(split[1]));
    }
    if (command.startsWith("PAUSE")) {
      midiCommand = createPAUSECommand();
      if (split.length != 2) {
        throw new IllegalArgumentException(String.format("wrong parameter count for PAUSE, actual value: %s", value));
      }
      ((MidiCommandPAUSE) midiCommand).setTime(Integer.parseInt(split[1]));
    }
    if (midiCommand == null) {
      throw new Exception(String.format("Can't parse string \"%s\" to midi command", value));
    }
    return midiCommand;
  }

}
