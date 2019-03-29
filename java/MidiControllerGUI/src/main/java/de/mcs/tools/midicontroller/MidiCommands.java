/**
 * 
 */
package de.mcs.tools.midicontroller;

import java.util.ArrayList;
import java.util.List;

import de.mcs.utils.StringUtils;

/**
 * @author w.klaas
 *
 */
public class MidiCommands {

  List<MidiCommand> commandList = new ArrayList<>();

  public static MidiCommands parse(String midiString) {
    MidiCommands midiCommands = new MidiCommands();
    String[] csvStringToArray = StringUtils.csvStringToArray(midiString);
    for (String string : csvStringToArray) {
      midiCommands.addMidiCommand(MidiCommand.parse(string));
    }
    return null;
  }

  private void addMidiCommand(MidiCommand command) {
    commandList.add(command);
  }

  @Override
  public String toString() {
    List<String> commandStrings = new ArrayList<>();
    commandList.forEach(c -> commandStrings.add(c.toString()));
    return StringUtils.listToCSVString(commandStrings);
  }

  public List<MidiCommand> getCommands() {
    return commandList;
  }
}
