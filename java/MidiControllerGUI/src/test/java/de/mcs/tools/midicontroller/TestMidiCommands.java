package de.mcs.tools.midicontroller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class TestMidiCommands {

  @BeforeEach
  void setUp() throws Exception {
  }

  @Test
  void test() {
    String midiCommandsString = "CC@1.55.7A,PAUSE.1000,PC@15.23";
    MidiCommands commands = MidiCommands.parse(midiCommandsString);
    List<MidiCommand> list = commands.getCommands();
    assertEquals(3, list.size());
    assertTrue(list.get(0) instanceof MidiCommandCC);
    assertTrue(list.get(1) instanceof MidiCommandPAUSE);
    assertTrue(list.get(2) instanceof MidiCommandPC);
    fail("Not yet implemented");
  }

}
