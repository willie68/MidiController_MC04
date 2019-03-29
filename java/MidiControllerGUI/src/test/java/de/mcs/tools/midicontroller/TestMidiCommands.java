package de.mcs.tools.midicontroller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class TestMidiCommands {

  @BeforeEach
  void setUp() throws Exception {
  }

  @Test
  void test() throws Exception {
    String midiCommandsString = "CC@1.55.72,PAUSE.1000,PC@15.23";
    MidiCommands commands = MidiCommands.parse(midiCommandsString);
    List<MidiCommand> list = commands.getCommands();
    assertEquals(3, list.size());
    assertTrue(list.get(0) instanceof MidiCommandCC);

    MidiCommandCC cc = (MidiCommandCC) list.get(0);
    assertEquals(1, cc.getChannel());
    assertEquals(55, cc.getController());
    assertEquals(72, cc.getValue());

    assertTrue(list.get(1) instanceof MidiCommandPAUSE);

    MidiCommandPAUSE pause = (MidiCommandPAUSE) list.get(1);
    assertEquals(1000, pause.getTime());

    assertTrue(list.get(2) instanceof MidiCommandPC);

    MidiCommandPC pc = (MidiCommandPC) list.get(2);
    assertEquals(15, pc.getChannel());
    assertEquals(23, pc.getProgram());
  }

}
