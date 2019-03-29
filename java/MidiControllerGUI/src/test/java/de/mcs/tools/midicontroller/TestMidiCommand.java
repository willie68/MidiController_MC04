/**
 * 
 */
package de.mcs.tools.midicontroller;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * @author w.klaas
 *
 */
class TestMidiCommand {

  private static final int PROGRAM = 102;
  private static final int CHANNEL = 1;
  private static final String PROGRAM_STRING = "PC@1.102";
  private static final int CONTROL_VALUE = 76;
  private static final int CONTROL = 22;
  private static final String CONTROL_STRING = "CC@1.22.76";
  private static final int PAUSE_TIME = 5000;
  private static final String PAUSE_STRING = "PAUSE.5000";

  /**
   * @throws java.lang.Exception
   */
  @BeforeEach
  void setUp() throws Exception {
  }

  @Test
  void testChannelgt16() {
    Assertions.assertThrows(IllegalArgumentException.class, () -> MidiCommand.createPCCommand(17));
    Assertions.assertThrows(IllegalArgumentException.class, () -> MidiCommand.createPCCommand(0));
  }

  @Test
  void testPCOK() {
    MidiCommandPC midiCommand = MidiCommand.createPCCommand(CHANNEL).setProgram(PROGRAM);
    assertEquals(PROGRAM, midiCommand.getProgram());
    assertEquals(CHANNEL, midiCommand.getChannel());
    assertEquals(PROGRAM_STRING, midiCommand.toString());
  }

  @Test
  void testPCRange() {
    Assertions.assertThrows(IllegalArgumentException.class, () -> MidiCommand.createPCCommand(CHANNEL).setProgram(128));
    Assertions.assertThrows(IllegalArgumentException.class, () -> MidiCommand.createPCCommand(CHANNEL).setProgram(-1));
  }

  @Test
  void testCCOK() {
    MidiCommandCC midiCommand = MidiCommand.createCCCommand(CHANNEL).setController(CONTROL).setValue(CONTROL_VALUE);
    assertEquals(CONTROL, midiCommand.getController());
    assertEquals(CHANNEL, midiCommand.getChannel());
    assertEquals(CONTROL_VALUE, midiCommand.getValue());
    assertEquals(CONTROL_STRING, midiCommand.toString());
  }

  @Test
  void testCCControlRange() {
    Assertions.assertThrows(IllegalArgumentException.class,
        () -> MidiCommand.createCCCommand(CHANNEL).setController(128));
    Assertions.assertThrows(IllegalArgumentException.class,
        () -> MidiCommand.createCCCommand(CHANNEL).setController(-1));
  }

  @Test
  void testCCValueRange() {
    Assertions.assertThrows(IllegalArgumentException.class,
        () -> MidiCommand.createCCCommand(CHANNEL).setController(CONTROL).setValue(-1));
    Assertions.assertThrows(IllegalArgumentException.class,
        () -> MidiCommand.createCCCommand(CHANNEL).setController(CONTROL).setValue(128));
  }

  @Test
  void testPAUSEOK() {
    MidiCommandPAUSE midiCommand = MidiCommand.createPAUSECommand().setTime(PAUSE_TIME);
    assertEquals(PAUSE_TIME, midiCommand.getTime());
    assertEquals(PAUSE_STRING, midiCommand.toString());
  }

  @Test
  void testPAUSERange() {
    Assertions.assertThrows(IllegalArgumentException.class, () -> MidiCommand.createPAUSECommand().setTime(10001));
    Assertions.assertThrows(IllegalArgumentException.class, () -> MidiCommand.createPAUSECommand().setTime(-1));
  }

}
