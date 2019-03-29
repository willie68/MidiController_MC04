/**
 * 
 */
package application;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

import de.mcs.tools.midicontroller.MidiCommands;
import de.mcs.tools.midicontroller.data.ButtonData;
import de.mcs.tools.midicontroller.data.DataData;
import de.mcs.tools.midicontroller.data.SequenceData;
import de.mcs.utils.StringUtils;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;

/**
 * @author w.klaas
 *
 */
public class PedalControl extends GridPane {

  private static final int NAME_FIELD_LENGTH = 8;

  @FXML
  TextField changeText;

  @FXML
  Button btnChangeSwitch;

  @FXML
  URL location;

  @FXML
  ResourceBundle resources;

  private ButtonData buttonData;

  private List<SequenceData> sequenceDatas;

  /**
   * 
   */
  public PedalControl() {
    System.out.println("init PedalControl");
    FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("pedal.fxml"));
    fxmlLoader.setRoot(this);
    fxmlLoader.setController(this);

    try {
      fxmlLoader.load();
    } catch (IOException exception) {
      throw new RuntimeException(exception);
    }
  }

  @FXML
  public void initialize() {
    System.out.println("initialise PedalControl");
  }

  public void setResourceBundle(ResourceBundle bundle) {
    this.resources = bundle;
    initComponent();
  }

  public void initComponent() {
  }

  public void setButtonData(ButtonData data, List<SequenceData> sequenceDatas, int intChn, int extChn) {
    this.buttonData = data;
    this.sequenceDatas = sequenceDatas;

    for (SequenceData sequenceData : sequenceDatas) {
      DataData[] datas = sequenceData.getDatas();
      List<String> dataList = new ArrayList<>();
      for (DataData dataData : datas) {
        String humanString = dataData.toHumanString(intChn, extChn);
        if (humanString != null) {
          dataList.add(humanString);
        }
      }
      String dataString = StringUtils.listToCSVString(dataList);
      switch (sequenceData.getEvent()) {
      case VALUECHANGE:
        changeText.setText(dataString);
        break;
      default:
        break;
      }
    }
  }

  @FXML
  public void doBtnMidi(ActionEvent event) {
    if (event.getSource() instanceof Button) {
      String midiString = "";
      Button button = (Button) event.getSource();
      if (button.getId().equals(btnChangeSwitch.getId())) {
        midiString = changeText.getText();
      }
      MidiCommands commands = null;
      try {
        if ((midiString != null) && !midiString.isEmpty()) {
          commands = MidiCommands.parse(midiString);
        }
      } catch (Exception e) {
        e.printStackTrace();
      }

      System.out.println(event.toString());
    }
  }
}
