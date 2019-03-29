/**
 * 
 */
package application;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;
import java.util.function.UnaryOperator;

import de.mcs.tools.midicontroller.MidiCommands;
import de.mcs.tools.midicontroller.data.ButtonData;
import de.mcs.tools.midicontroller.data.DataData;
import de.mcs.tools.midicontroller.data.SequenceData;
import de.mcs.utils.StringUtils;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.Button;
import javafx.scene.control.ColorPicker;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;
import javafx.scene.control.TextFormatter;
import javafx.scene.control.TextFormatter.Change;
import javafx.scene.layout.GridPane;
import javafx.scene.paint.Color;

/**
 * @author w.klaas
 *
 */
public class PedalControl extends GridPane {

  private static final int NAME_FIELD_LENGTH = 8;

  @FXML
  TextField nameField;

  @FXML
  ColorPicker switchColor;

  @FXML
  RadioButton typeMomentary;

  @FXML
  RadioButton typeToggle;

  @FXML
  TextField pushText;

  @FXML
  TextField releaseText;

  @FXML
  TextField clickText;

  @FXML
  TextField dblclickText;

  @FXML
  TextField longText;

  @FXML
  Button btnPushSwitch;

  @FXML
  Button btnReleaseSwitch;

  @FXML
  Button btnClickSwitch;

  @FXML
  Button btnDblClickSwitch;

  @FXML
  Button btnLongSwitch;

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
    System.out.println("initialise SwitchControl");
    switchColor.getStyleClass().add("button");

    UnaryOperator<Change> modifyChange = c -> {
      if (c.isContentChange()) {
        int newLength = c.getControlNewText().length();
        if (newLength > NAME_FIELD_LENGTH) {
          // replace the input text with the last len chars
          String tail = c.getControlNewText().substring(0, NAME_FIELD_LENGTH);
          c.setText(tail);
        }
      }
      return c;
    };
    nameField.setTextFormatter(new TextFormatter(modifyChange));
  }

  public void setResourceBundle(ResourceBundle bundle) {
    this.resources = bundle;
    initComponent();
  }

  public void initComponent() {
    nameField.setPromptText(resources.getString("switch.text.prompt.name"));
  }

  public void setButtonData(ButtonData data, List<SequenceData> sequenceDatas, int intChn, int extChn) {
    this.buttonData = data;
    this.sequenceDatas = sequenceDatas;
    nameField.setText(data.getName());
    if (buttonData.getType().equals(ButtonData.TYPE.MOMENTARY)) {
      typeMomentary.setSelected(true);
    } else {
      typeToggle.setSelected(true);
    }

    int color = data.getColor();
    int red = color & 0b00000011;
    int green = color & 0b00001100;
    int blue = color & 0b00110000;
    // System.out.printf("color:%d, r: %d, g: %d, b:%d%n", color, red, green, blue);
    Color value = new Color((double) (red / 3.0), (double) (green / 12.0), (double) (blue / 48.0), 1.0);
    switchColor.setValue(value);

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
      case CLICK:
        clickText.setText(dataString);
        break;
      case DOUBLECLICK:
        dblclickText.setText(dataString);
        break;
      case LONGCLICK:
        longText.setText(dataString);
        break;
      case PUSH:
        pushText.setText(dataString);
        break;
      case RELEASE:
        releaseText.setText(dataString);
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
      if (button.getId().equals(btnClickSwitch.getId())) {
        midiString = clickText.getText();
      } else if (button.getId().equals(btnDblClickSwitch.getId())) {
        midiString = dblclickText.getText();
      } else if (button.getId().equals(btnLongSwitch.getId())) {
        midiString = longText.getText();
      } else if (button.getId().equals(btnPushSwitch.getId())) {
        midiString = pushText.getText();
      } else if (button.getId().equals(btnReleaseSwitch.getId())) {
        midiString = releaseText.getText();
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
