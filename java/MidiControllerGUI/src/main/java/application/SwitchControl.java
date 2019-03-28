/**
 * 
 */
package application;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;
import java.util.function.UnaryOperator;

import de.mcs.tools.midicontroller.data.ButtonData;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
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
public class SwitchControl extends GridPane {

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
  URL location;
  @FXML
  ResourceBundle resources;

  private ButtonData buttonData;

  /**
   * 
   */
  public SwitchControl() {
    System.out.println("init SwitchControl");
    FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("switch.fxml"));
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

  public void setButtonData(ButtonData data) {
    this.buttonData = data;
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
  }
}
