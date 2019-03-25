/**
 * 
 */
package application;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

import de.mcs.tools.midicontroller.data.ButtonData;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.ColorPicker;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;

/**
 * @author w.klaas
 *
 */
public class SwitchControl extends GridPane {

  @FXML
  TextField nameField;

  @FXML
  ColorPicker switchColor;
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
  }

  public void setButtonData(ButtonData data) {
    this.buttonData = data;
    nameField.setText(data.getName());
  }
}
