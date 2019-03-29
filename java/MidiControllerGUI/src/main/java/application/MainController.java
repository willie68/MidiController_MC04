package application;

import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

import de.mcs.tools.midicontroller.data.ButtonData;
import de.mcs.tools.midicontroller.data.ProgramData;
import de.mcs.tools.midicontroller.data.SequenceData;
import de.mcs.utils.Logger;
import javafx.concurrent.Task;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.input.DragEvent;
import javafx.scene.layout.VBox;

public class MainController {

  Logger log = Logger.getLogger(this.getClass());

  private Session session;

  @FXML
  Button exit;

  @FXML
  TextArea descriptionField;

  @FXML
  Label statustext;

  @FXML
  TabPane tabPane;

  @FXML
  Tab mc3Tab;

  @FXML
  Tab uploadTab;

  @FXML
  Button btnUpload;

  @FXML
  Button clear;

  @FXML
  Button btnBackup;

  @FXML
  ProgressBar progressBar;

  @FXML
  TextField searchField;

  private Scene scene;

  @FXML
  CheckBox deleteFile;

  @FXML
  Button btnRefresh;

  @FXML
  VBox switchArea;

  @FXML
  private SwitchControl switch1;
  @FXML
  private SwitchControl switch2;
  @FXML
  private SwitchControl switch3;

  private ResourceBundle resources;

  @FXML
  public void btnExit() {
    log.debug("button exit pressed.");
    session.exit();
  }

  public void init() {
    setStatusbarText("statusbar.load.manufacturer");

    tabPane.getSelectionModel().select(mc3Tab);

    setStatusbarText("statusbar.ready");
  }

  public void setScene(Scene scene) {
    this.scene = scene;
  }

  /**
   * @param text
   * 
   */
  private void setStatusbarText(String text, Object... args) {
    if (statustext.textProperty().isBound()) {
      statustext.textProperty().unbind();
    }
    statustext.setText(String.format(session.getResourceBundle().getString(text), args));
  }

  public void setSession(Session session) {
    this.session = session;
  }

  @FXML
  public void dragFileDetected(DragEvent event) {
  }

  @FXML
  public void doUpload(ActionEvent event) {
    setStatusbarText("statusbar.upload.running");
  }

  @FXML
  public void btnClear(ActionEvent event) {
    if (tabPane.getSelectionModel().getSelectedItem().equals(uploadTab)) {
    }
  }

  @FXML
  public void doBackup(ActionEvent event) {
    log.info("processing schematics");
  }

  @FXML
  public void doRefresh(ActionEvent event) {
    log.info("processing schematics");
  }

  private void bindProgress(Task<?> task) {
    progressBar.progressProperty().bind(task.progressProperty());
    statustext.textProperty().bind(task.messageProperty());
  }

  protected void unbindProgress() {
    progressBar.progressProperty().unbind();
    progressBar.setProgress(0);
    statustext.textProperty().unbind();
  }

  public void setButtonData(ProgramData programData) {
    ButtonData[] buttons = programData.getButtons();
    SequenceData[] sequences = programData.getSequences();

    switch1.setButtonData(buttons[0], getSquenceData(1, sequences), programData.getInternalMidi(),
        programData.getExternalMidi());
    switch2.setButtonData(buttons[1], getSquenceData(2, sequences), programData.getInternalMidi(),
        programData.getExternalMidi());
    switch3.setButtonData(buttons[2], getSquenceData(3, sequences), programData.getInternalMidi(),
        programData.getExternalMidi());
  }

  private List<SequenceData> getSquenceData(int btnNumber, SequenceData[] sequences) {
    List<SequenceData> sequenceDatas = new ArrayList<>();
    for (SequenceData sequenceData : sequences) {
      if (sequenceData.getType().equals(SequenceData.EVENTTYPE.BUTTON)) {
        if (sequenceData.getValue() == btnNumber) {
          sequenceDatas.add(sequenceData);
        }
      }
    }
    return sequenceDatas;
  }

  public void setResourceBundle(ResourceBundle bundle) {
    this.resources = bundle;
    switch1.setResourceBundle(bundle);
    switch2.setResourceBundle(bundle);
    switch3.setResourceBundle(bundle);
  }

}
