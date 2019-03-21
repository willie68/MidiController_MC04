package application;

import java.awt.Desktop;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.controlsfx.control.textfield.AutoCompletionBinding;
import org.controlsfx.control.textfield.TextFields;

import com.sun.xml.internal.ws.api.message.Attachment;

import de.mcs.utils.Files;
import de.mcs.utils.Logger;
import javafx.collections.ObservableList;
import javafx.concurrent.Task;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.Cursor;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.MenuItem;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.input.DragEvent;
import javafx.scene.input.Dragboard;
import javafx.scene.input.MouseEvent;
import javafx.scene.input.TransferMode;
import javafx.scene.layout.FlowPane;

public class MainController {

  Logger log = Logger.getLogger(this.getClass());

  private Session session;

  @FXML
  TextField modelField;

  @FXML
  Button exit;

  @FXML
  ComboBox<String> cbManufacturer;

  @FXML
  TextArea descriptionField;

  @FXML
  Label statustext;

  @FXML
  ListView<File> schematicFiles;

  @FXML
  TabPane tabPane;

  @FXML
  Tab searchTab;

  @FXML
  Tab uploadTab;

  @FXML
  Button btnUpload;

  @FXML
  TextField subtitleField;

  @FXML
  CheckBox privateFile;

  @FXML
  TextField tagsField;

  @FXML
  FlowPane tagbar;

  List<String> tagList;

  @FXML
  Button btnClearTags;

  @FXML
  Button clear;

  @FXML
  Button btnBackup;

  @FXML
  ProgressBar progressBar;

  private Map<String, String> checksums;

  @FXML
  TextField searchField;

  private Scene scene;

  private boolean newSchematic;

  @FXML
  CheckBox deleteFile;

  @FXML
  ListView<String> lvQuickSearch;

  @FXML
  Button btnRefresh;

  @FXML
  MenuItem ctxFileItemDelete;

  private List<String> tagNames;

  private AutoCompletionBinding<String> bindAutoCompletion;

  @FXML
  public void btnExit() {
    log.debug("button exit pressed.");
    session.exit();
  }

  public void init() {
    checksums = new HashMap<String, String>();
    tagList = new ArrayList<>();
    setStatusbarText("statusbar.load.manufacturer");
    // updateManufaturer();
    cbManufacturer.getSelectionModel().select(0);
    cbManufacturer.setEditable(true);
    TextFields.bindAutoCompletion(cbManufacturer.getEditor(), cbManufacturer.getItems());

    tagNames = new ArrayList<>();
    bindAutoCompletion = TextFields.bindAutoCompletion(tagsField, tagNames);

    // updateTags();

    tabPane.getSelectionModel().select(searchTab);

    // manufacturerColumn.setCellValueFactory(new PropertyValueFactory<Schematic, String>("manufacturer"));
    // modelColumn.setCellValueFactory(new PropertyValueFactory<Schematic, String>("mModel"));
    // subtitleColumn.setCellValueFactory(new PropertyValueFactory<Schematic, String>("subtitle"));
    // tagsColumn.setCellValueFactory(new PropertyValueFactory<Schematic, List<String>>("tags"));
    //
    // GUIUtils.autoFitTable(hitgrid);
    //
    // deleteFile.setSelected(session.getConfig().isAutoDelete());

    newSchematic = true;
    setStatusbarText("statusbar.ready");
  }

  private void updateTags() {
    // List<Tag> tags = Tag.getTags("");
    // tagNames.clear();
    // tags.stream().sorted((m1, m2) -> m1.getTagName().compareTo(m2.getTagName()))
    // .forEach(m -> tagNames.add(m.getTagName()));
    // bindAutoCompletion.dispose();
    // bindAutoCompletion = TextFields.bindAutoCompletion(tagsField, tagNames);

  }

  private void updateManufaturer() {
    // cbManufacturer.getItems().clear();
    // List<Manufacturer> manufacturers = Manufacturer.getManufacturers("");
    // manufacturers.stream().sorted((m1, m2) -> m1.getManufacturerName().compareTo(m2.getManufacturerName()))
    // .forEach(m -> cbManufacturer.getItems().add(m.getManufacturerName()));
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
  public void dragFileDropped(DragEvent event) {
    Dragboard db = event.getDragboard();
    boolean success = false;
    if (db.hasFiles()) {
      success = true;
      String model = "";
      for (File file : db.getFiles()) {
        schematicFiles.getItems().add(file);
        model = Files.extractName(file);
        model = model.replaceAll("\\+", " ").replaceAll("-", " ").replaceAll("_", " ").trim();
        if (model.endsWith("schem")) {
          model = model.substring(0, model.indexOf("schem"));
        }
        if (model.endsWith("layout")) {
          model = model.substring(0, model.indexOf("layout"));
        }
        log.debug("adding file: %s", file);
        buildChecksum(file);
      }
      if (newSchematic) {
        modelField.setText(model);
      }
    }
    event.setDropCompleted(success);
    event.consume();
  }

  @FXML
  public void dragFileOver(DragEvent event) {
    Dragboard db = event.getDragboard();
    if (db.hasFiles()) {
      event.acceptTransferModes(TransferMode.COPY);
    } else {
      event.consume();
    }
  }

  @FXML
  public void doUpload(ActionEvent event) {
    setStatusbarText("statusbar.upload.running");
    String manufacturer = cbManufacturer.getValue();
    manufacturer = cbManufacturer.getEditor().getText();
    if (StringUtils.isEmpty(manufacturer)) {
      setStatusbarText("statusbar.missing.manufacturer");
      return;
    }
    String model = modelField.getText();
    if (StringUtils.isEmpty(model)) {
      setStatusbarText("statusbar.missing.model");
      return;
    }
    ObservableList<File> items = schematicFiles.getItems();
    if (items.size() == 0) {
      setStatusbarText("statusbar.missing.files");
      return;
    }
    if (tagList.size() == 0) {
      setStatusbarText("statusbar.missing.tags");
      return;
    }
    newSchematic = false;
    // if (selectedSchematic == null) {
    // selectedSchematic = new Schematic();
    // newSchematic = true;
    // }
    // selectedSchematic.setDescription(descriptionField.getText());
    // selectedSchematic.setManufacturer(manufacturer);
    // selectedSchematic.setMModel(modelField.getText());
    // selectedSchematic.setSubtitle(subtitleField.getText());
    // selectedSchematic.setPrivateFile(privateFile.isSelected() ? 1L : 0L);
    // selectedSchematic.setTags(tagList);
    scene.setCursor(Cursor.WAIT);

    // UploadTask task = new UploadTask(selectedSchematic, items, this);
    // task.setSession(session);
    // bindProgress(task);
    // task.setOnSucceeded(new EventHandler<WorkerStateEvent>() {
    // @Override
    // public void handle(WorkerStateEvent t) {
    // setStatusbarText("statusbar.upload.ok");
    // scene.setCursor(Cursor.DEFAULT);
    // if (newSchematic) {
    // clearForm();
    // selectedSchematic = null;
    // }
    // unbindProgress();
    // }
    // });
    // task.setOnCancelled(new EventHandler<WorkerStateEvent>() {
    // @Override
    // public void handle(WorkerStateEvent t) {
    // unbindProgress();
    // scene.setCursor(Cursor.DEFAULT);
    // }
    // });
    // task.setOnFailed(new EventHandler<WorkerStateEvent>() {
    // @Override
    // public void handle(WorkerStateEvent t) {
    // unbindProgress();
    // scene.setCursor(Cursor.DEFAULT);
    // }
    // });
    //
    // new Thread(task).start();

  }

  private void clearForm() {
    modelField.setText("");
    subtitleField.setText("");
    descriptionField.setText("");
    schematicFiles.getItems().clear();
    // clearTagList();
  }

  @FXML
  public void addTag(ActionEvent event) {
    String tag = tagsField.getText();
    addTagToList(tag);
  }

  private void addTagToList(String tag) {
    if (!tagList.contains(tag)) {
      int size = tagbar.getChildren().size() - 1;

      Button tagButton = new Button(tag);
      tagButton.getStyleClass().add("tag-button");
      tagButton.setOnAction(e -> {
        tagbar.getChildren().remove(tagButton);
        tagList.remove(tag);
      });
      tagbar.getChildren().add(size, tagButton);
      tagsField.setText("");
      tagList.add(tag);
    }
  }

  private void clearTagList() {
    tagList.clear();
    List<Node> buttonsToRemove = tagbar.getChildren().stream().filter(p -> p instanceof Button)
        .collect(Collectors.toList());
    buttonsToRemove.stream().forEach(b -> tagbar.getChildren().remove(b));
  }

  @FXML
  public void btnClear(ActionEvent event) {
    if (tabPane.getSelectionModel().getSelectedItem().equals(uploadTab)) {
      clearForm();
      clearTagList();
    }
  }

  @FXML
  public void doBackup(ActionEvent event) {
    log.info("processing schematics");
    // ProgressAttachments task = new ProgressAttachments();
    // ExportFiles task = new ExportFiles(session.getConfig().getBackupPath());
    // task.setSession(session);
    // bindProgress(task);
    // new Thread(task).start();
  }

  private void buildChecksum(File file) {
    log.info("processing file %s", file.getName());
    // HashFile task = new HashFile(file);
    // task.setSession(session);
    // bindProgress(task);
    // task.setOnSucceeded(new EventHandler<WorkerStateEvent>() {
    // @Override
    // public void handle(WorkerStateEvent t) {
    // String result = task.getValue();
    // checksums.put(file.getAbsolutePath(), result);
    // testCheckSum(file, result);
    // unbindProgress();
    // }
    // });
    // task.setOnCancelled(new EventHandler<WorkerStateEvent>() {
    // @Override
    // public void handle(WorkerStateEvent t) {
    // unbindProgress();
    // }
    // });
    // task.setOnFailed(new EventHandler<WorkerStateEvent>() {
    // @Override
    // public void handle(WorkerStateEvent t) {
    // unbindProgress();
    // }
    // });
    // new Thread(task).start();
  }

  private void testCheckSum(File file, String result) {
    log.info("testing hash for file %s", file);
    // TestFile task = new TestFile(result);
    // task.setSession(session);
    // task.setOnSucceeded(new EventHandler<WorkerStateEvent>() {
    // @Override
    // public void handle(WorkerStateEvent t) {
    // boolean result = task.getValue();
    // if (result) {
    // setStatusbarText("statusbar.fileAlreadyAdded");
    // }
    // }
    // });
    // new Thread(task).start();
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

  @FXML
  public void doSearch(ActionEvent event) {
    log.info("do search with: %s", searchField.getText());
    String searchString = String.format(
        "manufacturer like \"%1$s\" or mModel like \"%1$s\" or tags like \"%1$s\" or subtitle like \"%1$s\" ",
        searchField.getText());
    // try {
    // long schematicsCount = Schematic.getSchematicsCount(searchString);
    // log.info("found %d hits with query: \"%s\"", schematicsCount, searchString);
    // setStatusbarText("statusbar.search.found", schematicsCount);
    // List<Schematic> schematics = Schematic.getSchematics(searchString);
    // ObservableList<Schematic> list = FXCollections.observableArrayList();
    // list.addAll(schematics);
    //
    // hitgrid.setItems(list);
    //
    // } catch (ApiomatRequestException e) {
    // log.error(e, "error searching in apiomat");
    // }
  }

  @FXML
  public void doQuery() {
    log.info("do search with: %s", searchField.getText());
    String searchString = "";
    if (StringUtils.isEmpty(cbManufacturer.getValue()) || cbManufacturer.getValue().toLowerCase().equals("unknown")) {
      searchString = String.format("mModel like \"%1$s\"", modelField.getText());
    } else {
      searchString = String.format("manufacturer like \"%1$s\" and mModel like \"%2$s\"", cbManufacturer.getValue(),
          modelField.getText());
    }
    // try {
    // long schematicsCount = Schematic.getSchematicsCount(searchString);
    // log.info("found %d hits with query: \"%s\"", schematicsCount, searchString);
    // setStatusbarText("statusbar.search.found", schematicsCount);
    // List<Schematic> schematics = Schematic.getSchematics(searchString);
    // ObservableList<String> list = FXCollections.observableArrayList();
    // schematics.forEach(s -> list.add(String.format("%s (%s)", s.getMModel(), s.getManufacturer())));
    //
    // lvQuickSearch.setItems(list);
    //
    // } catch (ApiomatRequestException e) {
    // log.error(e, "error searching in apiomat");
    // }
  }

  @FXML
  public void hitgridClicked(MouseEvent event) {
    // if (event.getClickCount() == 2) {
    // log.debug("doubleclick");
    // selectedSchematic = hitgrid.getSelectionModel().getSelectedItem();
    // bindSchematic();
    // newSchematic = false;
    // tabPane.getSelectionModel().select(uploadTab);
    // }
  }

  private void bindSchematic() {
    // if (selectedSchematic != null) {
    // int i = cbManufacturer.getItems().indexOf(selectedSchematic.getManufacturer());
    // cbManufacturer.getSelectionModel().select(i);
    //
    // modelField.setText(selectedSchematic.getMModel());
    // subtitleField.setText(selectedSchematic.getSubtitle());
    // descriptionField.setText(selectedSchematic.getDescription());
    // List<String> tags = selectedSchematic.getTags();
    // clearTagList();
    // tags.forEach(t -> addTagToList(t));
    // try {
    // schematicFiles.getItems().clear();
    // List<Attachment> filelist = selectedSchematic.loadFiles("");
    // filelist.forEach(a -> schematicFiles.getItems().add(extractAttachmentToTempdir(a)));
    // } catch (ApiomatRequestException e) {
    // log.error(e, "error adding files");
    // }
    // }
  }

  private File extractAttachmentToTempdir(Attachment attachment) {
    // String fileURL = attachment.getFileURL();
    //
    // try {
    // ResourceInfo resourceInfo = Datastore.getInstance().loadResourceInfo(fileURL);
    //
    // byte[] resultObject = attachment.loadFile();
    //
    // File file = new File(Files.getTempPath(), String.format("%s", resourceInfo.getName()));
    // try {
    // OutputStream out = new BufferedOutputStream(new FileOutputStream(file));
    // out.write(resultObject);
    // out.close();
    // } catch (IOException e) {
    // log.error(e, "error exporting file %s", resourceInfo.getName());
    // }
    // return file;
    // } catch (ApiomatRequestException e1) {
    // log.error(e1, "error exporting file");
    // }
    return null;
  }

  @FXML
  public void fileClick(MouseEvent event) {
    if (event.getClickCount() == 2) {
      log.debug("doubleclick");
      File file = schematicFiles.getSelectionModel().getSelectedItem();
      if (Desktop.isDesktopSupported()) {
        try {
          Desktop.getDesktop().open(file);
        } catch (IOException e) {
          log.error(e, "error showing file %s", file.getName());
        }
      }
    }
  }

  public Map<String, String> getCheckSums() {
    return checksums;
  }

  public boolean isDeleteFile() {
    return deleteFile.isSelected();
  }

  @FXML
  public void onDeleteFile(ActionEvent event) {
    // session.getConfig().setAutoDelete(deleteFile.isSelected());
    // session.saveConfig();
  }

  @FXML
  public void doRefresh() {
    updateManufaturer();
    updateTags();
  }

  @FXML
  public void doCtxFileItemDelete(ActionEvent event) {
    File selectedFile = schematicFiles.getSelectionModel().getSelectedItem();
    if (selectedFile != null) {
      log.info("delete file entry: \"%s\"", selectedFile.toString());
      schematicFiles.getItems().remove(selectedFile);
    }
  }
}
