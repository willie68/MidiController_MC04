package application;

import java.io.IOException;
import java.util.ResourceBundle;

import javafx.fxml.FXMLLoader;

public class MainGUI {

  private MainController controller;
  private ResourceBundle bundle;

  public MainGUI setController(MainController controller) {
    this.controller = controller;
    return this;
  }

  public MainGUI setResourceBundle(ResourceBundle bundle) {
    this.bundle = bundle;
    return this;
  }

  public void updateGui(FXMLLoader mainLoader) throws IOException {

    // URL inner = getClass().getResource("switch.fxml");
    // FXMLLoader innerLoader = new FXMLLoader(inner, bundle);
    //
    // // get insertion point from outer fxml
    // innerLoader.setRoot(mainLoader.getNamespace().get("switchArea"));
    //
    // innerLoader.load();
  }

}
