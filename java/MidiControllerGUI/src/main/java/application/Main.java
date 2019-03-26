package application;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ResourceBundle;

import de.mcs.tools.midicontroller.data.ProgramData;
import de.mcs.tools.midicontroller.data.Programs;
import de.mcs.utils.JacksonUtils;
import de.mcs.utils.Logger;
import javafx.application.Application;
import javafx.application.Platform;
import javafx.event.EventHandler;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.layout.Pane;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;

public class Main extends Application {
  private Logger log = Logger.getLogger(this.getClass());
  private ResourceBundle bundle;
  private Session session;
  private Stage stage;
  private MainGUI mainGUI;
  private static File programFile = null;

  @Override
  public void start(Stage primaryStage) {

    bundle = ResourceBundle.getBundle(this.getClass().getName());
    session = new Session();
    session.setMain(this);
    session.setResourceBundle(bundle);
    session.initConfig();

    try {
      primaryStage.setTitle("WK Music Midi Controller");
      primaryStage.getIcons()
          .add(new Image(this.getClass().getClassLoader().getResourceAsStream("application/ico/cloud10.png")));

      try {
        primaryStage.setScene(getMainScene());
        primaryStage.show();

        primaryStage.setOnCloseRequest(new EventHandler<WindowEvent>() {
          public void handle(WindowEvent ev) {
            if (false) {
              ev.consume();
            }
            log.info(String.format("stopping application"));
            exit();
          }
        });
      } catch (Exception e) {
        log.error(e, "unknown error");
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
    if (programFile != null && programFile.exists()) {
      try (InputStream source = new BufferedInputStream(new FileInputStream(programFile))) {
        Programs programs = JacksonUtils.getJsonMapper().readValue(source, Programs.class);
        ProgramData programData = programs.getPrograms()[0];
        mainGUI.setProgramData(programData);
      } catch (IOException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
  }

  public static void main(String[] args) {
    if (args.length > 0) {
      programFile = new File(args[0]);
    }
    launch(args);
  }

  public void exit() {
    if (true) {
      log.info(String.format("exit platform"));
      Platform.exit();
    }
  }

  private Scene getMainScene() throws IOException {
    URL resource = getClass().getResource("MainGUI.fxml");
    FXMLLoader fxmlLoader = new FXMLLoader(resource, bundle);
    Pane mainPane = (Pane) fxmlLoader.load();
    MainController controller = fxmlLoader.getController();
    controller.setSession(session);
    controller.init();
    mainGUI = new MainGUI().setController(controller).setResourceBundle(bundle);
    mainGUI.updateGui(fxmlLoader);

    Scene myScene = new Scene(mainPane);
    controller.setScene(myScene);
    return myScene;
  }

  public void toMainScene() {
    try {
      stage.setScene(getMainScene());
    } catch (IOException e) {
      log.error(e, "unknown error");
    }
    stage.show();
  }
}
