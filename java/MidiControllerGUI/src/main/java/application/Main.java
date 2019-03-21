package application;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

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
  }

  public static void main(String[] args) {
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
