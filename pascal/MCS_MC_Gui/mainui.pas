unit mainUi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, JSONPropStorage,
  ExtCtrls, ComCtrls, Menus, ActnList, StdActns, StdCtrls, ColorBox, EditBtn;

const
  APPTITLE = 'MCS MC Gui';

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionList1: TActionList;
    ColorButton1: TColorButton;
    EditButton1: TEditButton;
    EditButton2: TEditButton;
    EditButton3: TEditButton;
    EditButton4: TEditButton;
    EditButton5: TEditButton;
    FileExit1: TFileExit;
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    HelpAbout: THelpOnHelp;
    Image1: TImage;
    ImageList1: TImageList;
    JSONPropStorage1: TJSONPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LabeledEdit1: TLabeledEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    FileOpen: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    N1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    tbHelp: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    TrayIcon1: TTrayIcon;
    procedure FormCreate(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure StatusBar1Resize(Sender: TObject);
    procedure ToolBar1Click(Sender: TObject);
  private
    ConfigFile: string;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses MCSMessageBox, MCSStrings, MCSLogging, MCSIniFiles,
  MCSIO, MCSTools, MCSAbout;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ConfigFile := GetAppConfigFile(False);
  JSONPropStorage1.JSONFileName := ConfigFile;
  ForceDirectories(ExtractFilePath(ConfigFile));

  Infobox.AppTitel := APPTITLE;
  Infobox.CopyRight := 'MCS (C) Wilfried Klaas' + CRLF + 'EMail: w.klaas@gmx.de';
  Infobox.Build := MCSGetVersion(Application.ExeName);
  Infobox.AppID := 0;
  Infobox.AppURL := 'http://www.wkmusic.de';

  Caption := Infobox.AppTitel;

  tbHelp.Align:=alRight;
end;

procedure TForm1.HelpAboutExecute(Sender: TObject);
begin
  Infobox.Show;
end;

procedure TForm1.Label4Click(Sender: TObject);
begin

end;

procedure TForm1.StatusBar1Resize(Sender: TObject);
var
  size: integer;
begin
  size := StatusBar1.ClientWidth - StatusBar1.Panels[1].Width -
    StatusBar1.Panels[2].Width;
  StatusBar1.Panels[0].Width := size;
end;

procedure TForm1.ToolBar1Click(Sender: TObject);
begin

end;

end.
