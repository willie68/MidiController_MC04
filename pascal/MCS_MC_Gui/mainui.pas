unit mainUi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, JSONPropStorage,
  ExtCtrls, ComCtrls, Menus, ActnList, StdActns, StdCtrls, ColorBox, EditBtn,
  ufrmMidiSwitch, ufrmPreset, ufrmexppedal;

const
  APPTITLE = 'MCS MC Gui';

type

  { TForm1 }

  TForm1 = class(TForm)
    PresetCopy: TAction;
    FlowPanel1: TFlowPanel;
    frmExpPedal1: TfrmExpPedal;
    frmMidiSwitch1: TfrmMidiSwitch;
    frmMidiSwitch2: TfrmMidiSwitch;
    frmMidiSwitch3: TfrmMidiSwitch;
    frmPreset1: TfrmPreset;
    ListView1: TListView;
    PresetDelete: TAction;
    PresetAdd: TAction;
    ActionList1: TActionList;
    FileExit1: TFileExit;
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    HelpAbout: THelpOnHelp;
    ImageList1: TImageList;
    JSONPropStorage1: TJSONPropStorage;
    Label9: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    FileOpen: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    N1: TMenuItem;
    Panel3: TPanel;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    tbHelp: TToolButton;
    ToolBar2: TToolBar;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
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

  tbHelp.Align := alRight;

  frmMidiSwitch1.Caption := 'Switch 1';
  frmMidiSwitch2.Caption := 'Switch 2';
  frmMidiSwitch3.Caption := 'Switch 3';
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
