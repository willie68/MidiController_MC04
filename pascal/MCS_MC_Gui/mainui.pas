unit mainUi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLType, JSONPropStorage,
  ExtCtrls, ComCtrls, Menus, ActnList, StdActns, StdCtrls, ColorBox, EditBtn,
  ufrmMidiSwitch, ufrmPreset, ufrmexppedal, fpjson, jsonparser, uModels;

const
  APPTITLE = 'MCS MC Gui';

type

  { TForm1 }

  TForm1 = class(TForm)
    frmExpPedal1: TfrmExpPedal;
    frmPreset1: TfrmPreset;
    PresetCopy: TAction;
    FlowPanel1: TFlowPanel;
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
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    TrayIcon1: TTrayIcon;
    procedure FileOpen1Accept(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure PresetAddExecute(Sender: TObject);
    procedure PresetCopyExecute(Sender: TObject);
    procedure PresetDeleteExecute(Sender: TObject);
    procedure StatusBar1Resize(Sender: TObject);
  private
    ConfigFile: string;
    FileName: string;
    FJSON: TJSONObject;

    Presets : TPresets;

    frmMidiSwitch1: TfrmMidiSwitch;
    frmMidiSwitch2: TfrmMidiSwitch;
    frmMidiSwitch3: TfrmMidiSwitch;

    procedure OpenFile();
    procedure GettingPrograms();
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses MCSMessageBox, MCSStrings, MCSLogging, MCSIniFiles,
  MCSIO, MCSTools, MCSAbout, jsonscanner;

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

  frmMidiSwitch1 := TfrmMidiSwitch.Create(FlowPanel1);
  frmMidiSwitch1.Name:= 'frmMidiSwitch1';
  frmMidiSwitch2 := TfrmMidiSwitch.Create(FlowPanel1);
  frmMidiSwitch2.Name:= 'frmMidiSwitch2';
  frmMidiSwitch3 := TfrmMidiSwitch.Create(FlowPanel1);
  frmMidiSwitch3.Name:= 'frmMidiSwitch3';

  FlowPanel1.InsertControl(frmMidiSwitch1, 0);
  FlowPanel1.InsertControl(frmMidiSwitch2, 1);
  FlowPanel1.InsertControl(frmMidiSwitch3, 2);
  FlowPanel1.ControlList.Items[2].Index:=1;
  FlowPanel1.ControlList.Items[3].Index:=2;
  FlowPanel1.ControlList.Items[4].Index:=3;

  frmMidiSwitch1.Caption := 'Switch 1';
  frmMidiSwitch2.Caption := 'Switch 2';
  frmMidiSwitch3.Caption := 'Switch 3';

  Presets := TPresets.Create;

  if (ParamCount > 0) then
  begin
    FileName := ParamStr(1);
    OpenFile();
  end;

end;

procedure TForm1.FileOpen1Accept(Sender: TObject);
var
  myFileName: string;
begin
  myFileName := FileOpen1.Dialog.FileName;
  FileName := myFileName;
  OpenFile();
end;

procedure TForm1.HelpAboutExecute(Sender: TObject);
begin
  Infobox.Show;
end;

procedure TForm1.PresetAddExecute(Sender: TObject);
var
  presetName: string;
  item: TListItem;
begin
  if (InputQuery('Name of the preset', 'Name:', presetName)) then
  begin
    item := ListView1.Items.Add;
    item.Caption := presetName;
  end;
end;

procedure TForm1.PresetCopyExecute(Sender: TObject);
begin

end;

procedure TForm1.PresetDeleteExecute(Sender: TObject);
begin
  if (not (ListView1.Selected = nil)) then
  begin
    if Application.MessageBox(PChar('Do you want to delete the preset "' +
      ListView1.Selected.Caption + '" ? '), 'Delete', MB_ICONQUESTION + MB_OKCANCEL) =
      ID_YES then
    begin
      ShowMessage('Caption ' + ListView1.Selected.Caption);
    end;
  end;
end;

procedure TForm1.StatusBar1Resize(Sender: TObject);
var
  size: integer;
begin
  size := StatusBar1.ClientWidth - StatusBar1.Panels[1].Width -
    StatusBar1.Panels[2].Width;
  StatusBar1.Panels[0].Width := size;
end;

procedure TForm1.OpenFile();
var
  jData: TJSONData;
  F: TFileStream;
begin
  Form1.Caption := Infobox.AppTitel + ' - ' + FileName;

  F := TFileStream.Create(FileName, fmOpenRead);
  try
    with TJSONParser.Create(F, jsonscanner.DefaultOptions) do
      try
        jData := Parse;
      finally
        Free;
      end;
  finally
    F.Free;
  end;
  if jData.JSONType <> jtObject then
    raise Exception.Create('no json found');
  if Assigned(FJSON) then
    FreeAndNil(FJSON);
  FJSON := jData as TJSONObject;

  GettingPrograms();
end;

procedure TForm1.GettingPrograms();
var MidiButton : TMidiButton;
var
  jsonPresets: TJsonArray;
  jsonPreset: TJSONObject;
  presetName: string;
  item: TListItem;
  i: integer;
begin
  ListView1.Clear;
  jsonPresets := FJSON.Arrays['programs'];
  for i := 0 to jsonPresets.Count - 1 do
  begin
    jsonPreset := jsonPresets.Objects[i];
    presetName := jsonPreset.Get('name');
    item := ListView1.Items.Add();
    item.Caption := presetName;
  end;

  MidiButton := TMidiButton.Create;
  MidiButton.Name:= 'Chorus';
  MidiButton.ButtonType:=TOGGLE;
  MidiButton.Color:=clRed;
  frmMidiSwitch1.SetMidiButton(MidiButton);
end;


end.