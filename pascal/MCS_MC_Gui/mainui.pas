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
    ToolButton9: TToolButton;
    TrayIcon1: TTrayIcon;
    procedure FileOpen1Accept(Sender: TObject);
    procedure FileSaveAs1Accept(Sender: TObject);
    procedure FileSaveAs1BeforeExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListView1Click(Sender: TObject);
    procedure PresetAddExecute(Sender: TObject);
    procedure PresetCopyExecute(Sender: TObject);
    procedure PresetDeleteExecute(Sender: TObject);
    procedure StatusBar1Resize(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
  private
    ConfigFile: string;
    FileName: string;
    FJSON: TJSONObject;

    Presets: TMidiPresets;


    frmPreset: TfrmPreset;
    frmexppedal: TfrmExpPedal;
    frmMidiSwitch1: TfrmMidiSwitch;
    frmMidiSwitch2: TfrmMidiSwitch;
    frmMidiSwitch3: TfrmMidiSwitch;

    procedure OpenFile();
    procedure GettingPrograms();
    function GetActualPreset(): TMidiPreset;
    procedure switchToPreset(Preset: TMidiPreset);
    procedure SaveOldPreset(Preset: TMidiPreset);
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

  frmPreset := TfrmPreset.Create(FlowPanel1);
  FlowPanel1.InsertControl(frmPreset, 0);

  frmexppedal := TfrmExpPedal.Create(FlowPanel1);
  frmexppedal.ExpressionNumber := 1;
  FlowPanel1.InsertControl(frmexppedal, 1);

  frmMidiSwitch1 := TfrmMidiSwitch.Create(FlowPanel1);
  frmMidiSwitch1.Name := 'frmMidiSwitch1';
  frmMidiSwitch1.ButtonNumber := 1;

  frmMidiSwitch2 := TfrmMidiSwitch.Create(FlowPanel1);
  frmMidiSwitch2.Name := 'frmMidiSwitch2';
  frmMidiSwitch2.ButtonNumber := 2;

  frmMidiSwitch3 := TfrmMidiSwitch.Create(FlowPanel1);
  frmMidiSwitch3.Name := 'frmMidiSwitch3';
  frmMidiSwitch3.ButtonNumber := 3;

  FlowPanel1.InsertControl(frmMidiSwitch1, 0);
  FlowPanel1.InsertControl(frmMidiSwitch2, 1);
  FlowPanel1.InsertControl(frmMidiSwitch3, 2);
  FlowPanel1.ControlList.Items[2].Index := 1;
  FlowPanel1.ControlList.Items[3].Index := 2;
  FlowPanel1.ControlList.Items[4].Index := 3;

  frmMidiSwitch1.Caption := 'Switch 1';
  frmMidiSwitch2.Caption := 'Switch 2';
  frmMidiSwitch3.Caption := 'Switch 3';

  if (ParamCount > 0) then
  begin
    FileName := ParamStr(1);
    OpenFile();
  end;

end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to Length(Presets) - 1 do
  begin
    FreeAndNil(Presets[i]);
  end;
  SetLength(Presets, 0);
end;

procedure TForm1.FileOpen1Accept(Sender: TObject);
var
  myFileName: string;
begin
  myFileName := FileOpen1.Dialog.FileName;
  FileName := myFileName;
  OpenFile();
end;

procedure TForm1.FileSaveAs1Accept(Sender: TObject);
var
  jsonString: string;
  preset: TMidiPreset;
  F: TextFile;
  jsonArray : TJsonArray;
  jsonObject : TJsonObject;
  i : integer;
begin
  filename := FileSaveAs1.Dialog.FileName;

  AssignFile(F, filename);
  try
    SaveOldPreset(GetActualPreset());

    jsonArray := TJsonArray.Create();
    for i := 0 to length(Presets) - 1 do
    begin
      jsonArray.Add(Presets[i].toJson);
    end;
    jsonObject := TJsonObject.Create;
    jsonObject.Add('version', 1);
    jsonObject.Add('programs', jsonArray);
    jsonString := jsonObject.AsJSON;
    jsonObject.Free;

    ReWrite(F);
    Write(F, jsonString);
  finally
    CloseFile(F);
  end;
  ShowMessage('presets saved');
end;

procedure TForm1.FileSaveAs1BeforeExecute(Sender: TObject);
begin
  if (Filename <> '') then
    FileSaveAs1.Dialog.FileName := Filename;
end;

procedure TForm1.HelpAboutExecute(Sender: TObject);
begin
  Infobox.Show;
end;

procedure TForm1.ListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
end;

procedure TForm1.ListView1Click(Sender: TObject);
var
  presetName: string;
  Preset: TMidiPreset;
  i: integer;
begin
  Preset := nil;
  if (ListView1.ItemIndex >= 0) then
  begin
    presetName := ListView1.Items[ListView1.ItemIndex].Caption;
    for i := 0 to Length(Presets) - 1 do
    begin
      if (presetName = Presets[i].Name) then
        Preset := Presets[i];
    end;
    if (Assigned(Preset)) then
      switchToPreset(Preset);
  end;
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


procedure TForm1.ToolButton9Click(Sender: TObject);
var
  jsonString: string;
  preset: TMidiPreset;
begin
  preset := GetActualPreset();
  jsonString := preset.toJson.AsJSON;
  ShowMessage(jsonString);
  preset.Free;
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
var
  jsonPresets: TJSONArray;
  jsonPreset: TJSONObject;
  item: TListItem;
  i: integer;
  Preset: TMidiPreset;
begin
  ListView1.Clear;
  jsonPresets := FJSON.Arrays['programs'];
  for i := 0 to jsonPresets.Count - 1 do
  begin
    jsonPreset := jsonPresets.Objects[i];

    Preset := TMidiPreset.parseJsonToMidiPreset(jsonPreset);
    item := ListView1.Items.Add();
    item.Caption := Preset.Name;

    SetLength(Presets, Length(Presets) + 1);
    Presets[Length(Presets) - 1] := Preset;
  end;

  if (Length(Presets) > 0) then
  begin
    preset := Presets[0];
    switchToPreset(preset);
  end;
end;

function TForm1.GetActualPreset(): TMidiPreset;
var
  myButton: TMidiButton;
  i: integer;
  mySequences: TMidiSequenceArray;
begin
  Result := frmPreset.Preset;
  if (Assigned(Result)) then
  begin

    myButton := frmMidiSwitch1.MidiButton;
    Result.AddButton(myButton);
    mySequences := frmMidiSwitch1.MidiSequences;
    if (assigned(mySequences)) then
    begin
      for i := 0 to Length(mySequences) - 1 do
      begin
        Result.AddSequence(mySequences[i]);
      end;
    end;

    myButton := frmMidiSwitch2.MidiButton;
    Result.AddButton(myButton);
    mySequences := frmMidiSwitch2.MidiSequences;
    if (assigned(mySequences)) then
    begin
      for i := 0 to Length(mySequences) - 1 do
      begin
        Result.AddSequence(mySequences[i]);
      end;
    end;

    myButton := frmMidiSwitch3.MidiButton;
    Result.AddButton(myButton);
    mySequences := frmMidiSwitch3.MidiSequences;
    if (assigned(mySequences)) then
    begin
      for i := 0 to Length(mySequences) - 1 do
      begin
        Result.AddSequence(mySequences[i]);
      end;
    end;

    mySequences := frmexppedal.MidiSequences;
    if (assigned(mySequences)) then
    begin
      for i := 0 to Length(mySequences) - 1 do
      begin
        Result.AddSequence(mySequences[i]);
      end;
    end;
  end
  else
  begin
    Result := TMidiPreset.Create;
  end;
end;

procedure TForm1.switchToPreset(Preset: TMidiPreset);
begin
  SaveOldPreset(GetActualPreset());
  frmPreset.Preset := Preset;
  if (Length(Preset.Buttons) > 0) then
    frmMidiSwitch1.MidiButton := Preset.Buttons[0];
  if (Length(Preset.Buttons) > 1) then
    frmMidiSwitch2.MidiButton := Preset.Buttons[1];
  if (Length(Preset.Buttons) > 2) then
    frmMidiSwitch3.MidiButton := Preset.Buttons[2];
  frmMidiSwitch1.MidiSequences := Preset.Sequences;
  frmMidiSwitch2.MidiSequences := Preset.Sequences;
  frmMidiSwitch3.MidiSequences := Preset.Sequences;

  frmexppedal.ExpressionNumber := 1;
  frmexppedal.MidiSequences := Preset.Sequences;
end;

procedure TForm1.SaveOldPreset(Preset: TMidiPreset);
var
  i: integer;
begin
  for i := 0 to Length(Presets) - 1 do
  begin
    if (Preset.Name = Presets[i].Name) then
    begin
      Presets[i].Free;
      Presets[i] := Preset;
    end;
  end;
end;


end.
