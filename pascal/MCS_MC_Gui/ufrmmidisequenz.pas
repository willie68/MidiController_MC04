unit ufrmMidiSequenz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, LCLType, uModels;

type

  { TFrmMidiSequenz }

  TFrmMidiSequenz = class(TForm)
    FlowPanel1: TFlowPanel;
    Label1: TLabel;
    LabeledEdit1: TLabeledEdit;
    Panel1: TPanel;
    sbCancel: TSpeedButton;
    ScrollBox1: TScrollBox;
    sbAddMidiCommand: TSpeedButton;
    sbSave: TSpeedButton;
    SpeedButton1: TSpeedButton;
    procedure sbAddMidiCommandClick(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    Fcounter: integer;
    FStatus: integer;
    FMidiDatas: TMidiDataArray;

    procedure FreeMidiDatas;
  public
    property Status: integer read FStatus;
    property MidiDatas: TMidiDataArray read FMidiDatas write FMidiDatas;
  end;

var
  FrmMidiSequenz: TFrmMidiSequenz;

implementation

{$R *.lfm}

uses ufrmMidiData;

{ TFrmMidiSequenz }

procedure TFrmMidiSequenz.sbAddMidiCommandClick(Sender: TObject);
var
  frmMidiData: TFrmMidiData;
begin
  Inc(FCounter);
  if (FlowPanel1.ControlCount < 17) then
  begin
    frmMidiData := TfrmMidiData.Create(FlowPanel1);
    frmMidiData.Name := 'frmMidiData' + IntToStr(Fcounter);

    FlowPanel1.InsertControl(frmMidiData, 0);
    FlowPanel1.ControlList.Items[FlowPanel1.ControlList.Count - 1].Index := 0;
  end
  else
  begin
    Application.MessageBox('no more midi commands possible', 'Information',
      MB_OK + MB_ICONWARNING);
  end;
end;

procedure TFrmMidiSequenz.sbCancelClick(Sender: TObject);
begin
  FStatus := ID_CANCEL;
  ModalResult := mrCancel;
end;

procedure TFrmMidiSequenz.sbSaveClick(Sender: TObject);
var
  i: integer;
  Count: integer;
  midiData: TfrmMidiData;
  commandString: string;
begin
  FStatus := ID_OK;
  Count := 0;
  for i := 0 to FlowPanel1.ControlCount - 1 do
    if (FlowPanel1.Controls[i] is TfrmMidiData) then
      Inc(Count);

  FreeMidiDatas();
  SetLength(FMidiDatas, Count);

  Count := 0;
  for i := 0 to FlowPanel1.ControlCount - 1 do
  begin
    if (FlowPanel1.Controls[i] is TfrmMidiData) then
    begin
      Inc(Count);
      midiData := FlowPanel1.Controls[i] as TfrmMidiData;
      FMidiDatas[Count] := midiData.MidiData.clone;
      Inc(Count);
    end;
  end;
  commandString := '';
  for i := 0 to Length(MidiDatas) - 1 do
  begin
    if (i > 0) then
      commandString := commandString + ', ';
    commandString := commandString + FMidiDatas[i].HumanString;
  end;
  LabeledEdit1.Text := commandString;

  ModalResult := mrOk;
end;

procedure TFrmMidiSequenz.SpeedButton1Click(Sender: TObject);
var
  i: integer;
  midiData: TfrmMidiData;
  commandString: string;
begin
  commandString := '';
  for i := 0 to FlowPanel1.ControlCount - 1 do
  begin
    if (FlowPanel1.Controls[i] is TfrmMidiData) then
    begin
      midiData := FlowPanel1.Controls[i] as TfrmMidiData;
      if (i > 0) then
        commandString := commandString + ', ';
      commandString := commandString + midiData.MidiData.HumanString;
    end;
  end;
  LabeledEdit1.Text := commandString;
end;

procedure TFrmMidiSequenz.FreeMidiDatas;
var
  i: integer;
  midiData: TfrmMidiData;
begin
  for i := 0 to length(FMidiDatas) - 1 do
    FreeAndNil(FMidiDatas[i]);
  SetLength(FMidiDatas, 0);
end;

end.
