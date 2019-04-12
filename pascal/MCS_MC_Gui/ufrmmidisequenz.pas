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
    sbReloadCommandString: TSpeedButton;
    procedure sbAddMidiCommandClick(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
    procedure sbReloadCommandStringClick(Sender: TObject);
  private
    Fcounter: integer;
    FStatus: integer;
    function GetMidiDatas: TMidiDataArray;
    procedure SetMidiDatas(AValue: TMidiDataArray);
  public
    property Status: integer read FStatus;
    property MidiDatas: TMidiDataArray read GetMidiDatas write SetMidiDatas;
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
begin
  FStatus := ID_OK;
  ModalResult := mrOk;
end;

procedure TFrmMidiSequenz.sbReloadCommandStringClick(Sender: TObject);
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

function TFrmMidiSequenz.GetMidiDatas: TMidiDataArray;
var
  i: integer;
  Count: integer;
  midiData: TfrmMidiData;
begin
  Count := 0;
  for i := 0 to FlowPanel1.ControlCount - 1 do
  begin
    if (FlowPanel1.Controls[i] is TfrmMidiData) then
      Inc(Count);
  end;

  SetLength(Result, Count);

  Count := 0;
  for i := 0 to FlowPanel1.ControlCount - 1 do
  begin
    if (FlowPanel1.Controls[i] is TfrmMidiData) then
    begin
      midiData := FlowPanel1.Controls[i] as TfrmMidiData;
      Result[Count] := midiData.MidiData;
      Inc(Count);
    end;
  end;
end;

procedure TFrmMidiSequenz.SetMidiDatas(AValue: TMidiDataArray);
var i : integer;
begin
  for i := FlowPanel1.ControlCount - 1 downto 0 do
  begin
    if (FlowPanel1.Controls[i] is TfrmMidiData) then
    begin
      FlowPanel1.RemoveControl(FlowPanel1.Controls[i]);
    end;
  end;
end;

end.
