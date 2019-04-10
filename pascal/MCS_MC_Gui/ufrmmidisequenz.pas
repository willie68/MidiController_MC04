unit ufrmMidiSequenz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons;

type

  { TFrmMidiSequenz }

  TFrmMidiSequenz = class(TForm)
    FlowPanel1: TFlowPanel;
    Label1: TLabel;
    LabeledEdit1: TLabeledEdit;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    sbAddMidiCommand: TSpeedButton;
    sbSave: TSpeedButton;
    SpeedButton1: TSpeedButton;
    procedure sbAddMidiCommandClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    Fcounter: integer;
  public

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
  FCOunter := Fcounter + 1;
  frmMidiData := TfrmMidiData.Create(FlowPanel1);
  frmMidiData.Name := 'frmMidiData' + IntToStr(Fcounter);

  FlowPanel1.InsertControl(frmMidiData, 0);
  FlowPanel1.ControlList.Items[FlowPanel1.ControlList.Count - 1].Index := 0;
end;

procedure TFrmMidiSequenz.sbSaveClick(Sender: TObject);
begin
  Hide;
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

end.
