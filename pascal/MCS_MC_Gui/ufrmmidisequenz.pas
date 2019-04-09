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
    LabeledEdit1: TLabeledEdit;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
   Fcounter : integer;
  public

  end;

var
  FrmMidiSequenz: TFrmMidiSequenz;

implementation

{$R *.lfm}

uses ufrmMidiData;
{ TFrmMidiSequenz }

procedure TFrmMidiSequenz.SpeedButton1Click(Sender: TObject);
var frmMidiData : TFrmMidiData;
   formName : String;
begin
  FCOunter := Fcounter + 1;
  frmMidiData := TfrmMidiData.Create(FlowPanel1);
  frmMidiData.Name := 'frmMidiData'+inttostr(Fcounter);

  FlowPanel1.InsertControl(frmMidiData, 0);
  FlowPanel1.ControlList.Items[FlowPanel1.ControlList.Count - 1].Index:=0;
end;

procedure TFrmMidiSequenz.SpeedButton2Click(Sender: TObject);
begin
  Hide;
end;

end.

