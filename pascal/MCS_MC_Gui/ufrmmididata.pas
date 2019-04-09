unit ufrmMidiData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, ComCtrls, ExtCtrls;

type

  { TfrmMidiData }

  TfrmMidiData = class(TFrame)
    cbMidiCOmmand: TComboBox;
    GroupBox1: TGroupBox;
    SpeedButton1: TSpeedButton;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    procedure SpeedButton1Click(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

{ TfrmMidiData }

procedure TfrmMidiData.SpeedButton1Click(Sender: TObject);
var FlowPanel : TFlowPanel;
begin
  FlowPanel := Parent as TFLowPanel;
  FlowPanel.RemoveControl(self);
end;

end.

