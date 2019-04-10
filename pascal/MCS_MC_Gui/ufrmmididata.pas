unit ufrmMidiData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, ComCtrls, ExtCtrls,
  Spin, uModels;

const
  MidiCommands: array[0..5] of string = (
    'Program Change', 'Control Change', 'Next Program', 'Previous Program',
    'Note On', 'Note Off'
    );

type

  { TfrmMidiData }

  TfrmMidiData = class(TFrame)
    cbMidiCOmmand: TComboBox;
    GroupBox1: TGroupBox;
    sbRemove: TSpeedButton;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    procedure cbMidiCOmmandChange(Sender: TObject);
    procedure sbRemoveClick(Sender: TObject);
    procedure SpinEditChange(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
  private
    FMidiData: TMidiData;
    procedure SyncMidiData();
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfrmMidiData }

procedure TfrmMidiData.sbRemoveClick(Sender: TObject);
var
  FlowPanel: TFlowPanel;
begin
  FlowPanel := Parent as TFLowPanel;
  FlowPanel.RemoveControl(self);
  self.Free;
end;

procedure TfrmMidiData.cbMidiCOmmandChange(Sender: TObject);
begin
  if (cbMidiCOmmand.ItemIndex = 0) then
  begin
    SpinEdit2.Visible := False;
    TrackBar2.Visible := False;
  end
  else if (cbMidiCOmmand.ItemIndex = 0) then
  begin
    SpinEdit2.Visible := True;
    TrackBar2.Visible := True;
  end;
end;

procedure TfrmMidiData.SpinEditChange(Sender: TObject);
begin
  TrackBar1.Position := SpinEdit1.Value;
  TrackBar2.Position := SpinEdit2.Value;
end;

procedure TfrmMidiData.TrackBarChange(Sender: TObject);
begin
  SpinEdit1.Value := TrackBar1.Position;
  SpinEdit2.Value := TrackBar2.Position;
end;

procedure TfrmMidiData.SyncMidiData();
begin

end;

constructor TfrmMidiData.Create(TheOwner: TComponent);
var i : integer;
begin
  inherited Create(TheOwner);
  FMidiData := TMidiData.Create;
  cbMidiCOmmand.items.clear;
  for i := 0 to length(MidiCommands)-1 do
  begin
    cbMidiCOmmand.items.add(MidiCommands[i]);
  end;
end;

destructor TfrmMidiData.Destroy;
begin
  FMidiData.Free;
  inherited Destroy;
end;

end.
