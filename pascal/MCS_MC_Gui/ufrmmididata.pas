unit ufrmMidiData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, ComCtrls, ExtCtrls,
  Spin, uModels;

const
  MidiCommands: array[0..6] of string = (
    'Program Change', 'Control Change', 'Next Program', 'Previous Program',
    'Note On', 'Note Off', 'WAIT');

type

  { TfrmMidiData }

  TfrmMidiData = class(TFrame)
    cbMidiCOmmand: TComboBox;
    GroupBox1: TGroupBox;
    lbChannel: TLabel;
    lbData1: TLabel;
    lbData2: TLabel;
    sbRemove: TSpeedButton;
    seData2: TSpinEdit;
    seData1: TSpinEdit;
    seChannel: TSpinEdit;
    tbData2: TTrackBar;
    tbData1: TTrackBar;
    tbChannel: TTrackBar;
    procedure cbMidiCommandChange(Sender: TObject);
    procedure sbRemoveClick(Sender: TObject);
    procedure SpinEditChange(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
  private
    FMidiData: TMidiData;
    procedure SyncMidiData;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetMidiData(MyMidiData: TMidiData);
  published
    property MidiData: TMidiData read FMidiData write SetMidiData;
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

procedure TfrmMidiData.cbMidiCommandChange(Sender: TObject);
begin
  if (cbMidiCOmmand.ItemIndex = 0) then
  begin
    lbChannel.Caption := 'Channel';
    seChannel.MaxValue:= 127;
    tbChannel.Max:=127;

    lbData1.Visible := True;
    lbData1.Caption := 'Prg Number';
    seData1.Visible := True;
    tbData1.Visible := True;

    lbData2.Visible := False;
    seData2.Visible := False;
    tbData2.Visible := False;
  end
  else if (cbMidiCOmmand.ItemIndex = 1) then
  begin
    lbChannel.Caption := 'Channel';
    seChannel.MaxValue:= 127;
    tbChannel.Max:=127;

    lbData1.Visible := True;
    lbData1.Caption := 'Controller';
    seData1.Visible := True;
    tbData1.Visible := True;

    lbData2.Visible := True;
    lbData2.Caption := 'Value';
    seData2.Visible := True;
    tbData2.Visible := True;
  end
  else if (cbMidiCOmmand.ItemIndex = 2) or (cbMidiCOmmand.ItemIndex = 3) then
  begin
    lbChannel.Caption := 'Channel';
    seChannel.MaxValue:= 127;
    tbChannel.Max:=127;

    lbData1.Visible := False;
    seData1.Visible := False;
    tbData1.Visible := False;

    seData2.Visible := False;
    tbData2.Visible := False;
    lbData2.Visible := False;
  end
  else if (cbMidiCOmmand.ItemIndex = 4) or (cbMidiCOmmand.ItemIndex = 5) then
  begin
    lbChannel.Caption := 'Channel';
    seChannel.MaxValue:= 127;
    tbChannel.Max:=127;

    lbData1.Visible := True;
    lbData1.Caption := 'Note';
    seData1.Visible := True;
    tbData1.Visible := True;

    lbData2.Visible := True;
    lbData2.Caption := 'Velocity';
    seData2.Visible := True;
    tbData2.Visible := True;
  end
  else if (cbMidiCOmmand.ItemIndex = 6) then
  begin
    seChannel.MaxValue:= 16000;
    tbChannel.Max:=16000;
    lbChannel.Caption := 'mseconds';

    lbData1.Visible := false;
    seData1.Visible := false;
    tbData1.Visible := false;

    lbData2.Visible := False;
    seData2.Visible := False;
    tbData2.Visible := False;
  end;
  SyncMidiData;
end;

procedure TfrmMidiData.SpinEditChange(Sender: TObject);
begin
  tbData1.Position := seData1.Value;
  tbData2.Position := seData2.Value;
  tbChannel.Position := seChannel.Value;
  SyncMidiData;
end;

procedure TfrmMidiData.TrackBarChange(Sender: TObject);
begin
  seData1.Value := tbData1.Position;
  seData2.Value := tbData2.Position;
  seChannel.Value := tbChannel.Position;
  SyncMidiData;
end;

procedure TfrmMidiData.SyncMidiData;
begin
  if (cbMidiCOmmand.ItemIndex = 0) then
    FMidiData.MidiType := PC;
  if (cbMidiCOmmand.ItemIndex = 1) then
    FMidiData.MidiType := CC;
  if (cbMidiCOmmand.ItemIndex = 2) then
    FMidiData.MidiType := PC_NEXT;
  if (cbMidiCOmmand.ItemIndex = 3) then
    FMidiData.MidiType := PC_PREV;
  if (cbMidiCOmmand.ItemIndex = 4) then
    FMidiData.MidiType := NON;
  if (cbMidiCOmmand.ItemIndex = 5) then
    FMidiData.MidiType := NOFF;
  if (cbMidiCOmmand.ItemIndex = 6) then
    FMidiData.MidiType := WAIT;

  FMidiData.Channel := seChannel.Value;
  FMidiData.Data1 := seData1.Value;
  FMidiData.Data2 := seData2.Value;
  if (cbMidiCOmmand.ItemIndex = 6) then
  begin
    FMidiData.Data1 := seChannel.Value DIV 127;
    FMidiData.Data2 := seChannel.Value MOD 127;
  end;
end;

constructor TfrmMidiData.Create(TheOwner: TComponent);
var
  i: integer;
begin
  inherited Create(TheOwner);
  FMidiData := TMidiData.Create;
  cbMidiCOmmand.items.Clear;
  for i := 0 to length(MidiCommands) - 1 do
  begin
    cbMidiCOmmand.items.add(MidiCommands[i]);
  end;

  cbMidiCommandChange(self);
end;

destructor TfrmMidiData.Destroy;
begin
  FMidiData.Free;
  inherited Destroy;
end;

procedure TfrmMidiData.SetMidiData(MyMidiData: TMidiData);
begin
  if (Assigned(FMidiData)) then
    FreeAndNil(FMidiData);
  FMidiData := MyMidiData;
end;

end.
