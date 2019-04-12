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
    cbMidiCommand: TComboBox;
    GroupBox1: TGroupBox;
    lbChannel: TLabel;
    lbData1: TLabel;
    lbData2: TLabel;
    sbRemove: TSpeedButton;
    seData2: TSpinEdit;
    seData1: TSpinEdit;
    seChannel: TSpinEdit;
    sbUp: TSpeedButton;
    sbDown: TSpeedButton;
    tbData2: TTrackBar;
    tbData1: TTrackBar;
    tbChannel: TTrackBar;
    procedure cbMidiCommandChange(Sender: TObject);
    procedure sbRemoveClick(Sender: TObject);
    procedure SpinEditChange(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
  private
    function GetMidiData: TMidiData;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetMidiData(MyMidiData: TMidiData);
  published
    property MidiData: TMidiData read GetMidiData write SetMidiData;
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
    seChannel.MaxValue := 127;
    tbChannel.Max := 127;

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
    seChannel.MaxValue := 127;
    tbChannel.Max := 127;

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
    seChannel.MaxValue := 127;
    tbChannel.Max := 127;

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
    seChannel.MaxValue := 127;
    tbChannel.Max := 127;

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
    seChannel.MaxValue := 16000;
    tbChannel.Max := 16000;
    lbChannel.Caption := 'mseconds';

    lbData1.Visible := False;
    seData1.Visible := False;
    tbData1.Visible := False;

    lbData2.Visible := False;
    seData2.Visible := False;
    tbData2.Visible := False;
  end;
end;

procedure TfrmMidiData.SpinEditChange(Sender: TObject);
begin
  tbData1.Position := seData1.Value;
  tbData2.Position := seData2.Value;
  tbChannel.Position := seChannel.Value;
end;

procedure TfrmMidiData.TrackBarChange(Sender: TObject);
begin
  seData1.Value := tbData1.Position;
  seData2.Value := tbData2.Position;
  seChannel.Value := tbChannel.Position;
end;

function TfrmMidiData.GetMidiData: TMidiData;
begin
  Result := TMidiData.Create;
  if (cbMidiCOmmand.ItemIndex = 0) then
    Result.MidiType := PC;
  if (cbMidiCOmmand.ItemIndex = 1) then
    Result.MidiType := CC;
  if (cbMidiCOmmand.ItemIndex = 2) then
    Result.MidiType := PC_NEXT;
  if (cbMidiCOmmand.ItemIndex = 3) then
    Result.MidiType := PC_PREV;
  if (cbMidiCOmmand.ItemIndex = 4) then
    Result.MidiType := NON;
  if (cbMidiCOmmand.ItemIndex = 5) then
    Result.MidiType := NOFF;
  if (cbMidiCOmmand.ItemIndex = 6) then
    Result.MidiType := WAIT;

  Result.Channel := seChannel.Value;
  Result.Data1 := seData1.Value;
  Result.Data2 := seData2.Value;
  if (cbMidiCOmmand.ItemIndex = 6) then
  begin
    Result.Data1 := seChannel.Value div 127;
    Result.Data2 := seChannel.Value mod 127;
  end;
end;

constructor TfrmMidiData.Create(TheOwner: TComponent);
var
  i: integer;
begin
  inherited Create(TheOwner);
  cbMidiCommand.items.Clear;
  for i := 0 to length(MidiCommands) - 1 do
  begin
    cbMidiCOmmand.items.add(MidiCommands[i]);
  end;

  cbMidiCommandChange(self);
end;

destructor TfrmMidiData.Destroy;
begin
  inherited Destroy;
end;

procedure TfrmMidiData.SetMidiData(MyMidiData: TMidiData);
begin
  case MyMidiData.MidiType of
    PC: cbMidiCOmmand.ItemIndex := 0;
    CC: cbMidiCOmmand.ItemIndex := 1;
    PC_NEXT: cbMidiCOmmand.ItemIndex := 2;
    PC_PREV: cbMidiCOmmand.ItemIndex := 3;
    NON: cbMidiCOmmand.ItemIndex := 4;
    NOFF: cbMidiCOmmand.ItemIndex := 5;
    WAIT: cbMidiCOmmand.ItemIndex := 6;
  end;
  cbMidiCommandChange(self);

  seChannel.Value := MyMidiData.Channel;
  seData1.Value := MyMidiData.Data1;
  seData2.Value := MyMidiData.Data2;
  if (MyMidiData.MidiType = WAIT) then
  begin
    seChannel.Value := MyMidiData.Data1 * 127 + MyMidiData.Data2;
  end;
end;

end.
