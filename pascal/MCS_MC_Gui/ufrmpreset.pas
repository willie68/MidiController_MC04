unit ufrmPreset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Spin, EditBtn, uModels;

type

  { TfrmPreset }

  TfrmPreset = class(TFrame)
    ebStart: TEditButton;
    ebStop: TEditButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lebName: TLabeledEdit;
    sePCNumber: TSpinEdit;
    seIntChannel: TSpinEdit;
    seExtChannel: TSpinEdit;
    procedure EditButtonButtonClick(Sender: TObject);
    function GetPreset: TMidiPreset;
    procedure SetPreset(AValue: TMidiPreset);
  private
    FMidiStartSequence: TMidiSequence;
    FMidiStopSequence: TMidiSequence;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property Preset: TMidiPreset read GetPreset write SetPreset;
  end;

implementation

{$R *.lfm}

uses ufrmMidiSequenz;

function getMidiDataString(MidiDatas: TMidiDataArray): string;
var
  i: integer;
begin
  for i := 0 to Length(MidiDatas) - 1 do
  begin
    if (i > 0) then
      Result := Result + ', ';
    Result := Result + MidiDatas[i].HumanString;
  end;
end;

{ TfrmPreset }

procedure TfrmPreset.EditButtonButtonClick(Sender: TObject);
var
  MidiDatas: TMidiDataArray;
  eb: TEditButton;
  i: integer;
  midiData: TMidiData;
  commandString: string;
begin
  SetLength(MidiDatas, 0);
  if (Sender = ebStart) then
    MidiDatas := FMidiStartSequence.Datas;
  if (Sender = ebStop) then
    MidiDatas := FMidiStopSequence.Datas;
  FrmMidiSequenz.MidiDatas := mididatas;
  if (FrmMidiSequenz.ShowModal() = mrOk) then
  begin
    commandString := '';
    MidiDatas := FrmMidiSequenz.MidiDatas;
    if (Sender is TEditButton) then
    begin
      eb := Sender as TEditButton;
      for i := 0 to Length(MidiDatas) - 1 do
      begin
        if (i > 0) then
          commandString := commandString + ', ';
        commandString := commandString + MidiDatas[i].HumanString;
      end;
      eb.Text := commandString;
      eb.Tag := Length(MidiDatas);
    end;
    if (Sender = ebStart) then
      FMidiStartSequence.AddMidiDatas(MidiDatas);
    if (Sender = ebStop) then
      FMidiStopSequence.AddMidiDatas(MidiDatas);
  end;
end;

procedure TfrmPreset.SetPreset(AValue: TMidiPreset);
var
  i: integer;
  mySequences: TMidiSequenceArray;
  mySequence: TMidiSequence;
begin
  lebName.Text := AValue.Name;
  sePCNumber.Value := AValue.ProgramNumber;
  seExtChannel.Value := AValue.ExternalMidi;
  seIntChannel.Value := AValue.InternalMidi;

  mySequences := AValue.Sequences;
  if (Assigned(mySequences)) then
  begin
    for i := 0 to Length(mySequences) - 1 do
    begin
      mySequence := mySequences[i];
      if (mySequence.SequenceType = INTERNAL) then
      begin
        if (mySequence.Event = START) then
        begin
          FMidiStartSequence := mySequence.Clone;
          ebStart.Text := getMidiDataString(FMidiStartSequence.Datas);
        end;
        if (mySequence.Event = STOP) then
        begin
          FMidiStopSequence := mySequence.Clone;
          ebStop.Text := getMidiDataString(FMidiStopSequence.Datas);
        end;
      end;
    end;
  end;
end;

function TfrmPreset.GetPreset: TMidiPreset;

begin
  Result := TMidiPreset.Create;
  Result.Name := lebName.Text;
  Result.ProgramNumber := sePCNumber.Value;
  Result.InternalMidi := seIntChannel.Value;
  Result.ExternalMidi := seExtChannel.Value;
  Result.AddSequence(FMidiStartSequence.Clone);
  Result.AddSequence(FMidiStopSequence.Clone);
end;

constructor TfrmPreset.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FMidiStartSequence := TMidiSequence.Create;
  FMidiStopSequence := TMidiSequence.Create;

  FMidiStartSequence.SequenceType := INTERNAL;
  FMidiStartSequence.Event := START;
  FMidiStopSequence.SequenceType := INTERNAL;
  FMidiStopSequence.Event := START;
end;

destructor TfrmPreset.Destroy;
begin
  if (Assigned(FMidiStartSequence)) then
    FreeAndNil(FMidiStartSequence);
  if (Assigned(FMidiStopSequence)) then
    FreeAndNil(FMidiStopSequence);
  inherited Destroy;
end;

end.
