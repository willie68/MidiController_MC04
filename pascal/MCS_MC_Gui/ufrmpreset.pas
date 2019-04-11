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
    LabeledEdit1: TLabeledEdit;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    procedure EditButtonButtonClick(Sender: TObject);
  private
    FMidiStartSequence: TMidiSequence;
    FMidiStopSequence: TMidiSequence;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

uses ufrmMidiSequenz;

{ TfrmPreset }

procedure TfrmPreset.EditButtonButtonClick(Sender: TObject);
var
  MidiDatas: TMidiDataArray;
  eb: TEditButton;
  i: integer;
  midiData: TMidiData;
  commandString: string;
begin
  if (FrmMidiSequenz.ShowModal() = mrOK) then
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

constructor TfrmPreset.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FMidiStartSequence := TMidiSequence.Create;
  FMidiStopSequence := TMidiSequence.Create;

  FMidiStartSequence.SequenceType := INTERNAL;
  FMidiStopSequence.SequenceType := INTERNAL;
end;

destructor TfrmPreset.Destroy;
begin
  FMidiStartSequence.Free;
  FMidiStopSequence.Free;
  inherited Destroy;
end;

end.

