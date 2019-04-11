unit ufrmMidiSwitch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, EditBtn, uModels;

type

  { TfrmMidiSwitch }

  TfrmMidiSwitch = class(TFrame)
    btnColor: TColorButton;
    ebClick: TEditButton;
    ebDblClick: TEditButton;
    ebLongClick: TEditButton;
    ebPush: TEditButton;
    ebRelease: TEditButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lbName: TLabeledEdit;
    rbToggle: TRadioButton;
    rbMomentary: TRadioButton;
    procedure ebClickButtonClick(Sender: TObject);
    procedure FrameClick(Sender: TObject);
  private
    FButtonNumber: integer;
    FCaption: TCaption;

    FButton: TMidiButton;
    FMidiClickSequence: TMidiSequence;
    FMidiDblClickSequence: TMidiSequence;
    FMidiLongClickSequence: TMidiSequence;
    FMidiPushSequence: TMidiSequence;
    FMidiReleaseSequence: TMidiSequence;

    function GetSequences: TMidiSequenceArray;
    procedure SetButtonNumber(AValue: integer);
    procedure SetCaption(Value: TCaption);
    function GetSequencesCount: integer;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetMidiButton(MidiButton: TMidiButton);
    procedure SetProperties;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property MidiButton: TMidiButton read FButton;
    property MidiSequences: TMidiSequenceArray read GetSequences;
    property ButtonNumber: integer read FButtonNumber write SetButtonNumber;
  end;

implementation

{$R *.lfm}

uses ufrmMidiSequenz;

{ TfrmMidiSwitch }

procedure TfrmMidiSwitch.FrameClick(Sender: TObject);
begin

end;

procedure TfrmMidiSwitch.ebClickButtonClick(Sender: TObject);
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
    if (Sender = ebClick) then
      FMidiClickSequence.AddMidiDatas(MidiDatas);
    if (Sender = ebDblClick) then
      FMidiDblClickSequence.AddMidiDatas(MidiDatas);
    if (Sender = ebLongClick) then
      FMidiLongClickSequence.AddMidiDatas(MidiDatas);
    if (Sender = ebPush) then
      FMidiPushSequence.AddMidiDatas(MidiDatas);
    if (Sender = ebRelease) then
      FMidiReleaseSequence.AddMidiDatas(MidiDatas);
  end;
end;

procedure TfrmMidiSwitch.SetCaption(Value: TCaption);
begin
  FCaption := Value;
  Label1.Caption := Value;
end;

function TfrmMidiSwitch.GetSequencesCount: integer;
begin
  Result := 0;
  if (Assigned(FMidiClickSequence)) then
    if (Length(FMidiClickSequence.Datas) > 0) then
      Inc(Result);
  if (Assigned(FMidiDblClickSequence)) then
    if (Length(FMidiDblClickSequence.Datas) > 0) then
      Inc(Result);
  if (Assigned(FMidiLongClickSequence)) then
    if (Length(FMidiLongClickSequence.Datas) > 0) then
      Inc(Result);
  if (Assigned(FMidiPushSequence)) then
    if (Length(FMidiPushSequence.Datas) > 0) then
      Inc(Result);
  if (Assigned(FMidiReleaseSequence)) then
    if (Length(FMidiReleaseSequence.Datas) > 0) then
      Inc(Result);

end;

function TfrmMidiSwitch.GetSequences: TMidiSequenceArray;
var
  Count: integer;
begin
  SetLength(Result, GetSequencesCount);
  Count := 0;
  if (Assigned(FMidiClickSequence)) then
    if (Length(FMidiClickSequence.Datas) > 0) then
    begin
      Result[Count] := FMidiClickSequence;
      Inc(Count);
    end;
  if (Assigned(FMidiDblClickSequence)) then
    if (Length(FMidiDblClickSequence.Datas) > 0) then
    begin
      Result[Count] := FMidiDblClickSequence;
      Inc(Count);
    end;
  if (Assigned(FMidiLongClickSequence)) then
    if (Length(FMidiLongClickSequence.Datas) > 0) then
    begin
      Result[Count] := FMidiLongClickSequence;
      Inc(Count);
    end;
  if (Assigned(FMidiPushSequence)) then
    if (Length(FMidiPushSequence.Datas) > 0) then
    begin
      Result[Count] := FMidiPushSequence;
      Inc(Count);
    end;
  if (Assigned(FMidiReleaseSequence)) then
    if (Length(FMidiReleaseSequence.Datas) > 0) then
    begin
      Result[Count] := FMidiReleaseSequence;
      Inc(Count);
    end;
end;

procedure TfrmMidiSwitch.SetButtonNumber(AValue: integer);
begin
  FButtonNumber := AValue;

  FMidiClickSequence.Value := FButtonNumber;
  FMidiDblClickSequence.Value := FButtonNumber;
  FMidiLongClickSequence.Value := FButtonNumber;
  FMidiPushSequence.Value := FButtonNumber;
  FMidiReleaseSequence.Value := FButtonNumber;
end;

constructor TfrmMidiSwitch.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FButton := TMidiButton.Create;

  FMidiClickSequence := TMidiSequence.Create;
  FMidiDblClickSequence := TMidiSequence.Create;
  FMidiLongClickSequence := TMidiSequence.Create;
  FMidiPushSequence := TMidiSequence.Create;
  FMidiReleaseSequence := TMidiSequence.Create;

  FMidiClickSequence.SequenceType := BUTTON;
  FMidiDblClickSequence.SequenceType := BUTTON;
  FMidiLongClickSequence.SequenceType := BUTTON;
  FMidiPushSequence.SequenceType := BUTTON;
  FMidiReleaseSequence.SequenceType := BUTTON;

  SetProperties;
end;

destructor TfrmMidiSwitch.Destroy;
begin
  FButton.Free;

  FMidiClickSequence.Free;
  FMidiDblClickSequence.Free;
  FMidiLongClickSequence.Free;
  FMidiPushSequence.Free;
  FMidiReleaseSequence.Free;

  inherited Destroy;
end;

procedure TfrmMidiSwitch.SetMidiButton(MidiButton: TMidiButton);
begin
  if Assigned(FButton) then
    FreeAndNil(FButton);
  FButton := MidiButton;
  SetProperties();
end;

procedure TfrmMidiSwitch.SetProperties;
begin
  if (Assigned(FButton)) then
  begin
    lbName.Text := FButton.Name;
    rbMomentary.Checked := (FButton.ButtonType = MOMENTARY);
    rbToggle.Checked := (FButton.ButtonType = TOGGLE);
    btnColor.ButtonColor := FButton.Color;
  end;
end;

end.
