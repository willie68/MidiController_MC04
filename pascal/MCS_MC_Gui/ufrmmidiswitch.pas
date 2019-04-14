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
  private
    FButtonNumber: integer;

    FMidiClickSequence: TMidiSequence;
    FMidiDblClickSequence: TMidiSequence;
    FMidiLongClickSequence: TMidiSequence;
    FMidiPushSequence: TMidiSequence;
    FMidiReleaseSequence: TMidiSequence;

    function GetButton: TMidiButton;
    function GetSequences: TMidiSequenceArray;
    procedure SetButton(AValue: TMidiButton);
    procedure SetButtonNumber(AValue: integer);
    function GetSequencesCount: integer;
    procedure SetSequences(AValue: TMidiSequenceArray);

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property MidiButton: TMidiButton read GetButton write SetButton;
    property MidiSequences: TMidiSequenceArray read GetSequences write SetSequences;
    property ButtonNumber: integer read FButtonNumber write SetButtonNumber;
  end;

implementation

{$R *.lfm}

uses ufrmMidiSequenz;

{ TfrmMidiSwitch }

procedure TfrmMidiSwitch.ebClickButtonClick(Sender: TObject);
var
  MidiDatas: TMidiDataArray;
  eb: TEditButton;
  i: integer;
  midiData: TMidiData;
  commandString: string;
begin
  SetLength(MidiDatas, 0);
  if (Sender = ebClick) then
    MidiDatas := FMidiClickSequence.Datas;
  if (Sender = ebDblClick) then
    MidiDatas := FMidiDblClickSequence.Datas;
  if (Sender = ebLongClick) then
    MidiDatas := FMidiLongClickSequence.Datas;
  if (Sender = ebPush) then
    MidiDatas := FMidiPushSequence.Datas;
  if (Sender = ebRelease) then
    MidiDatas := FMidiReleaseSequence.Datas;
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

procedure TfrmMidiSwitch.SetSequences(AValue: TMidiSequenceArray);
var
  i: integer;
  mySequence: TMidiSequence;
begin
  if (Assigned(AValue)) then
  begin
    for i := 0 to Length(AValue) - 1 do
    begin
      mySequence := AValue[i];
      if (mySequence.SequenceType = BUTTON) AND (mySequence.Value = FButtonNumber) then
      begin
        if (mySequence.Event = PUSH) then
        begin
          FMidiPushSequence := mySequence.Clone;
          ebPush.Text:= getMidiDataString(FMidiPushSequence.Datas);
        end;
        if (mySequence.Event = Release) then
        begin
          FMidiReleaseSequence := mySequence.Clone;
          ebRelease.Text:= getMidiDataString(FMidiReleaseSequence.Datas);
        end;
        if (mySequence.Event = SINGLECLICK) then
        begin
          FMidiClickSequence := mySequence.Clone;
          ebClick.Text:= getMidiDataString(FMidiClickSequence.Datas);
        end;
        if (mySequence.Event = DOUBLECLICK) then
        begin
          FMidiDblClickSequence := mySequence.Clone;
          ebDblClick.Text:= getMidiDataString(FMidiDblClickSequence.Datas);
        end;
        if (mySequence.Event = LONGCLICK) then
        begin
          FMidiLongClickSequence := mySequence.Clone;
          ebLongClick.Text:= getMidiDataString(FMidiLongClickSequence.Datas);
        end;
      end;
    end;
  end;

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
      FMidiClickSequence.Value := FButtonNumber;
      FMidiClickSequence.Event := SINGLECLICK;
      Result[Count] := FMidiClickSequence.Clone;
      Inc(Count);
    end;
  if (Assigned(FMidiDblClickSequence)) then
    if (Length(FMidiDblClickSequence.Datas) > 0) then
    begin
      FMidiDblClickSequence.Value := FButtonNumber;
      FMidiDblClickSequence.Event := DOUBLECLICK;
      Result[Count] := FMidiDblClickSequence.Clone;
      Inc(Count);
    end;
  if (Assigned(FMidiLongClickSequence)) then
    if (Length(FMidiLongClickSequence.Datas) > 0) then
    begin
      FMidiLongClickSequence.Value := FButtonNumber;
      FMidiLongClickSequence.Event := LONGCLICK;
      Result[Count] := FMidiLongClickSequence.Clone;
      Inc(Count);
    end;
  if (Assigned(FMidiPushSequence)) then
    if (Length(FMidiPushSequence.Datas) > 0) then
    begin
      FMidiPushSequence.Value := FButtonNumber;
      FMidiPushSequence.Event := PUSH;
      Result[Count] := FMidiPushSequence.Clone;
      Inc(Count);
    end;
  if (Assigned(FMidiReleaseSequence)) then
    if (Length(FMidiReleaseSequence.Datas) > 0) then
    begin
      FMidiReleaseSequence.Value := FButtonNumber;
      FMidiReleaseSequence.Event := Release;
      Result[Count] := FMidiReleaseSequence.Clone;
      Inc(Count);
    end;
end;

procedure TfrmMidiSwitch.SetButton(AValue: TMidiButton);
begin
  lbName.Text := AValue.Name;
  if AValue.ButtonType = MOMENTARY then
    rbMomentary.Checked := True;
  if AValue.ButtonType = SWITCH then
    rbToggle.Checked := True;
  btnColor.ButtonColor := AValue.Color;
end;

function TfrmMidiSwitch.GetButton: TMidiButton;
begin
  Result := TMidiButton.Create;
  Result.Name := lbName.Text;
  if rbMomentary.Checked then
    Result.ButtonType := MOMENTARY;
  if rbToggle.Checked then
    Result.ButtonType := SWITCH;
  Result.Color := btnColor.ButtonColor;
end;

procedure TfrmMidiSwitch.SetButtonNumber(AValue: integer);
begin
  FButtonNumber := AValue;

  FMidiClickSequence.Value := FButtonNumber;
  FMidiDblClickSequence.Value := FButtonNumber;
  FMidiLongClickSequence.Value := FButtonNumber;
  FMidiPushSequence.Value := FButtonNumber;
  FMidiReleaseSequence.Value := FButtonNumber;

  Label1.Caption := 'Switch ' + IntToStr(AValue);
end;

constructor TfrmMidiSwitch.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FMidiClickSequence := TMidiSequence.Create;
  FMidiClickSequence.SequenceType := BUTTON;
  FMidiClickSequence.Event := SINGLECLICK;

  FMidiDblClickSequence := TMidiSequence.Create;
  FMidiDblClickSequence.SequenceType := BUTTON;
  FMidiDblClickSequence.Event := DOUBLECLICK;

  FMidiLongClickSequence := TMidiSequence.Create;
  FMidiLongClickSequence.SequenceType := BUTTON;
  FMidiLongClickSequence.Event := LONGCLICK;

  FMidiPushSequence := TMidiSequence.Create;
  FMidiPushSequence.SequenceType := BUTTON;
  FMidiPushSequence.Event := PUSH;

  FMidiReleaseSequence := TMidiSequence.Create;
  FMidiReleaseSequence.SequenceType := BUTTON;
  FMidiReleaseSequence.Event := Release;
end;

destructor TfrmMidiSwitch.Destroy;
begin
  if (Assigned(FMidiClickSequence)) then
    FreeAndNil(FMidiClickSequence);
  if (Assigned(FMidiDblClickSequence)) then
    FreeAndNil(FMidiDblClickSequence);
  if (Assigned(FMidiLongClickSequence)) then
    FreeAndNil(FMidiLongClickSequence);
  if (Assigned(FMidiPushSequence)) then
    FreeAndNil(FMidiPushSequence);
  if (Assigned(FMidiReleaseSequence)) then
    FreeAndNil(FMidiReleaseSequence);

  inherited Destroy;
end;

end.
