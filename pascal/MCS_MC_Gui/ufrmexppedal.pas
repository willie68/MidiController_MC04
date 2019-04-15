unit ufrmexppedal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, EditBtn, uModels;

type

  { TfrmExpPedal }

  TfrmExpPedal = class(TFrame)
    ebValueChange: TEditButton;
    GroupBox2: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label4: TLabel;
    procedure ebValueChangeButtonClick(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private
    FCaption: TCaption;
    FExpressionNumber: integer;
    FMidiChangeSequence: TMidiSequence;

    function GetSequences: TMidiSequenceArray;
    procedure SetCaption(Value: TCaption);
    procedure SetExpressionNumber(AValue: integer);
    procedure SetSequences(AValue: TMidiSequenceArray);

    procedure ClearSequences();

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property Caption: TCaption read FCaption write SetCaption;

    property ExpressionNumber: integer read FExpressionNumber write SetExpressionNumber;
    property MidiSequences: TMidiSequenceArray read GetSequences write SetSequences;
  end;

implementation

{$R *.lfm}

uses ufrmMidiSequenz;

{ TfrmExpPedal }

procedure TfrmExpPedal.Image1Click(Sender: TObject);
begin

end;

procedure TfrmExpPedal.ebValueChangeButtonClick(Sender: TObject);
var
  MidiDatas: TMidiDataArray;
  eb: TEditButton;
begin
  SetLength(MidiDatas, 0);
  if (Sender = ebValueChange) then
    MidiDatas := FMidiChangeSequence.Datas;
  FrmMidiSequenz.MidiDatas := MidiDatas;
  if (FrmMidiSequenz.ShowModal() = mrOk) then
  begin
    MidiDatas := FrmMidiSequenz.MidiDatas;
    if (Sender is TEditButton) then
    begin
      eb := Sender as TEditButton;
      eb.Text := getMidiDataString(MidiDatas);
      eb.Tag := Length(MidiDatas);
    end;
    if (Sender = ebValueChange) then
      FMidiChangeSequence.AddMidiDatas(MidiDatas);
  end;
end;

procedure TfrmExpPedal.SetCaption(Value: TCaption);
begin
  FCaption := Value;
  Label1.Caption := Value;
end;

function TfrmExpPedal.GetSequences: TMidiSequenceArray;
begin
  SetLength(Result, 0);
  if (Assigned(FMidiChangeSequence)) then
    if (Length(FMidiChangeSequence.Datas) > 0) then
    begin
      SetLength(Result, 1);
      FMidiChangeSequence.SequenceType := EXPRESSION;
      FMidiChangeSequence.Value := FExpressionNumber;
      FMidiChangeSequence.Event := VALUECHANGE;
      Result[0] := FMidiChangeSequence.Clone;
    end;
end;

procedure TfrmExpPedal.SetExpressionNumber(AValue: integer);
begin
  if FExpressionNumber = AValue then
    Exit;
  FExpressionNumber := AValue;
end;

procedure TfrmExpPedal.SetSequences(AValue: TMidiSequenceArray);
var
  i: integer;
  mySequence: TMidiSequence;
begin

  clearSequences();

  if (Assigned(AValue)) then
  begin
    for i := 0 to Length(AValue) - 1 do
    begin
      mySequence := AValue[i];
      if (mySequence.SequenceType = EXPRESSION) and
        (mySequence.Value = FExpressionNumber) then
      begin
        if (mySequence.Event = VALUECHANGE) then
        begin
          if (Assigned(FMidiChangeSequence)) then
            FreeAndNil(FMidiChangeSequence);
          FMidiChangeSequence := mySequence.Clone;
          ebValueChange.Text := getMidiDataString(FMidiChangeSequence.Datas);
        end;
      end;
    end;
  end;
end;

procedure TfrmExpPedal.ClearSequences();
begin
  if (Assigned(FMidiChangeSequence)) then
    FreeAndNil(FMidiChangeSequence);

  FMidiChangeSequence := TMidiSequence.Create;

  ebValueChange.Text := '';
end;

constructor TfrmExpPedal.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FMidiChangeSequence := TMidiSequence.Create;
  FMidiChangeSequence.SequenceType := EXPRESSION;
end;

destructor TfrmExpPedal.Destroy;
begin
  FMidiChangeSequence.Free;
  inherited Destroy;
end;

end.
