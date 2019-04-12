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
    FMidiChangeSequence: TMidiSequence;

    procedure SetCaption(Value: TCaption);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property Caption: TCaption read FCaption write SetCaption;
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
  i: integer;
  midiData: TMidiData;
  commandString: string;
begin
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
    if (Sender = ebValueChange) then
      FMidiChangeSequence.AddMidiDatas(MidiDatas);
  end;
end;

procedure TfrmExpPedal.SetCaption(Value: TCaption);
begin
  FCaption := Value;
  Label1.Caption := Value;
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

