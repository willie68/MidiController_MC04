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
    FCaption: TCaption;

    FButton: TMidiButton;
    FMidiClickSequence: TSequence;
    FMidiDblClickSequence: TSequence;
    FMidiLongClickSequence: TSequence;
    FMidiPushSequence: TSequence;
    FMidiReleaseSequence: TSequence;

    procedure SetCaption(Value: TCaption);

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetMidiButton(MidiButton: TMidiButton);
    procedure SetProperties;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property MidiButton: TMidiButton read FButton;
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
  FrmMidiSequenz.ShowModal();
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

procedure TfrmMidiSwitch.SetCaption(Value: TCaption);
begin
  FCaption := Value;
  Label1.Caption := Value;
end;

constructor TfrmMidiSwitch.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FButton := TMidiButton.Create;
  FMidiClickSequence := TSequence.Create;
  FMidiDblClickSequence := TSequence.Create;
  FMidiLongClickSequence := TSequence.Create;
  FMidiPushSequence := TSequence.Create;
  FMidiReleaseSequence := TSequence.Create;
  SetProperties;
end;

destructor TfrmMidiSwitch.Destroy;
begin
  FButton.Free;
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
