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
    procedure ebLongClickChange(Sender: TObject);
    procedure FrameClick(Sender: TObject);
  private
    FCaption: TCaption;

    FButton: TMidiButton;

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
begin
  FrmMidiSequenz.Show();
end;

procedure TfrmMidiSwitch.ebLongClickChange(Sender: TObject);
begin

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
    btnColor.ButtonColor:= FButton.Color;
  end;
end;

end.

