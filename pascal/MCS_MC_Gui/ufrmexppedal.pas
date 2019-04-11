unit ufrmexppedal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, EditBtn, uModels;

type

  { TfrmExpPedal }

  TfrmExpPedal = class(TFrame)
    EditButton1: TEditButton;
    GroupBox2: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label4: TLabel;
    procedure EditButtonButtonClick(Sender: TObject);
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

procedure TfrmExpPedal.EditButtonButtonClick(Sender: TObject);
begin
  FrmMidiSequenz.Show();
end;

procedure TfrmExpPedal.SetCaption(Value: TCaption);
begin
  FCaption := Value;
  Label1.Caption := Value;
end;

constructor TfrmExpPedal.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FMidiChangeSequence:= TMidiSequence.Create;
  FMidiChangeSequence.SequenceType := EXPRESSION;
end;

destructor TfrmExpPedal.Destroy;
begin
  FMidiChangeSequence.free;
  inherited Destroy;
end;

end.

