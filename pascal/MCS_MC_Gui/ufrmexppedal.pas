unit ufrmexppedal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, EditBtn;

type

  { TfrmExpPedal }

  TfrmExpPedal = class(TFrame)
    EditButton1: TEditButton;
    GroupBox2: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label4: TLabel;
    procedure Image1Click(Sender: TObject);
  private
    FCaption: TCaption;
    procedure SetCaption(Value: TCaption);
  public
    property Caption: TCaption read FCaption write SetCaption;

  end;

implementation

{$R *.lfm}

{ TfrmExpPedal }

procedure TfrmExpPedal.Image1Click(Sender: TObject);
begin

end;

procedure TfrmExpPedal.SetCaption(Value: TCaption);
begin
  FCaption := Value;
  Label1.Caption := Value;
end;

end.

