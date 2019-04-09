unit ufrmMidiSwitch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, EditBtn;

type

  { TfrmMidiSwitch }

  TfrmMidiSwitch = class(TFrame)
    ColorButton1: TColorButton;
    EditButton1: TEditButton;
    EditButton2: TEditButton;
    EditButton3: TEditButton;
    EditButton4: TEditButton;
    EditButton5: TEditButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LabeledEdit1: TLabeledEdit;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure FrameClick(Sender: TObject);
  private
    FCaption : TCaption;
    procedure SetCaption(value : TCaption);
  public
     property Caption: TCaption read FCaption write SetCaption;
  end;

implementation

{$R *.lfm}

{ TfrmMidiSwitch }

procedure TfrmMidiSwitch.FrameClick(Sender: TObject);
begin

end;

procedure TfrmMidiSwitch.SetCaption(value: TCaption);
begin
  FCaption := value;
  Label1.Caption:= value;
end;

end.

