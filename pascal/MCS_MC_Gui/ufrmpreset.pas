unit ufrmPreset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Spin, EditBtn;

type

  { TfrmPreset }

  TfrmPreset = class(TFrame)
    EditButton1: TEditButton;
    EditButton2: TEditButton;
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

  public

  end;

implementation

{$R *.lfm}

uses ufrmMidiSequenz;

{ TfrmPreset }

procedure TfrmPreset.EditButtonButtonClick(Sender: TObject);
begin
  FrmMidiSequenz.Show();
end;

end.

