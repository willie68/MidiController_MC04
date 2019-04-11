unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, uModels;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses fpjson, jsonparser;
{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var midiData : TMidiData;
    jsonData : TJsonObject;
begin
  midiData := TMidiData.Create;
  midiData.Channel:= 12;
  midiData.Data1:=23;
  midiData.Data2:=12;
  midiData.MidiType:=PC;

  memo1.Append(midiData.HumanString);

  jsonData := midiData.toJson;
  memo1.Append(jsonData.AsJSON);
  midiData.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  memo1.Clear;
end;

end.

