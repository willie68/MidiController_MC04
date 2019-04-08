unit mainUi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, JSONPropStorage,
  ExtCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    JSONPropStorage1: TJSONPropStorage;
    StatusBar1: TStatusBar;
    TrayIcon1: TTrayIcon;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

