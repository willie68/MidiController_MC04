unit uModels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, contnrs, fpjson, jsonparser;

type
  TMidiButton = class;
  TMidiSequence = class;
  TMidiData = class;
  TMidiPreset = class;
  TMidiDataArray = array of TMidiData;
  TMidiSequenceArray = array of TMidiSequence;

  { TMidiPresets }

  TMidiPresets = class
    FPresets: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
  published
  end;

  { TMidiPreset }

  TMidiPreset = class
  private
    FName: string;
    FPrgNumber: integer;
    FInternalMidi: integer;
    FExternalMidi: integer;
    FButtons: TObjectList;
    FSequences: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddButton(Button: TMidiButton);
  published
    property Name: string read FName write FName;
    property ProgramNumber: integer read FPrgNumber write FPrgNumber default 0;
    property InternalMidi: integer read FInternalMidi write FInternalMidi default 0;
    property ExternalMidi: integer read FExternalMidi write FExternalMidi default 0;
  end;

  TMidiButtonType = (MOMENTARY, TOGGLE);

  TMidiButton = class
  private
    FName: string;
    FType: TMidiButtonType;
    FColor: TColor;
  public
  published
    property Name: string read FName write FName;
    property ButtonType: TMidiButtonType read FType write FType default MOMENTARY;
    property Color: TColor read FColor write FColor default 0;
  end;

  TMidiSequenceType = (INTERNAL, BUTTON, EXPRESSION);
  TMidiSequenceEvent = (PUSH, RELEASE, START, STOP, CLICK, DOUBLECLICK,
    LONGCLICK, VALUECHANGE);

  { TMidiSequence }

  TMidiSequence = class
  private
    FSequenceType: TMidiSequenceType;
    FEvent: TMidiSequenceEvent;
    FDatas: TMidiDataArray;
    FValue: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddMidiDatas(MidiDatas: TMidiDataArray);
  published
    property SequenceType: TMidiSequenceType read FSequenceType write FSequenceType;
    property Event: TMidiSequenceEvent read FEvent write FEvent;
    property Datas: TMidiDataArray read FDatas;
    property Value: integer read FValue write FValue;
  end;

  TMidiDataType = (PC, CC, NON, NOFF, PC_PREV, PC_NEXT, WAIT);

  { TMidiData }

  TMidiData = class
  private
    FMidiType: TMidiDataType;
    FChannel: byte;
    FData1: byte;
    FData2: byte;
    function GetHumanString: string;
    function GetJson: TJsonObject;
    procedure SetHumanString(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    function clone(): TMidiData;
  published
    property MidiType: TMidiDataType read FMidiType write FMidiType;
    property Channel: byte read FChannel write FChannel;
    property Data1: byte read FData1 write FData1;
    property Data2: byte read FData2 write FData2;
    property HumanString: string read GetHumanString write SetHumanString;
    property toJson: TJsonObject read GetJson;
  end;

implementation

function MidiDataTypeToString(mididatatype: TMidiDataType): string;
begin
  case mididatatype of
    PC: Result := 'PC';
    CC: Result := 'CC';
    NON: Result := 'NOTE_ON';
    NOFF: Result := 'NOTE_OFF';
    PC_PREV: Result := 'PC_PREV';
    PC_NEXT: Result := 'PC_NEXT';
    WAIT: Result := 'WAIT';
  end;
end;

{ TMidiData }

function TMidiData.GetHumanString: string;
var
  shortType, mask: string;
begin
  case FMidiType of
    PC:
    begin
      shortType := 'PC';
      mask := '%0:s%2:.2d@%1:.2d';
    end;
    CC:
    begin
      shortType := 'CC';
      mask := '%0:s%2:.2d:%3:d@%1:.2d';
    end;
    NON:
    begin
      shortType := 'NOTE_ON';
      mask := '%0:s%2:.2d:%3:d@%1:.2d';
    end;
    NOFF:
    begin
      shortType := 'NOTE_OFF';
      mask := '%0:s%2:.2d:%3:d@%1:.2d';
    end;
    PC_PREV:
    begin
      shortType := 'PC_PREV';
      mask := '%0:s@%1:.2d';
    end;
    PC_NEXT:
    begin
      shortType := 'PC_NEXT';
      mask := '%0:s@%1:.2d';
    end;
    WAIT:
    begin
      shortType := 'WAIT';
      mask := '%0:s@%4:d';
    end;
  end;
  Result := Format(mask, [shortType, FChannel, FData1, FData2, (FData1 * 127) + FData2]);
end;

function TMidiData.GetJson: TJsonObject;
begin
  Result := TJSONObject.Create;
  Result.Add('Type', MidiDataTypeToString(FMidiType));
  Result.Add('Channel', FChannel);
  Result.Add('Data1', FData1);
  Result.Add('Data2', FData2);
end;

procedure TMidiData.SetHumanString(AValue: string);
begin

end;

constructor TMidiData.Create;
begin

end;

destructor TMidiData.Destroy;
begin
  inherited Destroy;
end;

function TMidiData.clone(): TMidiData;
begin
  Result := TMidiData.Create;
  Result.FData1 := self.FData1;
  Result.FData2 := self.FData2;
  Result.FChannel:= self.FChannel;
  Result.FMidiType:=self.FMidiType;
end;

{ TMidiPresets }

constructor TMidiPresets.Create;
begin
  FPresets := TObjectList.Create(True);
end;

destructor TMidiPresets.Destroy;
begin
  inherited Destroy;
  FPresets.Free;
end;

{ TMidiSequence }

constructor TMidiSequence.Create;
begin
end;

destructor TMidiSequence.Destroy;
var i : integer;
begin
  if (Assigned(FDatas)) then
  begin
    for i := 0 to Length(FDatas)-1 do
      FDatas[i].Free;
  end;
  SetLength(FDatas,0);
  inherited Destroy;
end;

procedure TMidiSequence.AddMidiDatas(MidiDatas: TMidiDataArray);
begin
  FDatas := MidiDatas;
end;

{ TMidiPreset }

constructor TMidiPreset.Create;
begin
  FButtons := TObjectList.Create;
  FButtons.OwnsObjects := True;

  FSequences := TObjectList.Create;
  FSequences.OwnsObjects := True;
end;

destructor TMidiPreset.Destroy;
begin
  inherited Destroy;
  FButtons.Free;
  FSequences.Free;
end;

procedure TMidiPreset.AddButton(Button: TMidiButton);
begin
  FButtons.Add(Button);
end;

end.
