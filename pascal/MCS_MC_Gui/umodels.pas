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
  TMidiButtonArray = array of TMidiButton;
  TMidiDataArray = array of TMidiData;
  TMidiSequenceArray = array of TMidiSequence;
  TMidiPresets = array of TMidiPreset;

  { TMidiPreset }

  TMidiPreset = class
  private
    FName: string;
    FPrgNumber: integer;
    FInternalMidi: integer;
    FExternalMidi: integer;
    FButtons: TMidiButtonArray;
    FSequences: TMidiSequenceArray;
    function GetJson: TJsonObject;
    procedure SetButtons(AValue: TMidiButtonArray);
    procedure SetSequences(AValue: TMidiSequenceArray);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddButton(Button: TMidiButton);
    procedure ClearButtons;
    procedure AddSequence(Sequence: TMidiSequence);
    procedure ClearSequences;
  published
    property Name: string read FName write FName;
    property ProgramNumber: integer read FPrgNumber write FPrgNumber default 0;
    property InternalMidi: integer read FInternalMidi write FInternalMidi default 0;
    property ExternalMidi: integer read FExternalMidi write FExternalMidi default 0;
    property Buttons: TMidiButtonArray read FButtons write SetButtons;
    property Sequences: TMidiSequenceArray read FSequences write SetSequences;
    property toJson: TJsonObject read GetJson;
  end;

  TMidiButtonType = (MOMENTARY, TOGGLE);

  { TMidiButton }

  TMidiButton = class
  private
    FName: string;
    FType: TMidiButtonType;
    FColor: TColor;
    function GetJson: TJsonObject;
  public
  published
    property Name: string read FName write FName;
    property ButtonType: TMidiButtonType read FType write FType default MOMENTARY;
    property Color: TColor read FColor write FColor default 0;
    property toJson: TJsonObject read GetJson;
  end;

  TMidiSequenceType = (INTERNAL, BUTTON, EXPRESSION);
  TMidiSequenceEvent = (PUSH, Release, START, STOP, CLICK, DOUBLECLICK,
    LONGCLICK, VALUECHANGE);

  { TMidiSequence }

  TMidiSequence = class
  private
    FSequenceType: TMidiSequenceType;
    FEvent: TMidiSequenceEvent;
    FDatas: TMidiDataArray;
    FValue: integer;
    function GetJson: TJsonObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddMidiDatas(MidiDatas: TMidiDataArray);
  published
    property SequenceType: TMidiSequenceType read FSequenceType write FSequenceType;
    property Event: TMidiSequenceEvent read FEvent write FEvent;
    property Datas: TMidiDataArray read FDatas;
    property Value: integer read FValue write FValue;
    property toJson: TJsonObject read GetJson;
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

function MidiButtonTypeToString(midiButtonType: TMidiButtonType): string;
begin
  case midiButtonType of
    MOMENTARY: Result := 'MOMENTARY';
    TOGGLE: Result := 'TOGGLE';
  end;
end;

function MidiSequenceEventToString(midiSequenceEvent: TMidiSequenceEvent): string;
begin
  case midiSequenceEvent of
    PUSH: Result := 'PUSH';
    Release: Result := 'RELEASE';
    START: Result := 'START';
    STOP: Result := 'STOP';
    CLICK: Result := 'CLICK';
    DOUBLECLICK: Result := 'DOUBLECLICK';
    LONGCLICK: Result := 'LONGCLICK';
    VALUECHANGE: Result := 'VALUECHANGE';
  end;
end;

function MidiSequenceTypeToString(midiSequenceType: TMidiSequenceType): string;
begin
  case midiSequenceType of
    INTERNAL: Result := 'INTERNAL';
    BUTTON: Result := 'BUTTON';
    EXPRESSION: Result := 'EXPRESSION';
  end;
end;

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

{ TMidiButton }

function TMidiButton.GetJson: TJsonObject;
begin
  Result := TJSONObject.Create;
  Result.Add('name', FName);
  Result.Add('type', MidiButtonTypeToString(FType));
  Result.Add('color', '0x' + IntToHex(FColor, 6));
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
  Result.Add('type', MidiDataTypeToString(FMidiType));
  Result.Add('channel', FChannel);
  Result.Add('data1', FData1);
  Result.Add('data2', FData2);
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
  Result.FChannel := self.FChannel;
  Result.FMidiType := self.FMidiType;
end;

{ TMidiSequence }

function TMidiSequence.GetJson: TJsonObject;
var
  jsonDatas: TJsonArray;
  i: integer;
begin
  Result := TJSONObject.Create;
  Result.Add('type', MidiSequenceTypeToString(FSequenceType));
  Result.Add('value', FValue);
  Result.Add('event', MidiSequenceEventToString(FEvent));
  jsonDatas := TJSONArray.Create;
  for i := 0 to length(FDatas) - 1 do
  begin
    jsonDatas.add(FDatas[i].toJson);
  end;
  Result.Add('datas', jsonDatas);
end;

constructor TMidiSequence.Create;
begin
end;

destructor TMidiSequence.Destroy;
var
  i: integer;
begin
  if (Assigned(FDatas)) then
  begin
    for i := 0 to Length(FDatas) - 1 do
      FDatas[i].Free;
  end;
  SetLength(FDatas, 0);
  inherited Destroy;
end;

procedure TMidiSequence.AddMidiDatas(MidiDatas: TMidiDataArray);
begin
  FDatas := MidiDatas;
end;

{ TMidiPreset }

function TMidiPreset.GetJson: TJsonObject;
var
  jsonDatas: TJsonArray;
  i: integer;
begin
  Result := TJSONObject.Create;
  Result.Add('name', FName);
  Result.Add('prgNumber', FPrgNumber);
  Result.Add('internalMidi', FInternalMidi);
  Result.Add('externalMidi', FExternalMidi);
  jsonDatas := TJSONArray.Create;
  for i := 0 to length(FButtons) - 1 do
  begin
    jsonDatas.add(FButtons[i].toJson);
  end;
  Result.Add('buttons', jsonDatas);
  jsonDatas := TJSONArray.Create;
  for i := 0 to length(FSequences) - 1 do
  begin
    jsonDatas.add(FSequences[i].toJson);
  end;
  Result.Add('sequences', jsonDatas);
end;

procedure TMidiPreset.SetButtons(AValue: TMidiButtonArray);
begin
  if FButtons=AValue then Exit;
  FButtons:=AValue;
end;

procedure TMidiPreset.SetSequences(AValue: TMidiSequenceArray);
begin
  if FSequences=AValue then Exit;
  FSequences:=AValue;
end;

constructor TMidiPreset.Create;
begin
  inherited Create;
end;

destructor TMidiPreset.Destroy;
begin
  inherited Destroy;
end;

procedure TMidiPreset.AddButton(Button: TMidiButton);
begin
  SetLength(FButtons, Length(FButtons)+1);
  FButtons[Length(FButtons)-1] := Button;
end;

procedure TMidiPreset.ClearButtons;
begin

end;

procedure TMidiPreset.AddSequence(Sequence: TMidiSequence);
begin
  SetLength(FSequences, Length(FSequences)+1);
  FSequences[Length(FSequences)-1] := Sequence;
end;

procedure TMidiPreset.ClearSequences;
begin

end;

end.
