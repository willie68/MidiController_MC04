unit uModels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpjson, jsonparser;

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
    class function parseJsonToMidiPreset(jsonData: TJsonObject): TMidiPreset; static;
  published
    property Name: string read FName write FName;
    property ProgramNumber: integer read FPrgNumber write FPrgNumber default 0;
    property InternalMidi: integer read FInternalMidi write FInternalMidi default 0;
    property ExternalMidi: integer read FExternalMidi write FExternalMidi default 0;
    property Buttons: TMidiButtonArray read FButtons write SetButtons;
    property Sequences: TMidiSequenceArray read FSequences write SetSequences;
    property toJson: TJsonObject read GetJson;
  end;

  TMidiButtonType = (MOMENTARY, SWITCH);

  { TMidiButton }

  TMidiButton = class
  private
    FName: string;
    FType: TMidiButtonType;
    FColor: TColor;
    function GetJson: TJsonObject;
  public
    function Clone: TMidiButton;
    class function parseJsonToMidiButton(jsonData: TJsonObject): TMidiButton; static;
  published
    property Name: string read FName write FName;
    property ButtonType: TMidiButtonType read FType write FType default MOMENTARY;
    property Color: TColor read FColor write FColor default 0;
    property toJson: TJsonObject read GetJson;
  end;

  TMidiSequenceType = (INTERNAL, BUTTON, EXPRESSION);
  TMidiSequenceEvent = (PUSH, Release, START, STOP, SINGLECLICK, DOUBLECLICK,
    LONGCLICK, VALUECHANGE, CC_EVENT);

  { TMidiSequence }

  TMidiSequence = class
  private
    FSequenceType: TMidiSequenceType;
    FEvent: TMidiSequenceEvent;
    FValue: integer;
    FDatas: TMidiDataArray;
    function GetJson: TJsonObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddMidiDatas(MidiDatas: TMidiDataArray);
    procedure FreeDatas;
    function Clone: TMidiSequence;
    procedure AddMidiData(MidiData: TMidiData);
    class function parseJsonToMidiSequence(jsonData: TJsonObject): TMidiSequence; static;
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
  public
    constructor Create;
    destructor Destroy; override;
    function clone: TMidiData;
    class function parseJsonToMidiData(jsonData: TJsonObject): TMidiData; static;
  published
    property MidiType: TMidiDataType read FMidiType write FMidiType;
    property Channel: byte read FChannel write FChannel;
    property Data1: byte read FData1 write FData1;
    property Data2: byte read FData2 write FData2;
    property HumanString: string read GetHumanString;
    property toJson: TJsonObject read GetJson;
  end;

function getMidiDataString(MidiDatas: TMidiDataArray): string;
function StringToMidiButtonType(midiButtonType: string): TMidiButtonType;
function StringToMidiSequnceEvent(AValue: string): TMidiSequenceEvent;
function StringToMidiSequenceType(AValue: string): TMidiSequenceType;
function StringToMidiDataType(AValue: string): TMidiDataType;

function getOridnalEventType(SequenceEvent : TMidiSequenceEvent): integer;

implementation

function getMidiDataString(MidiDatas: TMidiDataArray): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Length(MidiDatas) - 1 do
  begin
    if (i > 0) then
      Result := Result + ', ';
    Result := Result + MidiDatas[i].HumanString;
  end;
end;

function MidiButtonTypeToString(midiButtonType: TMidiButtonType): string;
begin
  case midiButtonType of
    MOMENTARY: Result := 'MOMENTARY';
    SWITCH: Result := 'SWITCH';
  end;
end;

function StringToMidiButtonType(midiButtonType: string): TMidiButtonType;
begin
  if midiButtonType = 'MOMENTARY' then
    Result := MOMENTARY;
  if midiButtonType = 'SWITCH' then
    Result := SWITCH;
end;

function MidiSequenceEventToString(midiSequenceEvent: TMidiSequenceEvent): string;
begin
  case midiSequenceEvent of
    PUSH: Result := 'PUSH';
    Release: Result := 'RELEASE';
    START: Result := 'START';
    STOP: Result := 'STOP';
    SINGLECLICK: Result := 'CLICK';
    DOUBLECLICK: Result := 'DOUBLECLICK';
    LONGCLICK: Result := 'LONGCLICK';
    VALUECHANGE: Result := 'VALUECHANGE';
  end;
end;

function StringToMidiSequnceEvent(AValue: string): TMidiSequenceEvent;
begin
  if AValue = 'PUSH' then
    Result := PUSH;
  if AValue = 'RELEASE' then
    Result := Release;
  if AValue = 'START' then
    Result := START;
  if AValue = 'STOP' then
    Result := STOP;
  if AValue = 'CLICK' then
    Result := SINGLECLICK;
  if AValue = 'DOUBLECLICK' then
    Result := DOUBLECLICK;
  if AValue = 'LONGCLICK' then
    Result := LONGCLICK;
  if AValue = 'VALUECHANGE' then
    Result := VALUECHANGE;
end;


function MidiSequenceTypeToString(midiSequenceType: TMidiSequenceType): string;
begin
  case midiSequenceType of
    INTERNAL: Result := 'INTERNAL';
    BUTTON: Result := 'BUTTON';
    EXPRESSION: Result := 'EXPRESSION';
  end;
end;

function StringToMidiSequenceType(AValue: string): TMidiSequenceType;
begin
  if AValue = 'INTERNAL' then
    Result := INTERNAL;
  if AValue = 'BUTTON' then
    Result := BUTTON;
  if AValue = 'EXPRESSION' then
    Result := EXPRESSION;
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

function StringToMidiDataType(AValue: string): TMidiDataType;
begin
  if AValue = 'PC' then
    Result := PC;
  if AValue = 'CC' then
    Result := CC;
  if AValue = 'NOTE_ON' then
    Result := NON;
  if AValue = 'NOTE_OFF' then
    Result := NOFF;
  if AValue = 'PC_PREV' then
    Result := PC_PREV;
  if AValue = 'PC_NEXT' then
    Result := PC_NEXT;
  if AValue = 'WAIT' then
    Result := WAIT;
end;

function getOridnalEventType(SequenceEvent: TMidiSequenceEvent): integer;
begin
  case SequenceEvent of
    PUSH: Result := 2;
    Release: Result := 3;
    START: Result := 0;
    STOP: Result := 1;
    SINGLECLICK: Result := 4;
    DOUBLECLICK: Result := 5;
    LONGCLICK: Result := 6;
    VALUECHANGE: Result := 7;
    CC_EVENT: Result := 8;
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

function TMidiButton.Clone: TMidiButton;
begin
  Result := TMidiButton.Create;
  Result.FName := Self.FName;
  Result.FType := Self.FType;
  Result.FColor := Self.FColor;
end;

class function TMidiButton.parseJsonToMidiButton(jsonData: TJsonObject): TMidiButton;
begin
  Result := TMidiButton.Create;
  Result.Name := jsonData.Get('name');
  Result.ButtonType := uModels.StringToMidiButtonType(jsonData.Get('type'));
  Result.Color := jsonData.Get('color');
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

constructor TMidiData.Create;
begin
  inherited Create;
end;

destructor TMidiData.Destroy;
begin
  inherited Destroy;
end;

function TMidiData.clone: TMidiData;
begin
  Result := TMidiData.Create;
  Result.FData1 := self.FData1;
  Result.FData2 := self.FData2;
  Result.FChannel := self.FChannel;
  Result.FMidiType := self.FMidiType;
end;

class function TMidiData.parseJsonToMidiData(jsonData: TJsonObject): TMidiData;
begin
  Result := TMidiData.Create;
  if (jsonData.Find('channel') <> nil) then
    Result.Channel := jsonData.Get('channel');
  if (jsonData.Find('type') <> nil) then
    Result.MidiType := StringToMidiDataType(jsonData.Get('type'));
  if (jsonData.Find('data1') <> nil) then
    Result.Data1 := jsonData.Get('data1');
  if (jsonData.Find('data2') <> nil) then
    Result.Data2 := jsonData.Get('data2');
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
  if (Assigned(FDatas)) then
  begin
    for i := 0 to length(FDatas) - 1 do
    begin
      jsonDatas.add(FDatas[i].toJson);
    end;
  end;
  Result.Add('datas', jsonDatas);
end;

constructor TMidiSequence.Create;
begin
end;

destructor TMidiSequence.Destroy;
begin
  FreeDatas;
  inherited Destroy;
end;

procedure TMidiSequence.AddMidiDatas(MidiDatas: TMidiDataArray);
begin
  if (Assigned(FDatas)) then
    FreeDatas;
  FDatas := MidiDatas;
end;

procedure TMidiSequence.FreeDatas;
var
  i: integer;
begin
  if (Assigned(FDatas)) then
  begin
    for i := 0 to Length(FDatas) - 1 do
    begin
      FreeAndNil(FDatas[i]);
    end;
    SetLength(FDatas, 0);
  end;
end;

function TMidiSequence.Clone: TMidiSequence;
var
  i: integer;
begin
  Result := TMidiSequence.Create;
  Result.FValue := Self.FValue;
  Result.FEvent := Self.Event;
  Result.FSequenceType := Self.FSequenceType;
  if (Assigned(FDatas)) then
  begin
    SetLength(Result.FDatas, Length(Self.FDatas));
    for i := 0 to Length(Self.FDatas) - 1 do
    begin
      Result.FDatas[i] := Self.FDatas[i].Clone;
    end;
  end;
end;

procedure TMidiSequence.AddMidiData(MidiData: TMidiData);
begin
  if (not Assigned(FDatas)) then
    SetLength(FDatas, 1)
  else
    SetLength(FDatas, Length(FDatas) + 1);
  FDatas[Length(FDatas) - 1] := MidiData;
end;

class function TMidiSequence.parseJsonToMidiSequence(jsonData: TJsonObject):
TMidiSequence;

var
  jsonDatas: TJsonArray;
  jsonMidiData: TJsonObject;
  y: integer;
begin
  Result := TMidiSequence.Create;
  Result.Event := uModels.StringToMidiSequnceEvent(jsonData.Get('event'));
  Result.SequenceType :=
    uModels.StringToMidiSequenceType(jsonData.Get('type'));
  if (jsonData.Find('value') <> nil) then
    Result.Value := jsonData.Get('value');

  if (jsonData.Arrays['datas'] <> nil) then
  begin
    jsonDatas := jsonData.Arrays['datas'];
    for y := 0 to jsonDatas.Count - 1 do
    begin
      jsonMidiData := jsonDatas.Objects[y];
      Result.AddMidiData(TMidiData.parseJsonToMidiData(jsonMidiData));
    end;
  end;
end;

{ TMidiPreset }

function TMidiPreset.GetJson: TJsonObject;
var
  jsonDatas: TJsonArray;
  i: integer;
  sequence: TMidiSequence;
begin
  Result := TJSONObject.Create;
  Result.Add('name', FName);
  Result.Add('prgNumber', FPrgNumber);
  Result.Add('internalMidi', FInternalMidi);
  Result.Add('externalMidi', FExternalMidi);
  jsonDatas := TJSONArray.Create;
  if (Assigned(FButtons)) then
  begin
    for i := 0 to length(FButtons) - 1 do
    begin
      jsonDatas.add(FButtons[i].toJson);
    end;
  end;
  Result.Add('buttons', jsonDatas);
  jsonDatas := TJSONArray.Create;
  if (Assigned(FSequences)) then
  begin
    for i := 0 to length(FSequences) - 1 do
    begin
      sequence := FSequences[i];
      if (length(sequence.Fdatas) > 0) then
        jsonDatas.add(sequence.toJson);
    end;
  end;
  Result.Add('sequences', jsonDatas);
end;

procedure TMidiPreset.SetButtons(AValue: TMidiButtonArray);
begin
  if FButtons = AValue then
    Exit;
  FButtons := AValue;
end;

procedure TMidiPreset.SetSequences(AValue: TMidiSequenceArray);
begin
  if FSequences = AValue then
    Exit;
  FSequences := AValue;
end;

constructor TMidiPreset.Create;
begin
  inherited Create;
  SetLength(FButtons, 0);
  SetLength(FSequences, 0);
end;

destructor TMidiPreset.Destroy;
begin
  ClearButtons;
  ClearSequences;
  inherited Destroy;
end;

procedure TMidiPreset.AddButton(Button: TMidiButton);
begin
  if (not Assigned(FButtons)) then
    SetLength(FButtons, 1)
  else
    SetLength(FButtons, Length(FButtons) + 1);
  FButtons[Length(FButtons) - 1] := Button;
end;

procedure TMidiPreset.ClearButtons;
var
  i: integer;
begin
  for i := 0 to Length(FButtons) - 1 do
  begin
    FreeAndNil(FButtons[i]);
  end;
  SetLength(FButtons, 0);
end;

procedure TMidiPreset.AddSequence(Sequence: TMidiSequence);
begin
  SetLength(FSequences, Length(FSequences) + 1);
  FSequences[Length(FSequences) - 1] := Sequence;
end;

procedure TMidiPreset.ClearSequences;
var
  i: integer;
begin
  for i := 0 to Length(FSequences) - 1 do
  begin
    FreeAndNil(FSequences[i]);
  end;
  SetLength(FSequences, 0);
end;

class function TMidiPreset.parseJsonToMidiPreset(jsonData: TJsonObject): TMidiPreset;
var
  x: integer;
  jsonArray: TJSONArray;
  jsonObject: TJSONObject;

begin
  Result := TMidiPreset.Create;
  Result.Name := jsonData.Get('name');
  Result.ProgramNumber := jsonData.Get('prgNumber');
  Result.ExternalMidi := jsonData.Get('externalMidi');
  Result.InternalMidi := jsonData.Get('internalMidi');

  // get buttons
  jsonArray := jsonData.Arrays['buttons'];
  for x := 0 to jsonArray.Count - 1 do
  begin
    jsonObject := jsonArray.Objects[x];
    Result.AddButton(TMidiButton.parseJsonToMidiButton(jsonObject));
  end;

  // get sequences
  if (jsonData.Find('sequences') <> nil) then
  begin
    jsonArray := jsonData.Arrays['sequences'];

    for x := 0 to jsonArray.Count - 1 do
    begin
      jsonObject := jsonArray.Objects[x];
      Result.AddSequence(TMidiSequence.parseJsonToMidiSequence(jsonObject));
    end;
  end;
end;

end.
