unit uModels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, contnrs;

type
  TMidiButton = class;
  TSequence = class;
  TMidiData = class;
  TPreset = class;

  { TPresets }

  TPresets = class
    FPresets: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
  published
  end;

  { TPreset }

  TPreset = class
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

  TSequenceType = (INTERNAL, BUTTON, EXPRESSION);
  TSequenceEvent = (PUSH, RELEASE, START, STOP, CLICK, DOUBLECLICK,
    LONGCLICK, VALUECHANGE);

  { TSequence }

  TSequence = class
  private
    FSequenceType: TSequenceType;
    FEvent: TSequenceEvent;
    FDatas: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddMidiData(MidiData: TMidiData);
  published
    property SequenceType: TSequenceType read FSequenceType write FSequenceType;
    property Event: TSequenceEvent read FEvent write FEvent;
    property Datas: TObjectList read FDatas;
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
    procedure SetHumanString(AValue: string);
  public
  published
    property MidiType: TMidiDataType read FMidiType write FMidiType;
    property Channel: byte read FChannel write FChannel;
    property Data1: byte read FData1 write FData1;
    property Data2: byte read FData2 write FData2;
    property HumanString : string read GetHumanString write SetHumanString;
  end;

implementation

{ TMidiData }

function TMidiData.GetHumanString: string;
var shortType, mask : string;
begin
  case FMidiType of
   PC:
   begin
     shortType := 'PC';
     mask := '%0:s%2:.2d@%1:.2d'
   end;
   CC:
   begin
     shortType := 'CC';
     mask := '%0:s%2:.2d:%3:d@%1:.2d'
   end;
   NON:
   begin
     shortType := 'NOTE_ON';
     mask := '%0:s%2:.2d:%3:d@%1:.2d'
   end;
   NOFF:
   begin
     shortType := 'NOTE_OFF';
     mask := '%0:s%2:.2d:%3:d@%1:.2d'
   end;
   PC_PREV:
   begin
     shortType := 'PC_PREV';
     mask := '%0:s@%1:.2d'
   end;
   PC_NEXT:
   begin
     shortType := 'PC_NEXT';
     mask := '%0:s@%1:.2d'
   end;
   WAIT:
   begin
     shortType := 'WAIT';
     mask := '%0:s@%4:d'
   end;
  end;
  Result := Format(mask, [shortType, FChannel, FData1, FData2, (FData1*127)+FData2]);
end;

procedure TMidiData.SetHumanString(AValue: string);
begin

end;

{ TPresets }

constructor TPresets.Create;
begin
  FPresets := TObjectList.Create(True);
end;

destructor TPresets.Destroy;
begin
  inherited Destroy;
  FPresets.Free;
end;

{ TSequence }

constructor TSequence.Create;
begin
  FDatas := TObjectList.Create(True);
end;

destructor TSequence.Destroy;
begin
  inherited Destroy;
  FDatas.Free;
end;

procedure TSequence.AddMidiData(MidiData: TMidiData);
begin
  FDatas.Add(MidiData);
end;

{ TPreset }

constructor TPreset.Create;
begin
  FButtons := TObjectList.Create;
  FButtons.OwnsObjects := True;

  FSequences := TObjectList.Create;
  FSequences.OwnsObjects := True;
end;

destructor TPreset.Destroy;
begin
  inherited Destroy;
  FButtons.Free;
  FSequences.Free;
end;

procedure TPreset.AddButton(Button: TMidiButton);
begin
  FButtons.Add(Button);
end;

end.
