unit uModels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, contnrs;

type
  TButton = class;
  TSequence = class;
  TMidiData = class;

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
    procedure AddButton(Button: TButton);
  published
    property Name: string read FName write FName;
    property ProgramNumber: integer read FPrgNumber write FPrgNumber default 0;
    property InternalMidi: integer read FInternalMidi write FInternalMidi default 0;
    property ExternalMidi: integer read FExternalMidi write FExternalMidi default 0;
  end;

  TButtonType = (MOMENTARY, TOGGLE);

  TButton = class
  private
    FName: string;
    FType: TButtonType;
    FColor: TColor;
  public
  published
    property Name: string read FName write FName;
    property ButtonType: TButtonType read FType write FType default MOMENTARY;
    property Color: TColor read FColor write FColor default 0;
  end;

  TSequenceType = (INTERNAL, BUTTON, EXPRESSION);
  TSequenceEvent = (PUSH, Release, START, STOP, CLICK, DOUBLECLICK,
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

  TMidiDataType = (PC, CC, NON, NOFF, PC_PREV, PC_NEXT);

  TMidiData = class
  private
    FMidiType: TMidiDataType;
    FChannel: byte;
    FData1: byte;
    FData2: byte;
  public
  published
    property MidiType: TMidiDataType read FMidiType write FMidiType;
    property Channel: byte read FChannel write FChannel;
    property Data1: byte read FData1 write FData1;
    property Data2: byte read FData2 write FData2;
  end;

implementation

{ TSequence }

constructor TSequence.Create;
begin
  FDatas := TObjectList.Create(True);
end;

destructor TSequence.Destroy;
begin
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
  FButtons.Free;
  FSequences.Free;
end;

procedure TPreset.AddButton(Button: TButton);
begin
  FButtons.Add(Button);
end;

end.
