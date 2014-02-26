unit RadioModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, UComplex;

const
  // ParamH: TRadioDataStream
  // ParamL: index of data
  // Note: call TRadioDataStream.Realse after processed
  RM_DATA            = 0;

  // ParamH: Frequency (0)    ParamL: in Hz
  // ParamH: Bandwidth (1)    ParamL: in Hz
  // ParamH: gain      (2)    ParamL: in dB
  RM_SET_FEATURE     = 1;

  // ParamH: START (0)    ParamL: ignore
  // ParamH: STOP  (1)    ParamL: ignore
  // ParamH: PAUSE (2)    ParamL: ignore
  // ParamH: RESET (3)    ParamL: ignore
  RM_CONTROL         = 2;

  // ParamH: timer id    ParamL: timer interval
  RM_TIMER           = 3;

  // ParamH: error   (0)    ParamL: code
  // ParamH: warning (1)    ParamL: code
  // ParamH: info    (2)    ParamL: code
  // ParamH: debug   (3)    ParamL: code
  RM_REPORT          = 4;

type

  TDataStreamRec = record
    Counter: Integer;
    Allocated: Boolean;
    Data: array of Complex;
  end;

  { TRadioDataStream }

  TRadioDataStream = class
  private
    FName: string;
    FBlockSize: Integer;
    FFreeFlag: Boolean;
    FFreeSlot: Integer;
    FCurIndex: Integer;
    FBuffers: array [0..1] of TDataStreamRec;
    FOnBlockDone: TNotifyEvent;
  public
    constructor Create(const AName: string; const BlockSize: Integer);
    destructor Destroy; override;
    procedure SafeFree;

    function Alloc: PComplex;
    procedure Broadcast(Listeners: TList);
    procedure Release(Data: PComplex); // Listeners call this to release buffer

    property OnBlockDone: TNotifyEvent read FOnBlockDone write FOnBlockDone;
    property Name: string read FName;
  end;

  { TStreamRegulator }

  TStreamRegulator = class
  public
    constructor Create(const Size: Integer);
  end;

  TRadioModule = class;

  TRadioMessageId      = 0..31;
  TRadioMessageIdSet   = Cardinal;

  { TRadioMessage }

  TRadioMessage = record
    Sender: TRadioModule;
    Id: Integer;
    ParamH: PtrInt;
    ParamL: PtrInt;
  end;

  // TRadioMessageProc:
  //   @param Msg: the message
  //   @param Ret: return value
  //   @return a new message proc
  // TRadioMessageProc = function (const Msg: TRadioMessage; var Ret: Integer): TRadioMessageProc of object;
  TRadioMessageProc = function(const Msg: TRadioMessage; var Ret: Integer): TMethod of object;
  TRadioMessageProccessed = procedure(const Param: PtrInt; const Ret: Integer; const NewProc: TRadioMessageProc) of object;

  TRadioMessageHandler = record
    Filter: TRadioMessageIdSet;
    MessageProc: TRadioMessageProc;
  end;

  PRadioJobNode = ^TRadioJobNode;
  TRadioJobNode = record
    Next: PRadioJobNode;
    Param: PtrInt;
    Msg: TRadioMessage;
    MsgProc: TRadioMessageProc;
    MsgProcessed: TRadioMessageProccessed;
  end;

  TRadioRunQueue = class;
  PRadioThreadNode = ^TRadioThreadNode;

  { TRadioThread }

  TRadioThread = class(TThread)
  private
    FJob: PRadioJobNode;
    FJobScheduled: PRTLEvent;
    FNode: PRadioThreadNode;
    FRunQueue: TRadioRunQueue;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Job: PRadioJobNode read FJob write FJob;
    property Node: PRadioThreadNode read FNode write FNode;
  end;

  TRadioThreadNode = record
    Next: PRadioThreadNode;
    Thread: TRadioThread;
  end;

  { TRadioRunQueue }

  TRadioRunQueue = class
  private
    FFirstJob: TRadioJobNode;
    FRunningNode: TRadioThreadNode;
    FIdleNode: TRadioThreadNode;
    FWorkerReady: PRTLEvent;
    FReadyWorker: TRadioThread;
  private
    procedure RunJob(T: PRadioThreadNode; P: PRadioJobNode);
    procedure Schedule;
  public
    constructor Create(const SMP: Integer = 2);
    procedure AddJob(const Param: PtrInt; MsgProc: TRadioMessageProc;
      MsgProcessed: TRadioMessageProccessed; const Msg: TRadioMessage);
  end;

  TProcessingType = (ptBackground, ptForeground);

  { TRadioModule }

  TRadioModule = class
  private
    FDataListeners: TList;
    FDefOutput: TRadioDataStream;
    FFeatureListeners: TList;
    FMessageHandler: TRadioMessageHandler;
    FName: string;
    FRunning: Boolean;
    FRunQueue: TRadioRunQueue;
    procedure SetName(AValue: string);
    procedure SetMessageHandler(AValue: TRadioMessageHandler);
  protected
    procedure SetRunning(AValue: Boolean); virtual;

    procedure PostMessage(const Msg: TRadioMessage); virtual;
    function  DefProccessMessage(const Msg: TRadioMessage; var Ret: Integer): TRadioMessageProc;
    procedure MessageProccessed(const Param: PtrInt; const Ret: Integer;
      const NewProc: TRadioMessageProc); virtual;

    property MessageHandler: TRadioMessageHandler read FMessageHandler write SetMessageHandler;
  public
    constructor Create(RunQueue: TRadioRunQueue); virtual;
    destructor Destroy; override;

    procedure Configure; virtual;
    procedure Draw(ACanvas: TCanvas; ARect: TRect); virtual;
    function  SetParam(const Name: string; const Value: string): Boolean;

    procedure AddDataListener(Listener: TRadioModule);
    procedure AddFeatureListener(Listener: TRadioModule);
    procedure RemoveDataListener(Listener: TRadioModule);
    procedure RemoveFeatureListener(Listener: TRadioModule);
    procedure ClearDataListeners;
    procedure ClearFeatureListeners;

    procedure ReceiveData(const P: PComplex; const Len: Integer); virtual;

    property DefOutput: TRadioDataStream read FDefOutput;
    property Running: Boolean read FRunning write SetRunning;
    property Name: string read FName write SetName;
  end;

procedure RadioGlobalLock;
procedure RadioGlobalUnlock;

implementation

var
  RadioGlobalCS: TRTLCriticalSection;

procedure RadioGlobalLock;
begin
  EnterCriticalsection(RadioGlobalCS);
end;

procedure RadioGlobalUnlock;
begin
  LeaveCriticalsection(RadioGlobalCS);
end;

{ TRadioRunQueue }

procedure TRadioRunQueue.RunJob(T: PRadioThreadNode; P: PRadioJobNode);
var
  NewProc: TRadioMessageProc;
  M: TMethod;
  Ret: Integer;
begin
  M := P^.MsgProc(P^.Msg, Ret);
  NewProc := TRadioMessageProc(M);
  P^.MsgProcessed(P^.Param, Ret, NewProc);
  Dispose(P);

  RadioGlobalLock;
  T^.Next := FIdleNode.Next;
  FIdleNode.Next := T;
  RadioGlobalUnlock;
end;

procedure TRadioRunQueue.Schedule;
var
  T: PRadioThreadNode;
  P: PRadioJobNode;
begin
  if not Assigned(FFirstJob.Next) then Exit;
  if not Assigned(FIdleNode.Next) then Exit;
  RadioGlobalLock;
  T := FIdleNode.Next;
  FIdleNode.Next := T^.Next;
  P := FFirstJob.Next;
  FFirstJob.Next := P^.Next;
  RadioGlobalUnlock;

  T^.Thread.Job := P;
  RTLEventSetEvent(T^.Thread.FJobScheduled);
end;

constructor TRadioRunQueue.Create(const SMP: Integer);
var
  I: Integer;
  P: PRadioThreadNode;
begin
  for I := 1 to SMP do
  begin
    New(P);
    P^.Thread := TRadioThread.Create;
    P^.Thread.Node := P;
    P^.Thread.FRunQueue := Self;
    P^.Next := FIdleNode.Next;
    FIdleNode.Next := P;
  end;
end;

procedure TRadioRunQueue.AddJob(const Param: PtrInt;
  MsgProc: TRadioMessageProc; MsgProcessed: TRadioMessageProccessed;
  const Msg: TRadioMessage);
var
  P: PRadioJobNode;
begin
  New(P);
  P^.Msg := Msg;
  P^.Param := Param;
  P^.MsgProc := MsgProc;
  P^.MsgProcessed := MsgProcessed;
  RadioGlobalLock;
  P^.Next := FFirstJob.Next;
  FFirstJob.Next := P;
  RadioGlobalUnlock;
  Schedule;
end;

{ TRadioThread }

procedure TRadioThread.Execute;
begin
  while not Terminated do
  begin
    RTLEventWaitFor(FJobScheduled);
    RTLEventResetEvent(FJobScheduled);

    if not Assigned(FJob) then Break;
    FRunQueue.RunJob(FNode, FJob);
  end;
end;

constructor TRadioThread.Create;
begin
  FJobScheduled := RTLEventCreate;
  inherited Create(False);
end;

destructor TRadioThread.Destroy;
begin
  RTLEventDestroy(FJobScheduled);
  inherited Destroy;
end;

{ TStreamRegulator }

constructor TStreamRegulator.Create(const Size: Integer);
begin

end;

{ TRadioDataStream }

constructor TRadioDataStream.Create(const AName: string; const BlockSize: Integer);
begin
  FName := AName;
  FBlockSize := BlockSize;
  FCurIndex := -1;
end;

destructor TRadioDataStream.Destroy;
begin
  inherited Destroy;
end;

procedure TRadioDataStream.SafeFree;
var
  I: Integer;
begin
  RadioGlobalLock;
  FFreeFlag := False;
  for I := Low(FBuffers) to High(FBuffers) do
  begin
    if FBuffers[I].Allocated then
    begin
      FFreeFlag := True;
      Break;
    end;
  end;
  if not FFreeFlag then Free;
  RadioGlobalUnlock;
end;

function TRadioDataStream.Alloc: PComplex;
var
  I: Integer;
begin
  Result := nil;
  if FCurIndex >= 0 then Exit;

  RadioGlobalLock;
  for I := Low(FBuffers) to High(FBuffers) do
  begin
    if not FBuffers[I].Allocated then
    begin
      Result := @FBuffers[I].Data[0];
      FBuffers[I].Allocated := True;
      FCurIndex := I;
      Break;
    end;
  end;
  RadioGlobalUnlock;
end;

procedure TRadioDataStream.Broadcast(Listeners: TList);
var
  P: Pointer;
  D: PComplex;
begin
  if FCurIndex < 0 then Exit;

  RadioGlobalLock;
  FBuffers[FCurIndex].Counter := Listeners.Count;
  if FBuffers[FCurIndex].Counter < 1 then
    FBuffers[FCurIndex].Allocated := False;
  D := @FBuffers[FCurIndex].Data[0];
  FCurIndex := -1;
  RadioGlobalUnlock;

  for P in Listeners do
    TRadioModule(P).ReceiveData(D, FBlockSize, Self);
end;

procedure TRadioDataStream.Release(Data: PComplex);
var
  I: Integer;
begin
  for I := Low(FBuffers) to High(FBuffers) do
  begin
    if @FBuffers[I].Data[0] = Data then
    begin
      RadioGlobalLock;
      Dec(FBuffers[FCurIndex].Counter);
      if FBuffers[FCurIndex].Counter < 1 then
        FBuffers[FCurIndex].Allocated := False;
      RadioGlobalUnlock;
      Break;
    end;
  end;
end;

{ TRadioModule }

procedure TRadioModule.SetName(AValue: string);
begin
  if FName = AValue then Exit;
  FName := AValue;
end;

procedure TRadioModule.SetMessageHandler(AValue: TRadioMessageHandler);
begin

end;

procedure TRadioModule.SetRunning(AValue: Boolean);
begin
  if FRunning = AValue then Exit;
  FRunning := AValue;
  if not FRunning then
  begin
    // clear messages, etc
  end;
end;

procedure TRadioModule.PostMessage(const Msg: TRadioMessage);
begin
  if not Running then Exit;

  FRunQueue.AddJob(0, MessageHandler.MessageProc, @MessageProccessed, Msg);
end;

function TRadioModule.DefProccessMessage(const Msg: TRadioMessage;
  var Ret: Integer): TRadioMessageProc;
begin

end;

procedure TRadioModule.MessageProccessed(const Param: PtrInt;
  const Ret: Integer; const NewProc: TRadioMessageProc);
begin

end;

constructor TRadioModule.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create;
  FDataListeners := TList.Create;
  FFeatureListeners := TList.Create;
  FDefOutput := TRadioDataStream.Create('output',1024);
  FRunQueue := RunQueue;
end;

destructor TRadioModule.Destroy;
begin
  FDefOutput.SafeFree;
  FDataListeners.Free;
  FFeatureListeners.Free;
  inherited Destroy;
end;

procedure TRadioModule.Configure;
begin

end;

procedure TRadioModule.Draw(ACanvas: TCanvas; ARect: TRect);
begin

end;

function TRadioModule.SetParam(const Name: string; const Value: string
  ): Boolean;
begin

end;

procedure TRadioModule.AddDataListener(Listener: TRadioModule);
begin
  with FDataListeners do
    if IndexOf(Listener) < 0 then Add(Listener);
end;

procedure TRadioModule.AddFeatureListener(Listener: TRadioModule);
begin
  with FFeatureListeners do
    if IndexOf(Listener) < 0 then Add(Listener);
end;

procedure TRadioModule.RemoveDataListener(Listener: TRadioModule);
begin
  FDataListeners.Remove(Listener);
end;

procedure TRadioModule.RemoveFeatureListener(Listener: TRadioModule);
begin
  FFeatureListeners.Remove(Listener);
end;

procedure TRadioModule.ClearDataListeners;
begin
  FDataListeners.Clear;
end;

procedure TRadioModule.ClearFeatureListeners;
begin
  FFeatureListeners.Clear;
end;

procedure TRadioModule.ReceiveData(const P: PComplex; const Len: Integer);
begin

end;

initialization

  InitCriticalSection(RadioGlobalCS);

finalization

  DoneCriticalsection(RadioGlobalCS);

end.

