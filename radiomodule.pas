unit RadioModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, GraphType, UComplex, Genfft, radiomessage, gen_graph;

type

  TDataStreamRec = record
    Counter: Integer;
    Allocated: Boolean;
    Data: array of Complex;
  end;

  TRadioModule = class;

  TRadioLogLevel = (llVerbose, llInfo, llWarn, llError);

  { TRadioDataStream }

  TRadioDataStream = class
  private
    FName: string;
    FBlockSize: Integer;
    FFreeFlag: Boolean;
    FBuffers: array [0..10] of TDataStreamRec;
    FModule: TRadioModule;
    function GetBuffer(const Index: Integer): PComplex;
    function GetBufferCount: Integer;
    function GetDefBufferSize: Integer;
    procedure SetDefBufferSize(AValue: Integer);
  public
    constructor Create(Module: TRadioModule; const AName: string; const BlockSize: Integer);
    destructor Destroy; override;
    procedure SafeFree;

    procedure Lock;
    procedure Unlock;

    function TryAlloc(out Index: Integer): PComplex;
    procedure Broadcast(const Index: Integer; Listeners: TList);
    procedure Release(const Index: Integer); // Listeners call this to release buffer

    function GetBufferSize(const Index: Integer): Integer;

    property Name: string read FName;
    property Buffer[const Index: Integer]: PComplex read GetBuffer;
    property BufferSize: Integer read GetDefBufferSize write SetDefBufferSize;
    property BufferCount: Integer read GetBufferCount;
  end;

  TReceiveData = procedure (const P: PComplex; const Len: Integer) of object;

  { TStreamRegulator }

  TStreamRegulator = class
  private
    FData: array of Complex;
    FOnReceiveData: TReceiveData;
    FSize: Integer;
    FCursor: Integer;
    FOverlap: Integer;
    procedure SetOnReceiveData(AValue: TReceiveData);
    procedure SetOverlap(AValue: Integer);
    procedure SetSize(AValue: Integer);
    procedure CallOnRegulatedData;
  public
    constructor Create;
    procedure ReceiveData(P: PComplex; Len: Integer);

    property Size: Integer read FSize write SetSize;
    property Overlap: Integer read FOverlap write SetOverlap;
    property OnRegulatedData: TReceiveData read FOnReceiveData write SetOnReceiveData ;
  end;

  TRadioMessageId      = 0..31;
  TRadioMessageIdSet   = Cardinal;

  PRadioMessageNode = ^TRadioMessageNode;
  TRadioMessageNode = record
    Next: PRadioMessageNode;
    Msg: TRadioMessage;
  end;

  TRadioMessageQueue = class;
  TRadioRunQueue = class;
  PRadioThreadNode = ^TRadioThreadNode;

  { TRadioThread }

  TRadioThread = class(TThread)
  private
    FJob: TRadioMessageQueue;
    FNode: PRadioThreadNode;
    FRunQueue: TRadioRunQueue;
    FJobScheduled: PRTLEvent;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Job: TRadioMessageQueue read FJob write FJob;
    property Node: PRadioThreadNode read FNode write FNode;
    property Terminated;
  end;

  TRadioThreadNode = record
    Next: PRadioThreadNode;
    Thread: TRadioThread;
  end;

  PMessageQueueNode = ^TMessageQueueNode;
  TMessageQueueNode = record
    Next: PMessageQueueNode;
    Queue: TRadioMessageQueue;
  end;

  { TRadioRunQueue }

  TRadioRunQueue = class
  private
    FFirstJob: TMessageQueueNode;
    FIdleNode: TRadioThreadNode;
    FWorkers: array of TRadioThread;
  private
    procedure Schedule;
    procedure Lock;

    procedure UnLock;
    procedure WorkerIdle(Worker: TRadioThread);
  public
    constructor Create(const SMP: Integer = 4);
    destructor Destroy; override;
    procedure Request(Job: TRadioMessageQueue);
    procedure Terminate;
  end;

  { TRadioMessageQueue }

  TRadioMessageQueue = class
  private
    FName: string;
    FInQueue: Boolean;
    FFirstExecMsg: TRadioMessageNode;
    FLastExecMsg: PRadioMessageNode;
    FFirstMsg: TRadioMessageNode;
    FLastMsg: PRadioMessageNode;
    FMessageFilter: TRadioMessageIdSet;
    FRunQueue: TRadioRunQueue;
    FCPUTime: TTime;
    FRunThread: TRadioThread;
    function GetNotEmpty: Boolean;
    procedure SetInQueue(AValue: Boolean);
    procedure SetMessageFilter(AValue: TRadioMessageIdSet);
    procedure RequestSchudule;
  protected
    procedure MessageExceute;      // execute one message in a single call
    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: PtrInt); virtual; abstract;
  public
    constructor Create(RunQueue: TRadioRunQueue); virtual;
    procedure Lock;
    procedure UnLock;

    procedure StoreMessage(const Msg: TRadioMessage);
    procedure MessageQueueReset;

    property NotEmpty: Boolean read GetNotEmpty;
    property MessageFilter: TRadioMessageIdSet read FMessageFilter write SetMessageFilter;
    property InQueue: Boolean read FInQueue write SetInQueue;

    property Name: string read FName write FName;
    property CPUTime: TTime read FCPUTime;
    property RunThread: TRadioThread read FRunThread write FRunThread;
  end;

  TDataListener = record
    M: TRadioModule;
    Port: Integer;
  end;
  PDataListener = ^TDataListener;

  { TRadioModule }

  TRadioModule = class(TRadioMessageQueue, IGenDrawable)
  const
    ICON_SIZE = 50;
    HEADER_SIZE = 20;
    HEADER_FONT_HEIGHT = 15;
    BTN_CONFIG = '::';
    BTN_GUI    = 'gui';
    BODER_WIDTH = 2;
  private
    FGraphNode: TGenEntityNode;
    FRefCount: Integer;
    FDefOutput: TRadioDataStream;
    FRunning: Boolean;
    FDescStr: TStringList;
    FGUIBtnRect: TRect;
    FConfigBtnRect: TRect;
    function FindDataListener(Listener: TRadioModule): Integer;
  protected
    FIcon: string;
    FHasGUI: Boolean;
    FHasConfig: Boolean;
    FInvalidated: Boolean;
    procedure LoadIconRes(ResName: string = '');
{$IFDEF FPC}
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: tguid; out obj): longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
{$ELSE}
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
{$ENDIF}
    function _AddRef: Integer; virtual; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Integer; virtual; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    procedure Measure(ACanvas: TCanvas; out Extent: TSize);
    procedure Draw(ACanvas: TCanvas; ARect: TRect);
    procedure MouseClick(const Pt: TPoint);
    function  Invalidated: Boolean;
  protected
    FDataListeners: TList;
    FFeatureListeners: TList;

    function Alloc(Stream: TRadioDataStream; out Index: Integer): PComplex;

    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: Integer); override;

    procedure RMControl(const Msg: TRadioMessage; var Ret: Integer); virtual;
    procedure RMData(const Msg: TRadioMessage; var Ret: Integer); virtual;
    procedure RMSetFeature(const Msg: TRadioMessage; var Ret: Integer); virtual;
    procedure RMReport(const Msg: TRadioMessage; var Ret: Integer); virtual;
    procedure RMTimer(const Msg: TRadioMessage; var Ret: Integer); virtual;

    function RMSetFrequency(const Msg: TRadioMessage; const Freq: Cardinal): Integer; virtual;
    function RMSetBandwidth(const Msg: TRadioMessage; const Bandwidth: Cardinal): Integer; virtual;
    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer; virtual;
    function RMPhaseAdjust(const Msg: TRadioMessage; const Rate: Cardinal): Integer; virtual;

    procedure DoConfigure; virtual;
    procedure DoShowGUI; virtual;

    procedure DoReset; virtual;
    function DoStart: Boolean; virtual;
    function DoStop: Boolean; virtual;

    procedure Describe(Strs: TStrings); virtual;
  public
    procedure ShowConnections(Graph: TGenGraph);
    property GraphNode: TGenEntityNode read FGraphNode write FGraphNode;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure GraphInvalidate;

    procedure PostMessage(const Id: Integer; const ParamH, ParamL: PtrUInt);
    procedure PostMessage(const Msg: TRadioMessage); virtual;
    procedure Broadcast(const Msg: TRadioMessage); overload;
    procedure Broadcast(const AId: Integer; const AParamH, AParamL: PtrUInt); overload;

    procedure AddDataListener(Listener: TRadioModule; const Port: Integer);
    procedure AddFeatureListener(Listener: TRadioModule);
    procedure RemoveDataListener(Listener: TRadioModule);
    procedure RemoveFeatureListener(Listener: TRadioModule);
    procedure ClearDataListeners;
    procedure ClearFeatureListeners;

    procedure ReceiveData(const P: PComplex; const Len: Integer); virtual; overload;
    procedure ReceiveData(const Port: Integer; const P: PComplex; const Len: Integer); virtual;

    property DefOutput: TRadioDataStream read FDefOutput;
    property Running: Boolean read FRunning;
  end;

  TRadioModuleClass = class of TRadioModule;

  TBackgroundRadioModule = class;

  { TGenericRadioThread }

  TGenericRadioThread = class(TThread)
  private
    FModule: TBackgroundRadioModule;
    FParam: Pointer;
  protected
    procedure Execute; override;
  public
    constructor Create(Module: TBackgroundRadioModule);
    property Module: TBackgroundRadioModule read FModule write FModule;
    property Param: Pointer read FParam write FParam;
    property Terminated;
  end;

  { TBackgroundRadioModule }

  TBackgroundRadioModule = class(TRadioModule)
  protected
    FThread: TGenericRadioThread;
    procedure ThreadFun(Thread: TGenericRadioThread); virtual;
    function DoStart: Boolean; override;
    function DoStop: Boolean; override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;
  end;

  { TDataFlowNode }

  TDataFlowNode = class
  private
    FNext: TDataFlowNode;
    FOnSendToNext: TReceiveData;
    FCache: array of Complex;
    FHoldCount: Integer;
  protected
    procedure DoReceiveData(const P: PComplex; const Len: Integer); virtual;
  public
    destructor Destroy; override;

    procedure SendToNext(const P: PComplex; const Len: Integer);
    procedure ReceiveData(const P: PComplex; const Len: Integer);

    procedure Hold;
    procedure ReleaseHold;

    procedure Connect(ANext: TDataFlowNode);
    function  LastNode: TDataFlowNode;

    property Next: TDataFlowNode read FNext;
    property OnSendToNext: TReceiveData read FOnSendToNext write FOnSendToNext;
  end;

  { TRegulatorNode }

  TRegulatorNode = class(TDataFlowNode)
  private
    FRegulator: TStreamRegulator;
  protected
    procedure DoReceiveData(const P: PComplex; const Len: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;
    property Regulator: TStreamRegulator read FRegulator;
  end;

  { TWindowNode }

  TWindowNode = class(TDataFlowNode)
  private
    FWnd: array of Double;
    FRegulator: TStreamRegulator;
    function GetOverlap: Integer;
    function GetWindowLen: Integer;
    procedure RegulatedData(const P: PComplex; const Len: Integer);
    procedure SetOverlap(AValue: Integer);
  protected
    procedure DoReceiveData(const P: PComplex; const Len: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetWindow(const P: PDouble; const Len: Integer);

    property WindowLen: Integer read GetWindowLen;
    property Overlap: Integer read GetOverlap write SetOverlap;
  end;

  { TFIRNode }

  TFIRNode = class(TDataFlowNode)
  private
    FHFIR: array of Complex;
    FBuf: array of Complex;
    FRes: array of Complex;
    FFPlan: PFFTPlan;
    FIPlan: PFFTPlan;
    FRegulator: TStreamRegulator;
    FTaps: Integer;
    procedure SetTimeDomainFIR(const P: PComplex; const Len: Integer);
    procedure RegulatedData(const P: PComplex; const Len: Integer);
  protected
    procedure DoReceiveData(const P: PComplex; const Len: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetFIR(const P: PComplex; const Len: Integer; const FreqDomain: Boolean = False); overload;
    procedure SetFIR(const P: PDouble; const Len: Integer);
  end;

  { TResampleNode }

  TResampleNode = class(TDataFlowNode)
  private
    FBuf: array [0..2048 - 1] of Complex;
    FCursor: Integer;
    FLastInput: Complex;
    FLastScaledIndex: Double;
    FInputRate: Cardinal;
    FOutputRate: Cardinal;
    procedure SetInputRate(AValue: Cardinal);
    procedure SetOutputRate(AValue: Cardinal);
  protected
    procedure DoReceiveData(const P: PComplex; const Len: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;

    property InputRate: Cardinal read FInputRate write SetInputRate ;
    property OutputRate: Cardinal read FOutputRate write SetOutputRate;
  end;

  { TRadioLogger }

  TRadioLogger = class
  private
    FLevel: TRadioLogLevel;  static;
  protected
    FInstance: TRadioLogger; static;
    procedure DoReport(const ALevel: TRadioLogLevel; const S: string); virtual;
  public
    class procedure Report(const ALevel: TRadioLogLevel; const AFormat: string; Params: array of const); overload;
    class procedure Report(const ALevel: TRadioLogLevel; const AFormat: string);
    class property Level: TRadioLogLevel read FLevel write FLevel;

    class function MsgToStr(const M: TRadioMessage): string;
  end;

// I don't like to creae too many CriticalSections
procedure RadioGlobalLock;
procedure RadioGlobalUnlock;

function MakeMessage(const Id: Integer; const ParamH, ParamL: PtrUInt;
  const Sender: TRadioModule = nil): TRadioMessage;

function ClassNameToModuleName(const S: string): string;

implementation

uses
  Math, SignalBasic, utils, util_math, util_config;

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

function MakeMessage(const Id: Integer; const ParamH, ParamL: PtrUInt;
  const Sender: TRadioModule): TRadioMessage;
begin
  Result.Id := Id;
  Result.ParamH := ParamH;
  Result.ParamL := ParamL;
  if Assigned(Sender) then Result.Sender := Sender.Name;
end;

function ClassNameToModuleName(const S: string): string;
begin
  Result := S;
  if Length(Result) < 1 then Exit;
  if Pos('TRadio', S) = 1 then Delete(Result, 1, 6)
  else
    if Result[1] in ['t', 'T'] then Delete(Result, 1, 1);
  if Copy(Result, Length(Result) - 5, 6) = 'Module' then Delete(Result, Length(Result) - 5, 6);
end;

{ TRadioLogger }

procedure TRadioLogger.DoReport(const ALevel: TRadioLogLevel; const S: string);
begin

end;

class procedure TRadioLogger.Report(const ALevel: TRadioLogLevel;
  const AFormat: string; Params: array of const);
var
  S: string;
begin
  if not Assigned(FInstance) then Exit;
  if FLevel > ALevel then Exit;
  S := Format(AFormat, Params);
  FInstance.DoReport(ALevel, S);
end;

class procedure TRadioLogger.Report(const ALevel: TRadioLogLevel;
  const AFormat: string);
begin
  if not Assigned(FInstance) then Exit;
  if FLevel > ALevel then Exit;
  FInstance.DoReport(ALevel, AFormat);
end;

class function TRadioLogger.MsgToStr(const M: TRadioMessage): string;
begin
  if M.Sender <> '' then
    Result := 'from ' + M.Sender
  else
    Result := 'from unknown';
  Result := Result + Format(' Id = %d, ParamH = %d, ParamL = %d', [M.Id, M.ParamH, M.ParamL]);
end;

{ TResampleNode }

procedure TResampleNode.SetInputRate(AValue: Cardinal);
begin
  if FInputRate = AValue then Exit;
  FInputRate := Max(1, AValue);
  FLastScaledIndex := 0;
end;

procedure TResampleNode.SetOutputRate(AValue: Cardinal);
begin
  if FOutputRate = AValue then Exit;
  FOutputRate := Max(1, AValue);
  FLastScaledIndex := 0;
end;

procedure TResampleNode.DoReceiveData(const P: PComplex; const Len: Integer);
label
  again;
var
  I: Integer = 0;
  K: Double;
  V: Double;
begin
  if FInputRate = FOutputRate then
  begin
    if FCursor > 0 then
    begin
      SendToNext(@FBuf[0], FCursor);
      FCursor := 0;
    end;

    SendToNext(P, Len);
    Exit;
  end;

  V := FLastScaledIndex;
  K := FInputRate / FOutputRate;
again:
  while FCursor <= High(FBuf) do
  begin
    V := V + K;
    I := Trunc(V);
    if I >= Len - 1 then
    begin
      FLastScaledIndex := V - Len;
      FLastInput := P[Len - 1];
      Exit;
    end;
    if I >= 0 then
      FBuf[FCursor] := P[I] * (V - I) + P[I + 1] * (1 + I - V)
    else
      FBuf[FCursor] := FLastInput * (V + 1) + P[0] * (- V);
    Inc(FCursor);
  end;

  if FCursor = High(FBuf) + 1 then
  begin
    SendToNext(@FBuf[0], High(FBuf) + 1);
    FCursor := 0;
    goto again;
  end;
end;

constructor TResampleNode.Create;
begin
  FInputRate := 1;
  FOutputRate := 1;
end;

destructor TResampleNode.Destroy;
begin
  inherited Destroy;
end;

{ TWindowNode }

procedure TWindowNode.RegulatedData(const P: PComplex; const Len: Integer);
var
  I: Integer;
begin
  if Len = High(FWnd) + 1 then
    for I := 0 to Len - 1 do P[I] := P[I] * FWnd[I];
  SendToNext(P, Len);
end;

function TWindowNode.GetOverlap: Integer;
begin
  Result := FRegulator.Overlap;
end;

function TWindowNode.GetWindowLen: Integer;
begin
  Result := High(FWnd) + 1;
end;

procedure TWindowNode.SetOverlap(AValue: Integer);
begin
  FRegulator.Overlap := AValue;
end;

constructor TWindowNode.Create;
begin
  inherited;
  FRegulator := TStreamRegulator.Create;
  FRegulator.OnRegulatedData := @RegulatedData;
end;

destructor TWindowNode.Destroy;
begin
  FRegulator.Free;
  inherited Destroy;
end;

procedure TWindowNode.SetWindow(const P: PDouble; const Len: Integer);
begin
  SetLength(FWnd, Len);
  Move(P^, FWnd[0], Len * SizeOf(P^));
  FRegulator.Size := Len;
end;

procedure TWindowNode.DoReceiveData(const P: PComplex; const Len: Integer);
begin
  FRegulator.ReceiveData(P, Len);
end;

{ TFIRNode }

procedure TFIRNode.SetTimeDomainFIR(const P: PComplex; const Len: Integer);
var
  T: array of Complex;
  I: Integer;
begin
  if Assigned(FFPlan) then
  begin
    FinalizePlan(FFPlan);
    FinalizePlan(FIPlan);
  end;
  SetLength(T, Len);
  ModArg(P, @T[0], Len);
  I := NextFastSize(2 * Len - 1);
  FRegulator.Size := I;
  SetLength(FHFIR, I);
  SetLength(FBuf, I);
  SetLength(FRes, I);
  FillChar(FHFIR[0], I * SizeOf(FHFIR[0]), 0);
  FillChar(FBuf[0], I * SizeOf(FHFIR[0]), 0);
  Move(P^, FBuf[0], Len * SizeOf(FHFIR[0]));
  FFPlan := BuildFFTPlan(I, False);
  FFT(FFPlan, @FBuf[0], @FHFIR[0]);
  FIPlan := BuildFFTPlan(I, True);
  FRegulator.Overlap := Len - 1;
  FTaps := Len;
end;

procedure TFIRNode.RegulatedData(const P: PComplex; const Len: Integer);
var
  I: Integer;
begin
  if Assigned(FFPlan) then
  begin
    if Len <> High(FBuf) + 1 then
    begin
      TRadioLogger.Report(llWarn, 'TFIRNode.RegulatedData: Len <> High(FBuf) + 1');
      Exit;
    end;

    FFT(FFPlan, P, @FBuf[0]);
    for I := 0 to Len - 1 do
      FBuf[I] := FBuf[I] * FHFIR[I];
    FFT(FIPlan, @FBuf[0], @FRes[0]);
    I := FTaps - 1;
    SendToNext(@FRes[I], Len - I);
  end
  else
    TRadioLogger.Report(llError, 'TFIRNode.RegulatedData: FPlan = nil');
end;

constructor TFIRNode.Create;
begin
  inherited Create;
  FRegulator := TStreamRegulator.Create;
  FRegulator.OnRegulatedData := @RegulatedData;
end;

destructor TFIRNode.Destroy;
begin
  FRegulator.Free;
  inherited Destroy;
end;

procedure TFIRNode.SetFIR(const P: PComplex; const Len: Integer;
  const FreqDomain: Boolean);
var
  T: array of Complex;
  X: PFFTPlan;
begin
  if not FreqDomain then
    SetTimeDomainFIR(P, Len)
  else begin
    SetLength(T, Len);
    X := BuildFFTPlan(Len, True);
    FFT(X, P, @T[0]);
    FinalizePlan(X);
    SetTimeDomainFIR(@T[0], Len);
  end;
end;

procedure TFIRNode.SetFIR(const P: PDouble; const Len: Integer);
var
  X: array of Complex;
  I: Integer;
begin
  SetLength(X, Len);
  for I := 0 to Len - 1 do
  begin
    X[I].re := P[I];
    X[I].im := 0;
  end;
  SetTimeDomainFIR(PComplex(@X[0]), Len);
end;

procedure TFIRNode.DoReceiveData(const P: PComplex; const Len: Integer);
begin
  FRegulator.ReceiveData(P, Len);
end;

{ TRegulatorNode }

procedure TRegulatorNode.DoReceiveData(const P: PComplex; const Len: Integer);
begin
  FRegulator.ReceiveData(P, Len);
end;

constructor TRegulatorNode.Create;
begin
  inherited Create;
  FRegulator := TStreamRegulator.Create;
  FRegulator.OnRegulatedData := @SendToNext;
end;

destructor TRegulatorNode.Destroy;
begin
  FRegulator.Free;
  inherited Destroy;
end;

{ TDataFlowNode }

procedure TDataFlowNode.DoReceiveData(const P: PComplex; const Len: Integer);
begin

end;

destructor TDataFlowNode.Destroy;
begin
  if Assigned(FNext) then FNext.Free;
  inherited Destroy;
end;

procedure TDataFlowNode.SendToNext(const P: PComplex; const Len: Integer);
begin
  if Assigned(FOnSendToNext) then
    FOnSendToNext(P, Len)
  else if Assigned(FNext) then
    FNext.ReceiveData(P, Len);
end;

procedure TDataFlowNode.ReceiveData(const P: PComplex; const Len: Integer);
var
  I: Integer;
begin
  if FHoldCount <= 0 then
    DoReceiveData(P, Len)
  else begin
    I := High(FCache) + 1;
    SetLength(FCache, High(FCache) + Len + 1);
    Move(P^, FCache[I], Len * SizeOf(P^));
  end;
end;

procedure TDataFlowNode.Hold;
begin
  Inc(FHoldCount);
end;

procedure TDataFlowNode.ReleaseHold;
begin
  Dec(FHoldCount);
  if FHoldCount = 0 then
  begin
    if High(FCache) >= 0 then
    begin
      DoReceiveData(@FCache[0], High(FCache) + 1);
      SetLength(FCache, 0);
    end;
  end;
end;

procedure TDataFlowNode.Connect(ANext: TDataFlowNode);
begin
  FNext := ANext;
end;

function TDataFlowNode.LastNode: TDataFlowNode;
begin
  Result := Self;
  while Assigned(Result.FNext) do Result := Result.FNext;
end;

{ TRadioMessageQueue }

function TRadioMessageQueue.GetNotEmpty: Boolean;
begin
  Result := Assigned(FFirstExecMsg.Next);
end;

procedure TRadioMessageQueue.SetInQueue(AValue: Boolean);
var
  Req: Boolean = False;
begin
  Lock;
  if FInQueue <> AValue then
  begin
    FInQueue := AValue;
    Req := not AValue;
  end;
  Unlock;
  if Req and NotEmpty then
    RequestSchudule;
end;

procedure TRadioMessageQueue.SetMessageFilter(AValue: TRadioMessageIdSet);
var
  P: PRadioMessageNode;
  T: PRadioMessageNode;
  F: Boolean = False;
begin
  if FMessageFilter = AValue then Exit;
  FMessageFilter := AValue;
  P := @FFirstMsg;
  if not Assigned(P^.Next) then Exit;

  Lock;
  while Assigned(P^.Next) do
  begin
    T := P^.Next;
    if (FMessageFilter and (1 shl T^.Msg.Id)) > 0 then
    begin
      P^.Next := T^.Next;
      FLastExecMsg^.Next := T;
      T^.Next := nil;
      FLastExecMsg := T;
      F := True;
      if not Assigned(P^.Next) then
      begin
        FLastMsg := P;
        Break;
      end;
    end
    else
      P := T;
  end;
  UnLock;

  if F then RequestSchudule;
end;

procedure TRadioMessageQueue.RequestSchudule;
begin
  if not FInQueue then FRunQueue.Request(Self);
end;

procedure TRadioMessageQueue.MessageExceute;
var
  Msg: TRadioMessage;
  Ret: Integer = 0;
  P: PRadioMessageNode;
begin
  if not Assigned(FFirstExecMsg.Next) then Exit;
  Lock;
  P := FFirstExecMsg.Next;
  Msg := P^.Msg;
  FFirstExecMsg.Next := P^.Next;
  if not Assigned(FFirstExecMsg.Next) then
    FLastExecMsg := @FFirstExecMsg;
  UnLock;
  Dispose(P);

  TRadioLogger.Report(llVerbose, 'in thread #' + IntToStr(GetThreadID) + ', ' + Name + Format(' start exec msg %s', [TRadioLogger.MsgToStr(Msg)]));
  ProccessMessage(Msg, Ret);
  TRadioLogger.Report(llVerbose, Name + ' msg exec done');
end;

constructor TRadioMessageQueue.Create(RunQueue: TRadioRunQueue);
begin
  FMessageFilter := $FFFFFFFF;
  FLastExecMsg := @FFirstExecMsg;
  FLastMsg := @FFirstMsg;
  FRunQueue := RunQueue;
end;

procedure TRadioMessageQueue.Lock;
begin
  RadioGlobalLock;
end;

procedure TRadioMessageQueue.UnLock;
begin
  RadioGlobalUnlock;
end;

procedure TRadioMessageQueue.StoreMessage(const Msg: TRadioMessage);
var
  P: PRadioMessageNode;
begin
  New(P);
  P^.Msg := Msg;
  P^.Next := nil;
  if (MessageFilter and (1 shl Msg.Id)) > 0 then
  begin
    Lock;
    FLastExecMsg^.Next := P;
    FLastExecMsg := P;
    UnLock;
    RequestSchudule;
  end
  else begin
    Lock;
    FLastMsg^.Next := P;
    FLastMsg := P;
    UnLock;
  end;
end;

procedure TRadioMessageQueue.MessageQueueReset;
  procedure FreeList(P: PRadioMessageNode);
  var
    T: PRadioMessageNode;
  begin
    while Assigned(P) do
    begin
      T := P^.Next;
      Dispose(P);
      P := T;
    end;
  end;

begin
  Lock;
  FreeList(FFirstMsg.Next);
  FreeList(FFirstExecMsg.Next);
  FFirstMsg.Next := nil;
  FLastMsg := @FFirstMsg;
  FFirstExecMsg.Next := nil;
  FLastExecMsg := @FFirstExecMsg;
  UnLock;
end;

{ TBackgroundRadioModule }

procedure TBackgroundRadioModule.ThreadFun(Thread: TGenericRadioThread);
begin
  Sleep(10);
end;

function TBackgroundRadioModule.DoStart: Boolean;
begin
  FThread.Suspended := False;
  Result := inherited;
end;

function TBackgroundRadioModule.DoStop: Boolean;
begin
  FThread.Suspended := True;
  Result := inherited;
end;

constructor TBackgroundRadioModule.Create(RunQueue: TRadioRunQueue);
begin
  inherited;
  FThread := TGenericRadioThread.Create(Self);
end;

destructor TBackgroundRadioModule.Destroy;
begin
  FThread.Free;
  inherited;
end;

{ TGenericRadioThread }

procedure TGenericRadioThread.Execute;
begin
  while not Terminated do
    if Assigned(FModule) then FModule.ThreadFun(Self);
end;

constructor TGenericRadioThread.Create(Module: TBackgroundRadioModule);
begin
  inherited Create(True);
  FModule := Module;
end;

{ TRadioRunQueue }

procedure TRadioRunQueue.Schedule;
label
  again;
var
  T: PRadioThreadNode;
  P: PMessageQueueNode;
begin
again:
  Lock;
  if Assigned(FFirstJob.Next) and Assigned(FIdleNode.Next) then
  begin
    T := FIdleNode.Next;
    FIdleNode.Next := T^.Next;
    P := FFirstJob.Next;
    FFirstJob.Next := P^.Next;
    UnLock;

    T^.Thread.Job := P^.Queue;
    Dispose(P);
    RTLEventSetEvent(T^.Thread.FJobScheduled);
    goto again;
  end
  else
    Unlock;
end;

procedure TRadioRunQueue.Lock;
begin
  RadioGlobalLock;
end;

procedure TRadioRunQueue.UnLock;
begin
  RadioGlobalUnlock;
end;

procedure TRadioRunQueue.WorkerIdle(Worker: TRadioThread);
begin
  Lock;
  Worker.Node^.Next := FIdleNode.Next;
  FIdleNode.Next := Worker.Node;
  Unlock;
  Schedule;
end;

constructor TRadioRunQueue.Create(const SMP: Integer);
var
  I: Integer;
  P: PRadioThreadNode;
begin
  SetLength(FWorkers, SMP);
  for I := 1 to SMP do
  begin
    New(P);
    P^.Thread := TRadioThread.Create;
    P^.Thread.Node := P;
    P^.Thread.FRunQueue := Self;
    P^.Next := FIdleNode.Next;
    FIdleNode.Next := P;
    FWorkers[I - 1] := P^.Thread;
  end;
end;

destructor TRadioRunQueue.Destroy;
var
  T: TRadioThread;
begin
  for T in FWorkers do T.Free;
  inherited;
end;

procedure TRadioRunQueue.Request(Job: TRadioMessageQueue);
var
  T: PRadioThreadNode;
  P: PMessageQueueNode;
  F: Boolean = False;
begin
  Lock;
  if not Job.InQueue then
  begin
    F := True;
    Job.InQueue := True;
  end;
  Unlock;

  if not F then Exit;

  Lock;
  if Assigned(FIdleNode.Next) then
  begin
    T := FIdleNode.Next;
    FIdleNode.Next := T^.Next;
    UnLock;

    T^.Thread.Job := Job;
    RTLEventSetEvent(T^.Thread.FJobScheduled);

    Exit;
  end
  else
    UnLock;

  New(P);
  P^.Queue := Job;

  Lock;
  P^.Next := FFirstJob.Next;
  FFirstJob.Next := P;
  Unlock;

  Schedule;
end;

procedure TRadioRunQueue.Terminate;
var
  T: TRadioThread;
begin
  for T in FWorkers do T.Terminate;
end;

{ TRadioThread }

procedure TRadioThread.Execute;
var
  J: TRadioMessageQueue;
  T: TTime;
begin
  while not Terminated do
  begin
    RTLEventWaitFor(FJobScheduled);
    if Terminated then Break;

    RTLEventResetEvent(FJobScheduled);

    if Assigned(FJob) then
    begin
      J := FJob;
      FJob := nil;

      T := Now;

      J.RunThread := Self;
      while J.NotEmpty do
      begin
        J.MessageExceute;
        if Terminated then Break;
      end;
      J.RunThread := nil;
      J.FCPUTime := J.FCPUTime + Now - T;

      FRunQueue.WorkerIdle(Self);
      J.InQueue := False;
    end
    else;
  end;
end;

constructor TRadioThread.Create;
begin
  FJobScheduled := RTLEventCreate;
  inherited Create(False);
end;

destructor TRadioThread.Destroy;
begin
  Terminate;
  RTLeventSetEvent(FJobScheduled);
  WaitFor;
  RTLEventDestroy(FJobScheduled);
  inherited Destroy;
end;

{ TStreamRegulator }

procedure TStreamRegulator.SetOnReceiveData(AValue: TReceiveData);
begin
  if FOnReceiveData = AValue then Exit;
  FOnReceiveData := AValue;
end;

procedure TStreamRegulator.SetOverlap(AValue: Integer);
begin
  if AValue < 0 then
    FOverlap := 0
  else if AValue > FSize - 1 then
    FOverlap := FSize - 1
  else
    FOverlap := AValue;
end;

procedure TStreamRegulator.SetSize(AValue: Integer);
begin
  if AValue < 1 then Exit;
  if FSize = AValue then Exit;

  FSize := AValue;
  CallOnRegulatedData;

  SetLength(FData, FSize);
  Overlap := FOverlap;
end;

procedure TStreamRegulator.CallOnRegulatedData;
var
  I: Integer = 0;
begin
  while FCursor >= FSize do
  begin
    if Assigned(FOnReceiveData) then FOnReceiveData(@FData[I], FSize);
    Inc(I, FSize - FOverlap);
    Dec(FCursor, FSize - FOverlap);
  end;
  if I > 0 then
    Move(FData[I], FData[0], FCursor * SizeOf(FData[0]));
end;

constructor TStreamRegulator.Create;
begin
  FSize := 1024;
  SetLength(FData, FSize);
end;

procedure TStreamRegulator.ReceiveData(P: PComplex; Len: Integer);
var
  F: Integer;
  S: Integer;
begin
  while Len > 0 do
  begin
    F := FSize - FCursor;
    if F <= 0 then
    begin
      CallOnRegulatedData;
      F := FSize - FCursor;
    end;
    S := Min(F, Len);
    Move(P^, FData[FCursor], S * SizeOf(P^));
    Inc(P, S);
    Inc(FCursor, S);
    Dec(Len, S);
  end;
  CallOnRegulatedData;
end;

{ TRadioDataStream }

function TRadioDataStream.GetBuffer(const Index: Integer): PComplex;
begin
  Result := @FBuffers[Index].Data[0];
end;

function TRadioDataStream.GetBufferCount: Integer;
begin
  Result := High(FBuffers) + 1;
end;

function TRadioDataStream.GetDefBufferSize: Integer;
begin
  Result := FBlockSize;
end;

procedure TRadioDataStream.SetDefBufferSize(AValue: Integer);
var
  F: Boolean = False;
  I: Integer;
begin
  Lock;

  FBlockSize := AValue;
  for I := Low(FBuffers) to High(FBuffers) do
    if not FBuffers[I].Allocated then SetLength(FBuffers[I].Data, AValue);

  Unlock;
end;

constructor TRadioDataStream.Create(Module: TRadioModule; const AName: string;
  const BlockSize: Integer);
var
  I: Integer;
begin
  FName := AName;
  FBlockSize := BlockSize;
  FModule := Module;
end;

destructor TRadioDataStream.Destroy;
var
  I: Integer;
begin
  for I := Low(FBuffers) to High(FBuffers) do
    SetLength(FBuffers[I].Data, 0);
  inherited Destroy;
end;

procedure TRadioDataStream.SafeFree;
var
  I: Integer;
begin
  Lock;
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
  Unlock;
end;

procedure TRadioDataStream.Lock;
begin
  RadioGlobalLock;
end;

procedure TRadioDataStream.Unlock;
begin
  RadioGlobalUnlock;
end;

function TRadioDataStream.TryAlloc(out Index: Integer): PComplex;
var
  I: Integer;
begin
  Result := nil;

  Lock;
  for I := Low(FBuffers) to High(FBuffers) do
  begin
    if not FBuffers[I].Allocated then
    begin
      if High(FBuffers[I].Data) <> FBlockSize - 1 then
        SetLength(FBuffers[I].Data, FBlockSize);
      Result := @FBuffers[I].Data[0];
      FBuffers[I].Allocated := True;
      Index := I;
      Break;
    end;
  end;
  Unlock;
end;

procedure TRadioDataStream.Broadcast(const Index: Integer; Listeners: TList);
var
  M: TRadioMessage;
  P: Pointer;
  L: PDataListener;
begin
  Lock;
  FBuffers[Index].Counter := Listeners.Count;
  if FBuffers[Index].Counter < 1 then
    FBuffers[Index].Allocated := False;
  Unlock;

  with M do
  begin
    Id := RM_DATA;
    Sender := FModule.Name;
    ParamH := PtrInt(Self);
  end;

  if FBuffers[Index].Allocated then
  begin
    for P in Listeners do
    begin
      L := PDataListener(P);
      M.ParamL := (L^.Port shl 16) or Index;
      L^.M.PostMessage(M);
    end;
  end
  else;
end;

procedure TRadioDataStream.Release(const Index: Integer);
var
  M: TRadioMessage;
begin
  Lock;
  Dec(FBuffers[Index].Counter);
  if FBuffers[Index].Counter < 1 then
    FBuffers[Index].Allocated := False;
  Unlock;
end;

function TRadioDataStream.GetBufferSize(const Index: Integer): Integer;
begin
  Result := Length(FBuffers[Index].Data);
end;

{ TRadioModule }

procedure TRadioModule.PostMessage(const Msg: TRadioMessage);
begin
  //if (not Running) and (Msg.Id <> RM_CONTROL) then Exit;
  StoreMessage(Msg);
end;

procedure TRadioModule.Broadcast(const Msg: TRadioMessage);
var
  P: Pointer;
begin
  for P in FFeatureListeners do
    TRadioModule(P).PostMessage(Msg);
end;

procedure TRadioModule.Broadcast(const AId: Integer; const AParamH,
  AParamL: PtrUInt);
var
  M: TRadioMessage;
begin
  with M do
  begin
    Sender := Name;
    Id := AId;
    ParamH := AParamH;
    ParamL := AParamL;
  end;
  Broadcast(M);
end;

function TRadioModule.FindDataListener(Listener: TRadioModule): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FDataListeners.Count - 1 do
  begin
    if PDataListener(FDataListeners[I])^.M = Listener then Exit(I);
  end;
end;

procedure TRadioModule.LoadIconRes(ResName: string);
var
  N: string;
  L: TStrings;
begin
  if ResName = '' then ResName := ClassNameToModuleName(ClassName);
  ResName := ResName + '.icon.txt';
  N := GetResFullName(ResName);
  if FileExists(N) then
  begin
    L := TStringList.Create;
    try
      L.LoadFromFile(N);
      L.StrictDelimiter := True;
      L.Delimiter := ';';
      FIcon := L.DelimitedText;
    finally
      L.Free
    end;
  end
  else
    FIcon := 'text (0,0),' + ClassNameToModuleName(ClassName);
end;

function TRadioModule.QueryInterface(constref iid: tguid; out obj): longint;
  stdcall;
begin
   if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TRadioModule._AddRef: Integer; stdcall;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TRadioModule._Release: Integer; stdcall;
begin
  Result := InterlockedDecrement(FRefCount);
  //if Result = 0 then
  //  Destroy;
end;

procedure TRadioModule.Measure(ACanvas: TCanvas; out Extent: TSize);
var
  E, F: TSize;
begin
  with ACanvas do
  begin
    Font.Height := HEADER_FONT_HEIGHT;
    Font.Style := [];
    E := TextExtent(Name);
    if FHasConfig then Inc(E.cx, TextExtent(BTN_CONFIG).cx + 20) else Inc(E.cx, 5);
    if FHasGUI then Inc(E.cx, TextExtent(BTN_GUI).cx + 20) else Inc(E.cx, 5);
  end;
  FDescStr.Clear;
  Describe(FDescStr);
  F := StyledTextExtent(ACanvas, FDescStr);
  Extent.cy := Max(ICON_SIZE, F.cy);
  Extent.cx := Max(ICON_SIZE, F.cx);
  Extent.cx := Max(Extent.cx, E.cx);
  Inc(Extent.cy, HEADER_SIZE + BODER_WIDTH * 2 + 4);
  Inc(Extent.cx, BODER_WIDTH * 2 + 4);
end;

procedure TRadioModule.Draw(ACanvas: TCanvas; ARect: TRect);
var
  IconRect: TRect;
  C: TPoint;
  E: TSize;
begin
  FInvalidated := False;
  with ACanvas do
  begin
    Pen.Style := psSolid;
    Pen.Width := BODER_WIDTH;
    Pen.Color := TColor($00AAFF);
    Brush.Color := clCream; // $fafafa;
    Brush.Style := bsSolid;
    RoundRect(ARect, 8, 8);
    FloodFill(ARect.Left + 10, ARect.Top + 10, clWhite, fsSurface);

    // shrink, border excluded
    Inc(ARect.Left, BODER_WIDTH + 1);
    Inc(ARect.Top, BODER_WIDTH + 1);
    Dec(ARect.Right, BODER_WIDTH - 1);
    Dec(ARect.Bottom, BODER_WIDTH - 1);

    Font.Style := [];
    Font.Height := HEADER_FONT_HEIGHT;
    Font.Color := clBlack;
    Font.Bold  := True;
    TextRect(ARect, ARect.Left + 3, ARect.Top, Name);

    // buttons
    Font.Color := TColor($FF5050);
    if FHasConfig then
    begin
      E := TextExtent(BTN_CONFIG);
      with FConfigBtnRect do
      begin
        Left := ARect.Right - E.cx - 8;
        Top  := ARect.Top;
        Right := Left + E.cx;
        Bottom := Top + E.cy;
        TextRect(ARect, Left, Top, BTN_CONFIG);
      end;
    end
    else
      FConfigBtnRect.Left := ARect.Right;
    if FHasGUI then
    begin
      E := TextExtent(BTN_GUI);
      with FGUIBtnRect do
      begin
        Left := FConfigBtnRect.Left - E.cx - 15;
        Top  := ARect.Top;
        Right := Left + E.cx;
        Bottom := Top + E.cy;
        TextRect(ARect, Left, Top, BTN_GUI);
      end;
    end;


    Pen.Width := 1;
    Inc(ARect.Top, HEADER_SIZE - BODER_WIDTH);
    Line(ARect.Left, ARect.Top, ARect.Right, ARect.Top);

    // icon
    C.x := (ARect.Left + ARect.Right) div 2;
    C.y := (ARect.Top + ARect.Bottom) div 2;
    IconRect.Left   := C.x - ICON_SIZE div 2;
    IconRect.Right  := C.x + ICON_SIZE div 2;
    IconRect.Top    := C.y - ICON_SIZE div 2;
    IconRect.Bottom := C.y + ICON_SIZE div 2;
    StrIconDraw(ACanvas, IconRect, FIcon);

    // description strings
    Font.Height := HEADER_FONT_HEIGHT;
    FDescStr.Clear;
    Describe(FDescStr);
    StyledTextOut(ACanvas, ARect, FDescStr);
  end;
end;

procedure TRadioModule.MouseClick(const Pt: TPoint);
begin
  if IsPtInRect(Pt, FGUIBtnRect) then
    PostMessage(RM_SHOW_MAIN_GUI, 0, 0)
  else if IsPtInRect(Pt, FConfigBtnRect) then
    PostMessage(RM_CONFIGURE, 0, 0);
end;

function TRadioModule.Invalidated: Boolean;
begin
  Result := FInvalidated;
end;

function TRadioModule.Alloc(Stream: TRadioDataStream; out Index: Integer
  ): PComplex;
begin
  Result := Stream.TryAlloc(Index);
  while (not RunThread.Terminated) and (Result = nil) do
  begin
    Sleep(10);
    Result := Stream.TryAlloc(Index);
  end;
end;

procedure TRadioModule.ProccessMessage(const Msg: TRadioMessage;
  var Ret: Integer);
begin
  case Msg.Id of
    RM_Control: RMControl(Msg, Ret);
    RM_DATA     : RMData(Msg, Ret);
    RM_REPORT   : RMReport(Msg, Ret);
    RM_SET_FEATURE: RMSetFeature(Msg, Ret);
    RM_TIMER      : RMTimer(Msg, Ret);
    RM_CONFIGURE:   TThread.Synchronize(nil, @DoConfigure);
    RM_SHOW_MAIN_GUI:   TThread.Synchronize(nil, @DoShowGUI)
  else
  end;
end;

procedure TRadioModule.RMControl(const Msg: TRadioMessage; var Ret: Integer);
begin
  case Msg.ParamH of
    0: FRunning := DoStart;
    1: FRunning := not DoStop;
    2: DoReset;
  end;
end;

procedure TRadioModule.RMData(const Msg: TRadioMessage; var Ret: Integer);
var
  B: TRadioDataStream;
  Port: Integer;
begin
  B := TRadioDataStream(Pointer(Msg.ParamH));
  Port := Msg.ParamL shr 16;
  if Port = 0 then
    ReceiveData(B.Buffer[Msg.ParamL], B.GetBufferSize(Msg.ParamL))
  else
    ReceiveData(Port, B.Buffer[Msg.ParamL and $FFFF], B.GetBufferSize(Msg.ParamL));
  B.Release(Msg.ParamL and $FFFF);
end;

procedure TRadioModule.RMSetFeature(const Msg: TRadioMessage; var Ret: Integer);
begin
  case Msg.ParamH of
    RM_FEATURE_BANDWIDTH: RMSetBandwidth(Msg, Msg.ParamL);
    RM_FEATURE_FREQ:      RMSetFrequency(Msg, Msg.ParamL);
    RM_FEATURE_PHASE_ADJ: RMPhaseAdjust(Msg, Msg.ParamL);
    RM_FEATURE_SAMPLE_RATE: RMSetSampleRate(Msg, Msg.ParamL);
  end;
end;

procedure TRadioModule.RMReport(const Msg: TRadioMessage; var Ret: Integer);
begin
end;

procedure TRadioModule.RMTimer(const Msg: TRadioMessage; var Ret: Integer);
begin
end;

function TRadioModule.RMSetFrequency(const Msg: TRadioMessage;
  const Freq: Cardinal): Integer;
begin
  Broadcast(Msg);
  Result := 0;
end;

function TRadioModule.RMSetBandwidth(const Msg: TRadioMessage;
  const Bandwidth: Cardinal): Integer;
begin
  Result := 0;
end;

function TRadioModule.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  Broadcast(Msg);
  Result := 0;
end;

function TRadioModule.RMPhaseAdjust(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  Result := 0;
end;

procedure TRadioModule.DoReset;
begin
  MessageQueueReset;
end;

function TRadioModule.DoStart: Boolean;
begin
  Result := True;
end;

function TRadioModule.DoStop: Boolean;
begin
  Result := True;
end;

procedure TRadioModule.Describe(Strs: TStrings);
begin
end;

procedure TRadioModule.ShowConnections(Graph: TGenGraph);
const
  PORT_FEATURE = 0;
  PORT_DATA    = 1;
var
  I: Integer;
  P: Pointer;
begin
  for P in FDataListeners do
  begin
    Graph.AddConnection(GraphNode, PDataListener(P)^.M.GraphNode,
                        PORT_DATA, PDataListener(P)^.Port + PORT_DATA);
  end;

  for P in FFeatureListeners do
  begin
    Graph.AddConnection(GraphNode, TRadioModule(P).GraphNode,
                        PORT_FEATURE, PORT_FEATURE).PenStyle := psDash;
  end;
end;

constructor TRadioModule.Create(RunQueue: TRadioRunQueue);
begin
  inherited;
  FHasConfig := True;
  FDataListeners := TList.Create;
  FFeatureListeners := TList.Create;
  FDefOutput := TRadioDataStream.Create(Self, 'output', 1024 * 5);
  FLastMsg := @FFirstMsg;
  FDescStr := TStringList.Create;
  LoadIconRes;
end;

destructor TRadioModule.Destroy;
begin
  FDescStr.Free;
  ClearDataListeners;
  FDefOutput.SafeFree;
  FDataListeners.Free;
  FFeatureListeners.Free;
  inherited Destroy;
end;

procedure TRadioModule.GraphInvalidate;
begin
  FInvalidated := True;
end;

procedure TRadioModule.PostMessage(const Id: Integer; const ParamH,
  ParamL: PtrUInt);
var
  M: TRadioMessage;
begin
  M.Id := Id;
  M.ParamH := ParamH;
  M.ParamL := ParamL;
  PostMessage(M);
end;

procedure TRadioModule.DoConfigure;
begin

end;

procedure TRadioModule.DoShowGUI;
begin

end;

procedure TRadioModule.AddDataListener(Listener: TRadioModule;
  const Port: Integer);
var
  P: PDataListener;
begin
  if FindDataListener(Listener) < 0 then
  begin
    New(P);
    P^.M := Listener;
    P^.Port := Port;
    FDataListeners.Add(P);
  end;
end;

procedure TRadioModule.AddFeatureListener(Listener: TRadioModule);
begin
  with FFeatureListeners do
    if IndexOf(Listener) < 0 then Add(Listener);
end;

procedure TRadioModule.RemoveDataListener(Listener: TRadioModule);
var
  I: Integer;
  P: PDataListener;
begin
  I := FindDataListener(Listener);
  if I >= 0 then
  begin
    P := PDataListener(FDataListeners[I]);
    FDataListeners.Delete(I);
    Dispose(P);
  end;
end;

procedure TRadioModule.RemoveFeatureListener(Listener: TRadioModule);
begin
  FFeatureListeners.Remove(Listener);
end;

procedure TRadioModule.ClearDataListeners;
var
  P: Pointer;
begin
  for P in FDataListeners do Dispose(PDataListener(P));
  FDataListeners.Clear;
end;

procedure TRadioModule.ClearFeatureListeners;
begin
  FFeatureListeners.Clear;
end;

procedure TRadioModule.ReceiveData(const P: PComplex; const Len: Integer);
begin

end;

procedure TRadioModule.ReceiveData(const Port: Integer; const P: PComplex;
  const Len: Integer);
begin

end;

initialization

  InitCriticalSection(RadioGlobalCS);

finalization

  DoneCriticalsection(RadioGlobalCS);

end.

