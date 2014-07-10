unit rm_dump;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex, RadioModule, RadioSystem;

const
  RM_DUMP_START = RM_USER + 0;              // ParamH: TStream, ParamL: MaxSize
  RM_DUMP_STOP  = RM_USER + 1;              // no param, TStream is closed here

  RM_DUMP_PLAYER_START = RM_USER + 0;       // ParamH: TStream
  RM_DUMP_PLAYER_STOP  = RM_USER + 1;       // no param, TStream is closed here

type

  TDumpTag = (dtMsgBlob, dtDataBlob);

  TDumpHeader = record
    Id: array [0..7] of Char;
    Size: Cardinal;
    MagicCardinal: Cardinal;
    MagicDouble: Double;
    Verion: Integer;
  end;

  TMsgBlob = record
    Tag: TDumpTag;
    Id: Integer;
    ParamH: PtrUInt;
    ParamL: PtrUInt;
  end;

  TDataBlob = record
    Tag: TDumpTag;
    SampleRate: Cardinal;
    Len: Cardinal;
  end;

  { TRadioDump }

  TRadioDump = class(TRadioModule)
  private
    FFile: TStream;
    FQuota: Cardinal;
    FSampleRate: Cardinal;
    procedure DumpMsg(const Msg: TRadioMessage);
  protected
    function  RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer;
      override;
    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: Integer); override;
  public
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

  { TRadioDumpPlayer }

  TRadioDumpPlayer = class(TBackgroundRadioModule)
  private
    FFile: TStream;
    FStartEvent: PRTLEvent;
    FStoppedEvent: PRTLEvent;
    FRegulator: TStreamRegulator;
    FSampleRate: Cardinal;
    procedure Suspend;
  protected
    procedure ThreadFun(Thread: TGenericRadioThread); override;
    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: Integer); override;
    procedure ReceiveRegulatedData(const P: PComplex; const Len: Integer);
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;
  end;

implementation

const
  DUMP_FILE_VERSION = 1;

  DUMP_HEADER: TDumpHeader =
    (Id: ('L', 'a', 'z', 'R', 'a', 'd', 'i', 'o');
     Size: Sizeof(TDumpHeader);
     MagicCardinal: $12345678;
     MagicDouble: 1.5/65536;
     Verion: DUMP_FILE_VERSION);

{ TRadioDumpPlayer }

procedure TRadioDumpPlayer.Suspend;
begin
  FFile := nil;
  RTLeventWaitFor(FStoppedEvent);
end;

procedure TRadioDumpPlayer.ThreadFun(Thread: TGenericRadioThread);
label
  Wait;
var
  F: TStream;
  H: TDumpHeader;
  D: TDataBlob;
  M: TMsgBlob;
  T: array of Complex;

  function ReadRec(P: Pointer; const S: Integer): Boolean;
  begin
    Result := F.Read(PByte(P)^, S) = S;
  end;

begin
  RTLeventWaitFor(FStartEvent);
  RTLeventResetEvent(FStartEvent);
  RTLeventResetEvent(FStoppedEvent);
  F := FFile;
  if not Assigned(F) then goto Wait;

  F.Position := 0;
  if not ReadRec(@H, SizeOf(H)) then goto Wait;
  if CompareMem(@H, @DUMP_HEADER, SizeOf(H)) then goto Wait;

  while ReadRec(@D, SizeOf(D)) do
  begin
    case D.Tag of
      dtDataBlob:
        begin
          if FSampleRate <> D.SampleRate then
          begin
            FSampleRate := D.SampleRate;
            Broadcast(RM_SET_FEATURE, RM_FEATURE_SAMPLE_RATE, FSampleRate);
          end;
          SetLength(T, D.Len);
          if not ReadRec(@T[0], D.Len * SizeOf(T[0])) then Break;
          FRegulator.ReceiveData(@T[0], D.Len);
        end;
      dtMsgBlob:
        begin
          F.Seek(-SizeOf(D), soCurrent);
          if not ReadRec(@M, SizeOf(M)) then Break;
          Broadcast(M.Id, M.ParamH, M.ParamL);
        end;
    end;
    if F.Position = F.Size then
      F.Position := SizeOf(DUMP_HEADER);
  end;
Wait:
  RTLeventSetEvent(FStoppedEvent);
end;

procedure TRadioDumpPlayer.ProccessMessage(const Msg: TRadioMessage;
  var Ret: Integer);
begin
  Ret := 0;
  case Msg.Id of
    RM_DUMP_PLAYER_START:
      begin
        Suspend;
        FFile := TStream(Msg.ParamH);
        RTLeventSetEvent(FStartEvent);
      end;
    RM_DUMP_PLAYER_STOP: Suspend;
  end;
end;

procedure TRadioDumpPlayer.ReceiveRegulatedData(const P: PComplex;
  const Len: Integer);
var
  I: Integer;
  D: PComplex;
begin
  D := DefOutput.Alloc(I);
  while D = nil do
  begin
    Sleep(20);
    D := DefOutput.Alloc(I);
  end;
  Move(P^, D^, Len * SizeOf(P^));
  DefOutput.Broadcast(I, FDataListeners);
end;

constructor TRadioDumpPlayer.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  FRegulator := TStreamRegulator.Create;
  FRegulator.Size := DefOutput.BufferSize;
  FRegulator.OnRegulatedData := @ReceiveRegulatedData;
  FStartEvent := RTLEventCreate;
  FStoppedEvent := RTLEventCreate;
  RTLeventSetEvent(FStoppedEvent);
end;

destructor TRadioDumpPlayer.Destroy;
begin
  Suspend;
  RTLeventdestroy(FStartEvent);
  RTLeventdestroy(FStoppedEvent);
  inherited Destroy;
end;

{ TRadioDump }

procedure TRadioDump.DumpMsg(const Msg: TRadioMessage);
var
  X: TMsgBlob;
begin
  if FFile = nil then Exit;
  if FFile.Size > FQuota then FreeAndNil(FFile);
  with X do
  begin
    Tag := dtMsgBlob;
    Id := Msg.Id;
    ParamH := Msg.ParamH;
    ParamL := Msg.ParamL;
  end;
  FFile.Write(X, SizeOf(X));
end;

function TRadioDump.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  FSampleRate := Rate;
  Result := 0;
end;

procedure TRadioDump.ProccessMessage(const Msg: TRadioMessage; var Ret: Integer
  );
begin
  case Msg.Id of
    RM_DUMP_START:
      begin
        FFile := TStream(Msg.ParamH);
        FQuota := Msg.ParamL;
        if FQuota = 0 then FQuota := MaxSIntValue;
        if Assigned(FFile) then
        begin
          FFile.Size := 0;
          FFile.Write(DUMP_HEADER, SizeOf(DUMP_HEADER));
        end;
      end;
    RM_DUMP_STOP: FreeAndNil(FFile);
    RM_DATA:
      inherited;
    else
      inherited;
      DumpMsg(Msg);
  end;
end;

destructor TRadioDump.Destroy;
begin
  FFile.Free;
  inherited Destroy;
end;

procedure TRadioDump.ReceiveData(const P: PComplex; const Len: Integer);
var
  X: TDataBlob;
begin
  if FFile = nil then Exit;
  if FFile.Size > FQuota then FreeAndNil(FFile);
  X.Tag := dtDataBlob;
  X.SampleRate := FSampleRate;
  X.Len := Len;
  FFile.Write(X, SizeOf(X));
  FFile.Write(P^, Len * SizeOf(P^));
end;

initialization

  RegisterModule('Dump',       TRadioModuleClass(TRadioDump.ClassType));
  RegisterModule('DumpPlayer', TRadioModuleClass(TRadioDumpPlayer.ClassType));

end.

