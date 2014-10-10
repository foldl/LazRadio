unit rm_dump;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, UComplex, RadioModule, RadioSystem, RadioMessage;

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
    procedure DoConfigure; override;
    procedure Describe(Strs: TStrings); override;
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
    procedure StopPlaying;
  protected
    procedure ThreadFun(Thread: TGenericRadioThread); override;
    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: Integer); override;
    procedure ReceiveRegulatedData(const P: PComplex; const Len: Integer);
    procedure DoConfigure; override;
    procedure DoStopThreadFun; override;
    procedure Describe(Strs: TStrings); override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;
  end;

implementation

uses
  SignalBasic;

const
  DUMP_FILE_VERSION = 1;

  DUMP_HEADER: TDumpHeader =
    (Id: ('L', 'a', 'z', 'R', 'a', 'd', 'i', 'o');
     Size: Sizeof(TDumpHeader);
     MagicCardinal: $12345678;
     MagicDouble: 1.5/65536;
     Verion: DUMP_FILE_VERSION);

{ TRadioDumpPlayer }

procedure TRadioDumpPlayer.StopPlaying;
var
  X: TStream;
begin
  if not Assigned(FFile) then Exit;
  X := FFile;
  FFile := nil;
  RTLeventWaitFor(FStoppedEvent);
  X.Free;
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
  if FThread.Terminated then goto Wait;

  if not Assigned(F) then goto Wait;

  F.Position := 0;
  if not ReadRec(@H, SizeOf(H)) then goto Wait;
  if (H.Id <> DUMP_HEADER.Id) or (H.Verion <> DUMP_HEADER.Verion)
     or (H.Size <> DUMP_HEADER.Size) or (H.MagicCardinal <> DUMP_HEADER.MagicCardinal)
     or (H.MagicDouble <> DUMP_HEADER.MagicDouble) then goto Wait;

  while (not FThread.Terminated) and (FFile <> nil) and ReadRec(@D, SizeOf(D)) do
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
        StopPlaying;
        FFile := TFileStream.Create(PString(Msg.ParamH)^, fmOpenRead);
        RTLeventSetEvent(FStartEvent);
        GraphInvalidate;
      end;
    RM_DUMP_PLAYER_STOP:
      begin
        StopPlaying;
        GraphInvalidate;
      end
    else
      inherited;
  end;
end;

procedure TRadioDumpPlayer.ReceiveRegulatedData(const P: PComplex;
  const Len: Integer);
var
  I: Integer;
  D: PComplex;
begin
  D := DefOutput.TryAlloc(I);
  while (D = nil) and (not FThread.Terminated) do
  begin
    Sleep(10);
    D := DefOutput.TryAlloc(I);
  end;
  if not Assigned(D) then Exit;
  Move(P^, D^, Len * SizeOf(P^));
  DefOutput.Broadcast(I, FDataListeners);
end;

procedure TRadioDumpPlayer.DoConfigure;
var
  D: TOpenDialog;
  F: TFileStream = nil;
begin
  D := TOpenDialog.Create(nil);
  D.Filter := 'radio dump (*.dump)|*.dump';
  if D.Execute then
  begin
    try
      F := TFileStream.Create(D.FileName, fmOpenRead);
      RadioPostMessage(RM_DUMP_PLAYER_START, PtrUInt(F), 0, Self);
    except
    end;
  end;
  D.Free;
end;

procedure TRadioDumpPlayer.DoStopThreadFun;
begin
  StopPlaying;
  RTLeventSetEvent(FStartEvent);
end;

procedure TRadioDumpPlayer.Describe(Strs: TStrings);
begin
  if Assigned(FFile) then
    Strs.Add('^bPlaying');
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
        FreeAndNil(FFile);
        FFile := TFileStream.Create(PString(Msg.ParamH)^, fmCreate);
        FQuota := Msg.ParamL;
        if FQuota = 0 then FQuota := MaxSIntValue;
        if Assigned(FFile) then
          FFile.Write(DUMP_HEADER, SizeOf(DUMP_HEADER));
        GraphInvalidate;
      end;
    RM_DUMP_STOP:
      begin
        FreeAndNil(FFile);
        GraphInvalidate;
      end;
    RM_DATA:
      inherited;
    else
      inherited;
      DumpMsg(Msg);
  end;
end;

procedure TRadioDump.DoConfigure;
var
  D: TSaveDialog;
begin
  D := TSaveDialog.Create(nil);
  D.Filter := 'radio dump (*.dump)|*.dump';
  if D.Execute then
    RadioPostMessage(RM_DUMP_START, D.FileName, 0, Self);
  D.Free;
end;

procedure TRadioDump.Describe(Strs: TStrings);
begin
  if FFile is TFileStream then
    Strs.Add(Format('^bFile name: ^n%s', [ExtractFileName((FFile as TFileStream).FileName)]))
  else
    Strs.Add('^bFile name unknown');
  Strs.Add(Format('^bQuota: ^n%d', [FQuota]));
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
  if FFile.Size > FQuota then
  begin
    FreeAndNil(FFile);
    Exit;
  end;
  X.Tag := dtDataBlob;
  X.SampleRate := FSampleRate;
  X.Len := Len;
  FFile.Write(X, SizeOf(X));
  FFile.Write(P^, Len * SizeOf(P^));
end;

initialization

  RegisterModule(TRadioModuleClass(TRadioDump.ClassType));
  RegisterModule(TRadioModuleClass(TRadioDumpPlayer.ClassType));

end.

