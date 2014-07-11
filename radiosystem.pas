unit RadioSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioModule, SuperObject;

type

  TModuleId = Cardinal;

  { TRadioSystem }

  TRadioSystem = class
  private
    FRunQueue: TRadioRunQueue;
    FSuspended: Boolean;
    FModuleDict: TSuperTableString;
    FInstance: TRadioSystem; static;
    function GetModule(const Name: string): TRadioModule;
    procedure SetSuspended(AValue: Boolean);
  protected
    procedure Lock;
    procedure Unlock;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    function AddModule(const Name, T: string): Boolean;
    function ConnectModuel(const ModuleFrom, ModuleTo: string): Boolean;
    function ConfigModule(const Name: string): Boolean;

    property Module[const Name: string]: TRadioModule read GetModule;
    property Suspended: Boolean read FSuspended write SetSuspended;
    class property Instance: TRadioSystem read FInstance;
  end;

procedure RegisterModule(const T: string; AClass: TRadioModuleClass);

procedure RadioPostMessage(const M: TRadioMessage; Receiver: TRadioModule); overload;
procedure RadioPostMessage(const M: TRadioMessage; const Receiver: string);
procedure RadioPostMessage(const M: TRadioMessage; const Receiver: TModuleId);
procedure RadioPostMessage(const Id: Integer; const ParamH, ParamL: PtrUInt; const Receiver: string);
procedure RadioPostMessage(const Id: Integer; const ParamH, ParamL: PtrUInt; Receiver: TRadioModule);

implementation

uses
  Math;

var
  ModuleClassDict: TSuperTableString = nil;

procedure RegisterModule(const T: string; AClass: TRadioModuleClass);
begin
  if not Assigned(ModuleClassDict) then ModuleClassDict := TSuperTableString.Create;
  ModuleClassDict.I[LowerCase(T)] := Int64(AClass);
end;

procedure RadioPostMessage(const M: TRadioMessage; Receiver: TRadioModule);
begin
  Receiver.PostMessage(M);
end;

procedure RadioPostMessage(const M: TRadioMessage; const Receiver: string);
var
  R: TRadioModule;
begin
  R := TRadioSystem.Instance.Module[Receiver];
  if Assigned(R) then RadioPostMessage(M, R);
end;

procedure RadioPostMessage(const M: TRadioMessage; const Receiver: TModuleId);
begin

end;

procedure RadioPostMessage(const Id: Integer; const ParamH, ParamL: PtrUInt;
  const Receiver: string);
begin
  RadioPostMessage(MakeMessage(Id, ParamH, ParamL), Receiver);
end;

procedure RadioPostMessage(const Id: Integer; const ParamH, ParamL: PtrUInt;
  Receiver: TRadioModule);
begin
  RadioPostMessage(MakeMessage(Id, ParamH, ParamL), Receiver);
end;

{ TRadioSystem }

procedure TRadioSystem.SetSuspended(AValue: Boolean);
var
  M: TRadioMessage;
  E: TSuperAvlEntry;
begin
  if FSuspended = AValue then Exit;
  FSuspended := AValue;
  with M do
  begin
    Id := RM_CONTROL;
    ParamH := IfThen(AValue, RM_CONTROL_PAUSE, RM_CONTROL_RUN);
    ParamL := 0;
  end;
  for E in FModuleDict do
    RadioPostMessage(M, TRadioModule(Pointer(E.Value.AsInteger)));
  Sleep(500);
end;

function TRadioSystem.GetModule(const Name: string): TRadioModule;
begin
  Lock;
  Result := TRadioModule(Pointer(FModuleDict.I[Name]));
  Unlock;
end;

procedure TRadioSystem.Lock;
begin
  RadioGlobalLock;
end;

procedure TRadioSystem.Unlock;
begin
  RadioGlobalUnlock;
end;

constructor TRadioSystem.Create;
begin
  if Assigned(FInstance) then raise Exception.Create('TRadioSystem is singleton');
  FRunQueue   := TRadioRunQueue.Create;
  FModuleDict := TSuperTableString.Create;
  FInstance := Self;
end;

destructor TRadioSystem.Destroy;
begin
  FInstance := nil;
  Reset;
  FModuleDict.Free;
  FRunQueue.Free;
  inherited;
end;

procedure TRadioSystem.Reset;
var
  A: TSuperAvlEntry;
  M: TRadioModule;
begin
  //Suspended := True;
  for A in FModuleDict do
  begin
    TRadioLogger.Report(llError, 'free %s', [A.Name]);
    M := TRadioModule(Pointer(A.Value.AsInteger));
    M.Free;
  end;
  TRadioLogger.Report(llError, 'all freed');
  FModuleDict.Clear(True);
end;

function TRadioSystem.AddModule(const Name, T: string): Boolean;
label
  Quit;
var
  C: TRadioModuleClass;
  I: SuperInt;
  M: TRadioModule;
begin
  I := ModuleClassDict.I[LowerCase(T)];
  Result := I <> 0;
  if not Result then Exit;
  Lock;
  C := TRadioModuleClass(Pointer(I));
  I := FModuleDict.I[Name];
  Result := I = 0;
  if not Result then goto Quit;
  M := C.Create(FRunQueue);
  M.Name := Format('%s:%s', [Name, T]);
  FModuleDict.I[Name] := SuperInt(Pointer(M));
  Result := True;
  if not Suspended then;
    RadioPostMessage(RM_CONTROL, RM_CONTROL_RUN, 0, M);
Quit:
  Unlock;
end;

function TRadioSystem.ConnectModuel(const ModuleFrom, ModuleTo: string
  ): Boolean;
var
  MF, MT: TRadioModule;
begin
  MF := Module[ModuleFrom];
  MT := Module[ModuleTo];
  if Assigned(MF) and Assigned(MT) then
  begin
    MF.AddFeatureListener(MT);
    MF.AddDataListener(MT);
  end;
end;

function TRadioSystem.ConfigModule(const Name: string): Boolean;
var
  M: TRadioModule;
begin
  M := Module[Name];
  Result := Assigned(M);
  M.PostMessage(RM_CONFIGURE, 0, 0);
end;

end.

