unit RadioSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioModule, superobject;

type

  { TRadioSystem }

  TRadioSystem = class
  private
    FRunQueue: TRadioRunQueue;
    FSuspended: Boolean;
    FModuleDict: TSuperTableString;
    procedure SetSuspended(AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    function AddModule(const Name, T: string): Boolean;
    function ConnectModuel(const ModuleFrom, ModuleTo: string): Boolean;
    function ConfigModule(const Name: string): Boolean;

    property Suspended: Boolean read FSuspended write SetSuspended;
  end;

procedure RegisterModule(const T: string; AClass: TRadioModuleClass);

implementation

var
  ModuleClassDict: TSuperTableString = nil;

procedure RegisterModule(const T: string; AClass: TRadioModuleClass);
begin
  if not Assigned(ModuleClassDict) then ModuleClassDict := TSuperTableString.Create;
  ModuleClassDict.I[LowerCase(T)] := Int64(AClass);
end;

{ TRadioSystem }

procedure TRadioSystem.SetSuspended(AValue: Boolean);
begin
  if FSuspended = AValue then Exit;
  FSuspended := AValue;
end;

constructor TRadioSystem.Create;
begin
  FRunQueue   := TRadioRunQueue.Create;
  FModuleDict := TSuperTableString.Create;
end;

destructor TRadioSystem.Destroy;
begin
  Reset;
  FModuleDict.Free;
  FRunQueue.Free;
  inherited;
end;

procedure TRadioSystem.Reset;
var
  A: TSuperAvlEntry;
begin
  for A in FModuleDict do
    TRadioModule(Pointer(A.Value.AsInteger)).Free;
  FModuleDict.Clear(True);
end;

function TRadioSystem.AddModule(const Name, T: string): Boolean;
var
  C: TRadioModuleClass;
  I: SuperInt;
  M: TRadioModule;
begin
  I := ModuleClassDict.I[LowerCase(T)];
  Result := I <> 0;
  if not Result then Exit;
  C := TRadioModuleClass(Pointer(I));
  I := FModuleDict.I[Name];
  Result := I = 0;
  if not Result then Exit;
  M := C.Create(FRunQueue);
  FModuleDict.I[Name] := SuperInt(Pointer(M));
  Result := True;
  M.Running := not Suspended;
end;

function TRadioSystem.ConnectModuel(const ModuleFrom, ModuleTo: string
  ): Boolean;
begin

end;

function TRadioSystem.ConfigModule(const Name: string): Boolean;
var
  M: TRadioModule;
begin
  M := TRadioModule(Pointer(FModuleDict.I[Name]));
  Result := Assigned(M);
  if Result then M.Configure;
end;

end.

