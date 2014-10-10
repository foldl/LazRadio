unit RadioLang;

{$mode objfpc}{$H+}

{
a LazRadio project is defined by a .lzr file.

lazradio SYS-NAME;

var
  VAR-NAME: ModuleName;
  ...

begin

  // connect feature
  VAR-NAME :> VAR-NAME [ :> VAR-NAME];

  // connect data port 0
  VAR-NAME -> VAR-NAME [ -> VAR-NAME];

  // connect data port, soure port i, target port j
  VAR-NAME[I] -> [J]VAR-NAME;

  // connect feature and data port 0
  VAR-NAME => VAR-NAME [ => VAR-NAME];

  // post a message
  VAR-NAME ! {M-ID, PRAMH, PRAML} [! ... ];
end.
}

interface

uses
  Classes, SysUtils, superobject, RadioModule, RadioSystem;

type

  { TRadioLangRT }

  TRadioLangRT = class
  private
    FConstTable: ISuperObject;
    FModuleTypes: TStringList;
    FOnProjNameChanged: TNotifyEvent;
    FProjName: string;
    procedure LoadMsgConsts;
    function CBSendMessage(const Name: string; const V1, V2, V3: PtrUInt): Boolean;
    function CBConnectFeature(const Source, Target: string): Boolean;
    function CBConnectData(const Source, Target: string; const SourcePort, TargetPort: Integer): Boolean;
    function CBMakeStrParam(const S: string): PtrUInt;
    function CBMakeStrParam0(const S: string): PtrUInt;
    procedure CBEmitMessage(const Line: string);
    procedure CBProjName(const Line: string);
    procedure CBCreateModules(Sender: TObject);
    procedure SetOnProjNameChanged(AValue: TNotifyEvent);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;
    function Exec(const Fn: string): Boolean;
    function Check(const Fn: string): Boolean;

    procedure GetRTSymbols(L: TStrings);
    property ModuleTypes: TStringList read FModuleTypes;
    property ProjName: string read FProjName;
    property OnProjNameChanged: TNotifyEvent read FOnProjNameChanged write SetOnProjNameChanged;
  end;

implementation

uses
  util_config, lzr_interpreter;

{ TRadioLangRT }

procedure TRadioLangRT.LoadMsgConsts;
var
  O: ISuperObject;
  procedure LoadConsts(Obj: ISuperObject);
  var
    I: Integer;
    A: TSuperArray;
    N: string;
  begin
    if not Assigned(Obj) then Exit;
    if Obj.IsType(stObject) then
    begin
      N := Obj.S['name'];
      if N <> '' then
        FConstTable.I[N] := Obj.I['id'];
      LoadConsts(Obj.O['paramh.vals']);
      LoadConsts(Obj.O['paraml.vals']);
    end;
    if not Obj.IsType(stArray) then Exit;
    A := Obj.AsArray;
    for I := 0 to A.Length - 1 do
    begin
      LoadConsts(A.O[I]);
    end;
  end;
begin
  O := TSuperObject.ParseFile(GetResFullName('messages.json'), False);
  LoadConsts(O);
end;

function TRadioLangRT.CBSendMessage(const Name: string; const V1, V2,
  V3: PtrUInt): Boolean;
begin
  Result := RadioPostMessage(V1, V2, V3, Name);
end;

function TRadioLangRT.CBConnectFeature(const Source, Target: string): Boolean;
begin
  Result := TRadioSystem.Instance.ConnectFeature(Source, Target);
end;

function TRadioLangRT.CBConnectData(const Source, Target: string;
  const SourcePort, TargetPort: Integer): Boolean;
begin
  Result := TRadioSystem.Instance.ConnectData(Source, Target, SourcePort, TargetPort);
end;

function TRadioLangRT.CBMakeStrParam(const S: string): PtrUInt;
begin
  Result := TRadioSystem.Instance.MakeStrParam(S);
end;

function TRadioLangRT.CBMakeStrParam0(const S: string): PtrUInt;
begin
  Result := 0;
end;

procedure TRadioLangRT.CBEmitMessage(const Line: string);
begin
  TRadioLogger.Report(llSystem, Line);
end;

procedure TRadioLangRT.CBProjName(const Line: string);
begin
  FProjName := Line;
  if Assigned(FOnProjNameChanged) then FOnProjNameChanged(Self);
end;

procedure TRadioLangRT.CBCreateModules(Sender: TObject);
var
  A: TSuperAvlEntry;
begin
  for A in SymTable.AsObject do
  begin
    if A.Value.AsObject.B['obj'] then
    begin
      TRadioSystem.Instance.AddModule(A.Value.AsObject.S['disp'], A.Value.AsObject.S['type']);
    end;
  end;
end;

procedure TRadioLangRT.SetOnProjNameChanged(AValue: TNotifyEvent);
begin
  if FOnProjNameChanged = AValue then Exit;
  FOnProjNameChanged := AValue;
end;

constructor TRadioLangRT.Create;
begin
  FModuleTypes := TStringList.Create;
  FConstTable := TSuperObject.ParseFile(GetResFullName('lzr_internal.json'), False);
  LoadMsgConsts;
end;

destructor TRadioLangRT.Destroy;
begin
  FModuleTypes.Free;
  inherited Destroy;
end;

procedure TRadioLangRT.Reset;
var
  S: string;
  A: TSuperAvlEntry;
begin
  ObjTypes := SO('{}');
  for S in FModuleTypes do
    ObjTypes.O[UpperCase(S)] := SO(Format('{disp: "%s"}', [S]));

  RegTable := SO('{}');

  SymTable := SO('{}');
  for A in FConstTable.AsObject do
    PredefineInt(A.Name, A.Value.AsInteger);
end;

function TRadioLangRT.Exec(const Fn: string): Boolean;
begin
  Reset;
  OnCreateModules := @CBCreateModules;
  OnSendMessage   := @CBSendMessage;
  OnConnectFeature:= @CBConnectFeature;
  OnConnectData   := @CBConnectData;
  OnWriteLn       := @CBEmitMessage;
  OnProjName      := @CBProjName;
  OnMakeStrParam  := @CBMakeStrParam;
  Result := Interpret(Fn);
  if Result then
    TRadioLogger.Report(llSystem, 'successfully executed.')
  else
    TRadioLogger.Report(llSystem, 'there are errors.');
end;

function TRadioLangRT.Check(const Fn: string): Boolean;
begin
  Reset;
  OnCreateModules := nil;
  OnSendMessage   := nil;
  OnConnectFeature:= nil;
  OnConnectData   := nil;
  OnWriteLn       := @CBEmitMessage;
  OnProjName      := @CBProjName;
  OnMakeStrParam  := @CBMakeStrParam0;
  Result := Interpret(Fn);
  if Result then
    TRadioLogger.Report(llSystem, 'successfully checked.')
  else
    TRadioLogger.Report(llSystem, 'there are errors.');
end;

procedure TRadioLangRT.GetRTSymbols(L: TStrings);
var
  A: TSuperAvlEntry;
begin
  for A in FConstTable.AsObject do
    L.Add(A.Name);
end;

end.

