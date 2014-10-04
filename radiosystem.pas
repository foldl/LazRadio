unit RadioSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Graphics, RadioModule, SuperObject, radiomessage, gen_graph;

{
  A module has a name and an Id. Name/Id can used by users and scripts, while Id is *only*
  used for inter-module communications.

  An Id is unique. Before a module is being destroyed, it becomes un-accessible by its Id,
  so inter-module communications are disabled.
}

type

  { TRadioSystem }

  TRadioSystem = class
  private
    FRunQueue: TRadioRunQueue;
    FModuleDict: TSuperTableString;
    FId2ModuleDict: TSuperTableString;
    FInstance: TRadioSystem; static;
    FGraph: TGenGraph;
    FIdCounter: TModuleId;
    FCS: TRTLCriticalSection;
    function GetModule(const Name: string): TRadioModule;
    function GetModuleFromId(const Id: TModuleId): TRadioModule;
  protected
    procedure Lock;
    procedure Unlock;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    procedure ShowSystem;

    function AddModule(const Name, T: string): Boolean;
    function ConnectBoth(const ModuleFrom, ModuleTo: string; const TargetPort: Integer = 0; const SourcePort: Integer = 0): Boolean;
    function ConnectData(const ModuleFrom, ModuleTo: string; const TargetPort: Integer = 0; const SourcePort: Integer = 0): Boolean;
    function ConnectFeature(const ModuleFrom, ModuleTo: string): Boolean;
    function ConfigModule(const Name: string): Boolean;

    property Module[const Name: string]: TRadioModule read GetModule;
    property ModuleFromId[const Id: TModuleId]: TRadioModule read GetModuleFromId;
    class property Instance: TRadioSystem read FInstance;

    function MakeId: TModuleId;

    procedure GetModList(ModList: TStrings; const bDispFmt: Boolean);
    procedure ShowModules(Tree: TTreeView);
    property Graph: TGenGraph read FGraph write FGraph;
  end;

procedure RegisterModule(AClass: TRadioModuleClass);

function RadioPostMessage(const M: TRadioMessage; Receiver: TRadioModule): Boolean; overload;
function RadioPostMessage(const M: TRadioMessage; const Receiver: string): Boolean;
function RadioPostMessage(const M: TRadioMessage; const Receiver: TModuleId): Boolean;
function RadioPostMessage(const Id: Integer; const ParamH, ParamL: PtrUInt; const Receiver: string): Boolean;
function RadioPostMessage(const Id: Integer; const ParamH, ParamL: PtrUInt; Receiver: TRadioModule): Boolean;

implementation

uses
  Math, utils;

var
  ModuleClassDict: TSuperTableString = nil;

procedure RegisterModule(AClass: TRadioModuleClass);
var
  S: string;
begin
  S := ClassNameToModuleName(AClass.ClassName);
  if not Assigned(ModuleClassDict) then ModuleClassDict := TSuperTableString.Create;
  ModuleClassDict.I[LowerCase(S)] := Int64(AClass);
end;

function RadioPostMessage(const M: TRadioMessage; Receiver: TRadioModule
  ): Boolean;
begin
  Result := Assigned(Receiver);
  if Result then
    Receiver.PostMessage(M);
end;

function RadioPostMessage(const M: TRadioMessage; const Receiver: string
  ): Boolean;
var
  R: TRadioModule;
begin
  R := TRadioSystem.Instance.Module[Receiver];
  Result := Assigned(R);
  if Result then
    RadioPostMessage(M, R)
  else
    TRadioLogger.Report(llError, 'module "%s" not found', [Receiver]);
end;

function RadioPostMessage(const M: TRadioMessage; const Receiver: TModuleId
  ): Boolean;
var
  R: TRadioModule;
begin
  Result := False;
  if not Assigned(TRadioSystem.Instance) then Exit;
  R := TRadioSystem.Instance.ModuleFromId[Receiver];
  Result := Assigned(R);
  if Result then
    RadioPostMessage(M, R)
  else
    TRadioLogger.Report(llError, 'module "%d" not found', [Receiver]);
end;

function RadioPostMessage(const Id: Integer; const ParamH, ParamL: PtrUInt;
  const Receiver: string): Boolean;
begin
  RadioPostMessage(MakeMessage(Id, ParamH, ParamL), Receiver);
end;

function RadioPostMessage(const Id: Integer; const ParamH, ParamL: PtrUInt;
  Receiver: TRadioModule): Boolean;
begin
  RadioPostMessage(MakeMessage(Id, ParamH, ParamL), Receiver);
end;

{ TRadioSystem }

function TRadioSystem.GetModule(const Name: string): TRadioModule;
begin
  Lock;
  Result := TRadioModule(PtrUInt(FModuleDict.I[Name]));
  Unlock;
end;

function TRadioSystem.GetModuleFromId(const Id: TModuleId): TRadioModule;
begin
  Lock;
  Result := TRadioModule(PtrUInt(FId2ModuleDict.I[IntToStr(Id)]));
  Unlock;
end;

procedure TRadioSystem.Lock;
begin
  EnterCriticalsection(FCS);
end;

procedure TRadioSystem.Unlock;
begin
  LeaveCriticalsection(FCS);
end;

constructor TRadioSystem.Create;
begin
  if Assigned(FInstance) then raise Exception.Create('TRadioSystem is singleton');
  InitCriticalSection(FCS);
  FRunQueue   := TRadioRunQueue.Create(4);
  FModuleDict := TSuperTableString.Create;
  FId2ModuleDict := TSuperTableString.Create;
  FInstance := Self;
  FGraph := TGenGraph.Create;
end;

destructor TRadioSystem.Destroy;
begin
  FGraph.Free;

  Reset;
  FRunQueue.Free;

  FInstance := nil;
  FId2ModuleDict.Free;
  FModuleDict.Free;
  DoneCriticalsection(FCS);
  inherited;
end;

procedure TRadioSystem.Reset;
var
  A: TSuperAvlEntry;
  M: TRadioModule;
begin
  Lock;
  FGraph.Clear;
  FId2ModuleDict.Clear(True);
  Unlock;
  for A in FModuleDict do
  begin
    M := TRadioModule(PtrUInt(A.Value.AsInteger));
    M.PostMessage(RM_DESTROY, 0, 0);
  end;
  FModuleDict.Clear(True);
end;

procedure TRadioSystem.ShowSystem;
var
  A: TSuperAvlEntry;
  M: TRadioModule;
  N: TGenEntityNode;
  I: Integer;
begin
  FGraph.BeginUpdate;
  FGraph.Clear;
  for A in FModuleDict do
  begin
    M := TRadioModule(PtrUInt(A.Value.AsInteger));
    N := TGenEntityNode.Create;
    N.Drawable := M;
    M.GraphNode := N;
    FGraph.AddEntity(N);
  end;

  // add connections
  for A in FModuleDict do
  begin
    M := TRadioModule(PtrUInt(A.Value.AsInteger));
    M.ShowConnections(FGraph);
  end;

  for A in FModuleDict do
  begin
    M := TRadioModule(PtrUInt(A.Value.AsInteger));
    if M.GraphNode.GetPortsNum(epIn) >= 1 then
      M.GraphNode.SetPortName(epIn, 0, 'f');
    for I := 1 to M.GraphNode.GetPortsNum(epIn) do
      M.GraphNode.SetPortName(epIn, I, IntToStr(I - 1)[1]);
    if M.GraphNode.GetPortsNum(epOut) >= 1 then
      M.GraphNode.SetPortName(epOut, 0, 'f');
    for I := 1 to M.GraphNode.GetPortsNum(epOut) do
      M.GraphNode.SetPortName(epOut, I, IntToStr(I - 1)[1]);
  end;
  FGraph.EndUpdate;
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
  if not Result then
  begin
    TRadioLogger.Report(llError, 'module type "%s" not found', [T]);
    Exit;
  end;
  Lock;
  C := TRadioModuleClass(PtrUInt(I));
  I := FModuleDict.I[Name];
  Result := I = 0;
  if not Result then
  begin
    TRadioLogger.Report(llError, 'module "%s" already defined', [T]);
    goto Quit;
  end;
  M := C.Create(FRunQueue);
  M.Name := Name; //Format('%s:%s', [Name, T]);
  M.Id := MakeId;
  FModuleDict.I[Name] := SuperInt(PtrUInt(M));
  FId2ModuleDict.I[IntToStr(M.Id)] := SuperInt(PtrUInt(M));
  Result := True;
Quit:
  Unlock;
end;

function TRadioSystem.ConnectBoth(const ModuleFrom, ModuleTo: string;
  const TargetPort: Integer; const SourcePort: Integer): Boolean;
var
  MF, MT: TRadioModule;
begin
  MF := Module[ModuleFrom];
  MT := Module[ModuleTo];
  if Assigned(MF) and Assigned(MT) then
  begin
    MF.PostMessage(RM_ADD_FEATURE_LISTENER, 0, MT.Id);
    MF.PostMessage(RM_ADD_DATA_LISTENER, (SourcePort shl 16) or TargetPort, MT.Id);
  end
  else
    TRadioLogger.Report(llError, 'one or more modules (%s, %s) not found', [ModuleFrom, ModuleTo]);
end;

function TRadioSystem.ConnectData(const ModuleFrom, ModuleTo: string;
  const TargetPort: Integer; const SourcePort: Integer): Boolean;
var
  MF, MT: TRadioModule;
begin
  MF := Module[ModuleFrom];
  MT := Module[ModuleTo];
  if Assigned(MF) and Assigned(MT) then
    MF.PostMessage(RM_ADD_DATA_LISTENER, (SourcePort shl 16) or TargetPort, MT.Id)
  else
    TRadioLogger.Report(llError, 'one or more modules (%s, %s) not found', [ModuleFrom, ModuleTo]);
end;

function TRadioSystem.ConnectFeature(const ModuleFrom, ModuleTo: string
  ): Boolean;
var
  MF, MT: TRadioModule;
begin
  MF := Module[ModuleFrom];
  MT := Module[ModuleTo];
  if Assigned(MF) and Assigned(MT) then
    MF.PostMessage(RM_ADD_FEATURE_LISTENER, 0, MT.Id)
  else
    TRadioLogger.Report(llError, 'one or more modules (%s, %s) not found', [ModuleFrom, ModuleTo]);
end;

function TRadioSystem.ConfigModule(const Name: string): Boolean;
var
  M: TRadioModule;
begin
  M := Module[Name];
  Result := Assigned(M);
  if Result then M.PostMessage(RM_CONFIGURE, 0, 0);
end;

function TRadioSystem.MakeId: TModuleId;
begin
  Inc(FIdCounter);
  Result := FIdCounter;
end;

procedure TRadioSystem.GetModList(ModList: TStrings; const bDispFmt: Boolean);
var
  A: TSuperAvlEntry;
  K: TRadioModuleClass;
begin
  for A in ModuleClassDict do
  begin
    K := TRadioModuleClass(PtrUInt(A.Value.AsInteger));
    if bDispFmt then
      ModList.Add(ClassNameToModuleName(K.ClassName))
    else
      ModList.Add(UpperCase(ClassNameToModuleName(K.ClassName)));
  end;
end;

procedure TRadioSystem.ShowModules(Tree: TTreeView);
const
  ICON_SIZE = 44;
  ICON_MARGIN = 4;
var
  N: TTreeNode;
  C: TTreeNode;
  A: TSuperAvlEntry;
  B: TBitmap;
  K: TRadioModuleClass;
  R: TRect = (Left: ICON_MARGIN; Top: ICON_MARGIN; Right: ICON_SIZE - ICON_MARGIN; Bottom: ICON_SIZE - ICON_MARGIN);
begin
  Tree.BeginUpdate;
  Tree.Items.Clear;

  B := TBitmap.Create;
  B.Width := ICON_SIZE;
  B.Height := ICON_SIZE;
  B.Canvas.Brush.Color := clWhite;

  // TODO: module category
  C := Tree.Items.Add(nil, 'Built-in');
  Tree.Images.Clear;
  Tree.Images.Height := ICON_SIZE;
  Tree.Images.Width  := ICON_SIZE;
  for A in ModuleClassDict do
  begin
    B.Canvas.Brush.Style := bsSolid;
    B.Canvas.Brush.Color := clWhite;
    B.Canvas.FillRect(0, 0, ICON_SIZE, ICON_SIZE);
    B.Canvas.Brush.Color := TColor($505050);
    B.Canvas.Pen.Style := psSolid;
    B.Canvas.Pen.Color := B.Canvas.Brush.Color;
    B.Canvas.RoundRect(0, 0, ICON_SIZE, ICON_SIZE, 8, 8);
    K := TRadioModuleClass(PtrUInt(A.Value.AsInteger));
    StrIconDraw(B.Canvas, R, K.LoadDefIconRes);
    Tree.Images.AddMasked(B, clWhite);
    N := Tree.Items.AddChild(C, VarNameToWords(ClassNameToModuleName(K.ClassName)));
    N.Height := ICON_SIZE + 2;
    N.ImageIndex := Tree.Images.Count - 1;
    N.SelectedIndex := N.ImageIndex;
    N.Data := K;
  end;
  Tree.EndUpdate;
end;

end.

