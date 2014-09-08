unit RadioSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Graphics, RadioModule, SuperObject, radiomessage, gen_graph;

type

  TModuleId = Cardinal;

  { TRadioSystem }

  TRadioSystem = class
  private
    FRunQueue: TRadioRunQueue;
    FSuspended: Boolean;
    FModuleDict: TSuperTableString;
    FInstance: TRadioSystem; static;
    FGraph: TGenGraph;
    function GetModule(const Name: string): TRadioModule;
    procedure SetSuspended(AValue: Boolean);
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
    property Suspended: Boolean read FSuspended write SetSuspended;
    class property Instance: TRadioSystem read FInstance;

    procedure ShowModules(Tree: TTreeView);

    property Graph: TGenGraph read FGraph write FGraph;
  end;

procedure RegisterModule(AClass: TRadioModuleClass);

procedure RadioPostMessage(const M: TRadioMessage; Receiver: TRadioModule); overload;
procedure RadioPostMessage(const M: TRadioMessage; const Receiver: string);
procedure RadioPostMessage(const M: TRadioMessage; const Receiver: TModuleId);
procedure RadioPostMessage(const Id: Integer; const ParamH, ParamL: PtrUInt; const Receiver: string);
procedure RadioPostMessage(const Id: Integer; const ParamH, ParamL: PtrUInt; Receiver: TRadioModule);

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

procedure RadioPostMessage(const M: TRadioMessage; Receiver: TRadioModule);
begin
  Receiver.PostMessage(M);
end;

procedure RadioPostMessage(const M: TRadioMessage; const Receiver: string);
var
  R: TRadioModule;
begin
  R := TRadioSystem.Instance.Module[Receiver];
  if Assigned(R) then
    RadioPostMessage(M, R)
  else
    TRadioLogger.Report(llError, 'module "%s" not found', [Receiver]);
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
    RadioPostMessage(M, TRadioModule(PtrUInt(E.Value.AsInteger)));
  Sleep(500);
end;

function TRadioSystem.GetModule(const Name: string): TRadioModule;
begin
  Lock;
  Result := TRadioModule(PtrUInt(FModuleDict.I[Name]));
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
  FRunQueue   := TRadioRunQueue.Create(6);
  FModuleDict := TSuperTableString.Create;
  FInstance := Self;
  FGraph := TGenGraph.Create;
end;

destructor TRadioSystem.Destroy;
begin
  FGraph.Free;
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
  for A in FModuleDict do
  begin
    M := TRadioModule(PtrUInt(A.Value.AsInteger));
    TRadioLogger.Report(llWarn, 'free %s, cpu time %.f', [A.Name, M.CPUTime * MSecsPerDay]);
    M.Free;
  end;                                     ;
  TRadioLogger.Report(llWarn, 'all freed');
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
  FModuleDict.I[Name] := SuperInt(PtrUInt(M));
  Result := True;
  if not Suspended then;
    RadioPostMessage(RM_CONTROL, RM_CONTROL_RUN, 0, M);
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
    MF.AddFeatureListener(MT);
    MF.AddDataListener(MT, SourcePort, TargetPort);
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
    MF.AddDataListener(MT, SourcePort, TargetPort)
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
    MF.AddFeatureListener(MT)
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

