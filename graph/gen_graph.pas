unit gen_graph;

{$mode objfpc}{$H+}

// This is a generic directed graph class:

interface

uses
  Classes, SysUtils, Types, Graphics, ExtCtrls, Utils, Controls, Math;

type

  TLayoutAlgorithm = (laLevel, laForceDirected);
  TRouteAlgorithm = (raDirect, raAStar);

  IGenDrawable = interface
    procedure Measure(ACanvas: TCanvas; out Extent: TSize);
    procedure Draw(ACanvas: TCanvas; ARect: TRect);
    procedure MouseClick(const Pt: TPoint);
  end;

  TGenGraph = class;
  TGenEntityNode = class;

  TGenEntityPortType = (epIn, epOut);

  TGenEntityPortRef = record
    Entity: TGenEntityNode;
    Index: Integer;
  end;

  { TGenEntity }

  TGenEntity = class
  private
    FInValidated: Boolean;
  protected
    FExtent: TSize;
    function GetBox: TRect; virtual;
  public
    procedure Invalidate;

    procedure Measure(ACanvas: TCanvas); virtual;
    procedure Draw(ACanvas: TCanvas); virtual;
    function  IsPtOn(const Pt: TPoint): Boolean; virtual;
    procedure MouseClick(const Pt: TPoint); virtual;

    procedure Move(const OffsetX, OffsetY: Integer); virtual;

    property Extent: TSize read FExtent;
    property Box: TRect read GetBox;
    property Invalidated: Boolean read FInValidated;
  end;

  TGenAlgoFactory = class;

  { TGenEntityNode }

  TGenEntityNode = class(TGenEntity)
  private
    FDegIn: Integer;
    FDegOut: Integer;
    FDrawable: IGenDrawable;
    FTag: Integer;
    FInPorts: array of Char;
    FOutPorts: array of Char;

    function  CalcFirstPortY(const N: Integer): Integer;
    procedure DrawPorts(ACanvas: TCanvas);
  protected
    function GetBox: TRect; override;
  public
    Pos: TPoint;
  public
    constructor Create;

    procedure Measure(ACanvas: TCanvas); override;
    procedure Draw(ACanvas: TCanvas); override;
    function  IsPtOn(const Pt: TPoint): Boolean; override;
    procedure MouseClick(const Pt: TPoint); override;

    procedure SetPortsNum(const T: TGenEntityPortType; const N: Integer);
    procedure SetPortsNumAtLeast(const T: TGenEntityPortType; const N: Integer);
    function  GetPortsNum(const T: TGenEntityPortType): Integer;
    procedure SetPortName(const T: TGenEntityPortType; const N: Cardinal; const Name: Char);

    procedure Move(const OffsetX, OffsetY: Integer); override;

    function  GetPortConnectPos(T: TGenEntityPortType; Index: Integer): TPoint;
    property Drawable: IGenDrawable read FDrawable write FDrawable;

    property Tag: Integer read FTag write FTag;
    property DegIn: Integer read FDegIn write FDegIn;
    property DegOut: Integer read FDegOut write FDegOut;
  end;

  { TGenEntityConnection }

  TGenEntityConnection = class(TGenEntity)
  private
    FCtrlPts: array of TPoint;
    FFromPort: TGenEntityPortRef;
    FPenStyle: TPenStyle;
    FToPort: TGenEntityPortRef;
    function GetCtrlPoints(const Index: Integer): TPoint;
    function GetEndPt: TPoint;
    function GetStartPt: TPoint;
    procedure SetCtrlPoints(const Index: Integer; AValue: TPoint);
  protected
    function GetBox: TRect; override;
  public
    constructor Create;

    procedure SetCtrlPointsNumber(const N: Integer);

    procedure Measure(ACanvas: TCanvas); override;
    procedure Draw(ACanvas: TCanvas); override;

    property CtrlPoints[const Index: Integer]: TPoint read GetCtrlPoints write SetCtrlPoints;
    property FromPt: TPoint read GetStartPt;
    property ToPt: TPoint read GetEndPt;

    procedure Move(const OffsetX, OffsetY: Integer); override;

    property FromPort: TGenEntityPortRef read FFromPort write FFromPort;
    property ToPort: TGenEntityPortRef read FToPort write FToPort;
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
  end;

  { TGenGraph }

  TGenGraph = class
  private
    FDBuffer: TDoubleBuffer;
    FEntities: TList;
    FConns: TList;
    FUpdateCount: Integer;
    FFactory: TGenAlgoFactory;
    FLayoutAlgo: TLayoutAlgorithm;
    FRouteAlgo: TRouteAlgorithm;
    function GetPaintBox: TPaintBox;
    procedure SetPaintBox(AValue: TPaintBox);
    function  FindConnection(AFrom, ATo: TGenEntityNode; const AFromPort,
      AToPort: Integer): TGenEntityConnection;
    procedure PaintBoxClick(Sender: TObject);
  protected
    procedure Layout;
    procedure Route(const BoundingBox: TRect);
  public
    procedure FullRender;
    procedure PartialRender;

    procedure RemoveEntity(AEntity: TGenEntityNode);
    procedure AddEntity(AEntity: TGenEntityNode);
    function  AddConnection(AFrom, ATo: TGenEntityNode; const AFromPort, AToPort: Integer): TGenEntityConnection;
    procedure RemoveConnecttion(AFrom, ATo: TGenEntityNode; const AFromPort, AToPort: Integer);
    procedure Clear;

    procedure BeginUpdate;
    procedure EndUpdate;

    constructor Create;
    destructor Destroy; override;

    property PaintBox: TPaintBox read GetPaintBox write SetPaintBox;
  end;

  { TGenLayout }

  TGenLayout = class
  const
    V_MARGIN = 60;
    H_MARGIN = 80;
  public
    procedure Layout(Entities, Conns: TList); virtual; abstract;
  end;

  PGenRouteNodeRec = ^TGenRouteNodeRec;
  TGenRouteNodeRec = record
    W: Float;
    D: Word;
    Status: Byte;
    Pre: PGenRouteNodeRec;             // for backtracing
  end;

  { TGenRoute }

  TGenRoute = class
  const
    NOT_GOOD  = 15;
    EMPTY     = 0;
    VISITED   = 1;
    WIRE_USED = 2;
    FORBID_PT = 120;
  protected
    FSortedOpenNode: TList;
    FGrid: array of TGenRouteNodeRec;
    FWorking: array of TGenRouteNodeRec;
    FSize: TPoint;
    FSize1: TPoint;
    procedure MarkPortPath(ANode: TGenEntityNode);
    procedure MarkBlockForbid(const R: TRect);
    procedure MarkPathForbid(const X0, Y0, X1, Y1: Integer);
    procedure MarkPathForbid(Conn: TGenEntityConnection);
    procedure MarkBlock(const R: TRect);
    procedure InitGrid(Entities: TList; BoundingBox: TRect);
    procedure Restore;
    function  At(const X, Y: Integer): PGenRouteNodeRec; overload;
    function  At(const Pt: TPoint): PGenRouteNodeRec;

    function  GridAt(const X, Y: Integer): PGenRouteNodeRec; overload;
    function  GridAt(const Pt: TPoint): PGenRouteNodeRec;

    procedure DbgPrint(const R: TRect);

    procedure ClearOpenNode;
    procedure RemoteOpenNode(N: PGenRouteNodeRec);
    procedure InsertOpenNode(N: PGenRouteNodeRec);
    procedure UpdateOpenNode(N: PGenRouteNodeRec);
    function  PopOpenNode: PGenRouteNodeRec;

    procedure PtoXY(N: PGenRouteNodeRec; out X, Y: Integer);
    function  GetNeighbors(N: PGenRouteNodeRec; var A: array of PGenRouteNodeRec): Integer; overload;
    function  GetNeighbors(const X, Y: Integer; var A: array of PGenRouteNodeRec): Integer; overload;

    function  IsSegmentClear(const Pt1, Pt2: TPoint): Boolean;
    procedure Backtrace(Conn: TGenEntityConnection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Route(Entities, Conntections: TList; BoundingBox: TRect); virtual; abstract;
  end;

  { TGenAlgoFactory }

  TGenAlgoFactory = class
  private
    FLayout: TGenLayout;
    FRoute: TGenRoute;
  public
    constructor Create;
    destructor Destroy; override;

    function GetLayout(const ALayout: TLayoutAlgorithm): TGenLayout;
    function GetRoute(const ARoute: TRouteAlgorithm): TGenRoute;
  end;

implementation

const
  PORT_MARK_SIZE   = 15;
  PORT_MARK_MARGIN = 25;

type

  { TGenLevelLayout }

  TGenLevelLayout = class(TGenLayout)
  public
    procedure Layout(Entities, Conns: TList); override;
  end;

  { TGenForceDirectedLayout }

  TGenForceDirectedLayout = class(TGenLayout)
  public
    procedure Layout(Entities, Conns: TList); override;
  end;

  { TGenAStarRoute }

  TGenAStarRoute = class(TGenRoute)
  private
    function  CalcWeight(const X, Y: Integer): Integer;
    function  TryRouteOne(Conn: TGenEntityConnection; const Frame: TRect): Boolean;
    procedure AStarRouteOne(Conn: TGenEntityConnection);
    procedure RouteOne(Conn: TGenEntityConnection);
  public
    procedure Route(Entities, Conntections: TList; BoundingBox: TRect); override;
  end;

  { TGenDirectRoute }

  TGenDirectRoute = class(TGenAStarRoute)
  private
    procedure RouteOne(Conn: TGenEntityConnection);
  public
    procedure Route(Entities, Conntections: TList; BoundingBox: TRect); override;
  end;

function MergeRect(const R1, R2: TRect): TRect;
begin
  with Result do
  begin
    Left := Min(R1.Left, R2.Left);
    Right := Max(R1.Right, R2.Right);
    Top   := Min(R1.Top, R2.Top);
    Bottom := Max(R1.Bottom, R2.Bottom);
  end;
end;

function IsPtInRect(const X, Y: Integer; const R: TRect): Boolean; overload;
begin
  Result := InRange(X, R.Left, R.Right) and InRange(Y, R.Top, R.Bottom);
end;

function IsPtInRect(const Pt: TPoint; const R: TRect): Boolean;
begin
  Result := InRange(Pt.x, R.Left, R.Right) and InRange(Pt.y, R.Top, R.Bottom);
end;

{ TGenDirectRoute }

procedure TGenDirectRoute.RouteOne(Conn: TGenEntityConnection);
const
  HEADER = 15;
var
  I: Integer;
  Pt1, Pt2: TPoint;
  Y1, Y2: Integer;
begin;
  Pt1 := Conn.FromPt;
  Inc(Pt1.x, HEADER * (Conn.FromPort.Index + 1));
  Pt2 := Conn.ToPt;
  Dec(Pt2.x, HEADER * (Conn.ToPort.Index + 1));

  if Pt1.y = Pt2.y then Exit;
  if Pt1.x = Pt2.x then
  begin
    Conn.SetCtrlPointsNumber(2);
    Conn.CtrlPoints[0] := Pt1;
    Conn.CtrlPoints[1] := Pt2;
    Exit;
  end;

  if Pt1.x < Pt2.x then
  begin
    Conn.SetCtrlPointsNumber(2);
    //Pt1.x := (Pt1.x + Pt2.x) div 2;
    Pt2.x := Pt1.x;
    Conn.CtrlPoints[0] := Pt1;
    Conn.CtrlPoints[1] := Pt2;
  end;

  Conn.SetCtrlPointsNumber(4);
  Conn.CtrlPoints[0] := Pt1;
  Conn.CtrlPoints[3] := Pt2;
  Y1 := Min(Pt1.y, Pt2.y);
  Y2 := Max(Pt1.y, Pt2.y);
  for I := (Y1 + Y2) div 2 to Y2 do
  begin
    Pt1.y := I;
    Pt2.y := I;
    if IsSegmentClear(Pt1, Pt2) then
    begin
      Conn.CtrlPoints[1] := Pt1;
      Conn.CtrlPoints[2] := Pt2;
      Exit;
    end;
  end;
  for I := Y1 to (Y1 + Y2) div 2 do
  begin
    Pt1.y := I;
    Pt2.y := I;
    if IsSegmentClear(Pt1, Pt2) then
    begin
      Conn.CtrlPoints[1] := Pt1;
      Conn.CtrlPoints[2] := Pt2;
      Exit;
    end;
  end;
  Pt1.y := (Y1 + Y2) div 2;
  Pt2.y := Pt1.y;
  Conn.CtrlPoints[1] := Pt1;
  Conn.CtrlPoints[2] := Pt2;
end;

procedure TGenDirectRoute.Route(Entities, Conntections: TList;
  BoundingBox: TRect);
var
  P: Pointer;
begin
  InitGrid(Entities, BoundingBox);
  for P in Conntections do
    RouteOne(TGenEntityConnection(P));
end;

{ TGenRoute }

procedure TGenRoute.MarkPortPath(ANode: TGenEntityNode);
  procedure MarkIn(const N: Integer);
  var
    I, K: Integer;
    Pt: TPoint;
  begin
    for K := 0 to N do
    begin
      Pt := ANode.GetPortConnectPos(epIn, K);
      for I := Max(0, Pt.x - NOT_GOOD) to Pt.x do
        FGrid[Pt.y * FSize1.x + I].Status := EMPTY;
    end;
  end;
  procedure MarkOut(const N: Integer);
  var
    I, K: Integer;
    Pt: TPoint;
  begin
    for K := 0 to N do
    begin
      Pt := ANode.GetPortConnectPos(epOut, K);
      for I := Pt.x + 1 to Min(FSize.x, Pt.x + NOT_GOOD) do
        FGrid[Pt.y * FSize1.x + I].Status := EMPTY;
    end;
  end;
begin
  MarkIn(High(ANode.FInPorts));
  MarkOut(High(ANode.FOutPorts));
end;

procedure TGenRoute.MarkBlockForbid(const R: TRect);
var
  I, J: Integer;
begin
  for I := Max(0, R.Top - NOT_GOOD) to R.Top - 1 do
  begin
    for J := Max(0, R.Left - NOT_GOOD) to Min(FSize.x, R.Right + NOT_GOOD) do
      FGrid[I * FSize1.x + J].Status := FORBID_PT;
  end;
  for I := R.Bottom to Min(FSize.y, R.Bottom + NOT_GOOD) do
  begin
    for J := Max(0, R.Left - NOT_GOOD) to Min(FSize.x, R.Right + NOT_GOOD) do
      FGrid[I * FSize1.x + J].Status := FORBID_PT;
  end;

  for I := R.Top to R.Bottom do
  begin
    for J := Max(0, R.Left - NOT_GOOD) to R.Left - 1 do
      FGrid[I * FSize1.x + J].Status := FORBID_PT;
    for J := R.Right + 1 to Min(FSize.x, R.Right + NOT_GOOD) do
      FGrid[I * FSize1.x + J].Status := FORBID_PT;
  end;
{
  for I := Max(0, R.Top - NOT_GOOD) to R.Top - 1 do
  begin
    for J := Max(0, R.Left - NOT_GOOD) to Min(FSize.x, R.Right + NOT_GOOD) do
      Inc(FGrid[I * FSize1.x + J].WOffset, MULTI * (NOT_GOOD + 1 + I - R.Top));
  end;
  for I := R.Bottom + 1 to Min(FSize.y, R.Bottom + NOT_GOOD) do
  begin
    for J := Max(0, R.Left - NOT_GOOD) to Min(FSize.x, R.Right + NOT_GOOD) do
      Inc(FGrid[I * FSize1.x + J].WOffset, MULTI * (NOT_GOOD - 1 + I - R.Bottom));
  end;

  for I := R.Top to R.Bottom do
  begin
    for J := Max(0, R.Left - NOT_GOOD) to R.Left - 1 do
      Inc(FGrid[I * FSize1.x + J].WOffset, MULTI * (NOT_GOOD + 1 + I - R.Left));
    for J := R.Right + 1 to Min(FSize.x, R.Right + NOT_GOOD) do
      Inc(FGrid[I * FSize1.x + J].WOffset, MULTI * (NOT_GOOD - 1 + I - R.Right));
  end;
}
end;

procedure TGenRoute.MarkPathForbid(const X0, Y0, X1, Y1: Integer);
const
  RANGE = 10;
var
  I: Integer;
  J: Integer;
begin
  if X0 = X1 then
  begin
    for I := Max(0, X0 - RANGE) to Min(FSize.x, X0 + RANGE) do
    begin
      if Y0 < Y1 then
        for J := Y0 to Y1 do
          FGrid[J * FSize1.x + I].Status := FORBID_PT
      else
        for J := Y1 to Y0 do
          FGrid[J * FSize1.x + I].Status := FORBID_PT;
    end;
  end
  else if Y0 = Y1 then
  begin
    for I := Max(0, Y0 - RANGE) to Min(FSize.y, Y0 + RANGE) do
    begin
      if X0 < X1 then
        for J := X0 to X1 do
          FGrid[I * FSize1.x + J].Status := FORBID_PT
      else
        for J := X1 to X0 do
          FGrid[I * FSize1.x + J].Status := FORBID_PT;
    end;
  end;
end;

procedure TGenRoute.MarkPathForbid(Conn: TGenEntityConnection);
var
  I: Integer;
begin
  Conn.FCtrlPts[0] := Conn.FromPt;
  Conn.FCtrlPts[High(Conn.FCtrlPts)] := Conn.ToPt;
  for I := 1 to High(Conn.FCtrlPts) do
    MarkPathForbid(Conn.FCtrlPts[I - 1].x, Conn.FCtrlPts[I - 1].y,
                    Conn.FCtrlPts[I - 0].x, Conn.FCtrlPts[I - 0].y);
end;

procedure TGenRoute.MarkBlock(const R: TRect);
var
  I, J: Integer;
begin
  for I := R.Top to R.Bottom do
  begin
    for J := R.Left to R.Right do
      FGrid[I * FSize1.x + J].Status := FORBID_PT;
  end;
end;

procedure TGenRoute.InitGrid(Entities: TList; BoundingBox: TRect);
var
  P: Pointer;
begin
  FSize.x := BoundingBox.Right;
  FSize.y := BoundingBox.Bottom;
  FSize1.x := FSize.x + 1;
  FSize1.y := FSize.y + 1;
  SetLength(FGrid, FSize1.x * FSize1.y);
  SetLength(FWorking, FSize1.x * FSize1.y);
  FillByte(FGrid[0], FSize1.x * SizeOf(FGrid[0]), 0);
  for P in Entities do
  begin
    with TGenEntityNode(P) do
    begin
      MarkBlock(Box);
      MarkBlockForbid(Box);
    end;
    MarkPortPath(TGenEntityNode(P));
  end;
end;

procedure TGenRoute.Restore;
begin
  Move(FGrid[0], FWorking[0], SizeOf(FGrid[0]) * (High(FGrid) + 1));
end;

function TGenRoute.At(const X, Y: Integer): PGenRouteNodeRec;
begin
  if InRange(X, 0, FSize.x) and InRange(Y, 0, FSize.y) then
    Result := @FWorking[Y * FSize1.x + X]
  else
    Result := nil;
end;

function TGenRoute.At(const Pt: TPoint): PGenRouteNodeRec;
begin
  Result := At(Pt.x, Pt.y);
end;

function TGenRoute.GridAt(const X, Y: Integer): PGenRouteNodeRec;
begin
  if InRange(X, 0, FSize.x) and InRange(Y, 0, FSize.y) then
    Result := @FGrid[Y * FSize1.x + X]
  else
    Result := nil;
end;

function TGenRoute.GridAt(const Pt: TPoint): PGenRouteNodeRec;
begin
  Result := GridAt(Pt.x, Pt.y);
end;

procedure TGenRoute.DbgPrint(const R: TRect);
var
  I, J: Integer;
  F: TFileStream;
  S: string;
begin
  F := TFileStream.Create('route_dbg.txt', fmCreate);
  try
    for I := R.Top to R.Bottom do
    begin
      for J := R.Left to R.Right do
      begin
        S := Format('  %5.8f', [FWorking[I * FSize1.x + J].W]);
        F.WriteBuffer(S[1], Length(S));
      end;
      S := #13#10;
      F.WriteBuffer(S[1], Length(S));
    end;
  finally
    F.Free;
  end;
end;

procedure TGenRoute.ClearOpenNode;
begin
  FSortedOpenNode.Clear;
end;

procedure TGenRoute.RemoteOpenNode(N: PGenRouteNodeRec);
begin
  FSortedOpenNode.Remove(N);
end;

procedure TGenRoute.InsertOpenNode(N: PGenRouteNodeRec);
var
  I: Integer;
begin
  for I := 0 to FSortedOpenNode.Count - 1 do
  begin
    if PGenRouteNodeRec(FSortedOpenNode[I])^.W < N^.W then Break;
  end;
  FSortedOpenNode.Insert(I, N);
end;

procedure TGenRoute.UpdateOpenNode(N: PGenRouteNodeRec);
begin
  FSortedOpenNode.Remove(N);
  InsertOpenNode(N);
end;

function TGenRoute.PopOpenNode: PGenRouteNodeRec;
begin
  Result := nil;
  if FSortedOpenNode.Count > 0 then
  begin
    Result := FSortedOpenNode.Last;
    FSortedOpenNode.Delete(FSortedOpenNode.Count - 1);
  end;
end;

procedure TGenRoute.PtoXY(N: PGenRouteNodeRec; out X, Y: Integer);
var
  T: Integer;
begin
  T := (Cardinal(N) - Cardinal(@FWorking[0])) div SizeOf(FWorking[0]);
  Y := T div FSize1.x;
  X := T mod FSize1.x;
end;

function TGenRoute.GetNeighbors(N: PGenRouteNodeRec;
  var A: array of PGenRouteNodeRec): Integer;
var
  X, Y: Integer;
begin
  PtoXY(N, X, Y);
  Result := GetNeighbors(X, Y, A);
end;

function TGenRoute.GetNeighbors(const X, Y: Integer;
  var A: array of PGenRouteNodeRec): Integer;

  procedure Check(X0, Y0: Integer);
  var
    P: PGenRouteNodeRec;
  begin
    P := At(X0, Y0);
    if Assigned(P) and (P^.Status <> FORBID_PT) then
    begin
      A[GetNeighbors] := P;
      Inc(GetNeighbors);
    end;
  end;

begin
  GetNeighbors := 0;
  Check(X + 1, Y);
  Check(X, Y + 1);
  Check(X - 1, Y);
  Check(X, Y - 1);
end;

function TGenRoute.IsSegmentClear(const Pt1, Pt2: TPoint): Boolean;
var
  X0, Y0, X1, Y1: Integer;
  K: Double;
  X, Y: Integer;
begin
  Result := True;
  X0 := Pt1.x;
  Y0 := Pt1.y;
  X1 := Pt2.x;
  Y1 := Pt2.y;
  if X0 <> X1 then
  begin
    if X0 > X1 then
    begin
      Result := IsSegmentClear(Pt2, Pt1);
      Exit;
    end;
    K := (Y1 - Y0) / (X1 - X0);
    for X := 1 to X1 - X0 - 1 do
    begin
      Y := Round(X * K) + Y0;
      if FWorking[Y * FSize1.x + X + X0].Status = FORBID_PT then
      begin
        Result := False;
        Exit;
      end;
    end;
  end
  else begin
    if Y0 > Y1 then
    begin
      Result := IsSegmentClear(Pt2, Pt1);
      Exit;
    end;
    for Y := Y0 + 1 to Y1 - 1 do
    begin
      if FWorking[Y * FSize1.x + X0].Status = FORBID_PT then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

procedure TGenRoute.Backtrace(Conn: TGenEntityConnection);
var
  P1, P2: PGenRouteNodeRec;
  C: array [0..100] of TPoint;
  I: Integer = 0;
  D: Integer;
  D2: Integer;
  X0, Y0, X1, Y1: Integer;

  function GetDir: Integer;
  begin
    PtoXY(P2, X1, Y1);
    if X0 = X1 then
      Result := IfThen(Y0 < Y1, 1, 3)
    else if Y0 = Y1 then
      Result := IfThen(X0 < X1, 0, 2);
  end;

begin
  P1 := At(Conn.ToPt);
  P2 := P1^.Pre;
  PtoXY(P1, X0, Y0);
  D := GetDir;
  while Assigned(P1^.Pre) do
  begin
    P2 := P1^.Pre;
    D2 := GetDir;
    if D <> D2 then
    begin
      C[I].x := X0;
      C[I].y := Y0;
      Inc(I);
      if I > High(C) then
      begin
        Break;
      end;
    end;
    D := D2;
    P1 := P2;
    X0 := X1;
    Y0 := Y1;
  end;
  Conn.SetCtrlPointsNumber(I);
  for D := 0 to I - 1 do
    Conn.CtrlPoints[D] := C[I - 1 - D];
  MarkPathForbid(Conn);
end;

constructor TGenRoute.Create;
begin
  inherited;
  FSortedOpenNode := TList.Create;
end;

destructor TGenRoute.Destroy;
begin
  FSortedOpenNode.Free;
  inherited Destroy;
end;

{ TGenForceDirectedLayout }

procedure TGenForceDirectedLayout.Layout(Entities, Conns: TList);
begin
end;

{ TGenAlgoFactory }

constructor TGenAlgoFactory.Create;
begin

end;

destructor TGenAlgoFactory.Destroy;
begin
  if Assigned(FLayout) then FLayout.Free;
  if Assigned(FRoute) then FRoute.Free;
  inherited Destroy;
end;

function TGenAlgoFactory.GetLayout(const ALayout: TLayoutAlgorithm): TGenLayout;
begin
  case ALayout of
    laLevel:
      begin
        if Assigned(FLayout) and (FLayout is TGenLevelLayout) then Exit(FLayout);
        FLayout.Free;
        FLayout := TGenLevelLayout.Create;
      end;
    laForceDirected:
      begin
        if Assigned(FLayout) and (FLayout is TGenForceDirectedLayout) then Exit(FLayout);
        FLayout.Free;
        FLayout := TGenForceDirectedLayout.Create;
      end;
  end;
  Result := FLayout;
end;

function TGenAlgoFactory.GetRoute(const ARoute: TRouteAlgorithm): TGenRoute;
begin
  case ARoute of
    raAStar:
      begin
        if Assigned(FRoute) and (FRoute is TGenAStarRoute) then Exit(FRoute);
        FRoute.Free;
        FRoute := TGenAStarRoute.Create;
      end;
    raDirect:
      begin
        if Assigned(FRoute) and (FRoute is TGenDirectRoute) then Exit(FRoute);
        FRoute.Free;
        FRoute := TGenDirectRoute.Create;
      end;
  end;
  Result := FRoute;
end;

{ TGenMazeRoute }

function TGenAStarRoute.CalcWeight(const X, Y: Integer): Integer;
const
  PANISH_RANGE = 20;
var
  I: Integer;
  T: Integer;
  function CheckPt(const Index: Integer): Boolean; inline;
  var
    V: Integer;
  begin
    V := FGrid[Index].Status;
    Result := (V = FORBID_PT) or (V = WIRE_USED);
  end;

begin
  T := FSize.x + 1 - X;
  for I := X + 1 to FSize.x do
  begin
    if CheckPt(Y * FSize1.x + I) then
    begin
      T := Min(I - X, T);
      Break;
    end;
  end;

  for I := X - 1 downto 0 do
  begin
    if CheckPt(Y * FSize1.x + I) then
    begin
      T := Min(X - I, T);
      Break;
    end;
  end;

  for I := Y + 1 to FSize.y do
  begin
    if CheckPt(I * FSize1.x + X) then
    begin
      T := Min(I - Y, T);
      Break;
    end;
  end;

  for I := Y - 1 downto 0 do
  begin
    if CheckPt(I * FSize1.x + X) then
    begin
      T := Min(Y - I, T);
      Break;
    end;
  end;

  if T < PANISH_RANGE then
    Result := PANISH_RANGE - T
  else
    Result := 1;
end;

function TGenAStarRoute.TryRouteOne(Conn: TGenEntityConnection;
  const Frame: TRect): Boolean;
var
  S: TPoint;
  E: TPoint;
  T: Integer;

  function Propagate: Integer;
  var
    I: Integer;
    D: Integer;
    Neighbors: array [0..3] of PGenRouteNodeRec = (nil, nil, nil, nil);
    NN: Integer;
    This: PGenRouteNodeRec;
    function Check(ANode: PGenRouteNodeRec): Boolean;
    var
      AX, AY: Integer;
    begin
      Result := False;
      if ANode^.Status = VISITED then
      begin
        if ANode^.D > D + 1 then
          RemoteOpenNode(ANode)
        else
          Exit;
      end;

      PtoXY(ANode, AX, AY);
      if not IsPtInRect(AX, AY, Frame) then Exit;
      if (AX = E.x) and (AY = E.y) then
      begin
        At(E)^.Pre := This;
        Exit(True);
      end;

      ANode^.D := D + 1;      //;
      ANode^.W := ANode^.D + (Abs(AX - E.x) + Abs(AY - E.y));
      ANode^.Pre := This;
      InsertOpenNode(ANode);
      ANode^.Status := VISITED;
    end;
  begin
    Result := -1;
    This := PopOpenNode;
    D := This^.D;
    NN := GetNeighbors(This, Neighbors);
    for I := 0 to NN - 1 do
    begin
      if Check(Neighbors[I]) then Exit;
    end;
    Result := 1;
  end;

begin
  S := Conn.FromPt;
  E := Conn.ToPt;
  with At(S)^ do
  begin
    Status := VISITED;
  end;
  with At(E)^ do
  begin
    Status := EMPTY;
  end;
  ClearOpenNode;

  InsertOpenNode(At(S));
  while FSortedOpenNode.Count > 0 do
  begin
    T := Propagate;
    if T < 0 then Break;
  end;

  Result := T < 0;
  if Result then
    Backtrace(Conn)
  else;

  with At(S)^ do
  begin
    Status := FORBID_PT;
  end;
  with At(E)^ do
  begin
    Status := FORBID_PT;
  end;
end;

procedure TGenAStarRoute.AStarRouteOne(Conn: TGenEntityConnection);
var
  Frame: TRect;
  I: Integer;
  J: Integer = 0;
begin
  Frame := Conn.Box;
  for J := 0 to 2 do
  begin
    Restore;
    I := Round((Frame.Right - Frame.Left) * 0.1);
    Frame.Left := Max(0, Frame.Left - I);
    Frame.Right := Min(FSize.x, Frame.Right + I);
    I := Round((Frame.Bottom - Frame.Top) * 0.1);
    Frame.Top := Max(0, Frame.Top - I);
    Frame.Bottom := Min(FSize.y, Frame.Bottom + I);
    if TryRouteOne(Conn, Frame) then
      Break;
  end;
end;

procedure TGenAStarRoute.RouteOne(Conn: TGenEntityConnection);
  procedure Walk(const I, J: Integer);
  begin
    if not IsSegmentClear(Conn.FCtrlPts[I], Conn.FCtrlPts[J]) then
      AStarRouteOne(Conn);
  end;
begin
  Restore;
  Conn.SetCtrlPointsNumber(0);
  Conn.FCtrlPts[0] := Conn.FromPt;
  Conn.FCtrlPts[1] := Conn.ToPt;
  Walk(0, 1);
end;

procedure TGenAStarRoute.Route(Entities, Conntections: TList; BoundingBox: TRect
  );
var
  P: Pointer;
begin
  InitGrid(Entities, BoundingBox);
  for P in Conntections do
    RouteOne(TGenEntityConnection(P));
end;

function GenEntityCompare(Item1, Item2: TGenEntityNode): Integer;
begin
  Result := Item1.Tag - Item2.Tag;
  if Result = 0 then
    Result := (Item1.DegIn + Item1.DegOut) - (Item2.DegIn + Item2.DegOut);
end;

{ TGenLevelLayout }

procedure TGenLevelLayout.Layout(Entities, Conns: TList);
var
  P: Pointer;
  C: Pointer;
  Dirty: Boolean;
  I: Integer;
  J: Integer;
  K: Integer;
  V: Integer = 0;
  S: array of TRect;
  A: array of Pointer;
begin
  if Entities.Count < 1 then Exit;

  // here we use TGenEntityNode.Tag as Level

  for P in Entities do
  begin
    TGenEntityNode(P).Pos.x := -1;
    TGenEntityNode(P).DegIn := 0;
    TGenEntityNode(P).DegOut := 0;
  end;

  for C in Conns do
    with TGenEntityConnection(C) do
    begin
      ToPort.Entity.DegIn := ToPort.Entity.DegIn + 1;
      FromPort.Entity.DegOut := FromPort.Entity.DegOut + 1;
    end;

  for P in Entities do
  begin
    with TGenEntityNode(P) do
    begin
      Tag := IfThen(DegIn = 0, 0, MaxInt);
    end;
  end;

  repeat
    Dirty := False;
    for C in Conns do
    begin
      with TGenEntityConnection(C) do
      begin
        if FromPort.Entity.Tag = MaxInt then Continue;
        if ToPort.Entity.Tag > FromPort.Entity.Tag + 1 then
        begin
          ToPort.Entity.Tag := FromPort.Entity.Tag + 1;
          Dirty := True;
          Break;
        end;
      end;
    end;
  until not Dirty;

  Entities.Sort(TListSortCompare(@GenEntityCompare));

  // level should be continous
  I := 0;
  for I := 0 to Entities.Count - 1 do
  begin
    if TGenEntityNode(Entities[I]).Tag - V > 1 then
    begin
      K := TGenEntityNode(Entities[I]).Tag - V - 1;
      for J := I to Entities.Count - 1 do
        TGenEntityNode(Entities[J]).Tag := TGenEntityNode(Entities[J]).Tag - K;
    end;
    V := TGenEntityNode(Entities[I]).Tag;
  end;

  // sort Entities that have the same Level: maximum is at center, sides follows
  I := 0;
  while I < Entities.Count do
  begin
    J := I + 1;
    while J < Entities.Count do
    begin
      if TGenEntityNode(Entities[J]).Tag = TGenEntityNode(Entities[I]).Tag then
        Inc(J)
      else
        Break;
    end;
    SetLength(A, J - I);
    for K := 0 to J - I - 1 do
    begin
      if Odd(K) then
        A[J - I - 1 - (K - 1) div 2] := Entities[K + I]
      else
        A[K div 2] := Entities[K + I]
    end;

    for K := 0 to J - I - 1 do
      Entities[K + I] := A[K];

    I := J;
  end;

  // size of each level
  SetLength(S, TGenEntityNode(Entities[Entities.Count - 1]).Tag + 1);
  for P in Entities do
  begin
    with TGenEntityNode(P) do
    begin
      S[Tag].Right  := Max(S[Tag].Right, Extent.cx);
      S[Tag].Bottom := S[Tag].Bottom + V_MARGIN + Extent.cy;
    end;
  end;

  // get max v-size
  K := 0;
  for I := 0 to High(S) do
    K := Max(K, S[I].Bottom);

  // Left
  J := H_MARGIN div 2;
  for I := 0 to High(S) do
  begin
    with S[I] do
    begin
      Left := J;
      Top := (K - Bottom) div 2;
      Inc(J, Right + H_MARGIN);
    end;
  end;

  // layout
  for P in Entities do
  begin
    with TGenEntityNode(P) do
    begin
      Pos.x := S[Tag].Left + (S[Tag].Right - Extent.cx) div 2;
      Pos.y := S[Tag].Top;
      S[Tag].Top := S[Tag].Top + Extent.cy + V_MARGIN;
    end;
  end;
end;

{ TGenEntityNode }

function TGenEntityNode.CalcFirstPortY(const N: Integer): Integer;
begin
  Result := 0;
  if N > 0 then
   Result := Box.Bottom - Box.Top - (N - 1) * (PORT_MARK_MARGIN + PORT_MARK_SIZE) - PORT_MARK_SIZE;
  Result := Box.Top + Result div 2;
end;

procedure TGenEntityNode.DrawPorts(ACanvas: TCanvas);
var
  R: TRect;
  DrawRect: TRect;

  procedure DrawArrow(const N: Integer);
  const
    L = 10;
    H = 3;
  var
    Pts: array [0..2] of TPoint;
    I: Integer;
  begin
    for I := 0 to N - 1 do
    begin
      Pts[0] := GetPortConnectPos(epIn, I);
      Pts[1].x := Pts[0].x - L;
      Pts[1].y := Pts[0].y + H;
      Pts[2].x := Pts[1].x;
      Pts[2].y := Pts[0].y - H;
      ACanvas.Polygon(Pts);
    end;
  end;

  procedure DrawIt(N: Integer; const Names: array of Char);
  var
    I: Integer;
  begin
    for I := 0 to N - 1 do
    begin
      ACanvas.Rectangle(R);
      ACanvas.TextRect(R, R.Left + PORT_MARK_SIZE div 3 - 1, R.Top + 1, Names[I]);
      Inc(R.Top, PORT_MARK_MARGIN + PORT_MARK_SIZE);
      Inc(R.Bottom, PORT_MARK_MARGIN + PORT_MARK_SIZE);
    end;
  end;

begin
  DrawRect := Box;
  with R do
  begin
    Left := DrawRect.Left;
    Right := Left + PORT_MARK_SIZE;
    Top  := CalcFirstPortY(High(FInPorts) + 1);
    Bottom := Top + PORT_MARK_SIZE;
  end;
  with ACanvas do
  begin
    Font.Color := clWhite;
    Font.Height  := PORT_MARK_SIZE - 1;
    Font.Bold := True;
    Pen.Width := 1;
    Pen.Color := clBlack;
    Pen.Style := psSolid;
    Brush.Color := clFuchsia;
    Brush.Style := bsSolid;
  end;
  DrawIt(High(FInPorts) + 1, FInPorts);
  ACanvas.Brush.Color := clGray;
  DrawArrow(High(FInPorts) + 1);

  ACanvas.Brush.Color := clGreen;
  with R do
  begin
    Left := DrawRect.Right - PORT_MARK_SIZE;
    Right := DrawRect.Right;
    Top  := CalcFirstPortY(High(FOutPorts) + 1);
    Bottom := Top + PORT_MARK_SIZE;
  end;
  DrawIt(High(FOutPorts) + 1, FOutPorts);
end;

function TGenEntityNode.GetBox: TRect;
begin
  with Result do
  begin
    Left := Pos.x;
    Right := Left + Extent.cx;
    Top   := Pos.y;
    Bottom := Top + Extent.cy;
  end;
end;

constructor TGenEntityNode.Create;
begin
  inherited;
  //FInPorts[0] := 'def';
end;

procedure TGenEntityNode.Measure(ACanvas: TCanvas);
var
  I: Integer;
begin
  I := Max(High(FInPorts), High(FOutPorts)) + 1;
  if I >= 1 then I := I * (PORT_MARK_MARGIN + PORT_MARK_SIZE);

  if Assigned(FDrawable) then FDrawable.Measure(ACanvas, FExtent);
  FExtent.cx := FExtent.cx + 2 * PORT_MARK_SIZE;
  FExtent.cy := Max(FExtent.cy, I);
end;

procedure TGenEntityNode.Draw(ACanvas: TCanvas);
var
  DrawRect: TRect;
  NodeRect: TRect;
begin
  inherited;
  DrawRect := Box;

  NodeRect := DrawRect;
  with NodeRect do
  begin
    Left  := Left + PORT_MARK_SIZE;
    Right := Right - PORT_MARK_SIZE;
  end;

  with ACanvas do
  begin
    Brush.Color := clWhite;
    FillRect(DrawRect);
  end;
  if not Assigned(FDrawable) then Exit;
  FDrawable.Draw(ACanvas, NodeRect);
  DrawPorts(ACanvas);
end;

function TGenEntityNode.IsPtOn(const Pt: TPoint): Boolean;
begin
  Result := InRange(Pt.x, Pos.x, Pos.x + FExtent.cx)
         and InRange(Pt.y, Pos.y, Pos.y + FExtent.cy);
end;

procedure TGenEntityNode.MouseClick(const Pt: TPoint);
begin
  if Assigned(FDrawable) then FDrawable.MouseClick(Pt);
end;

procedure TGenEntityNode.SetPortsNum(const T: TGenEntityPortType;
  const N: Integer);
begin
  case T of
    epIn: SetLength(FInPorts, N);
    epOut: SetLength(FOutPorts, N);
  end;
end;

procedure TGenEntityNode.SetPortsNumAtLeast(const T: TGenEntityPortType;
  const N: Integer);
begin
  case T of
    epIn : if High(FInPorts) < N -1  then SetLength(FInPorts, N);
    epOut: if High(FOutPorts) < N -1 then SetLength(FOutPorts, N);
  end;
end;

function TGenEntityNode.GetPortsNum(const T: TGenEntityPortType): Integer;
begin
  Result := 0;
  case T of
    epIn : Result := High(FInPorts) + 1;
    epOut: Result := High(FOutPorts) + 1;
  end;
end;

procedure TGenEntityNode.SetPortName(const T: TGenEntityPortType;
  const N: Cardinal; const Name: Char);
begin
  case T of
    epIn : if N <= High(FInPorts) then FInPorts[N] := Name;
    epOut: if N <= High(FOutPorts) then FOutPorts[N] := Name;
  end;
end;

procedure TGenEntityNode.Move(const OffsetX, OffsetY: Integer);
begin
  Pos.x := Pos.x + OffsetX;
  Pos.y := Pos.y + OffsetY;
end;

function TGenEntityNode.GetPortConnectPos(T: TGenEntityPortType; Index: Integer
  ): TPoint;
begin
  case T of
    epIn:
      begin
        Result.x := Pos.x;
        if InRange(Index, 0, High(FInPorts)) then
          Result.y := CalcFirstPortY(High(FInPorts) + 1) + (PORT_MARK_MARGIN + PORT_MARK_SIZE) * Index
                   + PORT_MARK_SIZE div 2
        else
          Result.y := Pos.y;
      end;
    epOut:
      begin
        Result.x := Pos.x + FExtent.cx;
        if InRange(Index, 0, High(FOutPorts)) then
          Result.y := CalcFirstPortY(High(FOutPorts) + 1) + (PORT_MARK_MARGIN + PORT_MARK_SIZE) * Index
                   + PORT_MARK_SIZE div 2
        else
          Result.y := Pos.y;
      end;
  end;
end;

{ TGenEntityConnection }

function TGenEntityConnection.GetCtrlPoints(const Index: Integer): TPoint;
begin
  Result.x := 0; Result.y := 0;
  if InRange(Index, 0, High(FCtrlPts) - 1) then
    Result := FCtrlPts[Index + 1];
end;

function TGenEntityConnection.GetEndPt: TPoint;
begin
  Result := FToPort.Entity.GetPortConnectPos(epIn, FToPort.Index);
end;

function TGenEntityConnection.GetStartPt: TPoint;
begin
  Result := FFromPort.Entity.GetPortConnectPos(epOut, FFromPort.Index);
end;

procedure TGenEntityConnection.SetCtrlPoints(const Index: Integer; AValue: TPoint);
begin
  if InRange(Index, 0, High(FCtrlPts) - 1) then
    FCtrlPts[Index + 1] := AValue;
end;

function TGenEntityConnection.GetBox: TRect;
begin
  with Result do
  begin
    Left := Min(FromPt.x, ToPt.x);
    Right := Max(FromPt.x, ToPt.x);
    Top   := Min(FromPt.y, ToPt.y);
    Bottom := Max(FromPt.y, ToPt.y);
  end;
end;

constructor TGenEntityConnection.Create;
begin
  inherited;
  SetLength(FCtrlPts, 2);
  FPenStyle := psSolid;
end;

procedure TGenEntityConnection.SetCtrlPointsNumber(const N: Integer);
begin
  SetLength(FCtrlPts, N + 2);
end;

procedure TGenEntityConnection.Measure(ACanvas: TCanvas);
var
  S, E: TPoint;
begin
  S := FromPt;
  E := ToPt;
  FExtent.cx := Max(S.x, E.x) - Min(S.x, E.x);
  FExtent.cy := Max(S.y, E.y) - Min(S.y, E.y);
end;

procedure TGenEntityConnection.Draw(ACanvas: TCanvas);
begin
  inherited;
  FCtrlPts[0] := FromPt;
  FCtrlPts[High(FCtrlPts)] := ToPt;
  with ACanvas do
  begin
    Pen.Color := clGray;
    Pen.Style := FPenStyle;
    Pen.Width := 2;
    //PolyBezier(FCtrlPts);
    //Line(FCtrlPts[0], FCtrlPts[High(FCtrlPts)]);
    Polyline(FCtrlPts);
  end;
end;

procedure TGenEntityConnection.Move(const OffsetX, OffsetY: Integer);
var
  I: Integer;
begin
  for I := 1 to High(FCtrlPts) - 1 do
  begin
    FCtrlPts[I].x := FCtrlPts[I].x + OffsetX;
    FCtrlPts[I].y := FCtrlPts[I].y + OffsetY;
  end;
end;

{ TGenEntity }

function TGenEntity.GetBox: TRect;
begin
  with Result do
  begin
    Left := 0;
    Right := 10;
    Top := 0;
    Bottom := 10;
  end;
end;

procedure TGenEntity.Invalidate;
begin
  FInValidated := True;
end;

procedure TGenEntity.Measure(ACanvas: TCanvas);
begin

end;

procedure TGenEntity.Draw(ACanvas: TCanvas);
begin
  FInValidated := False;
end;

function TGenEntity.IsPtOn(const Pt: TPoint): Boolean;
begin
  Result := False;
end;

procedure TGenEntity.MouseClick(const Pt: TPoint);
begin

end;

procedure TGenEntity.Move(const OffsetX, OffsetY: Integer);
begin

end;

{ TGenGraph }

function TGenGraph.GetPaintBox: TPaintBox;
begin
  Result := FDBuffer.PaintBox;
end;

procedure TGenGraph.SetPaintBox(AValue: TPaintBox);
begin
  FDBuffer.PaintBox := AValue;
  AValue.OnClick := @PaintBoxClick;
end;

function TGenGraph.FindConnection(AFrom, ATo: TGenEntityNode; const AFromPort,
  AToPort: Integer): TGenEntityConnection;
var
  P: Pointer;
  C: TGenEntityConnection;
begin
  Result := nil;
  for P in FConns do
  begin
    C := TGenEntityConnection(P);
    if (C.FromPort.Entity = AFrom) and (C.FromPort.Index = AFromPort)
      and (C.ToPort.Entity = AFrom) and (C.ToPort.Index = AFromPort) then
    begin
      Result := C;
      Break;
    end;
  end;
end;

procedure TGenGraph.PaintBoxClick(Sender: TObject);
var
  Pt: TPoint;
  function foreach(L: TList): Boolean;
  var
    P: Pointer;
  begin
    Result := True;
    for P in FEntities do
    begin
      with TGenEntity(P) do
      begin
        if IsPtOn(Pt) then
        begin
          MouseClick(Pt);
          Exit;
        end;
      end;
    end;
    Result := False;
  end;

begin
  Pt := FDBuffer.PaintBox.ScreenToClient(Mouse.CursorPos);
  if foreach(FEntities) then Exit;
  foreach(FConns);
end;

procedure TGenGraph.Layout;
begin
  FFactory.GetLayout(FLayoutAlgo).Layout(FEntities, FConns);
end;

procedure TGenGraph.Route(const BoundingBox: TRect);
begin
  FFactory.GetRoute(FRouteAlgo).Route(FEntities, FConns, BoundingBox);
end;

procedure TGenGraph.FullRender;
const
  Margin_H = 10;
  Margin_V = 40;
var
  P: Pointer;
  R: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
begin
  if FUpdateCount > 0 then Exit;

  for P in FEntities do TGenEntityNode(P).Measure(FDBuffer.PaintBuffer.Canvas);

  Layout;
  for P in FEntities do
    R := MergeRect(R, TGenEntity(P).Box);
  Inc(R.Right, Margin_H * 2);
  Inc(R.Bottom, Margin_V * 2);
  for P in FEntities do
    TGenEntity(P).Move(Margin_H, Margin_V);

  Route(R);
  for P in FConns do
  begin
    TGenEntityConnection(P).Measure(FDBuffer.PaintBuffer.Canvas);
    R := MergeRect(R, TGenEntity(P).Box);
  end;

  FDBuffer.PaintBox.Width := R.Right;
  FDBuffer.PaintBox.Height := R.Bottom;
  FDBuffer.SetSize(R.Right, R.Bottom);

  // draw background
  with FDBuffer.PaintBuffer.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(R);
  end;
  for P in FEntities do TGenEntity(P).Draw(FDBuffer.PaintBuffer.Canvas);
  for P in FConns do TGenEntity(P).Draw(FDBuffer.PaintBuffer.Canvas);
end;

procedure TGenGraph.PartialRender;
var
  P: Pointer;
begin
  for P in FEntities do
  begin
    with TGenEntityNode(P) do
    begin
      if Invalidated then Draw(FDBuffer.DrawBuffer.Canvas);
    end;
  end;
  FDBuffer.Paint;
end;

procedure TGenGraph.RemoveEntity(AEntity: TGenEntityNode);
begin
  FEntities.Remove(AEntity);
end;

procedure TGenGraph.AddEntity(AEntity: TGenEntityNode);
begin
  FEntities.Add(AEntity);
end;

procedure TGenGraph.Clear;
var
  P: Pointer;
begin
  for P in FConns    do TGenEntityConnection(P).Free;
  for P in FEntities do TGenEntityNode(P).Free;
  FConns.Clear;
  FEntities.Clear;
end;

function TGenGraph.AddConnection(AFrom, ATo: TGenEntityNode; const AFromPort,
  AToPort: Integer): TGenEntityConnection;
begin
  Result := nil;
  if not Assigned(AFrom) then raise Exception.Create('not Assigned(AFrom)');
  if not Assigned(ATo) then raise Exception.Create('not Assigned(ATo)');
  if AFrom = ATo then raise Exception.Create('AFrom = ATo');

  AFrom.SetPortsNumAtLeast(epOut, AFromPort + 1);
  ATo.SetPortsNumAtLeast(epIn, AToPort + 1);
  Result := TGenEntityConnection.Create;
  with Result.FromPort do
  begin
    Entity := AFrom;
    Index := AFromPort;
  end;
  with Result.ToPort do
  begin
    Entity := ATo;
    Index := AToPort;
  end;
  FConns.Add(Result);
end;

procedure TGenGraph.RemoveConnecttion(AFrom, ATo: TGenEntityNode;
  const AFromPort, AToPort: Integer);
var
  C: TGenEntityConnection;
begin
  C := FindConnection(AFrom, ATo, AFromPort, AToPort);
  if Assigned(C) then FConns.Remove(C);
end;

procedure TGenGraph.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TGenGraph.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount <= 0 then FullRender;
end;

constructor TGenGraph.Create;
begin
  inherited;
  FDBuffer := TDoubleBuffer.Create;
  FDBuffer.PaintBuffer.Canvas.AntialiasingMode := amOn;
  FDBuffer.DrawBuffer.Canvas.AntialiasingMode := amOn;
  FEntities := TList.Create;
  FConns    := TList.Create;
  FFactory  := TGenAlgoFactory.Create;
end;

destructor TGenGraph.Destroy;
begin
  FDBuffer.PaintBox.OnClick := nil;
  Clear;
  FDBuffer.Free;
  inherited Destroy;
end;

end.

