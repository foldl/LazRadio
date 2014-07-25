unit gen_graph;

{$mode objfpc}{$H+}

// This is a generic directed graph class:

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, Utils, Math;

type

  IGenDrawable = interface
    procedure Measure(out Extent: TPoint);
    procedure Draw(ACanvas: TCanvas; ARect: TRect);
    procedure MouseClick(const Pt: TPoint; Shifts: TShiftState);
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
    FExtent: TPoint;

  public
    procedure Invalidate;

    procedure Measure; virtual;
    procedure Draw(ACanvas: TCanvas); virtual;
    function  IsPtOn(const Pt: TPoint): Boolean; virtual;
    procedure MouseClick(const Pt: TPoint; Shifts: TShiftState); virtual;

    property Extent: TPoint read FExtent;
    property Invalidated: Boolean read FInValidated;
  end;

  { TGenEntityNode }

  TGenEntityNode = class(TGenEntity)
  private
    FDrawable: IGenDrawable;
    FDrawRect: TRect;
    FNodeRect: TRect;
    FInPorts: array of string;
    FOutPorts: array of string;

    function GetPos: TPoint;
    procedure SetPos(AValue: TPoint);

    procedure DrawPorts(ACanvas: TCanvas);

  public
    constructor Create;

    procedure Measure; override;
    procedure Draw(ACanvas: TCanvas); override;
    function  IsPtOn(const Pt: TPoint): Boolean; override;
    procedure MouseClick(const Pt: TPoint; Shifts: TShiftState); override;

    procedure SetPortsNum(const T: TGenEntityPortType; const N: Integer);
    procedure SetPortsNumAtLeast(const T: TGenEntityPortType; const N: Integer);

    function  GetPortConnectPos(T: TGenEntityPortType; Index: Integer): TPoint;

    property Pos: TPoint read GetPos write SetPos;
    property Drawable: IGenDrawable read FDrawable write FDrawable;

    property Level: Integer read FLevel write FLevel;
  end;

  { TGenEntityConnection }

  TGenEntityConnection = class(TGenEntity)
  private
    FCtrlPts: array of TPoint;
    FFromPort: TGenEntityPortRef;
    FToPort: TGenEntityPortRef;
    function GetCtrlPoints(const Index: Integer): TPoint;
    function GetEndPt: TPoint;
    function GetStartPt: TPoint;
    procedure SetCtrlPoints(const Index: Integer; AValue: TPoint);
  public
    constructor Create;

    procedure SetCtrlPointsNumber(const N: Integer);

    procedure Measure; override;
    procedure Draw(ACanvas: TCanvas); override;

    property CtrlPoints[const Index: Integer]: TPoint read GetCtrlPoints write SetCtrlPoints;
    property FromPt: TPoint read GetStartPt;
    property ToPt: TPoint read GetEndPt;

    property FromPort: TGenEntityPortRef read FFromPort write FFromPort;
    property ToPort: TGenEntityPortRef read FToPort write FToPort;
  end;

  { TGenGraph }

  TGenGraph = class
  private
    FDBuffer: TDoubleBuffer;
    FEntities: TList;
    FConns: TList;
    FUpdateCount: Integer;
    function GetPaintBox: TPaintBox;
    procedure SetPaintBox(AValue: TPaintBox);
    function  FindConnection(AFrom, ATo: TGenEntityNode; const AFromPort,
      AToPort: Integer): TGenEntityConnection;
  protected
    procedure Layout;
  public
    procedure FullRender;
    procedure PartialRender;

    procedure RemoveEntity(AEntity: TGenEntityNode);
    procedure AddEntity(AEntity: TGenEntityNode);
    procedure AddConnection(AFrom, ATo: TGenEntityNode; const AFromPort, AToPort: Integer);
    procedure RemoveConnecttion(AFrom, ATo: TGenEntityNode; const AFromPort, AToPort: Integer);
    procedure Clear;

    procedure BeginUpdate;
    procedure EndUpdate;

    constructor Create;
    destructor Destroy; override;

    property PaintBox: TPaintBox read GetPaintBox write SetPaintBox;
  end;

implementation

const
  PORT_MARK_SIZE   = 12;
  PORT_MARK_MARGIN = 2 * PORT_MARK_SIZE;

{ TGenEntityNode }

function TGenEntityNode.GetPos: TPoint;
begin
  Result.x := FDrawRect.Left;
  Result.y := FDrawRect.Top;
end;

procedure TGenEntityNode.SetPos(AValue: TPoint);
begin
  with FDrawRect do
  begin
    Left := AValue.x;
    Top  := AValue.y;
  end;
end;

procedure TGenEntityNode.DrawPorts(ACanvas: TCanvas);
var
  R: TRect;
  procedure DrawIt(N: Integer);
  var
    I: Integer;
  begin
    for I := 0 to N - 1 do
    begin
      ACanvas.Rectangle(R);
      Inc(R.Top, PORT_MARK_MARGIN);
      Inc(R.Bottom, PORT_MARK_MARGIN);
    end;
  end;

begin
  with R do
  begin
    Left := FDrawRect.Left;
    Right := Left + PORT_MARK_SIZE;
    Top  := PORT_MARK_MARGIN div 2;
    Bottom := Top + PORT_MARK_SIZE;
  end;
  with ACanvas do
  begin
    Pen.Color := clBlack;
    Pen.Style := psSolid;
    Brush.Color := TColor($ffaaaa);
    Brush.Style := bsSolid;
  end;
  DrawIt(High(FInPorts) + 1);
  ACanvas.Brush.Color := TColor($aaaaff);
  with R do
  begin
    Left := FDrawRect.Right - PORT_MARK_SIZE;
    Right := FDrawRect.Right;
    Top  := PORT_MARK_MARGIN div 2;
    Bottom := Top + PORT_MARK_SIZE;
  end;
  DrawIt(High(FOutPorts) + 1);
end;

constructor TGenEntityNode.Create;
begin
  SetLength(FInPorts, 1);
  SetLength(FOutPorts, 1);
  FInPorts[0] := 'def';
end;

procedure TGenEntityNode.Measure;

begin
  if Assigned(FDrawable) then FDrawable.Measure(FExtent);
  FExtent.x := FExtent.x + 2 * PORT_MARK_SIZE;
  FExtent.y := Max(FExtent.y, (High(FOutPorts) + 1) * (PORT_MARK_MARGIN + PORT_MARK_SIZE));

  with FDrawRect do
  begin
    Right  := Left + FExtent.x;
    Bottom :=  Top + FExtent.y;
  end;
  FNodeRect := FDrawRect;
  with FNodeRect do
  begin
    Left  := Left + PORT_MARK_SIZE;
    Right := Right - PORT_MARK_SIZE;
  end;
end;

procedure TGenEntityNode.Draw(ACanvas: TCanvas);
begin
  inherited;
  with ACanvas do
  begin
    Brush.Color := clWhite;
    FillRect(FDrawRect);
  end;
  if not Assigned(FDrawable) then Exit;
  FDrawable.Draw(ACanvas, FNodeRect);
  DrawPorts(ACanvas);
end;

function TGenEntityNode.IsPtOn(const Pt: TPoint): Boolean;
begin
  Result := InRange(Pt.x, FDrawRect.Left, FDrawRect.Right)
         and InRange(Pt.y, FDrawRect.Top, FDrawRect.Bottom);
end;

procedure TGenEntityNode.MouseClick(const Pt: TPoint; Shifts: TShiftState);
begin
  if Assigned(FDrawable) then FDrawable.MouseClick(Pt, Shifts);
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

function TGenEntityNode.GetPortConnectPos(T: TGenEntityPortType; Index: Integer
  ): TPoint;
begin
  case T of
    epIn:
      begin
        if InRange(Index, 0, High(FInPorts)) then
          Result.y := (PORT_MARK_MARGIN div 2) + (PORT_MARK_MARGIN + PORT_MARK_SIZE) * Index;
        Result.x := FDrawRect.Left;
      end;
    epOut:
      begin
        if InRange(Index, 0, High(FOutPorts)) then
          Result.y := (PORT_MARK_MARGIN div 2) + (PORT_MARK_MARGIN + PORT_MARK_SIZE) * Index;
        Result.x := FDrawRect.Right;
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

constructor TGenEntityConnection.Create;
begin
  SetLength(FCtrlPts, 4);
end;

procedure TGenEntityConnection.SetCtrlPointsNumber(const N: Integer);
var
  T: TPoint;
begin
  T := ToPt;
  SetLength(FCtrlPts, N + 2);
end;

procedure TGenEntityConnection.Measure;
var
  S, E: TPoint;
begin
  S := FromPt;
  E := ToPt;
  FExtent.x := Max(S.x, E.x) - Min(S.x, E.x);
  FExtent.y := Max(S.y, E.y) - Min(S.y, E.y);
end;

procedure TGenEntityConnection.Draw(ACanvas: TCanvas);
begin
  inherited;
  FCtrlPts[0] := FromPt;
  FCtrlPts[High(FCtrlPts)] := ToPt;
  ACanvas.PolyBezier(FCtrlPts);
end;

{ TGenEntity }

procedure TGenEntity.Invalidate;
begin
  FInValidated := True;
end;

procedure TGenEntity.Measure;
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

procedure TGenEntity.MouseClick(const Pt: TPoint; Shifts: TShiftState);
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
      and (C.ToPort.Entity = AFrom) and (C.ToPort.Index = AFromPort) do
    begin
      Result := C;
      Break;
    end;
  end;
end;

procedure TGenGraph.Layout;
begin

end;

procedure TGenGraph.FullRender;
begin
  Layout;

end;

procedure TGenGraph.PartialRender;
var
  P: Pointer;
begin
  for P in FEntities do
  begin
    with TGenEntityNode(P) do
    begin
      if Invalidated then Draw(FDBuffer.DrawBuffer);
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

procedure TGenGraph.AddConnection(AFrom, ATo: TGenEntityNode; const AFromPort,
  AToPort: Integer);
var
  C: TGenEntityConnection;
begin
  C := TGenEntityConnection.Create;
  with C.FromPort do
  begin
    Entity := AFrom;
    Index := AFromPort;
  end;
  with C.ToPort do
  begin
    Entity := ATo;
    Index := AToPort;
  end;
  FConns.Add(C);
end;

procedure TGenGraph.RemoveConnecttion(AFrom, ATo: TGenEntityNode;
  const AFromPort, AToPort: Integer);
var
  C: TGenEntityConnection;
begin
  C := Find(AFrom, ATo, AFromPort, AToPort);
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
  FDBuffer := TDoubleBuffer.Create;
  FEntities := TList.Create;
  FConns    := TList.Create;
end;

destructor TGenGraph.Destroy;
begin
  Clear;
  FDBuffer.Free;
  inherited Destroy;
end;

end.

