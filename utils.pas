unit utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, GraphType, ExtCtrls, Math;

type

  { TDoubleBuffer }

  TDoubleBuffer = class
  private
    FDrawBuffer: TBitmap;
    FPaintBox: TPaintBox;
    FPaintBuffer: TBitmap;
    procedure OnPaint(Sender: TObject);
    procedure SetPaintBox(AValue: TPaintBox);
    procedure Draw2Paint2;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Draw2Paint;
    procedure Paint;

    procedure SetSize(const W, H: Integer); virtual;
    property DrawBuffer: TBitmap read FDrawBuffer;
    property PaintBuffer: TBitmap read FPaintBuffer;
    property PaintBox: TPaintBox read FPaintBox write SetPaintBox;
  end;

  { TTripleBuffer }

  TTripleBuffer = class(TDoubleBuffer)
  private
    FBackground: TBitmap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetSize(const W, H: Integer); override;
    property Background: TBitmap read FBackground;
  end;

function AtoF(const S: string): Double;
function FindNearest(L: array of Integer; const V, Def: Integer): Integer;

// draw an icon
// drawing commands are stored in a string separating by ';'
// coordinates are scaled to (-1, -1) (bottom-left), (1, 1)(top-right)
// Commands:
// Color R,G,B            // set pen/font/brush color
// Thickness value        // line width
// Font  size             // scaled to rect height
// Line (x0,y0),(x1,y1)[,...]
// Polygon (x0,y0),(x1,y1)[,...]
// Circle (x0,y0),r
// Text (x0,y0),text
procedure StrIconDraw(ACanvas: TCanvas; ARect: TRect; Icon: string);

{
// style command
// for each line, default = ^0^n
^0 = Black
^1 = Red
^2 = Green
^3 = Yellow
^4 = Blue
^5 = Cyan (light blue)
^6 = Magenta (purple)
^7 = White
^b = +fsBold
^i = +fsItalic
^s = +fsStrikeout
^_ = +fsUnderline
^n = []
}
function  SingleLineStyledTextOut(ACanvas: TCanvas; ARect: TRect; const S: string): TRect;
procedure StyledTextOut(ACanvas: TCanvas; ARect: TRect; const Strs: TStrings); overload;


implementation

function AtoF(const S: string): Double;
var
  I, J: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] in ['0'..'9', '.'] then
    begin
      for J := I + 1 to Length(S) do
        if not (S[J] in ['0'..'9', '.']) then
        begin
          Result := StrToFloatDef(Copy(S, I, J - I), 0.0);
          Exit;
        end;
      Result := StrToFloatDef(Copy(S, I, Length(S) + 1 - I), 0.0);
      Break;
    end;
end;

function FindNearest(L: array of Integer; const V, Def: Integer): Integer;
var
  I: Integer;
  O: Integer;
begin
  Result := Def;
  if Low(L) > High(L) then Exit;
  Result := L[Low(L)];
  O := Abs(Result - V);
  for I := Low(L) + 1 to High(L) do
    if Abs(L[I] - V) < O then
    begin
      O := Abs(L[I] - V);
      Result := L[I];
    end;
end;

procedure StrIconDraw(ACanvas: TCanvas; ARect: TRect; Icon: string);
const
  DEF_COLOR = $d0d0d0;
var
  Command, Tag, Param: string;
  Pts: array of TPoint;

  procedure SkipBlank(var S: string);
  var
    I: Integer;
  begin
    for I := 1 to Length(S) do
      if S[I] <> ' ' then
      begin
        Delete(S, 1, I - 1);
        Break;
      end;
  end;

  function RequireAndDel(var S: string; const Ch: Char): Boolean;
  begin
    Result := False;
    if Length(S) < 1 then Exit;
    if S[1] = ' ' then SkipBlank(S);
    Result := (Length(S) >= 1) and (S[1] = Ch);
    if Result then Delete(S, 1, 1);
  end;

  function Scale(const V: Real; const X0, X1: Integer): Integer;
  begin
    Result := Round((X1 - X0) * (EnsureRange(V, -1, 1) + 1) / 2 + X0);
  end;

  function ExtractCommand(var S: string): string;
  var
    I: Integer;
  begin
    if S = '' then Exit;
    for I := 1 to Length(S) do
    begin
      if S[I] = ';' then
      begin
        Result := Copy(S, 1, I - 1);
        Delete(S, 1, I);
        if Result = '' then Result := ExtractCommand(S);
        Exit;
      end;
    end;
    Result := S;
    S := '';
  end;

  procedure SplitCommand(const S: string; out Tag, Param: string);
  var
    I: Integer;
  begin
    I := Pos(' ', S);
    if I > 1 then
    begin
      Tag := Copy(S, 1, I - 1);
      Param := Copy(S, I + 1, Length(S));
      SkipBlank(Param);
    end
    else
      Tag := '';
  end;

  function ReadValue(var S: string; out V: Real): Boolean;
  var
    I: Integer;
  begin
    SkipBlank(S);
    V := -1;
    Result := False;
    if Length(S) < 1 then Exit;
    for I := 1 to Length(S) do
    begin
      if not (S[I] in ['-', '0'..'9', '.']) then
      begin
        if I <= 1 then Exit;
        V := StrToFloatDef(Copy(S, 1, I - 1), -1);
        Delete(S, 1, I - 1);
        Result := True;
        Exit;
      end;
    end;
    V := StrToFloatDef(S, -1);
    S := '';
    Result := True;
  end;

  // should we use reg-exp?
  function ReadCoord(var S: string; out Pt: TPoint): Boolean;
  var
    V: Real;
  begin
    SkipBlank(S);
    Result := False;
    if not RequireAndDel(S, '(') then Exit;
    if not ReadValue(S, V) then Exit;
    Pt.x := Scale(V, ARect.Left, ARect.Right);
    if not RequireAndDel(S, ',') then Exit;
    if not ReadValue(S, V) then Exit;
    Pt.y := Scale(V, ARect.Top, ARect.Bottom);
    if not RequireAndDel(S, ')') then Exit;
    Result := True;
  end;

  procedure HandleColor(Param: string);
  var
    V: Real;
    R, G, B: Integer;
    C: TColor;
  begin
    if not ReadValue(Param, V) then Exit;
    R := Round(V);
    if not RequireAndDel(Param, ',') then Exit;
    if not ReadValue(Param, V) then Exit;
    G := Round(V);
    if not RequireAndDel(Param, ',') then Exit;
    if not ReadValue(Param, V) then Exit;
    B := Round(V);
    C := RGBToColor(EnsureRange(R, 0, 255), EnsureRange(G, 0, 255), EnsureRange(B, 0, 255));
    with ACanvas do
    begin
      Font.Color := C;
      Pen.Color := C;
      Brush.Color := C;
    end;
  end;

  procedure HandleFont(Param: string);
  var
    V: Real;
  begin
    if not ReadValue(Param, V) then Exit;
    ACanvas.Font.Size := Max(8, Round(V * (ARect.Bottom - ARect.Top) / 2));
  end;

  procedure HandleThickness(Param: string);
  var
    V: Real;
  begin
    if not ReadValue(Param, V) then Exit;
    ACanvas.Pen.Width := Max(1, Round(V * (ARect.Bottom - ARect.Top) / 2));
  end;

  procedure ReadCoords(Param: string);
  var
    I: Integer = 0;
    Pt: TPoint;
  begin
    SetLength(Pts, 0);
    while ReadCoord(Param, Pt) do
    begin
      if I > High(Pts) then SetLength(Pts, I + 10);
      Pts[I] := Pt;
      Inc(I);
      if not RequireAndDel(Param, ',') then Break;
    end;
    SetLength(Pts, I);
  end;

  procedure HandleLine(Param: string);
  var
    V: Real;
  begin
    ReadCoords(Param);
    ACanvas.Polyline(Pts);
  end;

  procedure HandlePolygon(Param: string);
  var
    V: Real;
    Pt: TPoint = (x: 0; y: 0);
    I: Integer;
  begin
    ReadCoords(Param);
    if High(Pts) < 2 then Exit;
    ACanvas.Polygon(Pts);
  end;

  procedure HandleCircle(Param: string; const bFill: Boolean);
  var
    V: Real;
    R0, R1: Integer;
    Pt: TPoint = (x: 0; y: 0);
    I: Integer;
  begin
    if not ReadCoord(Param, Pt) then Exit;
    if not RequireAndDel(Param, ',') then Exit;
    if not ReadValue(Param, V) then Exit;
    R0 := Scale(V, 0, (ARect.Right - ARect.Left) div 2);
    R1 := Scale(V, 0, (ARect.Bottom - ARect.Top) div 2);
    if bFill then
      ACanvas.EllipseC(Pt.x, Pt.y, R0, R1)
    else
      ACanvas.Arc(Pt.x - R0, Pt.y - R1, Pt.x + R0, Pt.y + R1, 0, 360 * 16);
  end;

  procedure HandleText(Param: string);
  var
    V: Real;
    Pt: TPoint = (x: 0; y: 0);
    E: TSize;
  begin
    if not ReadCoord(Param, Pt) then Exit;
    if not RequireAndDel(Param, ',') then Exit;
    E := ACanvas.TextExtent(Param);
    Pt.x := Pt.x - E.cx div 2;
    Pt.y := Pt.y - E.cy div 2;
    ACanvas.TextRect(ARect, Pt.x, Pt.y, Param);
  end;

begin
  HandleColor('220, 220, 220');
  HandleThickness('0.1');
  HandleFont('0.5');

  while True do
  begin
    Command := ExtractCommand(Icon);
    if Command = '' then Break;
    SplitCommand(Command, Tag, Param);
    case LowerCase(Tag) of
      'color': HandleColor(Param);
      'thickness': HandleThickness(Param);
      'font': HandleFont(Param);
      'line': HandleLine(Param);
      'polygon': HandlePolygon(Param);
      'circle': HandleCircle(Param, False);
      'disk': HandleCircle(Param, True);
      'text': HandleText(Param);
    end;
  end;
end;

function SingleLineStyledTextOut(ACanvas: TCanvas; ARect: TRect; const S: string
  ): TRect;
const
  COLORS: array [0..7] of TColor = (clBlack, clRed, clGreen, clYellow, clBlue,
                                    $EBB700, $9000FF, clWhite);
begin

end;

procedure StyledTextOut(ACanvas: TCanvas; ARect: TRect; const Strs: TStrings);
begin

end;

{ TTripleBuffer }

constructor TTripleBuffer.Create;
begin
  inherited;
  FBackground := TBitmap.Create;
  with FBackground do
  begin
    Canvas.Font.Quality := fqCleartype;
  end;
end;

destructor TTripleBuffer.Destroy;
begin
  FBackground.Free;
  inherited Destroy;
end;

procedure TTripleBuffer.SetSize(const W, H: Integer);
begin
  FBackground.SetSize(W, H);
  inherited;
end;

{ TDoubleBuffer }

procedure TDoubleBuffer.OnPaint(Sender: TObject);
begin
  FPaintBox.Canvas.Draw(0, 0, FPaintBuffer);
end;

procedure TDoubleBuffer.SetPaintBox(AValue: TPaintBox);
begin
  if FPaintBox = AValue then Exit;
  FPaintBox := AValue;
  FPaintBox.OnPaint := @OnPaint;
end;

procedure TDoubleBuffer.Draw2Paint2;
begin
  //FPaintBox.Canvas.Draw(0, 0, FPaintBuffer);
  FPaintBox.Refresh;
end;

constructor TDoubleBuffer.Create;
begin
  FDrawBuffer := TBitmap.Create;
  FPaintBuffer := TBitmap.Create;
  with FDrawBuffer do
  begin
    Canvas.Font.Quality := fqCleartype;
  end;
  with FPaintBuffer do
  begin
    Canvas.Font.Quality := fqCleartype;
  end;
end;

destructor TDoubleBuffer.Destroy;
begin
  FDrawBuffer.Free;
  FPaintBuffer.Free;
end;

procedure TDoubleBuffer.Draw2Paint;
begin
  FPaintBuffer.Canvas.Draw(0, 0, FDrawBuffer);
end;

procedure TDoubleBuffer.Paint;
begin
  TThread.Synchronize(nil, @Draw2Paint2);
end;

procedure TDoubleBuffer.SetSize(const W, H: Integer);
begin
  FDrawBuffer.SetSize(W, H);
  FPaintBuffer.SetSize(W, H);
end;

end.

