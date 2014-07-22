unit utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls;

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

{ TTripleBuffer }

constructor TTripleBuffer.Create;
begin
  inherited;
  FBackground := TBitmap.Create;
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

