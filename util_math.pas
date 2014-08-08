unit util_math;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;


function AtoF(const S: string): Double;
function FindNearest(L: array of Integer; const V, Def: Integer): Integer;

function  IsPtInRect(const APt: TPoint; const ARect: TRect): Boolean;

function  GCD(X, Y: Cardinal): Cardinal;

function  DBtoMultiplier(const Db: Double): Integer;

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

function IsPtInRect(const APt: TPoint; const ARect: TRect): Boolean;
begin
  Result := InRange(APt.x, ARect.Left, ARect.Right)
         and InRange(APt.y, ARect.Top, ARect.Bottom);
end;

function GCD(X, Y: Cardinal): Cardinal;
begin
  Result := 0;
  if (X = 0) or (Y = 0) then Exit;
  Result := X mod Y;
  while True do
  begin
    if Result = 0 then
    begin
      Result := Y;
      Break;
    end;

    X := Y;
    Y := Result;
    Result := X mod Y;
  end;
end;

function DBtoMultiplier(const Db: Double): Integer;
begin
  Result := Round(Power(10, Db / 10));
end;

end.

