unit MathLUT;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFloat = Single;

function Sin(X: TFloat): TFloat;
function Cos(X: TFloat): TFloat;

implementation

const
  TAB_SIZE = 1000;
  PI2 = 2 * Pi;

var
  SinTable: array [0..TAB_SIZE] of TFloat;
  CosTable: array [0..TAB_SIZE] of TFloat;

function Sin(X: TFloat): TFloat;
var
  I: Integer;
begin
  I := Trunc(X / Pi2);
  if X > 0 then
    X := X - Pi2 * I
  else
    X := X + Pi2 * I;
  if X <= Pi / 2 then
    Result := SinTable[Round(X / (Pi / 2) * TAB_SIZE)]
  else if X <= Pi then
    Result := SinTable[Round((Pi - X) / (Pi / 2) * TAB_SIZE)]
  else if X <= 3 * Pi / 2 then
    Result := -SinTable[Round((X - Pi) / (Pi / 2) * TAB_SIZE)]
  else
    Result := -SinTable[Round((Pi2 - X) / (Pi / 2) * TAB_SIZE)];
end;

function Cos(X: TFloat): TFloat;
var
  I: Integer;
begin
  I := Trunc(X / Pi2);
  if X > 0 then
    X := X - Pi2 * I
  else
    X := X + Pi2 * I;
  if X <= Pi / 2 then
    Result := CosTable[Round(X / (Pi / 2) * TAB_SIZE)]
  else if X <= Pi then
    Result := -CosTable[Round((Pi - X) / (Pi / 2) * TAB_SIZE)]
  else if X <= 3 * Pi / 2 then
    Result := -CosTable[Round((X - Pi) / (Pi / 2) * TAB_SIZE)]
  else
    Result := CosTable[Round((Pi2 - X) / (Pi / 2) * TAB_SIZE)];
end;

procedure InitTables;
var
  I: Integer;
  R: Double;
begin
  for I := 0 to High(SinTable) do
  begin
    R := I / TAB_SIZE * Pi / 2;
    SinTable[I] := system.Sin(R);
    CosTable[I] := system.Cos(R);
  end;
end;

initialization

InitTables;

end.

