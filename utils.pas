unit utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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



end.

