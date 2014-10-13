unit util_math;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

procedure HSL2RGB(Hue, Saturation, Lightness: Double; out R, G, B: Byte);

procedure BeatifulTick(const ScreenWidth, MinTickWidth: Integer; const V0, V1: Double; out Start, Step: Double);

function AtoF(const S: string): Double;
function AtoI(const S: string): Int64;
function HexToInt(HexStr: string): Int64;

function FindNearest(L: array of Integer; const V, Def: Integer): Integer;

function  IsPtInRect(const APt: TPoint; const ARect: TRect): Boolean;

function  GCD(X, Y: Cardinal): Cardinal;

function  DBtoMultiplier(const Db: Double): Integer;

function FloatAsInt(const F: Single): Integer;
function IntAsFloat(const N: Integer): Float;

procedure QuickSort(AList: PPointer; const N: Integer; Compare: TListSortCompare);

implementation

procedure QuickSort0(FList: PPointer; L, R : Longint;
                     Compare: TListSortCompare);
var
  I, J : Longint;
  P, Q : Pointer;
begin
 repeat
   I := L;
   J := R;
   P := FList[ (L + R) div 2 ];
   repeat
     while Compare(P, FList[i]) > 0 do
       I := I + 1;
     while Compare(P, FList[J]) < 0 do
       J := J - 1;
     If I <= J then
     begin
       Q := FList[I];
       Flist[I] := FList[J];
       FList[J] := Q;
       I := I + 1;
       J := J - 1;
     end;
   until I > J;
   // sort the smaller range recursively
   // sort the bigger range via the loop
   // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
   if J - L < R - I then
   begin
     if L < J then
       QuickSort0(FList, L, J, Compare);
     L := I;
   end
   else
   begin
     if I < R then
       QuickSort0(FList, I, R, Compare);
     R := J;
   end;
 until L >= R;
end;

procedure QuickSort(AList: PPointer; const N: Integer; Compare: TListSortCompare);
begin
  QuickSort0(AList, 0, N - 1, Compare);
end;

procedure HSL2RGB(Hue, Saturation, Lightness: Double; out R, G, B: Byte);
var
  P, Q: Double;

  function hue2rgb(P, Q, T: Double): Double;
  begin
    if T < 0 then T := T + 1;
    if T > 1 then T := T - 1;
    if T < 1/6 then Result := P + (Q - P) * 6 * T
    else if T < 1/2 then Result := Q
    else if T < 2/3 then Result := P + (Q - P) * (2/3 - T) * 6
    else Result := P;
  end;
begin
  if Saturation = 0 then
  begin
    R := 255; G := 255; B := 255;
    Exit;
  end;
  q := IfThen(Lightness < 0.5, Lightness * (1 + Saturation), Lightness + Saturation - Lightness * Saturation);
  p := 2 * Lightness - q;
  R := Trunc(255 * hue2rgb(p, q, Hue + 1/3));
  G := Trunc(255 * hue2rgb(p, q, Hue));
  B := Trunc(255 * hue2rgb(p, q, Hue - 1/3));
end;

procedure BeatifulTick(const ScreenWidth, MinTickWidth: Integer; const V0, V1: Double; out Start, Step: Double);
var
  I: Integer;

  function Simplify(V: Double): Double;
  var
    S: string;
    K: Integer;
    T: Integer = 0;
    procedure ClearS(const Start: Integer);
    var
      L: Integer;
    begin
      for L := Start to Length(S) do
        if S[L] in ['0'..'9'] then S[L] := '0';
    end;

  begin
    S := FloatToStr(V);
    for K := 1 to Length(S) do
    begin
      if S[K] in ['1'..'9'] then
      begin
        if I < Length(S) then
          if StrToIntDef(S[K + 1], 0) >= 5 then T := 1;
        Inc(T, StrToInt(S[K]));
        case T of
          8, 9:
            T := 10;
          4, 6, 7:
            T := 5;
          3:
            T := 2;
        end;
        if T < 10 then
        begin
          S[K] := IntToStr(T)[1];
          ClearS(K + 1);
        end
        else begin
          if K > 1 then
          begin
            S[K - 1] := '1';
            ClearS(K);
          end
          else begin
            ClearS(1);
            S := '1' + S;
          end;
        end;
      end;
    end;
    Result := StrToFloat(S);
  end;

begin
  I := Trunc(ScreenWidth / MinTickWidth);
  Start := V0;
  Step := V1 - V0;
  if I <= 0 then Exit;
  if Step <= 0 then Exit;

  Step := Simplify((V1 - V0) / I);
  I := Round(V0 / Step);
  Start := I * Step;
  if Start > V0 then Start := Start - Step;
end;

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

function AtoI(const S: string): Int64;
var
  T: string;
begin
  T := S;
  Result := 0;
  if T = '' then
    Exit;
  if T[1] = '$' then
  begin
    Delete(T, 1, 1);
    Result := HexToInt(T);
  end
  else if (Length(T) > 2) and (T[1] in ['X', 'x']) then
  begin
    Delete(T, 1, 2);
    Result := HexToInt(T);
  end
  else
    Result := StrToInt64Def(T, 0);
end;

function HexToInt(HexStr: string): Int64;
var
  I: integer;
begin
  HexStr := UpperCase(HexStr);
  Result := 0;

  for i := 1 to length(HexStr) do
  begin
    Result := Result shl 4;
    if HexStr[i] in ['0'..'9'] then
      Result := Result + DWord(byte(HexStr[i]) - 48)
    else
    if HexStr[i] in ['A'..'F'] then
      Result := Result + DWord(byte(HexStr[i]) - 55)
    else
    begin
      Break;
    end;
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
  Result := Round(Power(10, Db / 20));
end;

function FloatAsInt(const F: Single): Integer;
begin
  Result := PInteger(@F)^;
end;

function IntAsFloat(const N: Integer): Float;
begin
  Result := PSingle(@N)^;
end;

end.

