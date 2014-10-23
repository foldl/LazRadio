unit FFT2;

{$mode objfpc}{$H+}
{$O+}

// Here, let's do FFT again, use Float instead of Double, and only support 2^n

interface

uses
  Classes, SysUtils, UComplex, Math;

type

  TSComplex = record
    Re: Single;
    Im: Single;
  end;
  PFComplex = ^TSComplex;

  TFFTState = record
    N: Integer;
    Pow2: Integer;
    NRep: Double;
    Inverse: Boolean;
    Twiddles: array of TSComplex;
    I: array of TSComplex;
  end;

  PFFTPlan = ^TFFTState;

function BuildFFTPlan(const N: Integer;
                      const Inverse: Boolean): PFFTPlan;
procedure ChangePlan(Plan: PFFTPlan; const N: Integer;
                      const Inverse: Boolean);
procedure FinalizePlan(P: PFFTPlan);

procedure FFT(Plan: PFFTPlan;
              Input: PComplex;
              Output: PComplex);

function NextFastSize(N: Integer): Integer;

implementation

operator + (z1 : TSComplex; z2 :TSComplex) z : TSComplex; inline;
begin
   with z do
   begin
     re := z1.Re + z2.Re;
     im := z1.im + z2.im;
   end;
end;

operator - (z1 : TSComplex; z2 :TSComplex) z : TSComplex; inline;
begin
   with z do
   begin
     re := z1.Re - z2.Re;
     im := z1.im - z2.im;
   end;
end;

operator * (z1 : TSComplex; z2 :TSComplex) z : TSComplex; inline;
begin
  z.re := (z1.re * z2.re) - (z1.im * z2.im);
  z.im := (z1.re * z2.im) + (z1.im * z2.re);
end;

operator := (z1 : Complex) z : TSComplex; inline;
begin
  z.re := z1.re;
  z.im := z1.im;
end;

operator := (z1 : TSComplex) z : Complex; inline;
begin
  z.re := z1.re;
  z.im := z1.im;
end;

function GetMinPow2(const N: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 1 to 30 do
  begin
    if N <= (1 shl I) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function BuildFFTPlan(const N: Integer; const Inverse: Boolean): PFFTPlan;
begin
  New(Result);
  ChangePlan(Result, N, Inverse);
end;

procedure ChangePlan(Plan: PFFTPlan; const N: Integer; const Inverse: Boolean);
var
  I: Integer;
  P: TSComplex = (re: 0; im: 0);
begin
  Plan^.N := N;
  Plan^.Pow2 := GetMinPow2(N);
  if 1 shl Plan^.Pow2 <> N then
    raise Exception.Create('bad FFT size');
  Plan^.Inverse := Inverse;
  Plan^.NRep := 1 / N;
  SetLength(Plan^.Twiddles, N);
  SetLength(Plan^.I, N);
  for I := 0 to N - 1 do
  begin
    P.im := -2 * Pi * I / N;
    if Inverse then P.im := -P.im;
    Plan^.Twiddles[I].Re := Cos(P.im);
    Plan^.Twiddles[I].Im := Sin(P.im);
  end;
end;

procedure FinalizePlan(P: PFFTPlan);
begin
  SetLength(P^.Twiddles, 0);
  SetLength(P^.I, 0);
  Dispose(P);
end;

procedure FFT0(Plan: PFFTPlan);
var
  nm1, nd2: Integer;
  i, j, jm1, k, l, m, n, le, le2, ip, nd4: Integer;
  W, T: TSComplex;
  B: array of TSComplex;
begin
  B := Plan^.I;
  nm1 := Plan^.N - 1;
  nd2 := Plan^.N div 2;
  M := Plan^.Pow2;
  I := Plan^.N;
  J := nd2;
   {
  // bit reverse index
  for I := 1 to nm1 - 1 do
  begin
    if I < J then
    begin
      T := B[J];
      B[J] := B[I];
      B[I] := T;
    end;

    K := nd2;

    while K <= J do
    begin
      Dec(J, K);
      K := K div 2;
    end;

    Inc(J, K);
  end;
    }
  for L := 1 to M do
  begin
    le := 1 shl L;
    le2 := le div 2;

    N := M - l;

    for J := 1 to le2 do
    begin
      jm1 := j - 1;
      W := Plan^.Twiddles[jm1 shl N];

      I := jm1;
      while I <= nm1 do
      begin
        ip := I + le2;
        T := W * B[ip];
        B[ip] := B[I] - T;
        B[I] := B[I] + T;
        Inc(I, le);
      end;
    end;
  end;
  exit;
  nd4 := nd2 div 2;
  for I := 0 to nd4 - 1 do
  begin
    T := B[I];
    B[I] := B[nd2 - I - 1];
    B[nd2 - I - 1] := T;

    T := B[nd2 + I];
    B[nd2 + I] := B[nd2 + nd2 - I - 1];
    B[nd2 + nd2 - I - 1] := T;
  end;
end;

procedure FFT(Plan: PFFTPlan; Input: PComplex; Output: PComplex);
var
  I: Integer;
begin
  for I := 0 to Plan^.N - 1 do
    Plan^.I[I] := Input[I];
  FFT0(Plan);
  if Plan^.Inverse then
  begin
    for I := 0 to Plan^.N - 1 do
      Output[I] := Plan^.I[I] * Plan^.NRep;
  end
  else
    for I := 0 to Plan^.N -1 do
      Output[I] := Plan^.I[I];
end;

function NextFastSize(N: Integer): Integer;
begin
  Result := GetMinPow2(N);
  if Result >= 1 then
    Result := 1 shl Result
  else
    raise Exception.Create('FFT size too large!');
end;

end.

