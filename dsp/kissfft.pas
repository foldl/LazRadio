unit KissFFT;

// Port KissFFT to Pascal

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex;

const
  MAX_FACTORS = 32;

type

  TFFTState = record
    N: Integer;
    NRep: Double;
    Inverse: Boolean;
    Factors: array [0..2 * MAX_FACTORS - 1] of Integer;
    Twiddles: array of Complex;
  end;

  PFFTPlan = ^TFFTState;

function BuildFFTPlan(const N: Integer;
                      const Inverse: Boolean): PFFTPlan;
procedure ChangePlan(Plan: PFFTPlan; const N: Integer;
                      const Inverse: Boolean);
procedure FinalizePlan(P: PFFTPlan);

procedure FFT(Plan: PFFTPlan;
              Input: PComplex;
              Output: PComplex); overload;

procedure FFT(Plan: PFFTPlan;
              Input: PComplex;
              Output: PComplex;
              const Stride: Integer);

function NextFastSize(N: Integer): Integer;

function ToString(Input: PComplex; const N: Integer): string;

implementation

uses
  Math;

procedure Butterfly2(Out1: PComplex; const Stride: Integer; var State: TFFTState;
                     const M: Integer);
var
  Out2: PComplex;
  Tw: PComplex;
  T: Complex;
  K: Integer;
begin
  Tw := @State.Twiddles[0];
  Out2 := Out1 + M;
  for K := 1 to M do
  begin
    T := Out2^ * Tw^;
    Inc(Tw, Stride);
    Out2^ := Out1^ - T;
    Out1^ := Out1^ + T;
    Inc(Out1);
    Inc(Out2);
  end;
end;

procedure Butterfly4(Out1: PComplex; const Stride: Integer; var State: TFFTState;
                     const M: Integer);
var
  Tw1, Tw2, Tw3: PComplex;
  Scratch: array [0..5] of Complex;
  K: Integer;
  M2, M3: Integer;
begin
  Tw1 := @State.Twiddles[0];
  Tw2 := @State.Twiddles[0];
  Tw3 := @State.Twiddles[0];
  M2  := 2 * M;
  M3  := 3 * M;
  for K := 1 to M do
  begin
    Scratch[0] := Out1[M ] * Tw1^;
    Scratch[1] := Out1[M2] * Tw2^;
    Scratch[2] := Out1[M3] * Tw3^;
    Scratch[5] := Out1^ - Scratch[1];
    Out1^ := Out1^ + Scratch[1];
    Scratch[3] := Scratch[0] + Scratch[2];
    Scratch[4] := Scratch[0] - Scratch[2];

    Out1[M2] := Out1^ - Scratch[3];
    Out1^    := Out1^ + Scratch[3];

    Inc(Tw1, Stride);
    Inc(Tw2, Stride * 2);
    Inc(Tw3, Stride * 3);

    if State.Inverse then
    begin
      with Out1[M] do
      begin
        re := Scratch[5].re - Scratch[4].im;
        im := Scratch[5].im + Scratch[4].re;
      end;
      with Out1[M3] do
      begin
        re := Scratch[5].re + Scratch[4].im;
        im := Scratch[5].im - Scratch[4].re;
      end;
    end
    else begin
      with Out1[M] do
      begin
        re := Scratch[5].re + Scratch[4].im;
        im := Scratch[5].im - Scratch[4].re;
      end;
      with Out1[M3] do
      begin
        re := Scratch[5].re - Scratch[4].im;
        im := Scratch[5].im + Scratch[4].re;
      end;
    end;
    Inc(Out1);
  end;
end;

procedure Butterfly3(Out1: PComplex; const Stride: Integer; var State: TFFTState;
                     const M: Integer);
var
  Tw1, Tw2: PComplex;
  Scratch: array [0..3] of Complex;
  K: Integer;
  M2: Integer;
  Epi3: Complex;
begin
  Tw1 := @State.Twiddles[0];
  Tw2 := @State.Twiddles[0];
  M2  := 2 * M;
  Epi3 := State.Twiddles[m * Stride];

  for K := 1 to M do
  begin
    Scratch[1] := Out1[M ] * Tw1^;
    Scratch[2] := Out1[M2] * Tw2^;
    Scratch[3] := Scratch[1] + Scratch[2];
    Scratch[0] := Scratch[1] - Scratch[2];

    Inc(Tw1, Stride);
    Inc(Tw2, Stride * 2);

    with Out1[M] do
    begin
      re := Out1^.re - Scratch[3].re / 2;
      im := Out1^.im - Scratch[3].im / 2;
    end;

    Scratch[0] := Scratch[0] * Epi3.im;
    Out1^ := Out1^ + Scratch[3];

    with Out1[M2] do
    begin
      re := Out1[M].re + Scratch[0].im;
      im := Out1[M].im - Scratch[0].re;
    end;
    with Out1[M] do
    begin
      re := re - Scratch[0].im;
      im := im + Scratch[0].re;
    end;

    Inc(Out1);
  end;
end;

procedure Butterfly5(Output: PComplex; const Stride: Integer; var State: TFFTState;
                     const M: Integer);
var
  Twiddles: PComplex;
  Tw: PComplex;
  Scratch: array [0..12] of Complex;
  K: Integer;
  Out0, Out1, Out2, Out3, Out4: PComplex;
  Ya, Yb: Complex;
begin
  Twiddles := @State.Twiddles[0];
  Ya := Twiddles[M * Stride];
  Yb := Twiddles[2 * M * Stride];
  Out0 := Output;
  Out1 := Output + M;
  Out2 := Output + M * 2;
  Out3 := Output + M * 3;
  Out4 := Output + M * 4;

  Tw := Twiddles;
  for K := 0 to M - 1 do
  begin
    Scratch[0] := Out0[0];

    Scratch[1] := Out1[0] * Tw[K * Stride];
    Scratch[2] := Out2[0] * Tw[2 * K * Stride];
    Scratch[3] := Out3[0] * Tw[3 * K * Stride];
    Scratch[4] := Out4[0] * Tw[4 * K * Stride];

    Scratch[7 ] := Scratch[1] + Scratch[4];
    Scratch[10] := Scratch[1] - Scratch[4];
    Scratch[8]  := Scratch[2] + Scratch[3];
    Scratch[9]  := Scratch[2] - Scratch[3];

    Out0[0].re := Out0[0].re + Scratch[7].re + Scratch[8].re;
    Out0[0].im := Out0[0].im + Scratch[7].im + Scratch[8].im;

    Scratch[5].re := Scratch[0].re + Scratch[7].re * Ya.re + Scratch[8].re * Yb.re;
    Scratch[5].im := Scratch[0].im + Scratch[7].im * Ya.re + Scratch[8].im * Yb.re;

    Scratch[6].re :=  Scratch[10].im * Ya.im + Scratch[9].im * Yb.im;
    Scratch[6].im := -Scratch[10].re * Ya.im - Scratch[9].re * Yb.im;

    Out1[0] := Scratch[5] - Scratch[6];
    Out4[0] := Scratch[5] + Scratch[6];

    Scratch[11].re := Scratch[0].re + Scratch[7].re * Yb.re + Scratch[8].re * Ya.re;
    Scratch[11].im := Scratch[0].im + Scratch[7].im * Yb.re + Scratch[8].im * Ya.re;

    Scratch[12].re := -Scratch[10].im * Yb.im + Scratch[9].im * Ya.im;
    Scratch[12].im :=  Scratch[10].re * Yb.im - Scratch[9].re * Ya.im;

    Out2[0] := Scratch[11] + Scratch[12];
    Out3[0] := Scratch[11] - Scratch[12];

    Inc(Out0); Inc(Out1); Inc(Out2); Inc(Out3); Inc(Out4);
  end;
end;

procedure ButterflyGeneric(Output: PComplex; const Stride: Integer; var State: TFFTState;
                     M, P: Integer);
var
  U, K, Q, I, J: Integer;
  Twiddles: PComplex;
  Scratch: PComplex;
  T: Complex;
begin
  Scratch := PComplex(GetMem(SizeOf(Complex) * P));
  Twiddles := @State.Twiddles[0];

  for U := 0 to M - 1 do
  begin
    K := U;
    for Q := 0 to P - 1 do
    begin
      Scratch[Q] := Output[K];
      Inc(K, M);
    end;

    K := U;
    for Q := 0 to P - 1 do
    begin
      I := 0;
      T := Scratch[0];
      Output[K] := T;
      for J := 1 to P - 1 do
      begin
        Inc(I, K * Stride);
        if I >= State.N then Dec(I, State.N);
        T := T + Scratch[J] * Twiddles[I];
      end;
      Output[K] := T;
      Inc(K, M);
    end;
  end;

  FreeMem(Scratch);
end;

procedure DoFFT(Output: PComplex; Input: PComplex; const Stride: Integer;
                InStride: Integer; Factors: PInteger; var State: TFFTState);
var
  O, E: PComplex;
  P, M: Integer;
begin
  O := Output;
  P := Factors[0]; // radix
  M := Factors[1]; // stage's fft length/p
  E := O + P * M;
  Inc(Factors, 2);

  if M = 1 then
  begin
    repeat
      O^ := Input^;
      Inc(Input, Stride * InStride);
      Inc(O);
    until O = E;
  end
  else begin
    repeat
      // recursive call:
      // DFT of size m*p performed by doing
      // p instances of smaller DFTs of size m,
      // each one takes a decimated version of the input
      DoFFT(O, Input, Stride * P, InStride, Factors, State);
      Inc(Input, Stride * InStride);
      Inc(O, M);
    until O = E;
  end;

  O := Output;
  case P of
    2: Butterfly2(O, Stride, State, M);
    3: Butterfly3(O, Stride, State, M);
    4: Butterfly4(O, Stride, State, M);
    5: Butterfly5(O, Stride, State, M);
    else
      ButterflyGeneric(O, Stride, State, M, P);
  end;
end;

procedure Factor(N: Integer; FacBuf: PInteger);
var
  P: Integer = 4;
  FlSqrt: Integer;
begin
  FlSqrt := Trunc(Sqrt(N));
  repeat
    while N mod P > 0 do
    begin
      case P of
        4: P := 2;
        2: P := 3;
        else
           Inc(P, 2);
      end;
      if P > FlSqrt then P := N;
    end;
    N := N div P;
    FacBuf^ := P; Inc(FacBuf);
    FacBuf^ := N; Inc(FacBuf);
  until N <= 1;
end;

function BuildFFTPlan(const N: Integer; const Inverse: Boolean): PFFTPlan;
begin
  New(Result);
  ChangePlan(Result, N, Inverse);
end;

procedure ChangePlan(Plan: PFFTPlan; const N: Integer; const Inverse: Boolean);
var
  I: Integer;
  P: Complex = (re: 0; im: 0);
begin
  Plan^.N := N;
  Plan^.Inverse := Inverse;
  Plan^.NRep := 1 / N;
  Factor(N, Plan^.Factors);
  SetLength(Plan^.Twiddles, N);
  for I := 0 to N - 1 do
  begin
    P.im := -2 * Pi * I / N;
    if Inverse then P.im := -P.im;
    Plan^.Twiddles[I] := cexp(p);
  end;
end;

procedure FinalizePlan(P: PFFTPlan);
begin
  SetLength(P^.Twiddles, 0);
  Dispose(P);
end;

procedure FFT(Plan: PFFTPlan; Input: PComplex; Output: PComplex);
begin
  FFT(Plan, Input, Output, 1);
end;

procedure FFT(Plan: PFFTPlan; Input: PComplex; Output: PComplex;
  const Stride: Integer);
var
  T: PComplex;
  I: Integer;
begin
  if Input = Output then
  begin
    T := GetMem(Plan^.N * SizeOf(Complex));
    DoFFT(T, Input, 1, Stride, Plan^.Factors, Plan^);
    Move(T^, Input^, Plan^.N * SizeOf(Complex));
    FreeMem(T);
  end
  else
    DoFFT(Output, Input, 1, Stride, Plan^.Factors, Plan^);
  if Plan^.Inverse then
  begin
    for I := 0 to Plan^.N - 1 do
      Output[I] := Output[I] * Plan^.NRep;
  end;
end;

function NextFastSize(N: Integer): Integer;
var
  M: Integer;
begin
  while True do
  begin
    M := N;
    while M mod 2 = 0 do M := M div 2;
    while M mod 3 = 0 do M := M div 3;
    while M mod 5 = 0 do M := M div 5;
    if M <= 1 then Break;
    Inc(N);
  end;
  Result := N;
end;

function ToString(Input: PComplex; const N: Integer): string;
var
  I: Integer;
begin
  if N > 0 then
  begin
    Result := '[' + cstr(Input[0]);
    for I := 1 to N - 1 do
      Result := Result + ',' + cstr(Input[I]);
    Result := StringReplace(Result + ']', 'i', 'j', [rfReplaceAll]);
  end
  else begin
    Result := '[]';
  end;
end;

end.

