unit SignalBasic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex;

type

  TWindowFunction = (wfRect, wfGaussian, wfHamming, wfHann, wfTriangular, wfBartlett,
                     wfBlackman, wfKaiser, wfBartlettHann);

  TFilterType = (ftLPF, ftBPF, ftBSF, ftHPF);

// Output.re = amplitude
// Output.im = arg
procedure ModArg(Input: PComplex; Output: PComplex; const N: Integer); overload;
procedure ModArg(IO: PComplex; const N: Integer);

// Output.re = power
// Output.im = arg
procedure PowArg(Input: PComplex; Output: PComplex; const N: Integer); overload;
procedure PowArg(IO: PComplex; const N: Integer);

procedure CreateWindowFunction(P: PDouble; const N: Integer; const Func: TWindowFunction;
  Param: Double = -1);

function BesselI0(const Z: Double): Double;

procedure Xpolate(Source: PDouble; Target: PDouble; const SourceLen, TargetLen: Integer);

procedure CancelDC(Signal: PComplex; const N: Integer);

function FormatFreq(F: Integer): string;

procedure FIRDesign(Coef: PDouble; const N: Integer;
  const AType: TFilterType;
  const OmegaC: Double; const Bandwidth: Double;
  const Wf: TWindowFunction; const WfParam: Double);

var
  gWindowFunctionNames: array [TWindowFunction] of string =
    ('Rect', 'Gaussian', 'Hamming', 'Hann', 'Triangular', 'Bartlett', 'Blackman', 'Kaiser', 'BartlettHann');

implementation

uses
  Math;

function Sinc(const X: Double): Double;
begin
  if Abs(X) > 1e-10 then
    Result := Sin(X) / X
  else
    Result := 1.0;
end;

// naive, not applicable for real-time
function BesselI0(const Z: Double): Double;
var
  A, B, T: Double;
  I: Integer = 1;
begin
  A := 1;
  B := 1;
  T := 1;
  Result := 1;
  repeat
    A := A * Z * Z / 4;
    B := B * I;
    T := A / (B * B);
    Result := Result + T;
    Inc(I);
  until T < 1e-8;
end;

procedure Xpolate(Source: PDouble; Target: PDouble; const SourceLen,
  TargetLen: Integer);
  procedure Smooth(Source: PDouble; Target: PDouble; const SourceLen,
    TargetLen: Integer);
  var
    Ratio: Double;
    P: Double = 0;
    I: Integer;
    K: Integer;
    L: Integer = 0;
    J: Integer;
    T: Double;
  begin
    Ratio := SourceLen / TargetLen;
    for I := 0 to TargetLen - 1 do
    begin
      K := Trunc(P);
      T := Source[L];
      for J := L + 1 to K do
        T := T + Source[J];
      Target[I] := T / (K - L + 1);
      L := K + 1;
      P := P + Ratio;
    end;
  end;

  procedure Interpolate(Source: PDouble; Target: PDouble; const SourceLen,
    TargetLen: Integer);
  var
    Ratio: Double;
    P: Double = 0;
    I: Integer;
    K: Integer;
  begin
    Ratio := SourceLen / TargetLen;
    for I := 0 to TargetLen - 1 do
    begin
      K := Trunc(P);
      Target[I] := Source[K] + (P - K) * (Source[K + 1] - Source[K]);
      P := P + Ratio;
    end;
  end;
begin
  if (SourceLen < 1) or (TargetLen < 1) then Exit;
  if SourceLen = TargetLen then
  begin
    Move(Source^, Target^, SourceLen * SizeOf(Source^));
    Exit;
  end;
  if SourceLen > TargetLen then
    Smooth(Source, Target, SourceLen, TargetLen)
  else
    Interpolate(Source, Target, SourceLen, TargetLen);
end;

procedure CancelDC(Signal: PComplex; const N: Integer);
var
  T: Complex = (re: 0; im: 0);
  I: Integer;
begin
  for I := 0 to N - 1 do
    T := T + Signal[I];
  T := T / N;
  for I := 0 to N - 1 do
    Signal[I] := Signal[I] - T;
end;

function FormatFreq(F: Integer): string;
const
  U: array [1..3] of string = ('K', 'M', 'G');
var
  I: Integer;
  C: Integer = 0;
begin
  Result := IntToStr(F);
  I := Length(Result);
  while Result[I] = '0' do
  begin
    Inc(C);
    Dec(I);
  end;
  I := Round(C / 3);
  if I > 0 then
  begin
    I := Min(I, High(U));
    Result := FloatToStr(F / power(1000, I)) + U[I];
  end;
end;

procedure FIRDesign(Coef: PDouble; const N: Integer; const AType: TFilterType;
  const OmegaC: Double; const Bandwidth: Double; const Wf: TWindowFunction;
  const WfParam: Double);
var
  J: Integer;
  W: array of Double;
  BL, BH: Double;
  function lpf(const I: Integer): Double;
  var
    Arg: Double;
  begin
    Arg := I - (N - 1) / 2;
    Result := OmegaC * Sinc(OmegaC * Arg * Pi);
  end;
  function hpf(const I: Integer): Double;
  var
    Arg: Double;
  begin
    Arg := I - (N - 1) / 2;
    Result := Sinc(Arg * Pi) - OmegaC * Sinc(OmegaC * Arg * Pi);
  end;
  function bpf(const I: Integer; const OmegaLow, OmegaHigh: Double): Double;
  var
    Arg: Double;
  begin
    Arg := I - (N - 1) / 2;
    if Arg = 0.0 then Exit(0.0);
    Result := (Cos(OmegaLow * Arg * Pi) - Cos(OmegaHigh * Arg * Pi)) / Pi / Arg;
  end;
  function bsf(const I: Integer; const OmegaLow, OmegaHigh: Double): Double;
  var
    Arg: Double;
  begin
    Arg := I - (N - 1) / 2;
    if Arg = 0.0 then Exit(0.0);
    Result := Sinc(Arg * Pi) - OmegaHigh * Sinc(OmegaHigh * Arg * Pi)
              - OmegaLow * Sinc(OmegaLow * Arg * Pi);
  end;
begin
  BL := Max(0, OmegaC - Bandwidth / 2);
  BH := Min(1, OmegaC + Bandwidth / 2);
  case AType of
    ftLPF: for J := 0 to N - 1 do Coef[J] := lpf(J);
    ftHPF: for J := 0 to N - 1 do Coef[J] := hpf(J);
    ftBPF: for J := 0 to N - 1 do Coef[J] := bpf(J, BL, BH);
    ftBSF: for J := 0 to N - 1 do Coef[J] := bsf(J, BL, BH);
  end;

  SetLength(W, N);
  CreateWindowFunction(@W[0], N, Wf, WfParam);
  for J := 0 to N - 1 do
    Coef[J] := Coef[J] * W[J];
end;

procedure ModArg(Input: PComplex; Output: PComplex; const N: Integer);
var
  I: Integer;
begin
  for I := 0 to N - 1 do
  begin
    Output[I].re := cmod(Input[I]);
    Output[I].im := carg(Input[I]);
  end;
end;

procedure ModArg(IO: PComplex; const N: Integer);
var
  I: Integer;
  T: Complex;
begin
  for I := 0 to N - 1 do
  begin
    T := IO[I];
    IO[I].re := cmod(T);
    IO[I].im := carg(T);
  end;
end;

procedure PowArg(Input: PComplex; Output: PComplex; const N: Integer);
var
  I: Integer;
begin
  for I := 0 to N - 1 do
  begin
    Output[I].re := Input[I].re * Input[I].re + Input[I].im * Input[I].im;
    Output[I].im := carg(Input[I]);
  end;
end;

procedure PowArg(IO: PComplex; const N: Integer);
var
  I: Integer;
  T: Complex;
begin
  for I := 0 to N - 1 do
  begin
    T := IO[I];
    IO[I].re := T.re * T.re + T.im * T.im;
    IO[I].im := carg(T);
  end;
end;

procedure CreateWindowFunction(P: PDouble; const N: Integer;
  const Func: TWindowFunction; Param: Double);
var
  I: Integer;
begin
  case Func of
    wfRect:
      for I := 0 to N - 1 do P[I] := 1.0;
    wfHamming:
      for I := 0 to N - 1 do P[I] := 0.53836 - 0.46164 * Cos(2 * Pi * I / (N - 1));
    wfHann:
      for I := 0 to N - 1 do P[I] := 0.5 * (1 - Cos(2 * Pi * I / (N - 1)));
    wfTriangular:
      for I := 0 to N - 1 do P[I] := 1 - (2 / N) * Abs(I - (N - 1) / 2);
    wfBartlett:
      for I := 0 to N - 1 do P[I] := 1 - (2 / (N - 1 )) * Abs(I - (N - 1) / 2);
    wfBlackman:
      for I := 0 to N - 1 do P[I] := 0.42659 - 0.49656 * Cos(2 * Pi * I / (N - 1)) + 0.076849 * Cos(4 * Pi * I / (N - 1));
    wfBartlettHann:
      for I := 0 to N - 1 do P[I] := 0.62 - 0.48 * Abs(I / (N - 1) - 0.5) - 0.38 * Cos(2 * Pi * I / (N - 1));
    wfGaussian:
      begin
        if Param < 0 then Param := 0.4;
        if Param > 0.5 then raise Exception.Create('Gaussian window alpha > 0.5');
        for I := 0 to N - 1 do P[I] := Exp(-0.5 * Sqr((2 * I / (N - 1) - 1) / Param));
      end;
    wfKaiser:
      begin
        if Param < 0 then Param := 3;
        for I := 0 to N - 1 do P[I] := BesselI0(Pi * Param * Sqrt(1 - Sqr(2 * I / (N - 1) - 1))) / BesselI0(Pi * Param);
      end;
    else
      raise Exception.Create(Format('unknow window: %d', [Func]))
  end;
end;

end.

