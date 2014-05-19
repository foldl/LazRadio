unit SignalBasic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex;

type

  TWindowFunction = (wfRect, wfGaussian, wfHamming, wfHann, wfTriangular, wfBartlett,
                     wfBlackman, wfKaiser, wfBartlettHann);

// Output.re = amplitude
// Output.im = arg
procedure ModArg(Input: PComplex; Output: PComplex; const N: Integer);

procedure CreateWindowFunction(P: PDouble; const N: Integer; const Func: TWindowFunction;
  Param: Double = -1);

function BesselI0(const Z: Double): Double;

implementation

uses
  Math;

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

