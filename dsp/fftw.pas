unit FFTW;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex, Math;

type

  PFFTWPlan = Pointer;

  TFFTState = record
    N: Integer;
    NRep: Double;
    Inverse: Boolean;
    I, O: PComplex;
    Plan: PFFTWPlan;
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

const
  FFTW_LIB = 'libfftw3-3.dll';
  FFTW_FORWARD = -1;
  FFTW_BACKWARD = 1;
  FFTW_MEASURE = 0;
  FFTW_ESTIMATE = 1 shl 6;

function fftw_alloc_complex(const N: Integer): PComplex; stdcall; external FFTW_LIB;
function fftw_plan_dft_1d(const N: Integer; I, O: PComplex; const Direction: Integer;
                          const Flags: Cardinal): PFFTWPlan; stdcall; external FFTW_LIB;
procedure fftw_execute(const P: PFFTWPlan); stdcall; external FFTW_LIB;
procedure fftw_destroy_plan(const P: PFFTWPlan); stdcall; external FFTW_LIB;
procedure fftw_free(P: PComplex); stdcall; external FFTW_LIB;

function BuildFFTPlan(const N: Integer; const Inverse: Boolean): PFFTPlan;
begin
  New(Result);
  FillByte(Result^, SizeOf(Result^), 0);
  ChangePlan(Result, N, Inverse);
end;

procedure ChangePlan(Plan: PFFTPlan; const N: Integer; const Inverse: Boolean);
begin
  if (Plan^.N = N) and (Plan^.Inverse = Inverse) then Exit;

  Plan^.N := N;
  Plan^.Inverse := Inverse;
  Plan^.NRep := 1 / N;
  if Assigned(Plan^.Plan) then
  begin
    fftw_destroy_plan(Plan^.Plan);
    fftw_free(Plan^.I);
    fftw_free(Plan^.O);
  end;
  Plan^.I := fftw_alloc_complex(N);
  Plan^.O := fftw_alloc_complex(N);
  Plan^.Plan := fftw_plan_dft_1d(N, Plan^.I, Plan^.O,
                                    IfThen(Inverse, FFTW_BACKWARD, FFTW_FORWARD), FFTW_MEASURE);
end;

procedure FinalizePlan(P: PFFTPlan);
begin
  if Assigned(P^.Plan) then
  begin
    fftw_destroy_plan(P^.Plan);
    fftw_free(P^.I);
    fftw_free(P^.O);
  end;
  Dispose(P);
end;

procedure FFT(Plan: PFFTPlan; Input: PComplex; Output: PComplex);
var
  I: Integer;
begin
  Move(Input^, Plan^.I^, Plan^.N * SizeOf(Input^));
  fftw_execute(Plan^.Plan);

  if Plan^.Inverse then
  begin
    for I := 0 to Plan^.N - 1 do
      Output[I] := Plan^.O[I] * Plan^.NRep;
  end
  else
    Move(Plan^.O^, Output^, Plan^.N * SizeOf(Input^));
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

end.

