unit rm_oscillator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioModule;

type

  { TRadioOscillator }

  TRadioOscillator = class(TBackgroundRadioModule)
  private
    FFreq: Cardinal;
    FSampleRate: Cardinal;
    FCounter: Cardinal;
    FPhase: Double;
  protected
    procedure DoReset; override;
    function RMSetFrequency(const Msg: TRadioMessage; const Freq: Cardinal): Integer; override;
    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer; override;

    procedure ThreadFun(Thread: TGenericRadioThread); override;
  end;

implementation

uses
  Math, UComplex;

{ TRadioOscillator }

procedure TRadioOscillator.DoReset;
begin
  inherited;
  FCounter := 0;
end;

function TRadioOscillator.RMSetFrequency(const Msg: TRadioMessage;
  const Freq: Cardinal): Integer;
begin
  FFreq := Freq;
end;

function TRadioOscillator.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  FSampleRate := Rate;
end;

procedure TRadioOscillator.ThreadFun(Thread: TGenericRadioThread);
label
  Wait;
var
  P: PComplex;
  I: Integer;
  F: Cardinal;
  R: Cardinal;
  A: Double;
  C: Integer;
  V: Double;
  W: Double;
  D: Double = 0.0;
begin
  if FSampleRate = 0 then goto Wait;

  while Assigned(DefOutput.Alloc(I)) do
  begin
    P := DefOutput.Buffer[I];
    C := DefOutput.BufferSize[I];
    A := 2 * Pi * FFreq / FSampleRate;
    V := FPhase;
    D := D + C / FSampleRate;
    for I := 0 to C - 1 do
    begin
      W := V + A;
      P[I].re := Cos(V);
      P[I].im := Sin(V);
      V := W;
    end;
    C := Trunc(V / (2 * Pi));
    FPhase := V - (2 * Pi * C);
    DefOutput.Broadcast(I, FDataListeners);
  end;

Wait:
  Sleep(Max(5, Round(D * 1000) - 20));
end;

end.

