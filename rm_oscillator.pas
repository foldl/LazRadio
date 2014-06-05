unit rm_oscillator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioModule;

const

  RM_OSC_WAVE = RM_USER;
                  SET_WAVE_SIN      = 0;
                  SET_WAVE_RECT     = 1;  // ParamL =
                  SET_WAVE_TRI      = 2;

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
  Math, UComplex, RadioSystem;

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
  J: Integer;
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
    for J := 0 to C - 1 do
    begin
      W := V + A;
      P[J].re := Cos(V);
      P[J].im := Sin(V);
      V := W;
    end;
    C := Trunc(V / (2 * Pi));
    FPhase := V - (2 * Pi * C);
    DefOutput.Broadcast(I, FDataListeners);
  end;

Wait:
  Sleep(Max(5, Round(D * 1000) - 20));
end;

initialization

  RegisterModule('Oscillator', TRadioModuleClass(TRadioOscillator.ClassType));

end.

