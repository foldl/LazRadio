unit rm_oscillator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioModule, UComplex, formoscillator;

const

  RM_OSC_WAVE = RM_USER;
                  SET_WAVE_SIN      = 0;
                  SET_WAVE_RECT     = 1;  // ParamL = DutyRadio (in percentage)
                  SET_WAVE_TRIANGLE = 2;  // ParamL = VertexPostition
                                          //          (in percentage: 0 = reverse sawtooth; 50 = triangle; 100 = (almost) sawtooth)

type

  TOscWaveFunction = procedure (P: PComplex; const Len: Integer) of object;

  { TRadioOscillator }

  TRadioOscillator = class(TBackgroundRadioModule)
  private
    FFreq: Cardinal;
    FSampleRate: Cardinal;
    FCounter: Cardinal;
    FPhase: Double;
    FWaveFunc: TOscWaveFunction;
    FRatio: Integer;
    FUI: TOscillatorForm;
    procedure GenSin(P: PComplex; const Len: Integer);
    procedure GenRect(P: PComplex; const Len: Integer);
    procedure GenTriangle(P: PComplex; const Len: Integer);
  protected
    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: Integer); override;

    procedure DoReset; override;
    function RMSetFrequency(const Msg: TRadioMessage; const Freq: Cardinal): Integer; override;
    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer; override;

    procedure ThreadFun(Thread: TGenericRadioThread); override;
    procedure DoConfigure; override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;
  end;

implementation

uses
  Math, RadioSystem, SignalBasic;

{ TRadioOscillator }

procedure TRadioOscillator.GenSin(P: PComplex; const Len: Integer);
var
  J: Integer;
  A: Double;
  C: Integer;
  V: Double;
  W: Double;
begin
  A := 2 * Pi * FFreq / FSampleRate;
  V := FPhase;
  for J := 0 to Len - 1 do
  begin
    W := V + A;
    P[J].re := Cos(V);
    P[J].im := Sin(V);
    V := W;
  end;
  C := Trunc(V / (2 * Pi));
  FPhase := V - (2 * Pi * C);
end;

procedure TRadioOscillator.GenRect(P: PComplex; const Len: Integer);
var
  J: Integer;
  A: Double;
  C: Integer;
  V: Double;
  W: Double;
  X: Double;
  T: Double;
  Z: Double;
begin
  X := FRatio / 101;
  A := FFreq / FSampleRate;
  V := FPhase;
  FillByte(P^, Len * SizeOf(P^), 0);
  for J := 0 to Len - 1 do
  begin
    W := V + A;
    T := Frac(W);
    P[J].re := IfThen(T <= X, 1, -1);
    P[J].im := 0;
    V := W;
  end;
  FPhase := Frac(V);
end;

procedure TRadioOscillator.GenTriangle(P: PComplex; const Len: Integer);
var
  J: Integer;
  A: Double;
  C: Integer;
  V: Double;
  W: Double;
  X: Double;
  T: Double;
begin
  X := FRatio / 101;
  A := FFreq / FSampleRate;
  V := FPhase;
  FillByte(P^, Len * SizeOf(P^), 0);
  for J := 0 to Len - 1 do
  begin
    W := V + A;
    T := Frac(W);
    P[J].re := IfThen(T <= X, 2 * T / X - 1, -2 * (T - X) / (1 - X) + 1);
    P[J].im := 0;
    V := W;
  end;
  FPhase := Frac(V);
end;

procedure TRadioOscillator.ProccessMessage(const Msg: TRadioMessage;
  var Ret: Integer);
begin
  case Msg.Id of
    RM_OSC_WAVE:
      begin
        case Msg.ParamH of
          SET_WAVE_RECT: FWaveFunc := @GenRect;
          SET_WAVE_TRIANGLE: FWaveFunc := @GenTriangle;
        else
          FWaveFunc := @GenSin;
        end;
        FRatio := Max(1, Msg.ParamL mod 101);
      end
  else
    inherited;
  end;
end;

constructor TRadioOscillator.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  FWaveFunc := @GenSin;
  FUI := TOscillatorForm.Create(nil);
  FUI.Module := Self;
  DefOutput.BufferSize := 1024 * 10;
end;

destructor TRadioOscillator.Destroy;
begin
  FUI.Free;
  inherited Destroy;
end;

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
  Broadcast(Msg);
end;

procedure TRadioOscillator.ThreadFun(Thread: TGenericRadioThread);
label
  Wait;
var
  I: Integer;
  D: Double = 0.0;
  C: Integer;
begin
  if FSampleRate = 0 then goto Wait;

  while Assigned(DefOutput.Alloc(I)) do
  begin
    C := DefOutput.BufferSize;
    D := D + C / FSampleRate;
    FWaveFunc(DefOutput.Buffer[I], C);
    CancelDC(DefOutput.Buffer[I], C);
    DefOutput.Broadcast(I, FDataListeners);
  end;

Wait:
  Sleep(1000);
end;

procedure TRadioOscillator.DoConfigure;
begin
  FUI.Show;
end;

initialization

  RegisterModule('Oscillator', TRadioModuleClass(TRadioOscillator.ClassType));

end.

