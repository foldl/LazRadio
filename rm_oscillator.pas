unit rm_oscillator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioModule, UComplex, formoscillator, RadioMessage;

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
    procedure Describe(Strs: TStrings); override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;
  end;

  TOscRec = record
    Freq: Integer;
    PhaseDelta: Double;
    SampleRate: Cardinal;
    Phase: Double;
  end;

  { TRadioFreqMixer }

  TRadioFreqMixer = class(TRadioModule)
  private
    FExternalOsc: Boolean;
    FBandIndex: Integer;
    FFreq: Integer;
    FOscFreq: Integer;
    FPhaseDelta: Double;
    FSampleRate: Cardinal;
    FCounter: Cardinal;
    FPhase: Double;
    FRegulator: TStreamRegulator;
    procedure SetOscFreq(const Freq: Integer);
  protected
    function RMSetFrequency(const Msg: TRadioMessage; const Freq: Cardinal): Integer; override;
    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer; override;
    procedure ReceiveRegulatedData(const P: PComplex; const Len: Integer);
    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: Integer); override;
    procedure Describe(Strs: TStrings); override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;
    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

  procedure InitSimpleOsc(var Osc: TOscRec; const Freq, SampleRate: Integer);
  procedure SimpleOscMix(var Osc: TOscRec; P: PComplex; const Len: Integer);
  procedure SimpleOscMixRe(var Osc: TOscRec; P: PComplex; const Len: Integer);

implementation

uses
  Math, RadioSystem, SignalBasic;

procedure InitSimpleOsc(var Osc: TOscRec; const Freq, SampleRate: Integer);
begin
  FillByte(Osc, SizeOf(Osc), 0);
  Osc.Freq := Freq;
  Osc.SampleRate := SampleRate;
  if SampleRate > 0 then Osc.PhaseDelta := 2 * Pi * Freq / SampleRate;
end;

procedure SimpleOscMix(var Osc: TOscRec; P: PComplex; const Len: Integer);
var
  J: Integer;
  C: Integer;
  V: Double;
  T: Complex;
  O: Double;
begin
  O := Osc.PhaseDelta;
  V := Osc.Phase;
  for J := 0 to Len - 1 do
  begin
    T.re := Cos(V);
    T.im := Sin(V);
    P[J] := P[J] * T;
    V := V + O;
  end;
  C := Trunc(V / (2 * Pi));
  Osc.Phase := V - (2 * Pi * C);
end;

procedure SimpleOscMixRe(var Osc: TOscRec; P: PComplex; const Len: Integer);
var
  J: Integer;
  C: Integer;
  V: Double;
  O: Double;
begin
  O := Osc.PhaseDelta;
  V := Osc.Phase;
  for J := 0 to Len - 1 do
  begin
    P[J].re := P[J].re * Cos(V);
    P[J].im := P[J].re * Sin(V);
    V := V + O;
  end;
  C := Trunc(V / (2 * Pi));
  Osc.Phase := V - (2 * Pi * C);
end;

{ TRadioFreqMixer }

procedure TRadioFreqMixer.SetOscFreq(const Freq: Integer);
begin
  FOscFreq := Integer(Freq);
  FPhase := 0;
  if FSampleRate > 0 then FPhaseDelta := 2 * Pi * (FFreq - FOscFreq) / FSampleRate;
end;

function TRadioFreqMixer.RMSetFrequency(const Msg: TRadioMessage;
  const Freq: Cardinal): Integer;
begin
  FFreq := Integer(Freq);
  FPhase := 0;
  if FSampleRate > 0 then FPhaseDelta := 2 * Pi * (FFreq - FOscFreq) / FSampleRate;
  Result := 0;
end;

function TRadioFreqMixer.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  FSampleRate := Rate;
  FPhase := 0;
  if FSampleRate > 0 then FPhaseDelta := 2 * Pi * (FFreq - FOscFreq) / FSampleRate;
  Result := inherited;
end;

procedure TRadioFreqMixer.ReceiveRegulatedData(const P: PComplex;
  const Len: Integer);
var
  I: Integer;
  J: Integer;
  X: PComplex;
  C: Integer;
  V: Double;
  T: Complex;
begin
  X := Alloc(DefOutput, I);
  if not Assigned(X) then Exit;

  V := FPhase;
  for J := 0 to Len - 1 do
  begin
    T.re := Cos(V);
    T.im := Sin(V);
    X[J] := P[J] * T;
    V := V + FPhaseDelta;
  end;
  C := Trunc(V / (2 * Pi));
  FPhase := V - (2 * Pi * C);

  DefOutput.Broadcast(I, FDataListeners);
end;

procedure TRadioFreqMixer.ProccessMessage(const Msg: TRadioMessage;
  var Ret: Integer);
begin
  if Msg.Id = RM_SPECTRUM_BAND_SELECT_1 + FBandIndex then
  begin
    SetOscFreq(FFreq + ((Integer(Msg.ParamH) + Integer(Msg.ParamL)) div 2));
    Ret := 0;
    Exit;
  end;

  case Msg.Id of
    RM_FREQMIXER_SET_FREQ:        SetOscFreq(Msg.ParamH);
    RM_FREQMIXER_USE_BAND_SELECT: FBandIndex := Msg.ParamH
  else
    inherited
  end;
end;

procedure TRadioFreqMixer.Describe(Strs: TStrings);
begin
  if FExternalOsc then
    Strs.Add(Format('^bExternal Osc.', []))
  else
    Strs.Add(Format('^bFrequency: ^n%s', [FormatFreq(FOscFreq)]));
end;

constructor TRadioFreqMixer.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  FRegulator := TStreamRegulator.Create;
  FRegulator.Size := DefOutput.BufferSize;
  FRegulator.OnRegulatedData := @ReceiveRegulatedData;
  FHasConfig := False;
end;

destructor TRadioFreqMixer.Destroy;
begin
  FRegulator.Free;
  inherited Destroy;
end;

procedure TRadioFreqMixer.ReceiveData(const P: PComplex; const Len: Integer);
begin
  FRegulator.ReceiveData(P, Len);
end;

{ TRadioOscillator }

procedure TRadioOscillator.GenSin(P: PComplex; const Len: Integer);
var
  J: Integer;
  A: Double;
  C: Integer;
  V: Double;
begin
  A := 2 * Pi * FFreq / FSampleRate;
  V := FPhase;
  for J := 0 to Len - 1 do
  begin
    P[J].re := Cos(V);
    P[J].im := Sin(V);
    V := V + A;
  end;
  C := Trunc(V / (2 * Pi));
  FPhase := V - (2 * Pi * C);
end;

procedure TRadioOscillator.GenRect(P: PComplex; const Len: Integer);
var
  J: Integer;
  A: Double;
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
        GraphInvalidate;
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

  while (not FThread.Terminated) and Assigned(DefOutput.TryAlloc(I)) do
  begin
    C := DefOutput.BufferSize;
    D := D + C / FSampleRate;
    FWaveFunc(DefOutput.Buffer[I], C);
    CancelDC(DefOutput.Buffer[I], C);
    DefOutput.Broadcast(I, FDataListeners);
  end;

Wait:
  Sleep(10);
end;

procedure TRadioOscillator.DoConfigure;
begin
  FUI.Show;
end;

procedure TRadioOscillator.Describe(Strs: TStrings);
begin
  if FWaveFunc = @GenSin then
    Strs.Add('^bWave: ^nSin')
  else if FWaveFunc = @GenRect then
    Strs.Add('^bWave: ^nRectangle')
  else
    Strs.Add('^bWave: ^nTriangle');
  if FWaveFunc <> @GenSin then
    Strs.Add(Format('^bDuty: ^n%d%', [FRatio]));
end;

initialization

  RegisterModule(TRadioModuleClass(TRadioOscillator.ClassType));
  RegisterModule(TRadioModuleClass(TRadioFreqMixer.ClassType));

end.

