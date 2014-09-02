unit rm_pll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioModule, RadioSystem, UComplex, Math, radiomessage;

type

  TPLLState = (psCapture, psLocked);

  { TPLLNode }

  TPLLNode = class
  private
    FZero: Double;
    FPreV: Double;
    FRealAmp: Double;
    FLPFAlpha: Double;
    FBandwidth: Cardinal;
    FErrorFilterTimeConst: Double;
    FFreqRange: Cardinal;
    FState: TPLLState;
    FDefaultFrequency: Cardinal;
    FOutputVoltage: Boolean;
    FSampleRate: Cardinal;
    FTolerance: Double;
    FNextPhase: Double;
    FZeta: Double;
    FError: Double;
    FErrorAlpha: Double;
    FError1MinusAlpha: Double;
    FAlpha: Double;
    FBeta: Double;
    FPhaseDelta: Double;
    FPhaseDeltaMin: Double;
    FPhaseDeltaMax: Double;
    function GetLocked: Boolean;
    procedure SetBandwidth(AValue: Cardinal);
    procedure SetDefaultFrequency(AValue: Cardinal);
    procedure SetErrorFilterTimeConst(AValue: Double);
    procedure SetFreqRange(AValue: Cardinal);
    procedure SetSampleRate(AValue: Cardinal);
    procedure Reset;
    procedure SetTolerance(AValue: Double);
    procedure SetZeta(AValue: Double);
  public
    constructor Create;

    // Phase.re = Phase; Phase.im = phase error
    procedure ProcessComplex(Data: PComplex; Phase: PComplex; const Len: Integer);
    procedure ProcessReal(IO: PComplex; const Len: Integer);

    property Locked: Boolean read GetLocked;

    property DefaultFrequency: Cardinal read FDefaultFrequency write SetDefaultFrequency;
    property SampleRate: Cardinal read FSampleRate write SetSampleRate;
    property FreqRange: Cardinal read FFreqRange write SetFreqRange;
    property Zeta: Double read FZeta write SetZeta; // damping factor
    property Tolerance: Double read FTolerance write SetTolerance;
    property ErrorFilterTimeConst: Double read FErrorFilterTimeConst write SetErrorFilterTimeConst;
    property Bandwidth: Cardinal read FBandwidth write SetBandwidth;
  end;

  TRadioPLLModule = class(TRadioModule)

  end;

implementation

uses
  SignalBasic;

{ TPLLNode }

procedure TPLLNode.SetDefaultFrequency(AValue: Cardinal);
begin
  if FDefaultFrequency = AValue then Exit;
  FDefaultFrequency := AValue;
  Reset;
end;

procedure TPLLNode.SetErrorFilterTimeConst(AValue: Double);
begin
  if FErrorFilterTimeConst = AValue then Exit;
  FErrorFilterTimeConst := AValue;
end;

procedure TPLLNode.SetFreqRange(AValue: Cardinal);
begin
  if FFreqRange = AValue then Exit;
  FFreqRange := AValue;
  Reset;
end;

function TPLLNode.GetLocked: Boolean;
begin
  Result := FState = psLocked;
end;

procedure TPLLNode.SetBandwidth(AValue: Cardinal);
begin
  if FBandwidth = AValue then Exit;
  FBandwidth := AValue;
  Reset;
end;

procedure TPLLNode.SetSampleRate(AValue: Cardinal);
begin
  if FSampleRate = AValue then Exit;
  FSampleRate := AValue;
  Reset;
end;

procedure TPLLNode.Reset;
var
  PhasePerSample: Double;
begin
  FState := psCapture;
  FNextPhase := 0.0;
  FError := 0.0;
  if FSampleRate < 1 then Exit;
  if FZeta <= 0.0 then Exit;
  PhasePerSample := 2 * Pi / FSampleRate;
  FPhaseDelta := FDefaultFrequency * PhasePerSample;
  FPhaseDeltaMin := (FDefaultFrequency - FFreqRange) * PhasePerSample;
  FPhaseDeltaMax := (FDefaultFrequency + FFreqRange) * PhasePerSample;
  FErrorAlpha := 1 - Exp(-1/(FSampleRate * FErrorFilterTimeConst));
  FError1MinusAlpha := 1 - FErrorAlpha;
  FAlpha := 2.0 * FZeta * FBandwidth * PhasePerSample;
  FBeta := (FAlpha * FAlpha) / (4.0 * FZeta * FZeta);
end;

procedure TPLLNode.SetZeta(AValue: Double);
begin
  if FZeta = AValue then Exit;
  FZeta := AValue;
  Reset;
end;

procedure TPLLNode.SetTolerance(AValue: Double);
begin
  if FTolerance = AValue then Exit;
  FTolerance := AValue;
  Reset;
end;

constructor TPLLNode.Create;
begin
  inherited;
  FSampleRate := 1024;
  FDefaultFrequency := 10;
  FZeta := 0.707;
  FTolerance := 1.0;
  FBandwidth := 10;
  FFreqRange := 20;
  FErrorFilterTimeConst := 0.5;
  FRealAmp := 1.0;
  FLPFAlpha := 0.1;
end;

procedure TPLLNode.ProcessComplex(Data: PComplex; Phase: PComplex;
  const Len: Integer);
var
  I: Integer;
  T: Complex;
  V: Double;
  A: Double;
  E: Double;
  procedure Save;
  var
    F: TFileStream;
    S: string;
    I: Integer;
  begin
    F := TFileStream.Create('e:\pll.txt', fmCreate);
    for I := 0 to Len - 1 do
    begin
      S := Format('%2.8f, %2.8f' + #13#10, [Phase[I].re, Phase[I].im]);
      F.Write(S[1], Length(S));
    end;

    F.Free;
  end;

begin
  V := FNextPhase;
  E := FError;
  for I := 0 to Len - 1 do
  begin
    Phase[I].re := V;

    T.re := Cos(V);
    T.im := Sin(V);
    T := T * Data[I];
    A := -ArcTan2(T.im, T.re);

    FPhaseDelta := EnsureRange(FPhaseDelta + FBeta * A, FPhaseDeltaMin, FPhaseDeltaMax);
    V := V + FPhaseDelta + FAlpha * A;
    if V > 2 * Pi then V := V - 2 * Pi;

    E := E * FError1MinusAlpha + A * A * FErrorAlpha;
    Phase[I].im := E;
  end;
  FNextPhase := V;
  FError     := E;
  if FError < FTolerance then
    FState := psLocked
  else
    FState := psCapture;
end;

procedure TPLLNode.ProcessReal(IO: PComplex; const Len: Integer);
var
  I: Integer;
  T, C: Complex;
  V: Double;
  A: Double;
  P: Double;
  E: Double;
  Dc: Double = 0.0;
  Amp: Double = 0.0;

  function arcsin(const X: Double): Double; inline;
  begin
    Result := Math.arcsin(EnsureRange(X, -1, 1));
  end;

begin
  V := FNextPhase;
  E := FError;

  for I := 0 to Len - 1 do
  begin
    T.re := IO[I].re - FZero;
    FZero := (1 - FLPFAlpha) * FZero + FLPFAlpha * T.re;

    if T.re > 0 then
    begin
      if FPrev <= 0 then
      begin
        // cross-zero, a new period starts
        if Amp > 0 then FRealAmp := Amp;
        Amp := 0;
      end;
      if T.re > FPreV then
      begin
        Amp := T.re;
        P := ArcSin(T.re / FRealAmp);
      end
      else begin
        P := Pi - ArcSin(T.re / FRealAmp);
      end;
    end
    else begin
      if T.re > FPreV then
      begin
        P := 2 * Pi + ArcSin(T.re / FRealAmp);
      end
      else begin
        Amp := Max(Amp, -T.re);
        P := Pi - ArcSin(T.re / FRealAmp);
      end;
    end;

    FPreV := T.re;

    // A in [-pi, pi]
    A := P - V;
    if A < -Pi then A := A + 2 * Pi;
    if A > Pi then A := 2 * Pi - A;

    FPhaseDelta := EnsureRange(FPhaseDelta + FBeta * A, FPhaseDeltaMin, FPhaseDeltaMax);
    V := V + FPhaseDelta + FAlpha * A;
    if V > 2 * Pi then V := V - 2 * Pi;

    E := E * FError1MinusAlpha + A * A * FErrorAlpha;
    IO[I].re := V;
    IO[I].im := E;
  end;
  FNextPhase := V;
  FError     := E;
  if FError < FTolerance then
    FState := psLocked
  else
    FState := psCapture;
end;

end.

