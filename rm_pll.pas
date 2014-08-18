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
    procedure ProcessData(const Data: PComplex; Phase: PComplex; const Len: Integer);

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
  PhasePerSample := 2 * Pi * / FSampleRate;
  FPhaseDelta := FDefaultFrequency * PhasePerSample;
  FPhaseDeltaMin := (FDefaultFrequency - FFreqRange) * PhasePerSample;
  FPhaseDeltaMax := (FDefaultFrequency + FFreqRange) * PhasePerSample;
  FErrorAlpha := 1 - Exp(-1/(FSampleRate * FErrorFilterTimeConst));
  FError1MinusAlpha := 1 - FErrorAlpha;
  FAlpha := 2.0 * FZeta * FBandwidth * PhasePerSample;
  FBeta := (FAlpha * FAlpha) / (4.0 * FZeta * FZeta);;
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
  FTolerance := 0.5;
  FBandwidth := 10;
  FFreqRange := 20;
end;

procedure TPLLNode.ProcessData(Data: PComplex; Phase: PComplex;
  const Len: Integer);
var
  I: Integer;
  T: Complex;
  V: Double;
  A: Double;
  E: Double;
begin
  V := FNextPhase;
  E := FError;
  for I := 0 to Len - 1 do
  begin
    Phase[I].re := V;

    T.re := Cos(V);
    T.im := Sin(V);
    T := T * Data[I];
    A := ArcTan2(T.im, T.re);

    FPhaseDelta := EnsureRange(FPhaseDelta + FBeta * A, FPhaseDeltaMin, FPhaseDeltaMax);
    V := FPhaseDelta + FAlpha * A;

    E := E * FError1MinusAlpha + A * A * FErrorAlpha;
    Phase[I].im := FError;
  end;
  FNextPhase := V;
  FError     := E;
  if FError < FTolerance then
    FState := psLocked
  else
    FState := psCapture;
end;

end.

