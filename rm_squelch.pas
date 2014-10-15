unit rm_squelch;

{$mode objfpc}{$H+}

{
   This module is for noise squelch, or rather voice/speech detection.

   Hiss filter is suitable for FM demodulation output, while time domain alg
   is suitable for AM.
}

interface

uses
  Classes, SysUtils, UComplex, RadioModule, RadioSystem, RadioMessage, Math,
  SignalBasic;

type

  { TRadioSquelch }

  TRadioSquelch = class(TRadioModule)
  private
    FHissFilter1: TIIRFilter;
    FHissFilter2: TIIRFilter;
    FCenter: Cardinal;
    FAlpha: Double;
    FBw: Cardinal;
    FRate: Cardinal;
    FThreshold: Double;
    FThresholdTime: Double;
    FChannelNum: Integer;
    FNoise1: Double;
    FNoise2: Double;
    FSig1: Double;
    FSig2: Double;
    FAlg: Integer;
    procedure Reconfig;
    procedure AlgHissFilter(const P: PComplex; O: PComplex; const Len: Integer);
    procedure AlgTimeDomain(const P: PComplex; O: PComplex; const Len: Integer);
  protected
    procedure ProccessCustomMessage(const Msg: TRadioMessage; var Ret: Integer); override;
    function  RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer;
      override;
    procedure Describe(Strs: TStrings); override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

implementation

{ TRadioSquelch }

procedure TRadioSquelch.Reconfig;
begin
  if FRate < 1 then Exit;
  FAlpha := 30 / FRate;
  AudioEQFilterDesign(FCenter, FRate, FCenter / FBw, ftBPF, FHissFilter1.A, FHissFilter1.B);
  AudioEQFilterDesign(FCenter, FRate, FCenter / FBw, ftBPF, FHissFilter2.A, FHissFilter2.B);
end;

procedure TRadioSquelch.AlgHissFilter(const P: PComplex; O: PComplex;
  const Len: Integer);
var
  I: Integer;
  Noise: Double;
begin
  IIRFilterReal(FHissFilter1, P, O, Len);
  Exit;
  Noise := FNoise1;
  for I := 0 to Len - 1 do
  begin
    Noise := (1 - FAlpha) * Noise + FAlpha * Abs(O[I].re);
    if Noise > FThreshold then
      O[I].re := 0
    else
      O[I].re := P[I].re;
  end;
  FNoise1 := Noise;

  if FChannelNum = 2 then
  begin
    IIRFilterReInIm(FHissFilter1, P, O, Len);
    Noise := FNoise2;
    for I := 0 to Len - 1 do
    begin
      Noise := (1 - FAlpha) * Noise + FAlpha * Abs(O[I].im);
      if Noise > FThreshold then
        O[I].im := 0
      else
        O[I].im := P[I].im;
    end;
    FNoise2 := Noise;
  end;
end;

procedure TRadioSquelch.AlgTimeDomain(const P: PComplex; O: PComplex;
  const Len: Integer);
var
  I: Integer;
  Sig: Double;
begin
  Sig := FSig1;
  for I := 0 to Len - 1 do
  begin
    Sig := 0.995 * Sig + 0.005 * 20 * log10(P[I].re);
    if Sig > FThresholdTime then
      O[I].re := P[I].re;
  end;
  FSig1 := Sig;

  if FChannelNum < 2 then Exit;

  Sig := FSig2;
  for I := 0 to Len - 1 do
  begin
    Sig := 0.995 * Sig + 0.005 * 20 * log10(P[I].im);
    if Sig > FThresholdTime then
      O[I].im := P[I].im;
  end;
  FSig2 := Sig;
end;

procedure TRadioSquelch.ProccessCustomMessage(const Msg: TRadioMessage;
  var Ret: Integer);
begin
  case Msg.Id of
    RM_SQUELCH_BPF_BAND:
      begin
        FCenter := Msg.ParamH;
        FBw     := Max(1, Msg.ParamL);
        Reconfig;
        GraphInvalidate;
      end;
    RM_SQUELCH_CFG:
      begin
        case Msg.ParamH of
          SQUELCH_NOISE_THRESHOLD:
            begin
              if FAlg = SQUELCH_ALG_TIME_DOMAIN then
                FThresholdTime := -150 + EnsureRange(Msg.ParamL, 0, 100)
              else
                FThreshold := 1 - EnsureRange(Msg.ParamL, 0, 100) / 100;
            end;
          SQUELCH_CHANNEL_NUM:
            FChannelNum := EnsureRange(Msg.ParamL, 1, 2);
          SQUELCH_ALG:
            FAlg := Msg.ParamL;
        end;
        GraphInvalidate;
      end;
  end;
end;

function TRadioSquelch.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  FRate := Rate;
  Reconfig;
  Result := inherited;
end;

procedure TRadioSquelch.Describe(Strs: TStrings);
begin
  if FAlg = SQUELCH_ALG_HISS_FILTER then
  begin
    Strs.Add(Format('^bRef Band   : ^n[%s, %s]', [FormatFreq(FCenter - FBw div 2), FormatFreq(FCenter + FBw div 2)]));
    Strs.Add(Format('^bNoise Level: ^n%f',       [FThreshold]));
  end
  else begin
    Strs.Add(Format('^bSignal Threshold: ^n%fdB',       [FThresholdTime]));
  end;
end;

constructor TRadioSquelch.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  FChannelNum := 1;
  FCenter := 5000;
  FBw     := 200;
  FThreshold := 0.3;
  FHasConfig := False;
  FThresholdTime := -100;
  SetIIROrders(FHissFilter1, 2, 2);
end;

destructor TRadioSquelch.Destroy;
begin
  inherited Destroy;
end;

procedure TRadioSquelch.ReceiveData(const P: PComplex; const Len: Integer);
var
  J: Integer;
  O: PComplex;
begin
  DefOutput.BufferSize := Len;
  O := Alloc(DefOutput, J);
  if not Assigned(O) then
  begin
    // it's better to forward input data
    TRadioLogger.Report(llWarn, 'TRadioSquelch.ReceiveData: data lost');
    Exit;
  end;

  FillByte(O^, Len * SizeOf(O^), 0);

  if FAlg = SQUELCH_ALG_TIME_DOMAIN then
    AlgTimeDomain(P, O, Len)
  else
    AlgHissFilter(P, O, Len);

  DefOutput.Broadcast(J, FDataListeners);
end;

initialization

  RegisterModule(TRadioModuleClass(TRadioSquelch.ClassType));

end.

