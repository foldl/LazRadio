unit rm_fm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex, RadioModule, RadioSystem, Math, SignalBasic,
  radiomessage, rm_pll;

type

  { TRadioFreqDiscriminator }

  TRadioFreqDiscriminator = class(TRadioModule)
  private
    FRegulator: TStreamRegulator;
    FLastValue: Complex;
    FSampleRate: Cardinal;
    procedure ReceiveRegulatedData(const P: PComplex; const Len: Integer);
  protected
    function RMSetFrequency(const Msg: TRadioMessage; const Freq: Cardinal): Integer; override;
    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer; override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

  { TDeEmphasisNode }

  TDeEmphasisNode = class(TDataFlowNode)
  private
    FReZ: Double;
    FImZ: Double;
    FAlpha: Double;
    FEmphasisTime: Double;
    FMono: Boolean;
    FSampleRate: Cardinal;
    procedure Config;
    procedure SetEmphasisTime(AValue: Double);
    procedure SetMono(AValue: Boolean);
    procedure SetSampleRate(AValue: Cardinal);
  protected
    procedure DoReceiveData(const P: PComplex; const Len: Integer); override;
  public
    procedure DeEmphasis(P: PComplex; const Len: Integer);
    property EmphasisTime: Double read FEmphasisTime write SetEmphasisTime;
    property SampleRate: Cardinal read FSampleRate write SetSampleRate;
    property Mono: Boolean read FMono write SetMono;
  end;

  { TRDSDecoder }

  TRDSDecoder = class(TRadioModule)
  private
    FLastTimingData: Double;
    FLastSlope: Double;
    FLastData: Double;
    FCounter: Integer;
    FData: array of Complex;
    FRate: Integer;
    FMatchedFilter: TFIRNode;
    FBPF: TIIRFilter;
    FTimingBPF: TIIRFilter;
    procedure ReceiveBit(const B: Integer);
  protected
    procedure ReceiveFilteredData(const P: PComplex; const Len: Integer);

    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer; override;
    procedure Describe(Strs: TStrings); override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

  // TRadioFMReceiver must be following a TRadioFreqDiscriminator
  { TRadioFMReceiver }

  TRadioFMReceiver = class(TRadioModule)
  private
    FPLLOutput: TRadioDataStream;
    FPLLPhases: array of Complex;
    FSampleRate: Integer;
    FPLL: TPLLNode;
    FMono: Boolean;
    FEmphasisTime: Cardinal;
    FPilotFilter: TIIRFilter;
    FDeEmphasis: TDeEmphasisNode;
    FAudioChain: TDataFlowNode;     // BFP -> De-emphasis -> Regulator
  protected
    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer; override;
    procedure ReceiveRegulatedData(const P: PComplex; const Len: Integer);
    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: Integer); override;
    procedure Describe(Strs: TStrings); override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

implementation

uses
  rm_filter;

{ TRDSDecoder }

procedure TRDSDecoder.ReceiveBit(const B: Integer);
begin

end;

procedure TRDSDecoder.ReceiveFilteredData(const P: PComplex; const Len: Integer
  );
var
  I: Integer;
  Slope: Double;
begin
  for I := 0 to Len - 1 do
  begin
    P[I].im := P[I].re;
    P[I].re := Abs(P[I].re);
  end;

  // construct sampling signal
  IIRFilterReal(FTimingBPF, P, Len);

  // now, sampling values at the peak of sampling signal
  for I := 0 to Len - 1 do
  begin
    Slope := P[I].re - FLastTimingData;
    if Slope = 0.0
    P[I].im := P[I].re;
    P[I].re := Abs(P[I].re);
  end;

  Inc(FCounter);
  if FCounter = 10 then
    FCounter := 0;
  DumpData(P, Len, 'e:\rds.txt');
end;

function TRDSDecoder.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
const
  td = 1/1187.5;
  td2 = td * td;
var
  h: array of Double;
  I, J, M: Integer;
  t: Double;
begin
  FRate := Integer(Rate);
  if FRate < 1 then Exit;

  AudioEQFilterDesign(57000, Rate, 57000 / 5000, ftBPF, FBPF.A, FBPF.B);
  AudioEQFilterDesign( 1 /td, Rate, 500, ftBPF, FTimingBPF.A, FTimingBPF.B);

  // design matched fielter
  {
     H(f) = td Cos[pi f td / 4], |f| <= 2/td
          = 0, otherwise

             8 td^2 Cos[4 pi t / td]
     h(t) = ----------------------------------
               pi (-64 t^2 + td^2)
  }

  // h length = 2 * td is enough
  I := Round(2 * td * FRate);
  if not Odd(I) then Inc(I);
  SetLength(h, I);
  M := I div 2;
  h[M] := 8 / Pi;
  for J := 1 to I div 2 do
  begin
    t := J / FRate;
    if Abs(t - td/8) > 1e-6 then
      h[M + J] := 8 * td2 * Cos(4 * Pi * t / td) / pi / (-64 * t * t + td2)
    else
      h[M + J] := 2.0;
    h[M - J] := h[M + J];
  end;
  FMatchedFilter.SetFIR(PDouble(@h[0]), Length(h));
end;

procedure TRDSDecoder.Describe(Strs: TStrings);
begin

end;

constructor TRDSDecoder.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  SetIIROrders(FBPF, 2, 2);
  SetIIROrders(FTimingBPF, 2, 2);
  FMatchedFilter := TFIRNode.Create;
  FMatchedFilter.OnSendToNext := @ReceiveFilteredData;
end;

destructor TRDSDecoder.Destroy;
begin
  FMatchedFilter.Free;
  inherited Destroy;
end;

procedure TRDSDecoder.ReceiveData(const P: PComplex; const Len: Integer);
var
  I: Integer;
begin
  if Len > Length(FData) then
    SetLength(FData, Len);

  for I := 0 to Len - 1 do
  begin
    FData[I].re := P[I].re * Sin(3 * P[I].im);
    FData[I].im := 0;
  end;

  FMatchedFilter.ReceiveData(@FData[0], Len);
end;

{ TDeEmphasisNode }

procedure TDeEmphasisNode.Config;
begin
  FReZ := 0;
  FImZ := 0;
  if FSampleRate * FEmphasisTime > 0 then
    FAlpha := 1.0 - Exp(-1.0 / (FSampleRate * FEmphasisTime));
end;

procedure TDeEmphasisNode.SetEmphasisTime(AValue: Double);
begin
  if FEmphasisTime = AValue then Exit;
  FEmphasisTime := AValue;
  Config;
end;

procedure TDeEmphasisNode.SetMono(AValue: Boolean);
begin
  if FMono = AValue then Exit;
  FMono := AValue;
end;

procedure TDeEmphasisNode.SetSampleRate(AValue: Cardinal);
begin
  if FSampleRate = AValue then Exit;
  FSampleRate := AValue;
  Config;
end;

procedure TDeEmphasisNode.DoReceiveData(const P: PComplex; const Len: Integer);
begin
  DeEmphasis(P, Len);
  SendToNext(P, Len);
end;

procedure TDeEmphasisNode.DeEmphasis(P: PComplex; const Len: Integer);
var
  I: Integer;
begin
  if FMono then
  begin
    for I := 0 to Len - 1 do
    begin
      FReZ := FReZ + FAlpha * (P[I].Re - FReZ);
      P[I].Re := FReZ;
      P[I].Im := FReZ;
    end;
  end
  else begin
    for I := 0 to Len - 1 do
    begin
      FReZ := FReZ + FAlpha * (P[I].Re - FReZ);
      FImZ := FImZ + FAlpha * (P[I].Im - FImZ);
      P[I].Re := FReZ;
      P[I].Im := FImZ;
    end;
  end;
end;

{ TRadioFMReceiver }

function TRadioFMReceiver.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  FSampleRate := Rate;
  TFilterModule.DesignBPFReal(FAudioChain as TFIRNode, 128, Rate, 30, 15000);
  FDeEmphasis.SampleRate := Rate;
  FPLL.SampleRate := Rate;
  AudioEQFilterDesign(19000, Rate, 200, ftBPF, FPilotFilter.A, FPilotFilter.B);
  Result := inherited;
end;

procedure TRadioFMReceiver.ReceiveRegulatedData(const P: PComplex;
  const Len: Integer);
var
  I: Integer;
  X: PComplex;
begin
  X := Alloc(DefOutput, I);
  if Assigned(X) then
  begin
    Move(P^, X^, SizeOf(P^) * Len);
    DefOutput.Broadcast(I, FDataListeners);
  end
  else begin
    TRadioLogger.Report(llWarn, 'TRadioFMReceiver.ReceiveRegulatedData: data lost');
  end;
end;

procedure TRadioFMReceiver.ProccessMessage(const Msg: TRadioMessage;
  var Ret: Integer);
begin
  case Msg.Id of
    RM_FMRECEIVER_CFG:
      begin
        case Msg.ParamH of
          FMRECEIVER_MODE:
            begin
              FMono := Msg.ParamL = FMRECEIVER_MONO;
              if FMono then
              begin
                FDeEmphasis.Mono := True;
                (FAudioChain as TRealFIRNode).Mono := True;
              end;
            end;
          FMRECEIVER_DEEMPHASIS_TIME_CONSTANT:
            begin
              FEmphasisTime := Msg.ParamL;
              FDeEmphasis.EmphasisTime := Msg.ParamL / 1000000;
            end;
          else
        end;
        GraphInvalidate;
      end
  else
    inherited
  end;
end;

procedure TRadioFMReceiver.Describe(Strs: TStrings);
begin
  if FMono then
  begin
    Strs.Add('^bMode: ^nMono');
  end
  else begin
    if FPLL.Locked then
      Strs.Add('^bMode: ^n((stereo))')
    else
      Strs.Add('^bMode: ^nMono');
  end;
end;

constructor TRadioFMReceiver.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  FPLLOutput := TRadioDataStream.Create(Self, IntToStr(1), 5 * 1024);
  FPLLOutput.PortId := 1;

  FPLL := TPLLNode.Create;
  FPLL.Bandwidth := 20;
  FPLL.FreqRange := 10;
  FPLL.DefaultFrequency := 19000;

  FAudioChain := TRealFIRNode.Create;
  FAudioChain.Connect(TDeEmphasisNode.Create).Connect(TRegulatorNode.Create);
  FDeEmphasis := FAudioChain.Next as TDeEmphasisNode;
  (FAudioChain.LastNode as TRegulatorNode).Regulator.Size := DefOutput.BufferSize;
  (FAudioChain.LastNode as TRegulatorNode).OnSendToNext := @ReceiveRegulatedData;
  FDeEmphasis.EmphasisTime := 50 / 1000000;  // 50us

  SetIIROrders(FPilotFilter, 2, 2);
end;

destructor TRadioFMReceiver.Destroy;
begin
  FPLLOutput.SafeFree;
  inherited Destroy;
end;

procedure TRadioFMReceiver.ReceiveData(const P: PComplex; const Len: Integer);
var
  I, J: Integer;
  R: PComplex;
  A, B: Double;
begin
  if FMono then
  begin
    FAudioChain.ReceiveData(P, Len);
    Exit;
  end;

  if Len > Length(FPLLPhases) then SetLength(FPLLPhases, Len);

  IIRFilter(FPilotFilter, P, @FPLLPhases[0], Len);

  FPLL.ProcessReal(@FPLLPhases[0], Len);

  FDeEmphasis.Mono := not FPLL.Locked;
  (FAudioChain as TRealFIRNode).Mono := not FPLL.Locked;

  if not FPLL.Locked then
  begin
    FAudioChain.ReceiveData(P, Len);
    Exit;
  end;

  // send out Data + PLL phases
  FPLLOutput.BufferSize := Len;
  R := Alloc(FPLLOutput, J);
  if Assigned(R) then
  begin
    for I := 0 to Len - 1 do
    begin
      R[I].re := P[I].re;
      R[I].im := FPLLPhases[I].re;
    end;
    FPLLOutput.Broadcast(J, FDataListeners);
  end
  else
    TRadioLogger.Report(llWarn, 'TRadioFMReceiver.ReceiveData: data + pll output lost');

  for I := 0 to Len - 1 do
  begin
    A := P[I].re;    // L+R
    B := 2 * P[I].re * Sin(2 * FPLLPhases[I].re);  // L-R, coherent demodulation with 2 * 19kHz

    FPLLPhases[I].re := A + B;
    FPLLPhases[I].im := A - B;
  end;
  FAudioChain.ReceiveData(@FPLLPhases[0], Len);
end;

{ TRadioFreqDiscriminator }

// Reference: http://www.digitalsignallabs.com/Digradio.pdf
// y[n] = A/2 exp(-j (2 pi f0 n Ts + f_delta integrate[x(tao), 0, n Ts]))
// y[n] * conj[y[n - 1]] = A^2 / 4 exp(-j (2 pi f0 Ts + f_delta Ts x(nTs)))
// arctan2 is in (-pi, pi)
procedure TRadioFreqDiscriminator.ReceiveRegulatedData(const P: PComplex;
  const Len: Integer);
var
  I: Integer;
  J: Integer;
  O: PComplex;
  T: Complex;
  X: Complex;
  F: Double;
begin
  if FSampleRate = 0 then Exit;
  F := Pi;  // in [0, Pi]
  O := Alloc(DefOutput, I);
  if not Assigned(O) then
  begin
    TRadioLogger.Report(llWarn, 'TRadioFreqDiscriminator.ReceiveRegulatedData: data lost');
    Exit;
  end;
  T := FLastValue;
  for J := 0 to Len - 1 do
  begin
    X := P[J] * cong(T);
    T := P[J];
    O[J].im := 0;
    if X.re <> 0 then
      O[J].re := arctan2(X.im, X.re) + F
    else
      O[J].re := IfThen(X.im > 0, Pi / 2, -Pi / 2) + F;
  end;
  CancelDC(O, Len);
  FLastValue := T;
  DefOutput.Broadcast(I, FDataListeners);
end;

function TRadioFreqDiscriminator.RMSetFrequency(const Msg: TRadioMessage;
  const Freq: Cardinal): Integer;
begin
  Result := 0;
end;

function TRadioFreqDiscriminator.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
 // if FSampleRate = Rate then Exit;
  FSampleRate := Rate;
  Result := inherited;
  Broadcast(RM_SET_FEATURE, RM_FEATURE_FREQ, 0);
end;

constructor TRadioFreqDiscriminator.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  FHasConfig := False;
  DefOutput.BufferSize := 50 * 1024;
  FRegulator := TStreamRegulator.Create;
  FRegulator.Size := DefOutput.BufferSize;
  FRegulator.OnRegulatedData := @ReceiveRegulatedData;
end;

destructor TRadioFreqDiscriminator.Destroy;
begin
  FRegulator.Free;
  inherited Destroy;
end;

procedure TRadioFreqDiscriminator.ReceiveData(const P: PComplex; const Len: Integer);
begin
  FRegulator.ReceiveData(P, Len);
end;

initialization

  RegisterModule(TRadioModuleClass(TRadioFreqDiscriminator.ClassType));
  RegisterModule(TRadioModuleClass(TRadioFMReceiver.ClassType));
  RegisterModule(TRadioModuleClass(TRDSDecoder.ClassType));

end.

