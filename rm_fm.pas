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
    FBandIndex: Integer;
    FCarrierFreq: Cardinal;
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

  // TRadioFMReceiver must be following a TRadioFreqDiscriminator
  { TRadioFMReceiver }

  TRadioFMReceiver = class(TRadioModule)
  private
    FSampleRate: Integer;
    FPLL: TPLLNode;
    FMono: Boolean;
    FEmphasisTime: Cardinal;
    FDeEmphasis: TDeEmphasisNode;
    FMonoChain: TDataFlowNode;     // BFP -> De-emphasis -> Regulator
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
  TFilterModule.DesignBPFComplex(FMonoChain as TFIRNode, 200, Rate, 30, 15000);
  (FMonoChain.Next as TDeEmphasisNode).SampleRate := Rate;
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
            FMono := Msg.ParamL = FMRECEIVER_MONO;
          FMRECEIVER_DEEMPHASIS_TIME_CONSTANT:
            begin
              (FMonoChain.Next as TDeEmphasisNode).EmphasisTime := Msg.ParamL / 1000000;
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
var
  FIR: TFIRNode;
  De: TDeEmphasisNode;
begin
  inherited Create(RunQueue);
  FPLL := TPLLNode.Create;
  FPLL.Bandwidth := 20;
  FPLL.FreqRange := 10;
  FPLL.DefaultFrequency := 19000;

  FMonoChain := TFIRNode.Create;
  FMonoChain.Connect(TDeEmphasisNode.Create).Connect(TRegulatorNode.Create);
  (FMonoChain.LastNode as TRegulatorNode).Regulator.Size := DefOutput.BufferSize;
  (FMonoChain.LastNode as TRegulatorNode).OnSendToNext := @ReceiveRegulatedData;
  (FMonoChain.Next as TDeEmphasisNode).EmphasisTime := 50 / 1000000;  // 50us
  (FMonoChain.Next as TDeEmphasisNode).Mono := True;
  FMono := True;
end;

destructor TRadioFMReceiver.Destroy;
begin
  inherited Destroy;
end;

procedure TRadioFMReceiver.ReceiveData(const P: PComplex; const Len: Integer);
begin
  if FMono then
  begin
    FMonoChain.ReceiveData(P, Len);
  end;
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

end.

