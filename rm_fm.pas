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

  TRDSDecodeState = procedure (const B: Boolean) of object;

  { TRDSDecoder }

  TRDSDecoder = class(TRadioModule)
  private
    FRadioText: string;
    FProgremmeName: string;
    FBlock: array [0..3] of Word;
    FLastTimingData: Double;
    FLastSlope: Double;
    FLastData: Double;
    FLastBit: Boolean;
    FCounter: Integer;
    FReg: Cardinal;
    FData: array of Complex;
    FRate: Integer;
    FMatchedFilter: TFIRNode;
    FBPF: TIIRFilter;
    FTimingBPF: TIIRFilter;
    FState: TRDSDecodeState;
    function  AFMap(V: Integer): Double;
    procedure DecodeBasicInfoA;
    procedure DecodeBasicInfoB;
    procedure DecodeRadioTextA;
    procedure DecodeRadioTextB;
    procedure DecodeClockTime;
    procedure GroupDecode;
    function  DecodeRec(const B: Boolean): Boolean;
    procedure DecodeB(const B: Boolean);
    procedure DecodeC(const B: Boolean);
    procedure DecodeD(const B: Boolean);
    procedure DecodeA(const B: Boolean);
    procedure Sync(const B: Boolean);
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

function EncodeMessage(M: Word): Cardinal;
function CalcSyndrome(M: Cardinal): Cardinal;
function DecodeMessage(M: Cardinal; out Decoded: Word; out ErrBits: Integer;
  const SyndromeOffset: Cardinal = 0): Boolean;

implementation

uses
  rm_filter, utils;

const
  SYNDROME_OFFSET_A  = $3d8;
  SYNDROME_OFFSET_B  = $3d4;
  SYNDROME_OFFSET_C  = $25c;
  SYNDROME_OFFSET_C2 = $3cc;
  SYNDROME_OFFSET_D  = $258;

  G = $5b9;   // x^{10, 8, 7, 5, 4, 3, 1}
  K = 16;
  N = 26;

function EncodeMessage(M: Word): Cardinal;
const
  H: array [0..15] of Cardinal =
    ( %0001110111,
      %1011100111,
      %1110101111,
      %1100001011,
      %1101011001,
      %1101110000,
      %0110111000,
      %0011011100,
      %0001101110,
      %0000110111,
      %1011000111,
      %1110111111,
      %1100000011,
      %1101011101,
      %1101110010,
      %0110111001);
var
  I: Integer;
  Q: Integer;
begin
  Q := 1 shl 15;
  Result := 0;
  for I := 0 to 15 do
  begin
    if (M and Q) <> 0 then
      Result := Result xor H[I];
    Q := Q shr 1;
  end;
  Result := (M shl 10) or Result;
end;

function CalcSyndrome(M: Cardinal): Cardinal;
const
  H: array [0..15] of Cardinal =
    ( %1011011100,
      %0101101110,
      %0010110111,
      %1010000111,
      %1110011111,
      %1100010011,
      %1101010101,
      %1101110110,
      %0110111011,
      %1000000001,
      %1111011100,
      %0111101110,
      %0011110111,
      %1010100111,
      %1110001111,
      %1100011011);
var
  I: Integer;
begin
  Result := (M shr 16) and $3FF;
  M := M and $FFFF;
  for I := 0 to 15 do
  begin
    if (M and $8000) <> 0 then
      Result := Result xor H[I];
    M := M shl 1;
  end;
end;

// ref: http://the-art-of-ecc.com/3_Cyclic_BCH/RBDS.c
function DecodeMessage(M: Cardinal; out Decoded: Word; out
  ErrBits: Integer; const SyndromeOffset: Cardinal): Boolean;
const
  K2  = 1 shl (K - 1);        // 2 ** (K - 1);
  K21 = K2 - 1;
  FLAG = 1 shl (N - K);       // 2 ** (N - K);     // 1024
  NK2  = 1 shl (N - K - 1);   // 2 ** (N - K - 1); // 512
  NK   = $7FF;                // n - k + 1 ones
  TRAP = %0000011111;
var
  S: Cardinal;
  I: Integer;
  J: Integer;
  B: Boolean;
  E: Boolean;
begin
  ErrBits := 0;
  S := CalcSyndrome(M) xor SyndromeOffset;
  M := (M shr 10) and $FFFF;
  J := K2;
  Decoded := 0;
  for I := 15 downto 0 do
  begin
    B := (J and M) <> 0;
    if (S and TRAP) = 0 then
    begin
      // check error
      if (S and NK2) <> 0 then
      begin
        B := not B;
        Inc(ErrBits);
      end
      else;
      S := (S shl 1) and NK;
    end
    else begin
      S := (S shl 1) and NK;
      if (S and FLAG) <> 0 then
        S := (S xor G) and NK;
    end;
    Decoded := (Decoded shl 1) or Ord(B);
    J := J shr 1;
  end;
  Result := (S and $3FF) = 0;
end;

{ TRDSDecoder }

function TRDSDecoder.AFMap(V: Integer): Double;
begin
  if (V >= 0) and (V <= 204) then
    Result := 87.5 + V / 10
  else
    Result := -1;
end;

procedure TRDSDecoder.DecodeBasicInfoA;
var
  F1, F2: Double;
begin
  DecodeBasicInfoB;
  F1 := AFMap(FBlock[2] shr 8);
  F2 := AFMap(FBlock[2] and $FF);
end;

procedure TRDSDecoder.DecodeBasicInfoB;
var
  I: Integer;
begin
  I := FBlock[1] and $3;
  FProgremmeName[I * 2 + 0] := Chr(FBlock[3] shr 8);
  FProgremmeName[I * 2 + 1] := Chr(FBlock[3] and $FF);

  TRadioLogger.Report(llError, 'programme = %s', [FProgremmeName]);
end;

procedure TRDSDecoder.DecodeRadioTextA;
var
  I: Integer;
begin
  I := FBlock[1] and $F;
  if Length(FRadioText) <> 64 then MakeBlankStr(FRadioText, 64);
  FRadioText[I * 4 + 0] := Chr(FBlock[2] shr 8);
  FRadioText[I * 4 + 1] := Chr(FBlock[2] and $FF);
  FRadioText[I * 4 + 2] := Chr(FBlock[3] shr 8);
  FRadioText[I * 4 + 3] := Chr(FBlock[3] and $FF);
  TRadioLogger.Report(llError, 'txt = %s', [FRadioText]);
end;

procedure TRDSDecoder.DecodeRadioTextB;
var
  I: Integer;
begin
  I := FBlock[1] and $F;
  if Length(FRadioText) <> 32 then MakeBlankStr(FRadioText, 32);
  FRadioText[I * 4 + 0] := Chr(FBlock[3] shr 8);
  FRadioText[I * 4 + 1] := Chr(FBlock[3] and $FF);
end;

procedure TRDSDecoder.DecodeClockTime;
var
  J: Integer;
  H, M: Integer;
  O: Integer;
begin
  J := (FBlock[1] and $3) shl 15 + FBlock[2] shr 1;
  H := (FBlock[2] and 1) shl 4 + FBlock[3] shr 12;
  M := (FBlock[3] shr 6) and $3F;
  O := FBlock[3] and $1F;
  if (FBlock[3] and $20) <> 0 then O := -O;
  TRadioLogger.Report(llError, 'J = %d, h = %d, m = %d, O = %d', [J, H, M, O]);
end;

procedure TRDSDecoder.GroupDecode;
begin
  case FBlock[0] and $f800 of
    $0000: DecodeBasicInfoA;
    $0800: DecodeBasicInfoB;
    $2000: DecodeRadioTextA;
    $2800: DecodeRadioTextB;
    $4000: DecodeClockTime;
  end;
end;

function TRDSDecoder.DecodeRec(const B: Boolean): Boolean;
begin
  FReg := (FReg shl 1) or Ord(B);
  Inc(FCounter);
  if FCounter >= N then
  begin
    Result := True;
    FCounter := 0;
  end
  else
    Result := False;
end;

procedure TRDSDecoder.DecodeB(const B: Boolean);
var
  E: Integer;
begin
  if not DecodeRec(B) then Exit;
  if DecodeMessage(FReg, FBlock[1], E, SYNDROME_OFFSET_B) then
    FState := @DecodeC
  else
    FState := @Sync;
end;

procedure TRDSDecoder.DecodeC(const B: Boolean);
var
  E: Integer;
begin
  if not DecodeRec(B) then Exit;
  if DecodeMessage(FReg, FBlock[2], E, SYNDROME_OFFSET_C) then
    FState := @DecodeD
  else if DecodeMessage(FReg, FBlock[2], E, SYNDROME_OFFSET_C2) then
    FState := @DecodeD
  else
    FState := @Sync;
end;

procedure TRDSDecoder.DecodeD(const B: Boolean);
var
  E: Integer;
begin
  if not DecodeRec(B) then Exit;
  if DecodeMessage(FReg, FBlock[3], E, SYNDROME_OFFSET_D) then
  begin
    GroupDecode;
    FState := @DecodeA
  end
  else
    FState := @Sync;
end;

procedure TRDSDecoder.DecodeA(const B: Boolean);
var
  E: Integer;
begin
  if not DecodeRec(B) then Exit;
  if DecodeMessage(FReg, FBlock[0], E, SYNDROME_OFFSET_A) then
    FState := @DecodeB
  else
    FState := @Sync;
end;

procedure TRDSDecoder.Sync(const B: Boolean);
var
  E: Integer;
begin
  FReg := (FReg shl 1) or Ord(B);
  if DecodeMessage(FReg, FBlock[0], E, SYNDROME_OFFSET_A) then
  begin
    FCounter := 0;
    FState := @DecodeB;
  end;
end;

procedure TRDSDecoder.ReceiveFilteredData(const P: PComplex; const Len: Integer
  );
var
  B: Boolean;
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
    FLastTimingData := P[I].re;
    if Slope > 0.0 then
    begin
      FLastSlope := Slope;
    end
    else if Slope < 0 then
    begin
      if FLastSlope > 0 then
      begin
        // sample data
        B := FLastData > 0;
        FState(B xor FLastBit);
        FLastBit := B;
      end;
      FLastSlope := Slope;
    end;

    FLastData := P[I].im;
  end;
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
  FState := @Sync;
  MakeBlankStr(FProgremmeName, 8);
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

