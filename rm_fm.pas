unit rm_fm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex, RadioModule, RadioSystem, RadioNode, Math, SignalBasic,
  radiomessage, rm_pll, rm_oscillator;

type

  { TRadioFreqDiscriminator }

  TRadioFreqDiscriminator = class(TRadioModule)
  private
    FRegulator: TStreamRegulator;
    FLastValue: Complex;
    FSampleRate: Cardinal;
    FLastPhase: Double;
    FMaxPhaseDiff: Double;
    FFreqDev: Integer;
    FCancelDC: Boolean;
    FSourceFm: Integer;
    procedure ReceiveRegulatedData(const P: PComplex; const Len: Integer);
  protected
    function RMSetFrequency(const Msg: TRadioMessage; const Freq: Cardinal): Integer; override;
    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer; override;
    procedure Describe(Strs: TStrings); override;
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
  const
    Td = 1/1187.5;
  private
    FOsc57: TOscRec;
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
    FPhase: array of Complex;
    FRate: Integer;
    FMatchedFilter: TFIRNode;
    FLPF: TFIRNode;
    FPLL: TPLLNode;
    FTimingBPF: TIIRFilter;
    FState: TRDSDecodeState;
    procedure DgbOutData(const P: PComplex; const Len: Integer);
    function  AFMap(V: Integer): Double;
    procedure Write2Ch(var S: string; const Index: Integer; const W: Word);
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

    procedure Algo2(const P: PComplex; const Len: Integer);
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
    procedure ProccessCustomMessage(const Msg: TRadioMessage; var Ret: Integer); override;
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
  FLAG = 1 shl (N - K);       // 2 ** (N - K);     // 1024
  NK2  = 1 shl (N - K - 1);   // 2 ** (N - K - 1); // 512
  NK   = $7FF;                // n - k + 1 ones
  TRAP = %0000011111;
var
  S: Cardinal;
  I: Integer;
  J: Integer;
  B: Boolean;
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

procedure TRDSDecoder.DgbOutData(const P: PComplex; const Len: Integer);
var
  I: Integer;
  X: PComplex;
begin
  DefOutput.BufferSize := Len;
  X := Alloc(DefOutput, I);
  if Assigned(X) then
  begin
    Move(P^, X^, Len * SizeOf(Complex));
    DefOutput.Broadcast(I, FDataListeners);
  end;
end;

function TRDSDecoder.AFMap(V: Integer): Double;
begin
  if (V >= 0) and (V <= 204) then
    Result := 87.5 + V / 10
  else
    Result := -1;
end;

procedure TRDSDecoder.Write2Ch(var S: string; const Index: Integer;
  const W: Word);
var
  X: Integer;
begin
  X := FBlock[3] shr 8;
  if (X >= Ord(' ')) and (X <= Ord('~')) then
    S[Index] := Chr(X);
  X := FBlock[3] and $FF;
  if (X >= Ord(' ')) and (X <= Ord('~')) then
    S[Index + 1] := Chr(X);
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
  Write2Ch(FProgremmeName, 2 * I, FBlock[3]);

  TRadioLogger.Report(llError, 'programme = %s', [FProgremmeName]);
end;

procedure TRDSDecoder.DecodeRadioTextA;
var
  I: Integer;
begin
  I := FBlock[1] and $F;
  if Length(FRadioText) <> 64 then MakeBlankStr(FRadioText, 64);
  Write2Ch(FRadioText, 4 * I, FBlock[2]);
  Write2Ch(FRadioText, 4 * I + 2, FBlock[3]);

  TRadioLogger.Report(llError, 'txt = %s', [FRadioText]);
end;

procedure TRDSDecoder.DecodeRadioTextB;
var
  I: Integer;
begin
  I := FBlock[1] and $F;
  if Length(FRadioText) <> 32 then MakeBlankStr(FRadioText, 32);
  Write2Ch(FRadioText, 2 * I, FBlock[3]);
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

var
  kkk: Integer = 0;

procedure TRDSDecoder.ReceiveFilteredData(const P: PComplex; const Len: Integer
  );
var
  B: Boolean;
  I, J, K: Integer;
  Slope: Double;
  T: Complex;
  Off: Double;
  Index: array of Integer;
  Sum: Double;
  MS: Double = -1;
  MI: Integer;
begin
  if Len > Length(FPhase) then
    SetLength(FPhase, Len);
{
  Off := FRate * Td;
  SetLength(Index, Trunc(Len / Off) + 1);

  for I := 0 to High(Index) do
    Index[I] := Round(I * Off);

  if High(Index) < 10 then Exit;

  for J := 0 to Index[1] - 1 do
  begin
    Sum := 0;
    for I := 0 to High(Index) do
    begin
      K := J + Index[I];
      if K < Len then
        Sum := Sum + Abs(P[K].re)
      else
        Break;
    end;
    Sum := Sum / I;
    if Sum > MS then
    begin
      MS := Sum;
      MI := J;
    end;
  end;

  for I := 0 to High(Index) do
  begin
    K := MI + Index[I];
    if K < Len then
    begin
      B := P[K].re > 0;
      FState(B xor FLastBit);
      FLastBit := B;
    end
    else
      Break;
  end;


  DumpData(P, Len, 'e:\1187.5.txt');
  Inc(kkk);
  if kkk = 20 then
    kkk := 0;
  Exit;
}

  FPLL.ProcessComplex(P, @FPhase[0], Len);

  for I := 0 to Len - 1 do
  begin
    P[I] := P[I] * Cexp(FPhase[I].re);
    P[I].re := Abs(P[I].re);
  end;   {
  DumpData(P, Len, 'e:\1187.5.txt');
  Inc(kkk);
  if kkk = 20 then
    kkk := 0;
  Exit;
        }
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
  td2 = td * td;
  td3 = td * td * td;
  td4 = td * td * td * td;
var
  h: array of Double;
  I, J, M: Integer;
  t: Double;
begin
  FRate := Integer(Rate);
  Result := 0;
  if FRate < 1 then Exit;

  with FLPF.LastNode as TRegulatorNode do
  begin
    Regulator.Size := Round(100 * FRate * td);
  end;

  FPLL.SampleRate := Rate;
  InitSimpleOsc(FOsc57, 57000, FRate);

  AudioEQFilterDesign( 1 /td, Rate, 1500, ftBPF, FTimingBPF.A, FTimingBPF.B);
  //AudioEQFilterDesign(57000, Rate, 5000, ftBPF, FTimingBPF.A, FTimingBPF.B);

  // design matched fielter
  {
     H(f) = td Cos[pi f td / 4], |f| <= 2/td
          = 0, otherwise

             8 td^2 Cos[4 pi t / td]
     h(t) = ----------------------------------
               pi (-64 t^2 + td^2)
     since it's biphase symbol, we should use h(t + td/4) - h(t - td/4), so

                (512 t td^3 Cos[(4 pi t)/td])
     h(t) =  ----------------------------------------
             (pi (4096 t^4 - 640 t^2 td^2 + 9 td^4))
  }

  // h length = 2 * td is enough
  I := Round(2 * td * FRate);
  if not Odd(I) then Inc(I);
  SetLength(h, I);
  M := I div 2;
  h[M] := 0;
  for J := 1 to M do
  begin
    t := J / FRate;
    h[M + J] := -512 * t * td3 * Cos(4 * Pi * t / td)
                / Pi / (4096 * Power(t, 4) - 640 * Sqr(td) * td2 + 9 * td4);
    h[M - J] := -h[M + J];
  end;

  FMatchedFilter.SetFIR(PDouble(@h[0]), Length(h));

  SetLength(h, 200);
  FIRDesign(@h[0], Length(h), ftLPF, 2500 / FRate * 2, 0, wfKaiser, 0);
  FLPF.SetFIR(PDouble(@h[0]), Length(h));

  //FMatchedFilter.SetFIR(PDouble(@h[0]), Length(h));

  Result := inherited; // for debugging
end;

procedure TRDSDecoder.Describe(Strs: TStrings);
begin

end;

constructor TRDSDecoder.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  SetIIROrders(FTimingBPF, 2, 2);
  FLPF := TFIRNode.Create;
  FMatchedFilter := TFIRNode.Create;

  FLPF.Connect(FMatchedFilter).Connect(TRegulatorNode.Create);
  FLPF.LastNode.OnSendToNext := @ReceiveFilteredData;

  FState := @Sync;
  MakeBlankStr(FProgremmeName, 8);

  FPLL := TPLLNode.Create;
  FPLL.Bandwidth := 50;
  FPLL.FreqRange := 100;
  FPLL.DefaultFrequency := 0;
  FHasConfig := False;
end;

destructor TRDSDecoder.Destroy;
begin
  FLPF.Free;
  inherited Destroy;
end;

var
  RDSFreq: Double = 1187.5;
  RDSPhase: Double = 0.0;
  LastClock: Boolean;
  LastSample: Double = 0.0;
  Acc: Double = 0.0;


procedure TRDSDecoder.Algo2(const P: PComplex; const Len: Integer);
var
  I: Integer;
  R: Double;
  C: Double;
  B: Boolean;
begin
  for I := 0 to Len - 1 do
  begin
    C := RDSPhase + 2 * Pi * RDSFreq / FRate;
    if C > 2 * Pi then C := C - 2 * Pi;
    B := C >= Pi;
    if B then
      Acc := Acc + P[I].re
    else
      Acc := Acc - P[I].re;
    if LastClock xor B then
    begin
      FState(Acc > 0);
      Acc := 0;
    end;

    if LastSample * P[I].re < 0 then
    begin
      R := C - Pi;
      RDSPhase := RDSPhase - 0.01 * R;
    end;
    LastClock := B;
    RDSPhase := C;
    LastSample := P[I].re;
  end;
end;

procedure TRDSDecoder.ReceiveData(const P: PComplex; const Len: Integer);
var
  I: Integer;
begin
  //Algo2(P, Len);
  //Exit;

  if Len > Length(FData) then
    SetLength(FData, Len);

{
  for I := 0 to Len - 1 do
  begin
    FData[I].re := P[I].re * Sin(-3 * P[I].im);
    FData[I].im := P[I].re * Cos(-3 * P[I].im);
  end;
}
  Move(P^, FData[0], Len * SizeOf(P^));
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
  TFilterModule.DesignBPFReal(FAudioChain as TFIRNode, 500, Rate, 20, 16000);
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

procedure TRadioFMReceiver.ProccessCustomMessage(const Msg: TRadioMessage;
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
    Strs.Add('^bMono');
  end
  else begin
    if FPLL.Locked then
      Strs.Add('(((^bstereo^n)))')
    else
      Strs.Add('^bMono');
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
  FHasConfig := False;
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
    B := 2 * P[I].re * Cos(-2 * FPLLPhases[I].re);  // L-R, coherent demodulation with 2 * 19kHz

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
//
// FM applications use peak deviations of 75 kHz (200 kHz spacing), 5 kHz (25 kHz spacing),
//    2.25 kHz (12.5 kHz spacing), and 2 kHz (8.33 kHz spacing)
procedure TRadioFreqDiscriminator.ReceiveRegulatedData(const P: PComplex;
  const Len: Integer);
var
  I: Integer;
  J: Integer;
  O: PComplex;
  T: Complex;
  X: Complex;
  A: Double;
begin
  if FSampleRate = 0 then Exit;
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
      A := arctan2(X.im, X.re)
    else
      A := IfThen(X.im > 0, Pi / 2, -Pi / 2);

    if A - FLastPhase > Pi then
      A := A - 2 * Pi
    else if A - FLastPhase < -Pi then
      A := A + 2 * Pi;

    A := EnsureRange(A, FLastPhase - FMaxPhaseDiff, FLastPhase + FMaxPhaseDiff);
    FLastPhase := A;
    O[J].re := A;
  end;

  if FCancelDC then CancelDC(O, Len);

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

  // 0.8 is chosen from trial
  if FSampleRate > 0 then
    FMaxPhaseDiff := 0.8 * 2 * Pi * FFreqDev * FSourceFm / FSampleRate / FSampleRate;
end;

procedure TRadioFreqDiscriminator.Describe(Strs: TStrings);
begin
  Strs.Add(Format('^bFreq Deviation : ^n%s', [FormatFreq(FFreqDev)]));
  Strs.Add(Format('^bSignal Max Freq: ^n%s', [FormatFreq(FSourceFm)]));
  Strs.Add(Format('^bDC Cancellation: ^n%s', [BoolToStr(FCancelDC)]));
end;

constructor TRadioFreqDiscriminator.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  FHasConfig := False;
  DefOutput.BufferSize := 50 * 1024;
  FRegulator := TStreamRegulator.Create;
  FRegulator.Size := DefOutput.BufferSize;
  FRegulator.OnRegulatedData := @ReceiveRegulatedData;
  FFreqDev := 75000;
  FSourceFm := 60000;
  FHasConfig := False;
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

