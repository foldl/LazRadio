unit rm_resampling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex, RadioModule, RadioSystem, RadioMessage, Math;

type

  TResamplingMode = (rmUnknown, rmIdentity, rmDecimation, rmInterpolation, rmResampling);

  { TRadioResampling }

  TRadioResampling = class(TRadioModule)
  const
    MAX_TAPS = 500;
  private
    FBandIndex: Integer;
    FMode: TResamplingMode;
    FRateIn: Cardinal;
    FRateOut: Cardinal;
    FInterpL: Integer;
    FDecimM: Integer;
    FBandwidth: Integer;
    FH: array of Double;
    FZ: array of Complex;
    FResampleBuff: array of Complex;
    FPhase: Integer;
    FRegulator: TStreamRegulator;
    FRegulator2: TStreamRegulator;
  private
    procedure Reconfig;
  protected
    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer; override;
    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: Integer); override;
    procedure ReceiveRegulatedData(const P: PComplex; const Len: Integer);
    procedure ReceiveRegulatedData2(const P: PComplex; const Len: Integer);
    procedure Describe(Strs: TStrings); override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

implementation

uses
  SignalBasic, util_math;

{ TRadioResampling }

procedure TRadioResampling.Reconfig;
var
  D: Integer;
  FilterRate: Cardinal;
  Taps: Integer;
  Bw: Integer;
begin
  if (FRateOut <= 0) or (FRateIn <= 0) or (FBandwidth < 1) then
  begin
    FMode := rmUnknown;
    Exit;
  end;
  D := GCD(FRateIn, FRateOut);
  FDecimM := FRateIn div D;
  FInterpL := FRateOut div D;

  if (FDecimM = 1) and (FInterpL = 1) then
  begin
    FMode := rmIdentity;
    FRegulator.Size := DefOutput.BufferSize;
    Exit;
  end;

  // LPF taps
  FilterRate := FInterpL * FRateIn;
  Bw := Min(FBandwidth, FilterRate div 2);
  Taps := Min(MAX_TAPS, Round(0.6 + 10 * (FilterRate / Bw)));

  if FInterpL = 1 then
  begin
    FMode := rmDecimation;
    SetLength(FH, Taps);
    SetLength(FZ, Taps);
    FillByte(FZ[0], Length(FZ) * SizeOf(FZ[0]), 0);
    FIRDesign(@FH[0], Taps, ftLPF, Bw / FilterRate * 2, 0, wfKaiser, 0);
    FRegulator.Size := FDecimM * DefOutput.BufferSize;
  end
  else begin
    Taps := Round(Taps / FInterpL) * FInterpL;
    SetLength(FH, Taps);
    SetLength(FZ, Taps div FInterpL);
    FillByte(FZ[0], Length(FZ) * SizeOf(FZ[0]), 0);
    FIRDesign(@FH[0], Taps, ftLPF, FBandwidth / FilterRate * 2, 0, wfKaiser, 0);

    if FDecimM = 1 then
    begin
      FMode := rmInterpolation;
      DefOutput.BufferSize := Round(DefOutput.BufferSize / FInterpL) * FInterpL;
      FRegulator.Size := DefOutput.BufferSize div FInterpL;
    end
    else begin
      FMode := rmResampling;
      FPhase := 0;
      SetLength(FResampleBuff, ((FRegulator.Size + 1) * FInterpL) div FDecimM + 1);
      FRegulator2.Size := DefOutput.BufferSize;
    end;
  end;
end;

function TRadioResampling.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  FRateIn := Rate;
  Reconfig;
  GraphInvalidate;
  Result := 0;
end;

procedure TRadioResampling.ProccessMessage(const Msg: TRadioMessage;
  var Ret: Integer);
begin
  if Msg.Id = RM_SPECTRUM_BAND_SELECT_1 + FBandIndex then
  begin
    FBandwidth := (Integer(Msg.ParamL) - Integer(Msg.ParamH)) div 4;
    Reconfig;
    GraphInvalidate;
    Ret := 0;
    Exit;
  end;

  case Msg.Id of
    RM_RESAMPLING_USE_BAND_SELECT: FBandIndex := Msg.ParamH;
    RM_RESAMPLING_CFG:
      begin
        FRateOut := Msg.ParamH;
        FBandwidth := Integer(Msg.ParamL);
        Reconfig;
        Broadcast(RM_SET_FEATURE, RM_FEATURE_SAMPLE_RATE, FRateOut);
        GraphInvalidate;
        Ret := 0;
      end
  else
    inherited;
  end;
end;

procedure TRadioResampling.ReceiveRegulatedData(const P: PComplex;
  const Len: Integer);
var
  I: Integer;
  T: Integer;
  X: PComplex;
begin
  case FMode of
    rmIdentity:
      begin
        if Len <> DefOutput.BufferSize then
        begin
          TRadioLogger.Report(llWarn, 'TRadioResampling: Len <> DefOutput.BufferSize');
          Exit;
        end;
        X := Alloc(DefOutput, I);
        if Assigned(X) then
        begin
          Move(P^, X^, Len * SizeOf(X^));
          DefOutput.Broadcast(I, FDataListeners);
        end;
      end;
    rmDecimation:
      begin
        if Len <> DefOutput.BufferSize * FDecimM then
        begin
          TRadioLogger.Report(llWarn, 'TRadioResampling: Len <> DefOutput.BufferSize * FDecimM');
          Exit;
        end;
        X := Alloc(DefOutput, I);
        if Assigned(X) then
        begin
          SignalBasic.Decimate(FDecimM, FH, FZ, P, Len, X, T);
          DefOutput.Broadcast(I, FDataListeners);
        end;
      end;
    rmInterpolation:
      begin
        if Len * FInterpL <> DefOutput.BufferSize then
        begin
          TRadioLogger.Report(llWarn, 'TRadioResampling: Len * FInterpL <> DefOutput.BufferSize');
          Exit;
        end;
        X := Alloc(DefOutput, I);
        if Assigned(X) then
        begin
          SignalBasic.Interpolate(FInterpL, FH, FZ, P, Len, X, T);
          DefOutput.Broadcast(I, FDataListeners);
        end;
      end;
    rmResampling:
      begin
        SignalBasic.Resample(FInterpL, FDecimM, FH, FZ, FPhase, P, Len, @FResampleBuff[0], T);
        FRegulator2.ReceiveData(@FResampleBuff[0], T);

        if T > Length(FResampleBuff) then
          raise Exception.Create('T > Length(FResampleBuff)');

      end;
  end;
end;

procedure TRadioResampling.ReceiveRegulatedData2(const P: PComplex;
  const Len: Integer);
var
  I: Integer;
  X: PComplex;
begin
  if Len <> DefOutput.BufferSize then
  begin
    TRadioLogger.Report(llWarn, 'TRadioResampling 2: Len <> DefOutput.BufferSize');
    Exit;
  end;
  X := Alloc(DefOutput, I);
  if Assigned(X) then
  begin
    Move(P^, X^, Len * SizeOf(X^));
    DefOutput.Broadcast(I, FDataListeners);
  end;
end;

procedure TRadioResampling.Describe(Strs: TStrings);
begin
  if FMode <> rmUnknown then
  begin
    Strs.Add(Format('^bReample Rate: ^n%d : %d', [FDecimM, FInterpL]));
    Strs.Add(Format('^bOutput Rate: ^n%d', [FRateOut]));
    if FMode <> rmIdentity then
    begin
      Strs.Add(Format('^bLPF Cutoff Freq: ^n%s', [FormatFreq(FBandwidth)]));
      Strs.Add(Format('^bLPF Taps: ^n%d', [Length(FH)]));
    end;
  end
  else
    Strs.Add('^b^1Not Running');
end;

constructor TRadioResampling.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  FRegulator := TStreamRegulator.Create;
  FRegulator.OnRegulatedData := @ReceiveRegulatedData;
  FRegulator2 := TStreamRegulator.Create;
  FRegulator2.OnRegulatedData := @ReceiveRegulatedData2;
end;

destructor TRadioResampling.Destroy;
begin
  FRegulator.Free;
  FRegulator2.Free;
  inherited Destroy;
end;

procedure TRadioResampling.ReceiveData(const P: PComplex; const Len: Integer);
begin
  if FMode <> rmUnknown then
    FRegulator.ReceiveData(P, Len);
end;

initialization

  RegisterModule(TRadioModuleClass(TRadioResampling.ClassType));

end.

