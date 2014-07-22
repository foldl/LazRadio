unit rm_filter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioSystem, RadioModule, SignalBasic, UComplex, formfilter,
  rm_spectrum, Math, radiomessage;


type

  { TFilterModule }

  TFilterModule = class(TRadioModule)
  private
    FCoeffDomain: Integer;
    FSampleRate: Cardinal;
    FNode: TDataFlowNode;
    FFIRNode: TFIRNode;
    FTaps: Integer;
    FType: TFilterType;
    FWnd: TWindowFunction;
    FOmega: Integer;
    FBandwidth: Integer;
    FWndParam: Double;
    FConfig: TFilterForm;
    FBandIndex: Integer;
    procedure Redesign;
    procedure RedesignReal;
    procedure RedesignComplex;
    procedure ReceiveFIRData(const P: PComplex; const Len: Integer);
    procedure DesignBPF(LowFreq, HighFreq: Integer);
    procedure DesignBPFReal(LowFreq, HighFreq: Integer);
    procedure DesignBPFComplex(LowFreq, HighFreq: Integer);
  protected
    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: Integer); override;
    function  RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer;
      override;
    procedure DoConfigure; override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

implementation

{ TFilterModule }

procedure TFilterModule.Redesign;
begin
  if FCoeffDomain = FILTER_COEFF_DOMAIN_REAL then
    RedesignReal
  else
    RedesignComplex;
end;

procedure TFilterModule.RedesignReal;
var
  Omega: Double;
  Bw: Double;
  Coeff: array of Double;
  N: Integer;
begin
  N := FTaps;
  if FSampleRate < 1 then Exit;
  if N < 5 then Exit;
  if (FType = ftHPF) and (not Odd(N)) then Inc(N);
  SetLength(Coeff, N);
  Omega := FOmega / FSampleRate * 2;
  Bw := FBandwidth / FSampleRate * 2;
  FIRDesign(@Coeff[0], N, FType,
            Omega, Bw,
            FWnd,
            FWndParam);
  FFIRNode.SetFIR(PDouble(@Coeff[0]), N);
end;

procedure TFilterModule.RedesignComplex;
var
  Omega: Double;
  Coeff: array of Double;
  C: array of Complex;
  N: Integer;
  I: Integer;
  P: Complex = (re: 0; im: 0);
begin
  case FType of
    ftLPF, ftHPF:
      begin
        RedesignReal;
        Exit;
      end;
  end;
  N := FTaps;
  if FSampleRate < 1 then Exit;
  if N < 5 then Exit;
  SetLength(Coeff, N);
  SetLength(C, N);

  Omega := FBandwidth / FSampleRate;
  if Omega >= 1 then
  begin
    case FType of
      ftBPF:
        begin
          Omega := 1;
          FFIRNode.SetFIR(PDouble(@Omega), 1);
        end;
      ftBSF:
        begin
          Omega := 0;
          FFIRNode.SetFIR(PDouble(@Omega), 1);
        end;
    end;
    Exit;
  end;

  case FType of
    ftBPF:
      FIRDesign(@Coeff[0], N, ftLPF,
            Omega, 0,
            FWnd,
            FWndParam);
    ftBSF:
      FIRDesign(@Coeff[0], N, ftHPF,
            Omega, 0,
            FWnd,
            FWndParam);
  end;

  Omega := FOmega;
  for I := 0 to N - 1 do
  begin
    P.im := 2 * Pi * FOmega * I / FSampleRate;
    C[I] := Coeff[I] * cexp(P);
  end;
  FFIRNode.SetFIR(PComplex(@C[0]), N);
end;

procedure TFilterModule.ReceiveFIRData(const P: PComplex; const Len: Integer);
var
  I: Integer;
  J: Integer;
  X: PComplex;
begin
  if Len <> DefOutput.BufferSize then
  begin
    TRadioLogger.Report(llWarn,
                        'TFilterModule.ReceiveFIRData: Len(%d) <> DefOutput.BufferSize(%d)',
                        [Len, DefOutput.BufferSize]);
    I := Min(Len, DefOutput.BufferCount);
  end
  else
    I := Len;
  X := Alloc(DefOutput, J);
  if not Assigned(X) then
  begin
    TRadioLogger.Report(llWarn,
                        'TFilterModule.ReceiveFIRData: Alloc failed, data lost');
    Exit;
  end;
  Move(P^, X^, I * SizeOf(X^));
  DefOutput.Broadcast(J, FDataListeners);
end;

procedure TFilterModule.DesignBPF(LowFreq, HighFreq: Integer);
begin
  if FCoeffDomain = FILTER_COEFF_DOMAIN_REAL then
    DesignBPFReal(LowFreq, HighFreq)
  else
    DesignBPFComplex(LowFreq, HighFreq);
end;

procedure TFilterModule.DesignBPFReal(LowFreq, HighFreq: Integer);
var
  K: Double = 1.0;
begin
  FType := ftBPF;
  FOmega := (LowFreq + HighFreq) div 2;
  FBandwidth := HighFreq - LowFreq;
  if LowFreq / FSampleRate < 1e-3 then
  begin
    FType := ftLPF;
    FOmega := HighFreq;
  end;
  if HighFreq / FSampleRate > (1 - 1e-3) then
  begin
    if FType = ftBPF then
    begin
      FType := ftHPF;
      FOmega := LowFreq;
    end
    else begin
      // full pass
      FFIRNode.SetFIR(PDouble(@K), 1);
      Exit;
    end;
  end;

  RedesignReal;
end;

procedure TFilterModule.DesignBPFComplex(LowFreq, HighFreq: Integer);
begin
  FType := ftBPF;
  FOmega := (LowFreq + HighFreq) div 2;
  FBandwidth := HighFreq - LowFreq;
  RedesignComplex;
end;

procedure TFilterModule.ProccessMessage(const Msg: TRadioMessage;
  var Ret: Integer);
begin
  if Msg.Id = RM_SPECTRUM_BAND_SELECT_1 + FBandIndex then
  begin
    DesignBPF(Integer(Msg.ParamH), Integer(Msg.ParamL));
    Exit;
  end;

  case Msg.Id of
    RM_FILTER_SET:
      begin
        FFIRNode.SetFIR(PComplex(Msg.ParamH), Msg.ParamL, False);
      end;
    RM_FILTER_REDESIGN:  Redesign;
    RM_FILTER_CONFIG:
      begin
        case Msg.ParamH of
          FILTER_TYPE:                       FType := TFilterType(Msg.ParamL);
          FILTER_OMEGA:                      FOmega := Msg.ParamL;
          FILTER_BANDWIDTH:                  FBandwidth := Msg.ParamL;
          FILTER_TAPS:                       FTaps := Msg.ParamL;
          FILTER_WINDOW:                     FWnd := TWindowFunction(Msg.ParamL);
          FILTER_WINDOW_PARAM:               FWndParam := PSingle(@Msg.ParamL)^;
          FILTER_COEFF_DOMAIN:               FCoeffDomain := Msg.ParamL;
        end;
      end;
  else
    inherited;
  end;
end;

function TFilterModule.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  FSampleRate := Rate;
  FConfig.EditRate.Text := IntToStr(Rate);
  Result := inherited;
end;

procedure TFilterModule.DoConfigure;
begin
  FConfig.EditRate.Text := IntToStr(FSampleRate);
  FConfig.Show;
end;

constructor TFilterModule.Create(RunQueue: TRadioRunQueue);
var
  R: TRegulatorNode;
begin
  inherited Create(RunQueue);
  FFIRNode      := TFIRNode.Create;
  R             := TRegulatorNode.Create;
  FFIRNode.Connect(R);
  FNode         := FFIRNode;
  R.Regulator.Size := DefOutput.BufferSize;
  R.OnSendToNext := @ReceiveFIRData;
  FWnd           := wfKaiser;
  FWndParam      := -1;
  FTaps          := 64;
  FConfig := TFilterForm.Create(nil);
  FConfig.Module := Self;
end;

destructor TFilterModule.Destroy;
begin
  FConfig.Free;
  FNode.Free;
  inherited Destroy;
end;

procedure TFilterModule.ReceiveData(const P: PComplex; const Len: Integer);
begin
  FNode.ReceiveData(P, Len);
end;

initialization

  RegisterModule('Filter', TRadioModuleClass(TFilterModule.ClassType));

end.

