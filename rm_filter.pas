unit rm_filter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioSystem, RadioModule, SignalBasic, UComplex, formfilter,
  rm_spectrum, Math;

const
  RM_FILTER_SET      = RM_USER;             // ParamH: Coeff(PDouble); ParamL: (SampleRate << 8) or (Filter taps)
  RM_FILTER_REDESIGN = RM_USER + 1;         // Apply RM_FILTER_CONFIG settings
  RM_FILTER_CONFIG   = RM_USER + 2;
                   FILTER_TYPE       = 0;   // ParamL: TFilterType
                   FILTER_OMEGA      = 1;   // ParamL: Omega (Hz)
                   FILTER_BANDWIDTH  = 2;   // ParamL: Bandwidth (Hz)
                   FILTER_TAPS       = 3;   // ParamL: Taps
                   FILTER_WINDOW     = 4;   // ParamL: TWindowFunction
                   FILTER_WINDOW_PARAM = 5; // ParamL: param1 of window function (Single)
type

  { TFilterModule }

  TFilterModule = class(TRadioModule)
  private
    FNode: TDataFlowNode;
    FResampleNode: TResampleNode;
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
    procedure ReceiveFIRData(const P: PComplex; const Len: Integer);
    procedure DesignBPF(LowFreq, HighFreq: Cardinal);
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
var
  Omega: Double;
  Bw: Double;
  Coeff: array of Double;
  N: Integer;
begin
  N := FTaps;
  if FResampleNode.OutputRate < 1 then Exit;
  if N < 5 then Exit;
  if (FType = ftHPF) and (not odd(N)) then Inc(N);
  SetLength(Coeff, N);
  Omega := FOmega / FResampleNode.OutputRate * 2;
  Bw := FBandwidth / FResampleNode.OutputRate * 2;
  FIRDesign(@Coeff[0], N, FType,
            Omega, Bw,
            FWnd,
            FWndParam);
  FFIRNode.SetFIR(PDouble(@Coeff[0]), N);
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
  X := DefOutput.Alloc(J);
  if not Assigned(X) then
  begin
    TRadioLogger.Report(llWarn,
                        'TFilterModule.ReceiveFIRData: Alloc failed, data lost');
    Exit;
  end;
  Move(P^, X^, I * SizeOf(X^));
  DefOutput.Broadcast(J, FDataListeners);
end;

procedure TFilterModule.DesignBPF(LowFreq, HighFreq: Cardinal);
var
  F: Cardinal;
begin
  F := FResampleNode.OutputRate div 2;
  FType := ftBPF;
  FOmega := (LowFreq + HighFreq) div 2;
  FBandwidth := HighFreq - LowFreq;
  if LowFreq / F < 1e-3 then
  begin
    FType := ftLPF;
    FOmega := HighFreq;
  end;
  if HighFreq / F > (1 - 1e-3) then
  begin
    FType := ftHPF;
    FOmega := LowFreq;
  end;

  Redesign;
end;

procedure TFilterModule.ProccessMessage(const Msg: TRadioMessage;
  var Ret: Integer);
begin
  if Msg.Id = RM_SPECTRUM_BAND_SELECT_1 + FBandIndex then
  begin
    FResampleNode.OutputRate := FResampleNode.InputRate;
    DesignBPF(Msg.ParamH, Msg.ParamL);
    Exit;
  end;

  case Msg.Id of
    RM_FILTER_SET:
      begin
        FResampleNode.OutputRate := Cardinal(Msg.ParamL) shr 8;
        FFIRNode.SetFIR(PDouble(Msg.ParamH), Msg.ParamL and $FF);
      end;
    RM_FILTER_REDESIGN:
      begin
        FResampleNode.OutputRate := FResampleNode.InputRate;
        Redesign;
      end;
    RM_FILTER_CONFIG:
      begin
        case Msg.ParamH of
          FILTER_TYPE:                       FType := TFilterType(Msg.ParamL);
          FILTER_OMEGA:                      FOmega := Msg.ParamL;
          FILTER_BANDWIDTH:                  FBandwidth := Msg.ParamL;
          FILTER_TAPS:                       FTaps := Msg.ParamL;
          FILTER_WINDOW:                     FWnd := TWindowFunction(Msg.ParamL);
          FILTER_WINDOW_PARAM:               FWndParam := PSingle(@Msg.ParamL)^;
        end;
      end;
  else
    inherited;
  end;
end;

function TFilterModule.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  FResampleNode.InputRate := Rate;
  FConfig.EditRate.Text := IntToStr(Rate);
  Result := 0;
  Broadcast(Msg);
end;

procedure TFilterModule.DoConfigure;
begin
  FConfig.EditRate.Text := IntToStr(FResampleNode.InputRate);
  FConfig.Show;
end;

constructor TFilterModule.Create(RunQueue: TRadioRunQueue);
var
  R: TRegulatorNode;
begin
  inherited Create(RunQueue);
  FResampleNode := TResampleNode.Create;
  FFIRNode      := TFIRNode.Create;
  R             := TRegulatorNode.Create;
  FResampleNode.Connect(FFIRNode);
  FFIRNode.Connect(R);
  FNode         := FResampleNode;
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

