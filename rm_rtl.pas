unit rm_rtl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex, Math, RadioModule, RtlSdr, formrtl;

const
  // ParamH: Device index
  RM_RTL_START = RM_USER + 0;
  RM_RTL_STOP  = RM_USER + 1;

  RM_RTL_DEV_CTL = RM_USER + 2;
                 RTL_SET_FREQ_CORRECTION  = 0;   // ParamL = (ppm)
                 RTL_SET_TUNNER_GAIN_MODE = 1;   // ParamL: 0 (auto), 1 (manual), 2 (software)
                                          RTL_TUNNER_GAIN_AUTO     = 0;
                                          RTL_TUNNER_GAIN_MANUAL   = 1;
                                          RTL_TUNNER_GAIN_SOFTWARE = 2;
                 RTL_SET_TUNNER_GAIN      = 2;   // ParamL = gain (Integer)
                 RTL_SET_TUNNER_IF_GAIN   = 3;   // ParamL = Stage << 16 | Gain (tenth dB)
                 RTL_SET_AGC_MODE         = 4;   // ParamL: Enable (1) or disable (0) the internal digital AGC
                 RTL_SET_DIRECT_SAMPLING  = 5;   // ParamL: 0 means disabled, 1 I-ADC input enabled, 2 Q-ADC input enabled
                                          RTL_SAMPLING_QUAD = 0;
                                          RTL_SAMPLING_I    = 1;
                                          RTL_SAMPLING_Q    = 2;
                 RTL_SET_OFFSET_TUNNING   = 6;   // ParamL: 0 means disabled, 1 enabled

  PRIV_RM_RTL_DATA = RM_USER + 3;         // ParamH: PByte; ParamL: Len
type

  { TRtlModule }

  TRtlModule = class(TBackgroundRadioModule)
  private
    FEvent: PRTLEvent;
    FDev: PRtlSdrDev;
    FClosing: Boolean;
    FConfig: TRTLForm;
    FSoftAGC: Boolean;
    FSampleFormat: Integer;
    FByteMapping: array [0..255] of Double;
    FGains: array of Integer;
    FFreq: Cardinal;
    FSampleRate: Cardinal;
    function  DevOK: Boolean;
    procedure CloseDev;
    procedure DataIn(Buf: PByte; const Len: Cardinal);
  protected
    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: Integer); override;
    procedure DoConfigure; override;
    function RMSetFrequency(const Msg: TRadioMessage; const Freq: Cardinal): Integer; override;
    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer; override;
    procedure ThreadFun(Thread: TGenericRadioThread); override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;
  end;

implementation

uses
  RadioSystem, Utils;

procedure SDRCallback(Buf: PByte; const Len: Cardinal; M: TRtlModule); cdecl;
begin
  RadioPostMessage(PRIV_RM_RTL_DATA, PtrUInt(Buf), Len, M);
end;

{ TRtlModule }

function TRtlModule.DevOK: Boolean;
begin
  Result := Assigned(FDev);
end;

procedure TRtlModule.CloseDev;
begin
  if Assigned(FDev) then
  begin
    FClosing := True;
    RtlSdrCancelAsync(FDev);
    RtlSdrClose(FDev);
    FDev := nil;
    FClosing := False;
  end;
end;

procedure TRtlModule.DataIn(Buf: PByte; const Len: Cardinal);
var
  P: PComplex;
  I: Integer;
  J: Integer;
  procedure fill;
  var
    J: Integer;
  begin
    if FSampleFormat = RTL_SAMPLING_I then
      for J := 0 to (Len div 2) - 1 do
      begin
        P[J].re := FByteMapping[Buf[J]];
        P[J].im := 0;
      end
    else
      for J := 0 to (Len div 2) - 1 do
      begin
        P[J].im := FByteMapping[Buf[J]];
        P[J].re := 0;
      end
  end;

begin
  if FClosing then Exit;
  P := DefOutput.TryAlloc(I);
  if not Assigned(P) then
  begin
    TRadioLogger.Report(llWarn, 'TRtlModule.DataIn: data lost');
    Exit;
  end;

  case FSampleFormat of
    RTL_SAMPLING_QUAD:
      begin
        for J := 0 to (Len div 2) - 1 do
        begin
          P[J].im := FByteMapping[Buf[2 * J + 0]];
          P[J].re := FByteMapping[Buf[2 * J + 1]];
        end;
      end;
    else
      Fill;
      DefOutput.Broadcast(I, FDataListeners);
      P := DefOutput.TryAlloc(I);
      Inc(Buf, Len div 2);
      if not Assigned(P) then
      begin
        TRadioLogger.Report(llWarn, 'TRtlModule.DataIn: data lost');
        Exit;
      end;
      Fill;
  end;
  DefOutput.Broadcast(I, FDataListeners);
end;

procedure TRtlModule.ProccessMessage(const Msg: TRadioMessage; var Ret: Integer
  );
var
  C: Integer;
begin
  Ret := 0;
  case Msg.Id of
    RM_RTL_START:
      begin
        CloseDev;
        if RtlSdrOpen(FDev, Msg.ParamH) <> 0 then
        begin
          CloseDev;
          Exit;
        end;
        C := RtlSdrGetTunerGains(FDev, nil);
        if C > 0 then
        begin
          SetLength(FGains, C);
          RtlSdrGetTunerGains(FDev, @FGains[0]);
        end
        else
          SetLength(FGains, 0);
        RtlSdrSetSampleRate(FDev, FSampleRate);
        RtlSdrSetCenterFreq(FDev, FFreq);

        RtlSdrSetTunerGainMode(FDev, 0);
        //RtlSdrSetOffsetTuning(FDev, 0);
        //RtlSdrSetDirectSampling(FDev, FSampleFormat);

        RtlSdrResetBuffer(FDev);
        FSampleFormat := RtlSdrGetDirectSampling(FDev);
        RTLeventSetEvent(FEvent);
      end;
    RM_RTL_STOP:
      begin
        CloseDev;
      end;
    RM_RTL_DEV_CTL:
      begin
        if not Assigned(FDev) then Exit;
        case Msg.ParamH of
          RTL_SET_FREQ_CORRECTION:
            RtlSdrSetFreqCorrection(FDev, Msg.ParamL);
          RTL_SET_TUNNER_GAIN_MODE:
            begin
              FSoftAGC := Msg.ParamL = RTL_TUNNER_GAIN_SOFTWARE;
              RtlSdrSetTunerGainMode(FDev,
                                     IfThen(Msg.ParamL = RTL_TUNNER_GAIN_AUTO,
                                            0,
                                            1));
            end;
          RTL_SET_TUNNER_GAIN:
            begin
              C := FindNearest(FGains, Msg.ParamL, -10000);
              if C <> -10000 then
                RtlSdrSetTunerGain(FDev,  C);
            end;
          RTL_SET_TUNNER_IF_GAIN:
            RtlSdrSetTunerIfGain(FDev, Msg.ParamL shr 16, Msg.ParamL and $FFFF);
          RTL_SET_AGC_MODE:
            RtlSdrSetAgcMode(FDev, Msg.ParamL);
          RTL_SET_DIRECT_SAMPLING:
            begin
              FSampleFormat := Msg.ParamL;
            end;
          RTL_SET_OFFSET_TUNNING:
            RtlSdrSetOffsetTuning(FDev, Msg.ParamL);
        end;
      end;
    PRIV_RM_RTL_DATA:
      DataIn(PByte(Msg.ParamH), Msg.ParamL);
  else
    inherited;
  end;
end;

constructor TRtlModule.Create(RunQueue: TRadioRunQueue);
var
  I: Integer;
begin
  inherited;
  for I := 0 to High(FByteMapping) do
    FByteMapping[I] := (I - 128) / 128;

  FEvent := RTLEventCreate;
  FConfig := TRTLForm.Create(nil);
  FConfig.Module := Self;
  DefOutput.BufferSize := 1024 * 100;
  FFreq                := 103000000;
  FSampleRate          :=   2048000;
end;

destructor TRtlModule.Destroy;
begin
  CloseDev;
  RTLeventSetEvent(FEvent);
  RTLeventdestroy(FEvent);
  FConfig.Free;
  RtlSdrClose(FDev);
  inherited Destroy;
end;

procedure TRtlModule.DoConfigure;
begin
  FConfig.Show;
end;

function TRtlModule.RMSetFrequency(const Msg: TRadioMessage;
  const Freq: Cardinal): Integer;
begin
  Result := 0;
  FFreq := Freq;
  if Assigned(FDev) then RtlSdrSetCenterFreq(FDev, Freq);
  Result := inherited;
end;

function TRtlModule.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  FSampleRate := Rate;
  Result := inherited;
end;

procedure TRtlModule.ThreadFun(Thread: TGenericRadioThread);
begin
  while True do
  begin
    RTLeventWaitFor(FEvent);
    RTLeventResetEvent(FEvent);
    if not Assigned(FDev) then
      Break;
    RtlSdrReadASync(FDev, TRtlSdrReadAsyncCB(@SDRCallback), Self, 0, DefOutput.BufferSize * 2);
  end;
end;

initialization

  RegisterModule('Rtl', TRadioModuleClass(TRtlModule.ClassType));

end.

