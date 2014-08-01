unit rm_rtl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex, Math, RadioModule, RtlSdr, formrtl, RadioMessage;

const
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
    procedure Describe(Strs: TStrings); override;
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
          P[J].re := FByteMapping[Buf[2 * J + 0]];
          P[J].im := FByteMapping[Buf[2 * J + 1]];
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
        GraphInvalidate;
      end;
    RM_RTL_STOP:
      begin
        CloseDev;
        GraphInvalidate;
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
        GraphInvalidate;
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
  RtlSdrSetCenterFreq(FDev, Freq);
  Result := inherited;
end;

function TRtlModule.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  if Assigned(FDev) then RtlSdrSetSampleRate(FDev, Rate);
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

procedure TRtlModule.Describe(Strs: TStrings);
begin
  if not Assigned(FDev) then
  begin
    Strs.Add('^bNot Running');
    Exit;
  end;
  Strs.Add(Format('^bTunner Gain: ^n%.1fdB', [RtlSdrGetTunerGain(FDev) / 10]));
end;

initialization

  RegisterModule(TRadioModuleClass(TRtlModule.ClassType));

end.

