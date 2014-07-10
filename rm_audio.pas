unit rm_audio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioModule, UComplex, SignalBasic, Math, mmsystem,
  Windows, formauin;

const

  RM_AUDIO_IN_START = RM_USER;   // ParamH = dev id; ParamL = samples per sec
  RM_AUDIO_IN_STOP  = RM_USER + 1;

  RM_AUDIO_OUT_FMT  = RM_USER;  // ParamH as follows:
                    AUDIO_OUT_FMT_MONO_I = 0;
                    AUDIO_OUT_FMT_MONO_Q = 1;
                    AUDIO_OUT_FMT_STEREO_IQ = 2;   // I -> left, Q -> right
                    AUDIO_OUT_FMT_STEREO_QI = 3;   // Q -> left, I -> right
  RM_AUDIO_OUT_GAIN      = RM_USER + 1; // ParamH: gain in dB (cast from Integer)
  RM_AUDIO_OUT_GAIN_AUTO = RM_USER + 2; // auto adjust gain for one-shot


  // private messages
  PRIV_RM_AUDIO_IN_DATA  = RM_USER + 100;  // ParamH = WAVEHDR index
  PRIV_RM_AUDIO_IN_CLOSE = RM_USER + 101;

  PRIV_RM_AUDIO_OUT_CLOSE = RM_USER + 101;

const
  AUDIO_OUT_SAMPLE_RATE = 44100;
  AUDIO_OUT_BLOCK_NUM   = 3;

type

  { TRadioAudioIn }

  TRadioAudioIn = class(TRadioModule)
  private
    FHDRs: array [0..1] of WAVEHDR;
    FBufs: array [0..1] of array of SmallInt;
    FConfig: TAudioInForm;
    FHandle: HWAVEIN;
    FClosing: Boolean;
    procedure CloseDev;
    procedure PrepareBufs(Hwi: HWAVEIN; const SampleRate: Integer);
    procedure WaveInData(const Index: Integer);
  protected
    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: Integer); override;
    procedure DoConfigure; override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;
  end;

  { TRadioAudioOut }

  TRadioAudioOut = class(TRadioModule)
  private
    FHDRs: array [0..AUDIO_OUT_BLOCK_NUM - 1] of WAVEHDR;
    FBufs: array [0..AUDIO_OUT_BLOCK_NUM - 1] of array [0..(AUDIO_OUT_SAMPLE_RATE div 2) - 1] of SmallInt;
    FEvents: array [0..AUDIO_OUT_BLOCK_NUM - 1] of Handle;
    FHandle: HWAVEOUT;
    FAutoGain: Boolean;
    FGain: Double;
    FFmt: Integer;
    FSampleRate: Cardinal;
    FClosing: Boolean;
    FResample: TResampleNode;
    procedure CloseDev;
    procedure PrepareBufs;
    procedure ReceiveRegulatedData(const P: PComplex; const Len: Integer);
    procedure AutoGain(P: PComplex; Len: Integer);
  protected
    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer;
      override;
    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: Integer); override;
    function DoStart: Boolean; override;
    function DoStop: Boolean; override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

implementation

uses
  RadioSystem;

procedure waveInProc(
                      hwi: HWAVEIN;
                      uMsg: UINT;
                      dwInstance: PDWord;
                      dwParam1: PDWord;
                      dwParam2: PDWord); stdcall;
var
  M: TRadioAudioIn;
begin
  M := TRadioAudioIn(dwInstance);
  case uMsg of
    WIM_DATA:  RadioPostMessage(PRIV_RM_AUDIO_IN_DATA, PWAVEHDR(dwParam1)^.dwUser, 0, M);
    WIM_CLOSE: RadioPostMessage(PRIV_RM_AUDIO_IN_CLOSE, 0, 0, M);
  end;
end;

procedure waveOutProc(
                      hwo: HWAVEOUT;
                      uMsg: UINT;
                      dwInstance: PDWord;
                      dwParam1: PDWord;
                      dwParam2: PDWord); stdcall;
var
  M: TRadioAudioOut;
  H: PWAVEHDR;
begin
  M := TRadioAudioOut(dwInstance);
  case uMsg of
    WOM_DONE:
      begin
        H := PWAVEHDR(dwParam1);
        with M do
        begin
          waveOutUnprepareHeader(M.FHandle, @FHDRs[H^.dwUser], Sizeof(FHDRs[0]));
          FHDRs[H^.dwUser].dwFlags := 0;
          SetEvent(FEvents[H^.dwUser]);
        end;
      end;
    WOM_CLOSE: RadioPostMessage(PRIV_RM_AUDIO_OUT_CLOSE, 0, 0, M);
  end;
end;

{ TRadioAudioOut }

procedure TRadioAudioOut.CloseDev;
begin
  if FHandle <> 0 then
  begin
    FClosing := True;
    waveOutReset(FHandle);
    waveOutClose(FHandle);
  end;
  FHandle := 0;
end;

procedure TRadioAudioOut.PrepareBufs;
var
  N: Integer;
  I: Integer;
begin
  N := High(FBufs[0]) + 1;
  for I := 0 to High(FHDRs) do
  begin
    FillChar(FHDRs[I], SizeOf(FHDRs[0]), 0);
    with FHDRs[I] do
    begin
      lpData := @FBufs[I][0];
      dwBufferLength := N * SizeOf(FBufs[0][0]);
      dwUser := I;
    end;
  end;
end;

procedure TRadioAudioOut.ReceiveRegulatedData(const P: PComplex;
  const Len: Integer);
var
  I: Integer;
  H: Integer = -1;
  N: Integer;
  W: PSmallInt;
  T: SmallInt;
  function Scale(const X: Double): Integer; inline;
  begin
    Result := EnsureRange(Round(X * FGain), -32768, 32767);
  end;

begin
  if FAutoGain then
  begin
    AutoGain(P, Len);
    FAutoGain := False;
  end;

  TRadioLogger.Report(llVerbose, 'audio out: wait');

  H := WaitForMultipleObjects(High(FEvents) + 1, @FEvents[0], False, INFINITE) - WAIT_OBJECT_0;

  if H < 0 then Exit;
  ResetEvent(FEvents[H]);
  W := @FBufs[H][0];
  N := Min(Len - 1, High(FBufs[H]) div 2);
  FillChar(W^, (H + 1) * SizeOf(W^), 0);
  case FFmt of
      AUDIO_OUT_FMT_MONO_I:
        for I := 0 to N do
        begin
          T := Scale(P[I].re);
          W[2 * I + 0] := T;
          W[2 * I + 1] := T;
        end;
      AUDIO_OUT_FMT_MONO_Q:
        for I := 0 to N do
        begin
          T := Scale(P[I].im);
          W[2 * I + 0] := T;
          W[2 * I + 1] := T;
        end;
      AUDIO_OUT_FMT_STEREO_IQ:
        for I := 0 to N do
        begin
          W[2 * I + 0] := Scale(P[I].re);
          W[2 * I + 1] := Scale(P[I].im);
        end;
      AUDIO_OUT_FMT_STEREO_QI:
        for I := 0 to N do
        begin
          W[2 * I + 0] := Scale(P[I].im);
          W[2 * I + 1] := Scale(P[I].re);
        end;
    end;

  waveOutPrepareHeader(FHandle, @FHDRs[H], Sizeof(FHDRs[0]));
  waveOutWrite(FHandle, @FHDRs[H], Sizeof(FHDRs[0]));

  TRadioLogger.Report(llVerbose, 'audio out: buffer send out');
end;

procedure TRadioAudioOut.AutoGain(P: PComplex; Len: Integer);
var
  I: Integer;
  M: Double = 0;
  procedure Update(const V: Double); inline;
  begin
    M := Math.Max(Abs(V), M);
  end;
begin
  if Len < 1 then Exit;
  case FFmt of
      AUDIO_OUT_FMT_MONO_I:
        begin
          for I := 0 to Len - 1 do Update(P[I].re);
        end;
      AUDIO_OUT_FMT_MONO_Q:
        begin
          for I := 0 to Len - 1 do Update(P[I].im);
        end;
    else
      for I := 0 to Len - 1 do
      begin
        Update(P[I].re);
        Update(P[I].im);
      end;
    end;

  FGain := Round(65535 / Math.Max(M, 1e-100));
end;

function TRadioAudioOut.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  FResample.InputRate := Rate;
  Result := inherited;
end;

procedure TRadioAudioOut.ProccessMessage(const Msg: TRadioMessage;
  var Ret: Integer);
begin
  case Msg.Id of
    RM_AUDIO_OUT_FMT:       FFmt := Integer(Msg.ParamH);
    RM_AUDIO_OUT_GAIN:      FGain := power(10, Integer(Msg.ParamH) / 10);
    RM_AUDIO_OUT_GAIN_AUTO: FAutoGain := True;
    PRIV_RM_AUDIO_IN_CLOSE:
      begin
        // TODO:
        FClosing := False;
      end
    else
      inherited ProccessMessage(Msg, Ret);
  end;
end;

function TRadioAudioOut.DoStart: Boolean;
var
  F: WAVEFORMATEX;
begin
  Result := False;
  CloseDev;
  with F do
  begin
    wFormatTag := WAVE_FORMAT_PCM;
    nChannels  := 2;
    nSamplesPerSec := AUDIO_OUT_SAMPLE_RATE;
    wBitsPerSample := 16;
    nBlockAlign    := (nChannels * wBitsPerSample) div 8;
    nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
    cbSize          := SizeOf(F);
  end;
  if MMSYSERR_NOERROR <> waveOutOpen(
    @FHandle,
    WAVE_MAPPER,
    @F,
    DWORD_PTR(@waveOutProc),
    DWORD_PTR(Self),
    CALLBACK_FUNCTION or WAVE_FORMAT_DIRECT) then
  begin
    FHandle := 0;
    Exit;
  end;
  PrepareBufs;
  Result := inherited;
end;

function TRadioAudioOut.DoStop: Boolean;
begin
  CloseDev;
  Result := inherited;
end;

constructor TRadioAudioOut.Create(RunQueue: TRadioRunQueue);
var
  R: TRegulatorNode;
  I: Integer;
begin
  inherited Create(RunQueue);
  FResample  := TResampleNode.Create;
  FResample.OutputRate := AUDIO_OUT_SAMPLE_RATE;
  FResample.InputRate := AUDIO_OUT_SAMPLE_RATE;
  R := TRegulatorNode.Create;
  R.Regulator.Size := (High(FBufs[0]) + 1) div 2;
  R.OnSendToNext := @ReceiveRegulatedData;
  FResample.Connect(R);
  FGain := 32760;
  for I := 0 to High(FEvents) do
    FEvents[I] := CreateEvent(nil, True, True, nil);
end;

destructor TRadioAudioOut.Destroy;
var
  I: Integer;
begin
  FResample.Free;
  for I := 0 to High(FEvents) do
    CloseHandle(FEvents[I]);
  inherited Destroy;
end;

procedure TRadioAudioOut.ReceiveData(const P: PComplex; const Len: Integer);
begin
  FResample.ReceiveData(P, Len);
end;

{ TRadioAudioIn }

procedure TRadioAudioIn.CloseDev;
begin
  if FHandle <> 0 then
  begin
    FClosing := True;
    waveInReset(FHandle);
    waveInClose(FHandle);
  end;
  FHandle := 0;
end;

procedure TRadioAudioIn.PrepareBufs(Hwi: HWAVEIN; const SampleRate: Integer);
var
  I: Integer;
  N: Integer;
begin
  N := SampleRate div 4;   // 0.5s per block
  DefOutput.BufferSize := N;
  for I := 0 to High(FHDRs) do
  begin
    SetLength(FBufs[I], N);
    FillChar(FHDRs[I], SizeOf(FHDRs[0]), 0);
    with FHDRs[I] do
    begin
      lpData := @FBufs[I][0];
      dwBufferLength := N * SizeOf(FBufs[0][0]);
      dwUser := I;
    end;
    waveInPrepareHeader(Hwi, @FHDRs[I], SizeOf(FHDRs[0]));
    waveInAddBuffer(Hwi, @FHDRs[I], SizeOf(FHDRs[0]));
  end;
end;

procedure TRadioAudioIn.WaveInData(const Index: Integer);
var
  I: Integer;
  P: PComplex;
  J: Integer;
  K: Integer;
begin
  waveInUnprepareHeader(FHandle, @FHDRs[Index], Sizeof(FHDRs[Index]));
  P := DefOutput.Alloc(I);
  if Assigned(P) then
  begin
    J := Min(FHDRs[Index].dwBytesRecorded, DefOutput.BufferSize);
    for K := 0 to J - 1 do
    begin
      P[K].Re := FBufs[Index][K] / 32768;
      P[K].Im := 0;
    end;
    FillChar(P[J], (DefOutput.BufferSize - J) * SizeOf(P[0]), 0);
    DefOutput.Broadcast(I, FDataListeners);
  end;
  with FHDRs[Index] do
  begin
    dwFlags := 0;
  end;
  if not FClosing then
  begin
    waveInPrepareHeader(FHandle, @FHDRs[Index], Sizeof(FHDRs[Index]));
    waveInAddBuffer(FHandle, @FHDRs[Index], Sizeof(FHDRs[Index]));
  end;
end;

procedure TRadioAudioIn.ProccessMessage(const Msg: TRadioMessage;
  var Ret: Integer);
var
  R: MMRESULT;
  F: WAVEFORMATEX;
begin
  case Msg.Id of
    RM_AUDIO_IN_START:
      begin
        CloseDev;
        with F do
        begin
          wFormatTag := WAVE_FORMAT_PCM;
          nChannels  := 1;
          nSamplesPerSec := DWord(Msg.ParamL);
          wBitsPerSample := 16;
          nBlockAlign    := (nChannels * wBitsPerSample) div 8;
          nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
          cbSize          := SizeOf(F);
        end;
        if MMSYSERR_NOERROR <> waveInOpen(
          @FHandle,
          UINT(Msg.ParamH),
          @F,
          DWORD_PTR(@waveInProc),
          DWORD_PTR(Self),
          CALLBACK_FUNCTION or WAVE_FORMAT_DIRECT) then
        begin
          FHandle := 0;
          Exit;
        end;
        PrepareBufs(FHandle, DWord(Msg.ParamL));
        waveInStart(FHandle);
        Broadcast(RM_SET_FEATURE, RM_FEATURE_SAMPLE_RATE, Msg.ParamL);
        Broadcast(RM_SET_FEATURE, RM_FEATURE_FREQ, Msg.ParamL div 4);
      end;
    RM_AUDIO_IN_STOP: CloseDev;
    PRIV_RM_AUDIO_IN_DATA: waveInData(Integer(Msg.ParamH));
    PRIV_RM_AUDIO_IN_CLOSE:
      begin
        // TODO:
        FClosing := False;
      end
  else
    inherited;
  end;
end;

procedure TRadioAudioIn.DoConfigure;
begin
  FConfig.Show;
end;

constructor TRadioAudioIn.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  FConfig := TAudioInForm.Create(nil);
  FConfig.Module := Self;
end;

destructor TRadioAudioIn.Destroy;
begin
  CloseDev;
  FConfig.Free;
  inherited Destroy;
end;

initialization

  RegisterModule('AudioIn', TRadioModuleClass(TRadioAudioIn.ClassType));
  RegisterModule('AudioOut', TRadioModuleClass(TRadioAudioOut.ClassType));

end.

