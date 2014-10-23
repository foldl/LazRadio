unit rm_audio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioModule, RadioNode, UComplex, SignalBasic, Math,
  mmsystem,
  Windows,
  MMDeviceAPI,
  formauin, radiomessage;

const

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
    FRate: Cardinal;
    procedure CloseDev;
    procedure PrepareBufs(Hwi: HWAVEIN; const SampleRate: Integer);
    procedure WaveInData(const Index: Integer);
  protected
    procedure ProccessCustomMessage(const Msg: TRadioMessage; var Ret: Integer); override;
    procedure DoConfigure; override;
    procedure Describe(Strs: TStrings); override;
    procedure DoSyncDestroy; override;
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
    FGainDb: Integer;
    FFmt: Integer;
    FClosing: Boolean;
    FResample: TResampleNode;
    procedure Lock;
    procedure Unlock;
    procedure CloseDev;
    procedure PrepareBufs;
    procedure ReceiveRegulatedData(const P: PComplex; const Len: Integer);
    procedure AutoGain(P: PComplex; Len: Integer);
  protected
    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer;
      override;
    procedure ProccessCustomMessage(const Msg: TRadioMessage; var Ret: Integer); override;
    procedure DoStart;
    procedure DoBeforeDestroy; override;
    procedure Describe(Strs: TStrings); override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

implementation

uses
  RadioSystem;

{
http://msdn.microsoft.com/en-us/library/ms679146(VS.85).aspx
Loopback Recording

//-----------------------------------------------------------
// Record an audio stream from the default audio capture
// device. The RecordAudioStream function allocates a shared
// buffer big enough to hold one second of PCM audio data.
// The function uses this buffer to stream data from the
// capture device. The main loop runs every 1/2 second.
//-----------------------------------------------------------

// REFERENCE_TIME time units per second and per millisecond
#define REFTIMES_PER_SEC  10000000
#define REFTIMES_PER_MILLISEC  10000

#define EXIT_ON_ERROR(hres)  \
              if (FAILED(hres)) { goto Exit; }
#define SAFE_RELEASE(punk)  \
              if ((punk) != NULL)  \
                { (punk)->Release(); (punk) = NULL; }

const CLSID CLSID_MMDeviceEnumerator = __uuidof(MMDeviceEnumerator);
const IID IID_IMMDeviceEnumerator = __uuidof(IMMDeviceEnumerator);
const IID IID_IAudioClient = __uuidof(IAudioClient);
const IID IID_IAudioCaptureClient = __uuidof(IAudioCaptureClient);

HRESULT RecordAudioStream(MyAudioSink *pMySink)
{
    HRESULT hr;
    REFERENCE_TIME hnsRequestedDuration = REFTIMES_PER_SEC;
    REFERENCE_TIME hnsActualDuration;
    UINT32 bufferFrameCount;
    UINT32 numFramesAvailable;
    IMMDeviceEnumerator *pEnumerator = NULL;
    IMMDevice *pDevice = NULL;
    IAudioClient *pAudioClient = NULL;
    IAudioCaptureClient *pCaptureClient = NULL;
    WAVEFORMATEX *pwfx = NULL;
    UINT32 packetLength = 0;
    BOOL bDone = FALSE;
    BYTE *pData;
    DWORD flags;

    hr = CoCreateInstance(
           CLSID_MMDeviceEnumerator, NULL,
           CLSCTX_ALL, IID_IMMDeviceEnumerator,
           (void**)&pEnumerator);
    EXIT_ON_ERROR(hr)

    hr = pEnumerator->GetDefaultAudioEndpoint(
                        eRender, eConsole, &pDevice);
    EXIT_ON_ERROR(hr)

    hr = pDevice->Activate(
                    IID_IAudioClient, CLSCTX_ALL,
                    NULL, (void**)&pAudioClient);
    EXIT_ON_ERROR(hr)

    hr = pAudioClient->GetMixFormat(&pwfx);
    EXIT_ON_ERROR(hr)

    hr = pAudioClient->Initialize(
                         AUDCLNT_SHAREMODE_SHARED,
                         AUDCLNT_STREAMFLAGS_LOOPBACK,
                         hnsRequestedDuration,
                         0,
                         pwfx,
                         NULL);
    EXIT_ON_ERROR(hr)

    // Get the size of the allocated buffer.
    hr = pAudioClient->GetBufferSize(&bufferFrameCount);
    EXIT_ON_ERROR(hr)

    hr = pAudioClient->GetService(
                         IID_IAudioCaptureClient,
                         (void**)&pCaptureClient);
    EXIT_ON_ERROR(hr)

    // Notify the audio sink which format to use.
    hr = pMySink->SetFormat(pwfx);
    EXIT_ON_ERROR(hr)

    // Calculate the actual duration of the allocated buffer.
    hnsActualDuration = (double)REFTIMES_PER_SEC *
                     bufferFrameCount / pwfx->nSamplesPerSec;

    hr = pAudioClient->Start();  // Start recording.
    EXIT_ON_ERROR(hr)

    // Each loop fills about half of the shared buffer.
    while (bDone == FALSE)
    {
        // Sleep for half the buffer duration.
        Sleep(hnsActualDuration/REFTIMES_PER_MILLISEC/2);

        hr = pCaptureClient->GetNextPacketSize(&packetLength);
        EXIT_ON_ERROR(hr)

        while (packetLength != 0)
        {
            // Get the available data in the shared buffer.
            hr = pCaptureClient->GetBuffer(
                                   &pData,
                                   &numFramesAvailable,
                                   &flags, NULL, NULL);
            EXIT_ON_ERROR(hr)

            if (flags & AUDCLNT_BUFFERFLAGS_SILENT)
            {
                pData = NULL;  // Tell CopyData to write silence.
            }

            // Copy the available capture data to the audio sink.
            hr = pMySink->CopyData(
                              pData, numFramesAvailable, &bDone);
            EXIT_ON_ERROR(hr)

            hr = pCaptureClient->ReleaseBuffer(numFramesAvailable);
            EXIT_ON_ERROR(hr)

            hr = pCaptureClient->GetNextPacketSize(&packetLength);
            EXIT_ON_ERROR(hr)
        }
    }

    hr = pAudioClient->Stop();  // Stop recording.
    EXIT_ON_ERROR(hr)

Exit:
    CoTaskMemFree(pwfx);
    SAFE_RELEASE(pEnumerator)
    SAFE_RELEASE(pDevice)
    SAFE_RELEASE(pAudioClient)
    SAFE_RELEASE(pCaptureClient)

    return hr;
}
}
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
        with M do
        begin
          Lock;
          if not FClosing then
          begin
            waveOutUnprepareHeader(FHandle, @FHDRs[PWAVEHDR(dwParam1)^.dwUser], Sizeof(FHDRs[0]));
          end;
          Unlock;

          FHDRs[PWAVEHDR(dwParam1)^.dwUser].dwFlags := 0;
          SetEvent(FEvents[PWAVEHDR(dwParam1)^.dwUser]);
        end;
      end;
    WOM_CLOSE: RadioPostMessage(PRIV_RM_AUDIO_OUT_CLOSE, 0, 0, M);
  end;
end;

{ TRadioAudioOut }

procedure TRadioAudioOut.Lock;
begin
  RadioGlobalLock;
end;

procedure TRadioAudioOut.Unlock;
begin
  RadioGlobalUnlock;
end;

procedure TRadioAudioOut.CloseDev;
begin
  if FHandle <> 0 then
  begin
    Lock;
    FClosing := True;
    UnLock;
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

  H := WaitForMultipleObjects(Length(FEvents), @FEvents[0], False, INFINITE) - WAIT_OBJECT_0;

  if FClosing or (H < 0) then
  begin
    TRadioLogger.Report(llWarn, 'TRadioAudioOut.ReceiveRegulatedData: no buffer, data lost');
    Exit;
  end;

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

  FGain := Round(65000 / Math.Max(M, 1e-100));
end;

function TRadioAudioOut.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  FResample.InputRate := Rate;
  Result := inherited;
end;

procedure TRadioAudioOut.ProccessCustomMessage(const Msg: TRadioMessage;
  var Ret: Integer);
begin
  case Msg.Id of
    RM_AUDIO_OUT_FMT:
      begin
        FFmt := Integer(Msg.ParamH);
        GraphInvalidate;
      end;
    RM_AUDIO_OUT_GAIN:
      begin
        FGain := power(10, Integer(Msg.ParamH) / 20);
        FGainDb := Integer(Msg.ParamH);
        GraphInvalidate;
      end;
    RM_AUDIO_OUT_GAIN_AUTO:
      begin
        FAutoGain := True;
        GraphInvalidate;
      end;
    PRIV_RM_AUDIO_OUT_CLOSE:
      begin
        FClosing := False;
      end
  else
      inherited;
  end;
end;

procedure TRadioAudioOut.DoStart;
var
  F: WAVEFORMATEX;
begin
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
end;

procedure TRadioAudioOut.DoBeforeDestroy;
begin
  CloseDev;
end;

procedure TRadioAudioOut.Describe(Strs: TStrings);
const
  FS: array [0..3] of string = ('Mono I', 'Mono Q', 'Stereo IQ', 'Stereo QI');
begin
  Strs.Add('^bFormat: ^n' + FS[FFmt mod 4]);
  if FAutoGain then
    Strs.Add('^bGain: ^nAutomatic')
  else
    Strs.Add(Format('^bGain: ^n%ddB', [FGainDb]));
  if FResample.InputRate <> FResample.OutputRate then
    Strs.Add(Format('^b^1Resampling: ^n %d : %d ', [FResample.InputRate, FResample.OutputRate]));
end;

constructor TRadioAudioOut.Create(RunQueue: TRadioRunQueue);
var
  R: TRegulatorNode;
  I: Integer;
begin
  inherited Create(RunQueue);
  FResample  := TResampleNode.Create;
  FHasConfig := False;
  FResample.OutputRate := AUDIO_OUT_SAMPLE_RATE;
  FResample.InputRate := AUDIO_OUT_SAMPLE_RATE;
  R := TRegulatorNode.Create;
  R.Regulator.Size := (High(FBufs[0]) + 1) div 2;
  R.OnSendToNext := @ReceiveRegulatedData;
  FResample.Connect(R);
  FGain := 3276;
  FGainDb := Round(20 * log10(FGain));
  for I := 0 to High(FEvents) do
    FEvents[I] := CreateEvent(nil, True, True, nil);
  FFmt := AUDIO_OUT_FMT_STEREO_IQ;
  DoStart;
end;

destructor TRadioAudioOut.Destroy;
var
  I: Integer;
begin
  FResample.Free;
  for I := 0 to High(FEvents) do
    CloseHandle(FEvents[I]);
  inherited
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
  if not FClosing then
    waveInUnprepareHeader(FHandle, @FHDRs[Index], Sizeof(FHDRs[Index]));
  P := DefOutput.TryAlloc(I);
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

procedure TRadioAudioIn.ProccessCustomMessage(const Msg: TRadioMessage;
  var Ret: Integer);
var
  F: WAVEFORMATEX;
begin
  case Msg.Id of
    RM_AUDIO_IN_START:
      begin
        CloseDev;
        FRate := DWord(Msg.ParamL);
        with F do
        begin
          wFormatTag := WAVE_FORMAT_PCM;
          nChannels  := 1;
          nSamplesPerSec := FRate;
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
        Broadcast(RM_SET_FEATURE, RM_FEATURE_FREQ, 0);
        GraphInvalidate;
      end;
    RM_AUDIO_IN_STOP:
      begin
        CloseDev;
        GraphInvalidate;
      end;
    PRIV_RM_AUDIO_IN_DATA: waveInData(Integer(Msg.ParamH));
    PRIV_RM_AUDIO_IN_CLOSE:  FClosing := False;
  else
    inherited;
  end;
end;

procedure TRadioAudioIn.DoConfigure;
begin
  FConfig.Show;
end;

procedure TRadioAudioIn.Describe(Strs: TStrings);
begin
  if FHandle <> 0 then
    Strs.Add(Format('^bSample rate: ^n%d', [FRate]))
  else
    Strs.Add('^bNot running');
end;

procedure TRadioAudioIn.DoSyncDestroy;
begin
  FConfig.Free;
  inherited DoSyncDestroy;
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
  inherited Destroy;
end;

initialization

  RegisterModule(TRadioModuleClass(TRadioAudioIn.ClassType));
  RegisterModule(TRadioModuleClass(TRadioAudioOut.ClassType));

end.


