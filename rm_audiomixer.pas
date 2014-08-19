unit rm_audiomixer;

// NOTE: it's assumed that all input streams' sample rates are all equal
{
bass, midrange, & treble frequencies in octaves
bass	range in Hz
sub bass	31 to 62 (and lower)
mid bass	62 to 125
upper bass	125 to 250
midrange
lower midrange	250 to 500
mid midrange	500 to 1K
upper midrange	1K to 2K
treble
lower treble	2K to 4K
mid treble	4K to 8K
upper treble	8K to 16K (and higher)
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioSystem, UComplex, RadioMessage, RadioModule, formaudiomixer,
  SignalBasic, Math;

const
  CACHE_SIZE = 50 * 1024; // we should use a large buffer here

type

  TAudioStreamSetting = record
    Cache: array [0..CACHE_SIZE-1] of Complex;
    Cursor: Integer;
    MixLen: Integer;
    TotalGain: Double;
    BassGain: Double;
    TrebleGain: Double;
    MixMethod: Integer;
    Bass: TIIRFilter;
    Treble: TIIRFilter;
  end;

  { TRadioAudioMixer }

  TRadioAudioMixer = class(TRadioModule)
  private
    FRegulator: TStreamRegulator;
    FRate: Integer;
    FStreams: array of TAudioStreamSetting;
    FConfig: TAudioMixerForm;
    procedure ReceiveMixedData(const P: PComplex; const Len: Integer);
    procedure Mixing;
    procedure SetupBassFilter(const Index: Integer);
    procedure SetupTrebleFilter(const Index: Integer);
  protected
    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer;
      override;
    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: Integer); override;

    procedure DoConfigure; override;
    procedure Describe(Strs: TStrings); override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
    procedure ReceiveData(const Port: Integer; const P: PComplex; const Len: Integer); override;
  end;

implementation

{ TRadioAudioMixer }

procedure TRadioAudioMixer.ReceiveMixedData(const P: PComplex;
  const Len: Integer);
var
  O: PComplex;
  I: Integer;
begin
  if Len <> DefOutput.BufferSize then
  begin
    TRadioLogger.Report(llWarn, 'TRadioAudioMixer.ReceiveMixedData: Len <> DefOutput.BufferSize, data lost');
    Exit;
  end;

  O := Alloc(DefOutput, I);
  if Assigned(O) then
  begin
    Move(P^, O^, Len * SizeOf(O^));
    DefOutput.Broadcast(I, FDataListeners);
  end;
end;

procedure TRadioAudioMixer.Mixing;
var
  L: Integer = -1;
  I: Integer;
  F: Integer = -1;
  T: array of Complex;
begin
  for I := 0 to High(FStreams) do
  begin
    with FStreams[I] do
    begin
      if MixMethod = AUDIOMIXER_STREAM_OUTPUT_OFF then Continue;
      if L < 0 then L := Cursor
      else
        L := Min(L, Cursor);
      if Cursor = CACHE_SIZE then F := I;
    end;
  end;

  for I := 0 to High(FStreams) do
  begin
    with FStreams[I] do
    begin
      MixLen := L;
    end;
  end;

  // *one* stream's cache is full, send it directly
  if L = 0 then
  begin
    if F < 0 then raise Exception.Create('TRadioAudioMixer.Mixing: impossible');
    FStreams[F].MixLen := CACHE_SIZE;
    L := CACHE_SIZE;
  end;

  SetLength(T, L);
  for I := 0 to High(FStreams) do
  begin
    with FStreams[I] do
    begin
      if MixMethod = AUDIOMIXER_STREAM_OUTPUT_OFF then Continue;
      if MixLen < 1 then Continue;

      case MixMethod of
        AUDIOMIXER_STREAM_OUTPUT_IQ_IQ:
          for F := 0 to L - 1 do
          begin
            T[F] := T[F] + Cache[F];
          end;
        AUDIOMIXER_STREAM_OUTPUT_QI_QI:
          for F := 0 to L - 1 do
          begin
            T[F].re := T[F].re + Cache[F].im;
            T[F].im := T[F].im + Cache[F].re;
          end;
        AUDIOMIXER_STREAM_OUTPUT_IQ_I :
          for F := 0 to L - 1 do
          begin
            T[F].re := T[F].re + Cache[F].re + Cache[F].im;
          end;
        AUDIOMIXER_STREAM_OUTPUT_IQ_Q :
          for F := 0 to L - 1 do
          begin
            T[F].im := T[F].im + Cache[F].re + Cache[F].im;
          end;
        AUDIOMIXER_STREAM_OUTPUT_I_I  :
          for F := 0 to L - 1 do
          begin
            T[F].re := T[F].re + Cache[F].re;
          end;
        AUDIOMIXER_STREAM_OUTPUT_I_Q  :
          for F := 0 to L - 1 do
          begin
            T[F].im := T[F].im + Cache[F].re;
          end;
        AUDIOMIXER_STREAM_OUTPUT_Q_I  :
          for F := 0 to L - 1 do
          begin
            T[F].re := T[F].re + Cache[F].im;
          end;
        AUDIOMIXER_STREAM_OUTPUT_Q_Q  :
          for F := 0 to L - 1 do
          begin
            T[F].im := T[F].im + Cache[F].im;
          end;
      end;

      Move(Cache[MixLen], Cache[0], (Cursor - MixLen) * SizeOf(Cache[0]));
      Dec(Cursor, MixLen);
    end;
  end;
  FRegulator.ReceiveData(@T[0], L);
end;

procedure TRadioAudioMixer.SetupBassFilter(const Index: Integer);
begin
  if FRate < 2 then Exit;
  if Abs(FStreams[Index].BassGain) < 1 then
  begin
    FStreams[Index].BassGain := 0;
    Exit;
  end;
  SetIIROrders(FStreams[Index].Bass, 2, 2);
  ShelvingFilterDesign(FStreams[Index].BassGain, 250, FRate, Sqrt(2), sfBassShelf,
                       FStreams[Index].Bass.A, FStreams[Index].Bass.B);
end;

procedure TRadioAudioMixer.SetupTrebleFilter(const Index: Integer);
begin
  if FRate < 2 then Exit;
  if Abs(FStreams[Index].TrebleGain) < 1 then
  begin
    FStreams[Index].TrebleGain := 0;
    Exit;
  end;
  SetIIROrders(FStreams[Index].Treble, 2, 2);
  ShelvingFilterDesign(FStreams[Index].TrebleGain, 2000, FRate, Sqrt(2), sfTrebleShelf,
                       FStreams[Index].Treble.A, FStreams[Index].Treble.B);
end;

function TRadioAudioMixer.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
var
  I: Integer;
begin
  FRate := Rate;
  for I := 0 to High(FStreams) do
  begin
    SetupBassFilter(I);
    SetupTrebleFilter(I);
  end;
  Result := inherited;
end;

procedure TRadioAudioMixer.ProccessMessage(const Msg: TRadioMessage;
  var Ret: Integer);
var
  I: Integer;
begin
  case Msg.Id of
    RM_AUDIOMIXER_CFG:
      begin
        case Msg.ParamH of
          AUDIOMIXER_STREAM_NUM:
            begin
              I := Length(FStreams);
              SetLength(FStreams, Msg.ParamL);
              while I <= High(FStreams) do
              begin
                with FStreams[I] do
                begin
                  TotalGain := 1.0;
                end;
                SetupBassFilter(I);
                Inc(I);
              end;
            end;
        end;
        GraphInvalidate;
      end;
    RM_AUDIOMIXER_SET_STREAM_OUPUT:
      begin
        if Msg.ParamH > High(FStreams) then Exit;
        FStreams[Msg.ParamH].MixMethod := Msg.ParamL;
        GraphInvalidate;
      end;
    RM_AUDIOMIXER_SET_STREAM_TOTAL_GAIN:
      begin
        if Msg.ParamH > High(FStreams) then Exit;
        FStreams[Msg.ParamH].TotalGain := Power(10, Integer(Msg.ParamL) / 10 / 20);
      end;
    RM_AUDIOMIXER_SET_STREAM_BASS_GAIN:
      begin
        if Msg.ParamH > High(FStreams) then Exit;
        FStreams[Msg.ParamH].BassGain := Integer(Msg.ParamL) / 10;
        SetupBassFilter(Msg.ParamH);
      end;
    RM_AUDIOMIXER_SET_STREAM_TREBLE_GAIN:
      begin
        if Msg.ParamH > High(FStreams) then Exit;
        FStreams[Msg.ParamH].TrebleGain := Integer(Msg.ParamL) / 10;
        SetupTrebleFilter(Msg.ParamH);
      end;
  else
    inherited;
  end;
end;

procedure TRadioAudioMixer.DoConfigure;
var
  I: Integer;
begin
  FConfig.ShowUI(Length(FStreams));
  for I := 0 to High(FStreams) do
    with FStreams[I] do
      FConfig.ConfigChannel(I, MixMethod, Round(TotalGain), Round(BassGain), Round(TrebleGain));
end;

procedure TRadioAudioMixer.Describe(Strs: TStrings);
begin
  Strs.Add(Format('^bChannel Number: ^n %d', [Length(FStreams)]));
  Strs.Add(Format('^bSample Rate: ^n %d', [FRate]));
end;

constructor TRadioAudioMixer.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  FConfig := TAudioMixerForm.Create(nil);
  FConfig.Module := Self;
  FRegulator := TStreamRegulator.Create;
  FRegulator.Size := DefOutput.BufferSize;
  FRegulator.OnRegulatedData := @ReceiveMixedData;
end;

destructor TRadioAudioMixer.Destroy;
begin
  FConfig.Free;
  inherited Destroy;
end;

procedure TRadioAudioMixer.ReceiveData(const P: PComplex; const Len: Integer);
begin
  ReceiveData(0, P, Len);
end;

procedure TRadioAudioMixer.ReceiveData(const Port: Integer; const P: PComplex;
  const Len: Integer);
var
  N, M: Integer;
  I: Integer = 0;
  J: Integer;
  G: Double;
begin
  if (Port < 0) or (Port > High(FStreams)) then Exit;
  if FStreams[Port].MixMethod = AUDIOMIXER_STREAM_OUTPUT_OFF then Exit;
  N := Len;
  while N >= 1 do
  begin
    M := Min(N, CACHE_SIZE - FStreams[Port].Cursor);
    if M < 1 then
    begin
      Mixing;
      Continue;
    end;

    with FStreams[Port] do
    begin
      for J := 0 to M - 1 do
        Cache[Cursor + J] := P[I + J] * TotalGain;
      if not IsZero(BassGain) then
        IIRFilter(Bass, @Cache[Cursor], M);
      if not IsZero(TrebleGain) then
        IIRFilter(Treble, @Cache[Cursor], M);
    end;

    Inc(FStreams[Port].Cursor, M);
    Inc(I, M);
    Dec(N, M);
  end;
end;

initialization

  RegisterModule(TRadioModuleClass(TRadioAudioMixer.ClassType));

end.

