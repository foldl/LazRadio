unit rm_audiomixer;

// NOTE: it's assumed that all input streams' sample rates are all equal

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioSystem, UComplex, RadioMessage, RadioModule, formaudiomixer,
  SignalBasic;

type

  TAudioStreamSetting = record
    TotalGain: Cardinal;
    BaseGain: Cardinal;
    TrebleGain: Cardinal;
    MixMethod: Integer;
    IIR: TIIRFilter;
  end;

  { TRadioAudioMixer }

  TRadioAudioMixer = class(TRadioModule)
  private
    FStreams: array of TAudioStreamSetting;
    FConfig: TAudioMixerForm;
    procedure Set;
  protected
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

procedure TRadioAudioMixer.ProccessMessage(const Msg: TRadioMessage;
  var Ret: Integer);
begin
  case Msg.Id of
    RM_AUDIOMIXER_CFG:
      case Msg.ParamH of
        AUDIOMIXER_STREAM_NUM: SetLength(FStreams, Msg.ParamL);
      end;
    RM_AUDIOMIXER_SET_STREAM_OUPUT,
    RM_AUDIOMIXER_SET_STREAM_TOTAL_GAIN,
    RM_AUDIOMIXER_SET_STREAM_BASE_GAIN,
    RM_AUDIOMIXER_SET_STREAM_TREBLE_GAIN:
      begin

      end;
  else
    inherited;
  end;
end;

procedure TRadioAudioMixer.DoConfigure;
begin
  FConfig.Show;
end;

procedure TRadioAudioMixer.Describe(Strs: TStrings);
begin

end;

constructor TRadioAudioMixer.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  FConfig := TAudioMixerForm.Create(nil);
  FConfig.Module := Self;
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
begin
  //
end;

initialization

  RegisterModule(TRadioModuleClass(TRadioAudioMixer.ClassType));

end.

