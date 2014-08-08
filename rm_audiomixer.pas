unit rm_audiomixer;

// NOTE: it's assumed that all input data streams' sample rates are all equal

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioSystem, UComplex, RadioMessage, RadioModule, formaudiomixer;

type

  TAudioStreamSetting = record
    TotalGain: Cardinal;
    BaseGain: Cardinal;
    TrebleGain: Cardinal;
    MixMethod: Integer;
  end;

  { TRadioAudioMixer }

  TRadioAudioMixer = class(TRadioModule)
  private
    FConfig: TAudioMixerForm;
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
  inherited ProccessMessage(Msg, Ret);
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

