unit formauin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, mmsystem, RadioModule, RadioSystem, radiomessage;

type

  { TAudioInForm }

  TAudioInForm = class(TForm)
    BtnGo: TBitBtn;
    BtnRefresh: TBitBtn;
    BtnStop: TBitBtn;
    DevList: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    SamplingRateList: TComboBox;
    procedure BtnGoClick(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure DevListChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCaps: array of WAVEINCAPS;
    FModule: TRadioModule;
    procedure SetModule(AValue: TRadioModule);
  public
    property Module: TRadioModule read FModule write SetModule;
  end;

var
  AudioInForm: TAudioInForm;

implementation

uses
  util_math;

{$R *.lfm}

{ TAudioInForm }

procedure TAudioInForm.FormShow(Sender: TObject);
begin
  if DevList.Items.Count > 0 then Exit;
  BtnRefresh.Click;
end;

procedure TAudioInForm.SetModule(AValue: TRadioModule);
begin
  if FModule = AValue then Exit;
  FModule := AValue;
end;

procedure TAudioInForm.BtnRefreshClick(Sender: TObject);
var
  I: Integer;
begin
  DevList.Items.Clear;
  I := waveInGetNumDevs;
  SetLength(FCaps, I);
  for I := 0 to High(FCaps) do
  begin
    waveInGetDevCaps(I, @FCaps[I], SizeOf(FCaps[0]));
    DevList.Items.Add(SysToUTF8(PChar(@FCaps[I].szPname[0])));
  end;
  if DevList.Items.Count > 0 then
  begin
    DevList.ItemIndex := 0;
    DevListChange(DevList);
  end;
end;

procedure TAudioInForm.BtnStopClick(Sender: TObject);
begin
  if Assigned(FModule) then
  begin
    RadioPostMessage(RM_AUDIO_IN_STOP, 0, 0, FModule);
    BtnStop.Enabled := False;
    BtnGo.Enabled := not BtnStop.Enabled;
  end;
end;

procedure TAudioInForm.BtnGoClick(Sender: TObject);
begin
  if Assigned(FModule) and (DevList.ItemIndex >= 0) and (SamplingRateList.ItemIndex >= 0) then
  begin
    RadioPostMessage(RM_AUDIO_IN_START, DevList.ItemIndex,
                     Round(AtoF(SamplingRateList.Text) * 1000),
                     FModule);
    BtnGo.Enabled := False;
    BtnStop.Enabled := not BtnGo.Enabled;
  end;
end;

procedure TAudioInForm.DevListChange(Sender: TObject);
var
  P: PWAVEINCAPS;
  procedure addrate(const F: Cardinal; const S: string);
  begin
    if P^.dwFormats and F > 0 then
    begin
      SamplingRateList.Items.Add(S);
    end;
  end;

begin
  SamplingRateList.Items.Clear;
  if DevList.ItemIndex < 0 then Exit;
  P := @FCaps[DevList.ItemIndex];
  addrate(WAVE_FORMAT_4M16, '44.1 kHz, mono, 16-bit');
  addrate(WAVE_FORMAT_2M16, '22.05  kHz, mono, 16-bit');
  addrate(WAVE_FORMAT_1M16, '11.025  kHz, mono, 16-bit');
  if SamplingRateList.Items.Count > 0 then SamplingRateList.ItemIndex := 0;
end;

end.

