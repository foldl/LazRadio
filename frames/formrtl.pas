unit formrtl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LCLType, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, Spin, RadioModule, RadioSystem, radiomessage;

type

  { TRTLForm }

  TRTLForm = class(TForm)
    BtnStop: TBitBtn;
    BtnGo: TBitBtn;
    BtnRefresh: TBitBtn;
    CheckDigitalGain: TCheckBox;
    CheckOffsetTunning: TCheckBox;
    DevList: TComboBox;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    FreqEdit: TLabeledEdit;
    TunnerIFStage: TLabeledEdit;
    TunnerIFGain: TLabeledEdit;
    SamplingRateList: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    AGCMode: TRadioGroup;
    ManualGain: TTrackBar;
    FreqCorrectionSpin: TSpinEdit;
    SamplingMode: TComboBox;
    procedure AGCModeClick(Sender: TObject);
    procedure BtnGoClick(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure CheckDigitalGainChange(Sender: TObject);
    procedure CheckOffsetTunningChange(Sender: TObject);
    procedure DevListChange(Sender: TObject);
    procedure FreqCorrectionSpinKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FreqEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ManualGainChange(Sender: TObject);
    procedure SamplingModeChange(Sender: TObject);
    procedure SamplingRateListChange(Sender: TObject);
    procedure TunnerIFStageKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FModule: TRadioModule;
    procedure SetModule(AValue: TRadioModule);
    procedure UpdateUIStatus(const Running: Boolean);
    procedure DevListed(L: TStringList);
  public
    property Module: TRadioModule read FModule write SetModule;
  end;

var
  RTLForm: TRTLForm;

implementation

uses
  RtlSdr, Async;

{$R *.lfm}

procedure EnumerateDevs(L: TStringList);
var
  I: Integer;
begin
  I := RtlSdrGetDeviceCount() - 1;
  while I >= 0 do
  begin
    L.Add(Format('%s - %s', [RtlSdrGetDeviceName(I),
                             FormatTunerType(RtlSdrGetTunerType(I))]));
    Dec(I);
  end;
end;

{ TRTLForm }

procedure TRTLForm.BtnRefreshClick(Sender: TObject);
var
  L: TStringList;
begin
  L := TStringList.Create;
  Enabled := False;
  Cursor := crHourGlass;
  AsyncCall(TAsyncCallFun(@EnumerateDevs), L, TAsyncCallNotify(@DevListed), L);
end;

procedure TRTLForm.BtnStopClick(Sender: TObject);
begin
  UpdateUIStatus(False);
  RadioPostMessage(RM_RTL_STOP, 0, 0, FModule);
end;

procedure TRTLForm.CheckDigitalGainChange(Sender: TObject);
begin
  RadioPostMessage(RM_RTL_DEV_CTL, RTL_SET_AGC_MODE, Ord(CheckDigitalGain.Checked), FModule);
end;

procedure TRTLForm.CheckOffsetTunningChange(Sender: TObject);
begin
  RadioPostMessage(RM_RTL_DEV_CTL, RTL_SET_OFFSET_TUNNING, Ord(CheckOffsetTunning.Checked), FModule);
end;

procedure TRTLForm.DevListChange(Sender: TObject);
begin
  //BtnGo.Enabled := (DevList.ItemIndex >= 0) and Assigned(FRtlMod) and FRtlMod.DevRunning;
end;

procedure TRTLForm.FreqCorrectionSpinKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key <> VK_RETURN then Exit;
  RadioPostMessage(RM_RTL_DEV_CTL, RTL_SET_FREQ_CORRECTION,
                   FreqCorrectionSpin.Value, FModule);
end;

procedure TRTLForm.FreqEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key <> VK_RETURN then Exit;
  RadioPostMessage(RM_SET_FEATURE, RM_FEATURE_FREQ,
                   StrToIntDef(FreqEdit.Text, 1000000), FModule);
end;

procedure TRTLForm.ManualGainChange(Sender: TObject);
begin
  if AGCMode.ItemIndex <> 1 then Exit;
  RadioPostMessage(RM_RTL_DEV_CTL, RTL_SET_TUNNER_GAIN, ManualGain.Position, FModule);
end;

procedure TRTLForm.SamplingModeChange(Sender: TObject);
begin
  RadioPostMessage(RM_RTL_DEV_CTL, RTL_SET_DIRECT_SAMPLING, SamplingMode.ItemIndex, FModule);
end;

procedure TRTLForm.SamplingRateListChange(Sender: TObject);
const
  Rates: array [0..10] of Integer = (3200000, 2800000, 2560000, 2400000, 2048000,
                                    1920000, 1800000, 1400000, 1024000,   900001,
                                     250000);
begin
  RadioPostMessage(RM_SET_FEATURE, RM_FEATURE_SAMPLE_RATE, Rates[SamplingRateList.ItemIndex], FModule);
end;

procedure TRTLForm.TunnerIFStageKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key <> VK_RETURN then Exit;
  RadioPostMessage(RM_RTL_DEV_CTL, RTL_SET_TUNNER_IF_GAIN,
                   (StrToIntDef(TunnerIFStage.Text, 1) shl 16) or StrToIntDef(TunnerIFGain.Text, 10), FModule);
end;

procedure TRTLForm.SetModule(AValue: TRadioModule);
begin
  if FModule = AValue then Exit;
  FModule := AValue;
end;

procedure TRTLForm.UpdateUIStatus(const Running: Boolean);
begin
  BtnRefresh.Enabled := not Running;
  BtnGo.Enabled := not Running;
  BtnStop.Enabled := Running;
  DevList.Enabled := not Running;
  SamplingRateList.Enabled := Running;
  SamplingMode.Enabled     := Running;
  AGCMode.Enabled          := Running;
  TunnerIFGain.Enabled := Running;
  TunnerIFStage.Enabled := Running;
  CheckDigitalGain.Enabled := Running;
  FreqCorrectionSpin.Enabled := Running;
end;

procedure TRTLForm.DevListed(L: TStringList);
begin
  DevList.Items.Assign(L);
  L.Free;
  Enabled := True;
  Cursor := crDefault;
  if DevList.Items.Count > 0 then DevList.ItemIndex := 0;
end;

procedure TRTLForm.AGCModeClick(Sender: TObject);
begin
  RadioPostMessage(RM_RTL_DEV_CTL, RTL_SET_TUNNER_GAIN_MODE, AGCMode.ItemIndex, FModule);
  ManualGainChange(Sender);
end;

procedure TRTLForm.BtnGoClick(Sender: TObject);
var
  Key: Word = VK_RETURN;
begin
  if DevList.ItemIndex < 0 then Exit;
  FreqEditKeyUp(Sender, Key, []);
  SamplingRateListChange(Sender);
  SamplingModeChange(Sender);

  UpdateUIStatus(True);
  RadioPostMessage(RM_RTL_START, DevList.ItemIndex, 0, FModule);

  {
  AGCModeClick(Sender);
  CheckDigitalGainChange(Sender);
  FreqCorrectionSpinKeyUp(Sender, Key, []);
  }

end;

end.

