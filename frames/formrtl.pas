unit formrtl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, Spin, rm_rtl;

type

  { TRTLForm }

  TRTLForm = class(TForm)
    BtnStop: TBitBtn;
    BtnGo: TBitBtn;
    BtnRefresh: TBitBtn;
    DevList: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
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
    procedure DevListChange(Sender: TObject);
    procedure ManualGainChange(Sender: TObject);
  private
    FRtlMod: TRtlModule;
    procedure UpdateUIStatus;
    procedure SetRtlMod(AValue: TRtlModule);
    procedure DevListed(L: TStringList);
  public
    property RtlMod: TRtlModule read FRtlMod write SetRtlMod;
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

end;

procedure TRTLForm.DevListChange(Sender: TObject);
begin
  BtnGo.Enabled := (DevList.ItemIndex >= 0) and Assigned(FRtlMod) and FRtlMod.DevRunning;
end;

procedure TRTLForm.ManualGainChange(Sender: TObject);
begin

end;

procedure TRTLForm.UpdateUIStatus;
var
  Running: Boolean;
begin
  Running := Assigned(FRtlMod) and FRtlMod.DevRunning;
  BtnRefresh.Enabled := not Running;
  BtnGo.Enabled := False;
  BtnStop.Enabled := Running;
  DevList.Enabled := not Running;
  SamplingRateList.Enabled := not Running;
  SamplingMode.Enabled     := not Running;
  AGCMode.Enabled          := Running;
  AGCMode.ItemIndex        := 0;
  AGCModeClick(nil);
  FreqCorrectionSpin.Enabled := Running;
end;

procedure TRTLForm.SetRtlMod(AValue: TRtlModule);
begin
  if FRtlMod = AValue then Exit;
  FRtlMod := AValue;
  UpdateUIStatus;
  if BtnRefresh.Enabled then BtnRefresh.Click;
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
  ManualGain.Enabled := AGCMode.ItemIndex = 1;
end;

procedure TRTLForm.BtnGoClick(Sender: TObject);
begin

end;

end.

