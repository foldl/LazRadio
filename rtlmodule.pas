unit RtlModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioModule, RtlSdr;

type

  { TRtlThread }

  TRtlThread = class(TThread)
  private
    FDev: PRtlSdrDev;
    procedure DoTerminate; override;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TRtlModule }

  TRtlModule = class(TRadioModule)
  private
    FDev: PRtlSdrDev;
    FThread: TRtlThread;
    function  DevOK: Boolean;
    procedure BtnRefreshClick(Sender: TObject);
    procedure BtnGoClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure AGCModeChanged(Sender: TObject);
    procedure GainChanged(Sender: TObject);
    procedure FreqCorrectionChanged(Sender: TObject);
  protected
    procedure SetRunning(AValue: Boolean); override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure Configure; override;
  end;

implementation

uses
  FormRtl;

{ TRtlThread }

procedure TRtlThread.DoTerminate;
begin
  inherited DoTerminate;
end;

procedure TRtlThread.Execute;
begin
  while not Terminated do
  begin

  end;
end;

constructor TRtlThread.Create;
begin
  inherited Create(True);
end;

destructor TRtlThread.Destroy;
begin
  inherited Destroy;
end;

{ TRtlModule }

function TRtlModule.DevOK: Boolean;
begin
  Result := Assigned(FDev);
end;

procedure TRtlModule.BtnRefreshClick(Sender: TObject);
var
  I: Integer;
begin
  if Running then Exit;
  RTLForm.DevList.Clear;
  I := RtlSdrGetDeviceCount() - 1;
  while I >= 0 do
  begin
    RTLForm.DevList.Items.Add(Format('%s - %s', [RtlSdrGetDeviceName(I), FormatTunerType(RtlSdrGetTunerType(I))]));
    Dec(I);
  end;
end;

procedure TRtlModule.BtnGoClick(Sender: TObject);
begin
  Running := True;
end;

procedure TRtlModule.BtnStopClick(Sender: TObject);
begin
  if not DevOK then Exit;
end;

procedure TRtlModule.AGCModeChanged(Sender: TObject);
begin
  if not DevOK then Exit;
end;

procedure TRtlModule.GainChanged(Sender: TObject);
begin
  if not DevOK then Exit;
end;

procedure TRtlModule.FreqCorrectionChanged(Sender: TObject);
begin
  if not DevOK then Exit;
end;

procedure TRtlModule.SetRunning(AValue: Boolean);
begin
  if AValue then
    FThread.Resume
  else
    FThread.Suspend;
  inherited SetRunning(AValue);
end;

constructor TRtlModule.Create(RunQueue: TRadioRunQueue);
begin
  inherited;
  FThread := TRtlThread.Create;
end;

destructor TRtlModule.Destroy;
begin
  RtlSdrClose(FDev);
  inherited Destroy;
end;

procedure TRtlModule.Configure;
begin
  RTLForm.Show;
  with RTLForm do
  begin
    BtnRefresh.OnClick := @BtnRefreshClick;
    BtnGo.OnClick      := @BtnGoClick;
    BtnStop.OnClick    := @BtnStopClick;
    AGCMode.OnClick    := @AGCModeChanged;
    ManualGain.OnChange:= @GainChanged;
    FreqCorrectionSpin.OnChange := @FreqCorrectionChanged;

    BtnRefresh.Enabled := not Running;
    BtnGo.Enabled := not Running;
    BtnStop.Enabled := Running;

    if Running then Exit;

    DevList.Clear;
    AGCMode.ItemIndex := 0;

    BtnRefresh.Click;
  end;
end;

end.

