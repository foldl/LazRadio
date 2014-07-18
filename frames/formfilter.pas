unit formfilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TATransformations, TASeries, Forms,
  Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, Buttons, StdCtrls,
  UComplex, Math, RadioModule;

type

  { TFilterForm }

  TFilterForm = class(TForm)
    BtnGo: TBitBtn;
    BtnGo1: TBitBtn;
    Chart: TChart;
    ChartAxisTransformations1AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    ChartAxisTransformations2: TChartAxisTransformations;
    ChartAxisTransformations2AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    ChartAxisTransformations1: TChartAxisTransformations;
    WindowFuncSel: TComboBox;
    EditBandwidth1: TLabeledEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    EditRate: TLabeledEdit;
    EditTaps: TLabeledEdit;
    EditCenterFreq: TLabeledEdit;
    EditBandwidth: TLabeledEdit;
    FilterType: TRadioGroup;
    procedure BtnGo1Click(Sender: TObject);
    procedure BtnGoClick(Sender: TObject);
    procedure FilterTypeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCoeff: array of Double;
    FModule: TRadioModule;
    FRate: Integer;
    FOmega: Double;
    FBandwidth: Double;
    FTaps: Integer;
    procedure SetModule(AValue: TRadioModule);
    procedure ShowFIR;
  public
    property Module: TRadioModule read FModule write SetModule;
  end;

var
  FilterForm: TFilterForm;

implementation

uses
  SignalBasic, GenFFT, rm_filter, RadioSystem;

{$R *.lfm}

{ TFilterForm }

procedure TFilterForm.FilterTypeClick(Sender: TObject);
begin
  case TFilterType(FilterType.ItemIndex) of
    ftBPF:
      begin
        EditCenterFreq.EditLabel.Caption := 'Passband center freq (Hz)';
        EditBandwidth.EditLabel.Caption := 'Bandwidth (Hz)';
      end;
    ftBSF:
      begin
        EditCenterFreq.EditLabel.Caption := 'Stopband center freq (Hz)';
        EditBandwidth.EditLabel.Caption := 'Bandwidth (Hz)';
      end;
    else
      EditCenterFreq.EditLabel.Caption := 'Cutoff freq (Hz)';
      EditBandwidth.EditLabel.Caption := '(ignore)';
  end;
end;

procedure TFilterForm.FormCreate(Sender: TObject);
var
  S: string;
begin
  for S in gWindowFunctionNames do
    WindowFuncSel.Items.Add(S);
  FilterTypeClick(Sender);
  WindowFuncSel.ItemIndex := Ord(wfKaiser);
end;

procedure TFilterForm.ShowFIR;
type
  SelFunc = function (A, B: Double): Double;
var
  T: array of Complex;
  F: array of Double;
  P: PFFTPlan;
  I, J: Integer;
  S1, S2: TLineSeries;
  F0: Double;
  BandLow, BandHigh: Double;
  BandLowI, BandHighI: Integer;
  M: Double;
  Ante: Double;
  Sel: SelFunc;
  RevSel: SelFunc;
  procedure AddConst(const V: Double; const bVertical: Boolean; const ATitle: string;
    AColor: TColor);
  var
    C: TConstantLine;
  begin
    C := TConstantLine.Create(Chart);
    Chart.AddSeries(C);
    C.AxisIndexY := 0;
    C.Position := V;
    C.SeriesColor := AColor;
    if bVertical then C.LineStyle := lsVertical else C.LineStyle := lsHorizontal;
    C.Pen.Style := psDot;
    C.Title := ATitle;
  end;

begin
  SetLength(T, Max(1024, High(FCoeff) + 1));
  SetLength(F, High(T) + 1);
  for I := 0 to High(FCoeff) do
    T[I].re := FCoeff[I];
  P := BuildFFTPlan(High(T) + 1, False);
  FFT(P, @T[0], @T[0]);
  FinalizePlan(P);
  PowArg(@T[0], High(T) + 1);
  Chart.Series.Clear;
  S1 := TLineSeries.Create(Chart);
  S2 := TLineSeries.Create(Chart);
  Chart.AddSeries(S1); Chart.AddSeries(S2);
  S1.AxisIndexY := 0; S2.AxisIndexY := 1;
  S1.Title := '|H(f)|'; S2.Title := 'Arg';
  S1.SeriesColor := clRed; S2.SeriesColor := clBlue;
  F0 := FRate / (High(T) + 1);
  for I := 0 to High(T) div 2 do
  begin
    T[I].re := 10 * Log10(T[I].re + 1e-20);
    F[I] := I * F0;
    S1.AddXY(F[I], T[I].re);
    S2.AddXY(F[I], T[I].im);
  end;

  Sel := @Math.Min;
  RevSel := @Math.Max;
  case TFilterType(FilterType.ItemIndex) of
    ftBPF:
      begin
        BandLow := FOmega -FBandwidth / 2;
        BandHigh := FOmega + FBandwidth / 2;
      end;
    ftBSF:
      begin
        BandLow := FOmega -FBandwidth / 2;
        BandHigh := FOmega + FBandwidth / 2;
        Sel := @Math.Max;
        RevSel := @Math.Min;
      end;
    ftLPF:
      begin
        BandLow := 0;
        BandHigh := FOmega;
      end;
    ftHPF:
      begin
        BandLow := FOmega;
        BandHigh := 1;
      end;
  end;
  BandLowI := Round(BandLow * FRate / 2 / F0);
  BandHighI := Round(BandHigh * FRate / 2 / F0);

  M := T[BandLowI].re;
  for I := BandLowI + 1 to BandHighI do
  begin
    M := Sel(M, T[I].re);
  end;
  AddConst(M, False, Format('%ddB', [Round(M)]), clRed);
  Ante := Sel(-1000, 1000);

  if BandLowI >= 1 then
  begin
    M := T[BandLowI - 1].re;
    for I := BandLowI - 2 downto 0 do
    begin
      if Sel(M, T[I].re) = M then
      begin
        for J := I downto 0 do
        begin
          M := RevSel(M, T[J].re);
        end;
        Ante := RevSel(Ante, M);
        Break;
      end
      else begin
        M := T[I].re;
      end;
    end;
  end;

  if BandHighI < High(T) then
  begin
    M := T[BandHighI + 1].re;
    for I := BandHighI + 2 to High(T) div 2 do
    begin
      if Sel(M, T[I].re) = M then
      begin
        for J := I to High(T) div 2 do
        begin
          M := RevSel(M, T[J].re);
        end;
        Ante := RevSel(Ante, M);
        Break;
      end
      else begin
        M := T[I].re;
      end;
    end;
  end;

  AddConst(Ante, False, Format('%ddB', [Round(Ante)]), clGreen);
  Chart.ZoomFull(True);
end;

procedure TFilterForm.SetModule(AValue: TRadioModule);
begin
  if FModule = AValue then Exit;
  FModule := AValue;
end;

procedure TFilterForm.BtnGo1Click(Sender: TObject);
begin
  FTaps := StrToIntDef(EditTaps.Text, 0);
  FRate := StrToIntDef(EditRate.Text, 0);
  if (FTaps < 10) or (FTaps >= 256) then
  begin
    ShowMessage('Taps out of range. only support [10, 255]');
    Exit;
  end;

  if FRate < 1000 then
  begin
    ShowMessage('Sample rate too small. at least 1000.');
    Exit;
  end;
  if WindowFuncSel.ItemIndex < 0 then
  begin
    ShowMessage('Please select a window function');
    Exit;
  end;

  SetLength(FCoeff, FTaps);
  FillChar(FCoeff[0], FTaps * SizeOf(FCoeff[0]), 0);

  FOmega := StrToIntDef(EditCenterFreq.Text, 0) / FRate * 2;
  if (FOmega < 0) or (FOmega > 1) then
  begin
    ShowMessage(EditCenterFreq.EditLabel.Caption + ' is out of range.');
    Exit;
  end;

  case TFilterType(FilterType.ItemIndex) of
    ftBPF, ftBSF:
      begin
        FBandwidth := StrToIntDef(EditBandwidth.Text, 0) / FRate * 2;
        if (FOmega - FBandwidth / 2 < 0) or (FOmega + FBandwidth / 2 > 1) then
        begin
          ShowMessage('Band is out of range.');
          Exit;
        end;
        EditCenterFreq.EditLabel.Caption := 'Passband center freq (Hz)';
      end;
  end;

  FIRDesign(@FCoeff[0], FTaps, TFilterType(FilterType.ItemIndex),
            FOmega, FBandwidth,
            TWindowFunction(WindowFuncSel.ItemIndex),
            StrToFloatDef(EditBandwidth1.Text, -1));
  ShowFIR;
end;

procedure TFilterForm.BtnGoClick(Sender: TObject);
begin
  if not Assigned(FModule) then Exit;
  if (FTaps < 10) or (FTaps >= 256) then
  begin
    Exit;
  end;
  RadioPostMessage(RM_FILTER_SET, PtrUInt(@FCoeff[0]), (FRate shl 8) or FTaps, FModule);
end;

end.

