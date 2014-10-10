unit formsysteminspector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TATransformations, Forms, Controls,
  Graphics, Dialogs, ComCtrls, ExtCtrls, TASeries, RadioSystem, superobject;

type

  { TSystemInpectorForm }

  TSystemInpectorForm = class(TForm)
    Chart1: TChart;
    ListView1: TListView;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Timer1: TTimer;
    procedure Timer1StartTimer(Sender: TObject);
  private
    FRadioSys: TRadioSystem;
    FThreadLoad: array of TLineSeries;
    FLoad: array of Double;
    FTimerCnt: Integer;
    procedure SetRadioSys(AValue: TRadioSystem);
    procedure CreateSeries;
  public
    property RadioSys: TRadioSystem read FRadioSys write SetRadioSys;
  end;

var
  SystemInpectorForm: TSystemInpectorForm;

implementation

const
  MAX_POINTS = 100;
  THEME: array [0..8] of TColor =
    (505739, 482559, 654847, 6450981, 15631106, 8199463, 9847672, 8192996, 2164712);

{$R *.lfm}

{ TSystemInpectorForm }

procedure TSystemInpectorForm.Timer1StartTimer(Sender: TObject);
var
  I: Integer;
  O: ISuperObject;
  A: TSuperAvlEntry;
  L: TListItem;
begin
  if not Visible then Exit;

  if not Assigned(FRadioSys) then Exit;
  if not FRadioSys.GetWorkerLoadInfo(FLoad) then Exit;
  if Length(FLoad) <> Length(FThreadLoad) then CreateSeries;

  for I := 0 to High(FLoad) do
  begin
    with FThreadLoad[I] do
    begin
      if Count > MAX_POINTS then
        Delete(0);
      Add(FLoad[I]);
    end;
  end;

  FTimerCnt := (FTimerCnt + 1) mod 5;
  if FTimerCnt <> 0 then Exit;

  O := FRadioSys.GetModuleUsageInfo;
  ListView1.BeginUpdate;
  ListView1.Clear;
  for A in O.AsObject do
  begin
    L := ListView1.Items.Add;
    L.Caption := A.Name;
    L.SubItems.Add(Format('%.2f', [A.Value.AsDouble]));
  end;
  ListView1.EndUpdate;
end;

procedure TSystemInpectorForm.SetRadioSys(AValue: TRadioSystem);

begin
  if FRadioSys = AValue then Exit;
  FRadioSys := AValue;
end;

procedure TSystemInpectorForm.CreateSeries;
var
  I: Integer;
  S: TLineSeries;
begin
  for I := 0 to High(FThreadLoad) do FThreadLoad[I].Free;

  SetLength(FThreadLoad, Length(FLoad));
  for I := 0 to High(FThreadLoad) do
  begin
    S := TLineSeries.Create(Owner);
    with S do
    begin
      // AxisIndexY := FAxisIndex;
      SeriesColor := THEME[I mod Length(THEME)];
      Pointer.Visible := False;
      LinePen.Style := psSolid;
      Marks.Visible := False;
      ShowPoints := False;
      Title := IntToStr(I);
      LinePen.Width := 2;
    end;
    FThreadLoad[I] := S;
    Chart1.AddSeries(S);
  end;
end;

end.

