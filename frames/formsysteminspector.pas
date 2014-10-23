unit formsysteminspector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TATransformations, Forms, Controls,
  Graphics, Dialogs, ComCtrls, ExtCtrls, Buttons, TASeries, RadioSystem,
  superobject, Math;

type

  { TSystemInspectorForm }

  TSystemInspectorForm = class(TForm)
    BitBtn1: TBitBtn;
    Chart1: TChart;
    ListView1: TListView;
    ListView2: TListView;
    PageControl1: TPageControl;
    Panel1: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Timer1: TTimer;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListView1Compare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure Timer1Timer(Sender: TObject);
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
  SystemInspectorForm: TSystemInspectorForm;

implementation

const
  MAX_POINTS = 100;
  THEME: array [0..8] of TColor =
    (482559, 505739, 6450981, 15631106, 8199463, 9847672, 8192996, 2164712, 654847);

{$R *.lfm}

{ TSystemInspectorForm }

procedure TSystemInspectorForm.FormCreate(Sender: TObject);
begin
  RadioSys := TRadioSystem.Instance;
end;

procedure TSystemInspectorForm.BitBtn1Click(Sender: TObject);
var
  I: Integer;
  O: ISuperObject;
  A: TSuperAvlEntry;
  L: TListItem;
begin
  if not Assigned(FRadioSys) then Exit;

  ListView2.BeginUpdate;
  ListView2.Clear;
  O := FRadioSys.GetRunningModules;
  for A in O.AsObject do
  begin
    L := ListView2.Items.Add;
    L.Caption := A.Name;
    L.SubItems.Add(A.Value.AsString);
  end;
  ListView2.EndUpdate;
end;

procedure TSystemInspectorForm.ListView1Compare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare := 0;
  if ListView1.SortColumn < 0 then Exit;
  if ListView1.SortColumn = 0 then
  begin
    Compare := CompareStr(Item1.Caption, Item2.Caption);
  end
  else
    Compare := Sign(StrToFloat(Item1.SubItems[0]) - StrToFloat(Item2.SubItems[0]));
  if ListView1.SortDirection = sdDescending then
    Compare := -Compare;
end;

procedure TSystemInspectorForm.Timer1Timer(Sender: TObject);
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
      Delete(0);
      Add(100 * FLoad[I]);
    end;
  end;

  FTimerCnt := (FTimerCnt + 1) mod 5;
  if FTimerCnt <> 0 then Exit;

  O := FRadioSys.GetModuleUsageInfo;
  ListView1.BeginUpdate;
  for I := ListView1.Items.Count - 1 downto 0 do
  begin
    L := ListView1.Items[I];
    if Assigned(O.O[L.Caption]) then
    begin
      L.SubItems[0] := Format('%.2f', [O.D[L.Caption]]);
      O.D[L.Caption] := -1;
    end
    else
      ListView1.Items.Delete(I);
  end;
  for A in O.AsObject do
  begin
    if A.Value.AsDouble < 0 then Continue;
    L := ListView1.Items.Add;
    L.Caption := A.Name;
    L.SubItems.Add(Format('%.2f', [A.Value.AsDouble]));
  end;
  ListView1.EndUpdate;
end;

procedure TSystemInspectorForm.SetRadioSys(AValue: TRadioSystem);

begin
  if FRadioSys = AValue then Exit;
  FRadioSys := AValue;
  if Assigned(FRadioSys) then
    SetLength(FLoad, FRadioSys.WorkerCount);
end;

procedure TSystemInspectorForm.CreateSeries;
var
  I: Integer;
  J: Integer;
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
    for J := 1 to MAX_POINTS do S.Add(0.0);
  end;
end;

end.

