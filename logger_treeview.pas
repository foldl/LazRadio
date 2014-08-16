unit logger_treeview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, GraphType, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, ActnList, ComCtrls,
  Buttons, RadioSystem, RadioModule, IntfGraphics, FPReadPNG;

type

  { TTreeViewLogger }

  TTreeViewLogger = class(TRadioLogger)
  private
    FLevelTab: TTabControl;
    FMessageTree: TTreeView;
    FImages: TImageList;
    FReportLevel: TRadioLogLevel;
    FReportS: string;
    constructor Create;
    procedure SetLevelTab(AValue: TTabControl);
    procedure SetMessageTree(AValue: TTreeView);
    procedure LevelTabChange(Sender: TObject);
    procedure DoReportLCL;
  protected
    procedure DoReport(const ALevel: TRadioLogLevel; const S: string); override;
  public
    destructor Destroy; override;
    class procedure Start;

    property LevelTab: TTabControl read FLevelTab write SetLevelTab;
    property MessageTree: TTreeView read FMessageTree write SetMessageTree ;
  end;

implementation

uses
  util_config;

const
  LogLevel2ImageID: array[TRadioLogLevel] of Integer = (0, 1, 2, 3);
  LogLevel2ImageFn: array[TRadioLogLevel] of string = ('state12x12_hint.png',
                    'state12x12_information.png', 'state12x12_warning.png', 'state12x12_error.png');
  LogLevel2Str: array[TRadioLogLevel] of string = ('verb', 'inf', 'warn', 'err');

{ TTreeViewLogger }

constructor TTreeViewLogger.Create;
var
  I: TBitmap;
  S: string;
  M: TLazIntfImage;
  R: TFPReaderPNG;
begin
  inherited;
  FImages := TImageList.Create(nil);
  I := TBitmap.Create;
  M := TLazIntfImage.Create(0, 0, [riqfRGB]);
  R := TFPReaderPNG.create;
  for S in LogLevel2ImageFn do
  begin
    M.LoadFromFile(GetResFullName(S), R);
    I.LoadFromIntfImage(M);
    FImages.AddMasked(I, clWhite);
  end;
  I.Free;
  R.Free;
  M.Free;
end;

procedure TTreeViewLogger.SetLevelTab(AValue: TTabControl);
var
  I: string;
begin
  if FLevelTab = AValue then Exit;
  if Assigned(FLevelTab) then FLevelTab.OnChange := nil;
  FLevelTab := AValue;
  FLevelTab.OnChange := @LevelTabChange;
  FLevelTab.Tabs.Clear;
  FLevelTab.Images := FImages;
  for I in LogLevel2Str do
    FLevelTab.Tabs.Add(I);
  FLevelTab.TabIndex := Integer(FLevel);
end;

procedure TTreeViewLogger.SetMessageTree(AValue: TTreeView);
begin
  if FMessageTree = AValue then Exit;
  FMessageTree := AValue;
  FMessageTree.ShowLines := False;
  FMessageTree.ShowButtons := False;
  FMessageTree.ReadOnly := True;
  FMessageTree.Indent := 2;
  FMessageTree.Images := FImages;
end;

procedure TTreeViewLogger.LevelTabChange(Sender: TObject);
begin
  FLevel := TRadioLogLevel(FLevelTab.TabIndex);
end;

procedure TTreeViewLogger.DoReportLCL;
var
  N: TTreeNode;
  T: string;
begin
  FMessageTree.BeginUpdate;
  if FMessageTree.Items.Count > 1000 then FMessageTree.Items.Clear;
  T := FormatDateTime('hh:nn:ss.z ', Now);
  N := FMessageTree.Items.Add(nil, T + FReportS);
  N.ImageIndex := LogLevel2ImageID[FReportLevel];
  N.SelectedIndex := LogLevel2ImageID[FReportLevel];
  FMessageTree.TopItem := N;
  FMessageTree.EndUpdate;
end;

procedure TTreeViewLogger.DoReport(const ALevel: TRadioLogLevel; const S: string
  );
begin
  if not Assigned(FMessageTree) then Exit;
  FReportLevel := ALevel;
  FReportS     := S;
  TThread.Synchronize(nil, @DoReportLCL);
end;

destructor TTreeViewLogger.Destroy;
begin
  if Assigned(FLevelTab) then
  begin
    FLevelTab.OnChange := nil;
    FLevelTab.Images := nil;
  end;
  if Assigned(FMessageTree) then
  begin
    FMessageTree.Images := nil;
  end;
  FImages.Free;
  inherited Destroy;
end;

class procedure TTreeViewLogger.Start;
begin
  FreeAndNil(FInstance);
  FInstance := TTreeViewLogger.Create;
end;

end.

