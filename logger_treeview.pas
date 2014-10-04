unit logger_treeview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, GraphType, Graphics, ComCtrls,
  RadioModule, IntfGraphics, FPReadPNG;

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
    procedure SetMessageTree(AValue: TTreeView);
    procedure DoReportLCL;
  protected
    procedure DoReport(const ALevel: TRadioLogLevel; const S: string); override;
  public
    destructor Destroy; override;
    class procedure Start;

    property MessageTree: TTreeView read FMessageTree write SetMessageTree ;
  end;

implementation

uses
  util_config;

const
  LogLevel2ImageID: array[TRadioLogLevel] of Integer = (0, 1, 2, 3, 4);
  LogLevel2ImageFn: array[TRadioLogLevel] of string = ('state12x12_hint.png',
                    'state12x12_information.png', 'state12x12_warning.png', 'state12x12_error.png',
                    'state12x12_system.png');

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
    FImages.AddMasked(I, clNone);
  end;
  I.Free;
  R.Free;
  M.Free;
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

