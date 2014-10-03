unit formmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, SynMemo,
  SynHighlighterAny, SynCompletion, TreeFilterEdit, ListFilterEdit, Forms,
  Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, ActnList, ComCtrls,
  Buttons, StdActns, RadioSystem, RadioModule, RadioLang, types, Math;

type

  { TMainForm }

  TMainForm = class(TForm)
    MenuItem11: TMenuItem;
    SystemCheck: TAction;
    Image1: TImage;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    SynCompletion: TSynCompletion;
    SystemReset: TAction;
    SystemRedraw: TAction;
    SystemFullRedraw: TAction;
    Edit2: TEdit;
    FileClose: TAction;
    FileNew: TAction;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    Label1: TLabel;
    ModuleIntro: TMemo;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    ModuleTree: TTreeView;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    PannelGraph: TAction;
    PannelCode: TAction;
    FileSave: TAction;
    MenuItem10: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem9: TMenuItem;
    ModuleImgs: TImageList;
    MenuItem2: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    SynAnySyn: TSynAnySyn;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    TreeFilterEdit1: TTreeFilterEdit;
    TreeView1: TTreeView;
    ViewLogError: TAction;
    ViewLogWarn: TAction;
    ViewLogInfo: TAction;
    ViewLogVerbose: TAction;
    SystemGo: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    FileExit: TFileExit;
    FileOpen: TFileOpen;
    FileSaveAs: TFileSaveAs;
    HelpOnHelp1: THelpOnHelp;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PageControl1: TPageControl;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    SynEdit: TSynEdit;
    SheetCode: TTabSheet;
    SheetGraph: TTabSheet;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FileCloseExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpenAccept(Sender: TObject);
    procedure FileSaveAsAccept(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PannelCodeExecute(Sender: TObject);
    procedure PannelCodeUpdate(Sender: TObject);
    procedure PannelGraphExecute(Sender: TObject);
    procedure PannelGraphUpdate(Sender: TObject);
    procedure SynEditChange(Sender: TObject);
    procedure SynEditMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SystemCheckExecute(Sender: TObject);
    procedure SystemFullRedrawExecute(Sender: TObject);
    procedure SystemGoExecute(Sender: TObject);
    procedure SystemRedrawExecute(Sender: TObject);
    procedure SystemResetExecute(Sender: TObject);
    procedure ViewLogLevelExecute(Sender: TObject);
    procedure ViewLogLevelUpdate(Sender: TObject);
  private
    FModified: Boolean;
    FSystem: TRadioSystem;
    FRuntime: TRadioLangRT;
    FSystemName: string;
    FFileName: string;
    procedure FMSys;
    procedure AMSys;
    procedure RDSSys;
    procedure SetModified(AValue: Boolean);
    procedure SetSystemName(AValue: string);
    procedure UpdateCaption;
    procedure OpenProject(const Fn: string);
    procedure CloseProject;
    function  AskForClose: Boolean;
  public
    property Modified: Boolean read FModified write SetModified;
    property SystemName: string read FSystemName write SetSystemName;
  end;

var
  MainForm: TMainForm;

implementation

uses
  Genfft, UComplex, SignalBasic, logger_treeview, logger, radiomessage, rm_fm;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
begin
  RDSSys;
end;

procedure TMainForm.BitBtn1Click(Sender: TObject);
begin

end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  Button1Click(Sender);
end;

procedure TMainForm.Button3Click(Sender: TObject);
var
  M: Cardinal = $1234;
  C: Cardinal;
  S: Cardinal;
  X: Word;
  E: Integer;
  B: Boolean;
begin
  {C := EncodeMessage(M);
  S := CalcSyndrome(C);
  TRadioLogger.Report(llError, 'encoded = %7x, syndrome = %3x', [C, S]);
  C := C xor ($1f shl 10);
  B := DecodeMessage(C, X, E);
  TRadioLogger.Report(llError, 'received = %7x', [C]);
  TRadioLogger.Report(llError, 'decoded = %7x, ok = %d, errbits = %d', [X, Ord(B), E]);
  }
  RadioPostMessage(RM_DUMP_PLAYER_STOP, 0, 0, 'src');
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
  FSystem.ShowSystem;
end;

procedure TMainForm.FileCloseExecute(Sender: TObject);
begin
  AskForClose;
end;

procedure TMainForm.FileNewExecute(Sender: TObject);
begin
  AskForClose;
end;

procedure TMainForm.FileOpenAccept(Sender: TObject);
begin
  FileClose.Execute;
  OpenProject(FileOpen.Dialog.FileName);
end;

procedure TMainForm.FileSaveAsAccept(Sender: TObject);
begin
  FFileName := FileSaveAs.Dialog.FileName;
  SynEdit.Lines.SaveToFile(FFileName);
  Modified := False;
end;

procedure TMainForm.FileSaveExecute(Sender: TObject);
begin
  if FFileName = '' then
  begin
    FileSaveAs.Execute;
    Exit;
  end;
  SynEdit.Lines.SaveToFile(FFileName);
  Modified := False;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := AskForClose;
  if CanClose then
    FSystem.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TRadioLogger.Level := llError;
  //TTextLogger.Start;
  TTreeViewLogger.Start;
  if TRadioLogger.GetInstance is TTreeViewLogger then
    with TRadioLogger.GetInstance as TTreeViewLogger do
    begin
      MessageTree := TreeView1;
    end;

  FSystem := TRadioSystem.Create;
  FSystem.ShowModules(ModuleTree);
  FSystem.GetModList(SynAnySyn.Objects);
  FSystem.GetModList(SynCompletion.ItemList);

  FRuntime := TRadioLangRT.Create;
  FSystem.GetModList(FRuntime.ModuleTypes);
  FSystem.Graph.PaintBox := PaintBox1;

  UpdateCaption;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  OpenProject('.\examples\fm.lzr');
end;

procedure TMainForm.PannelCodeExecute(Sender: TObject);
begin
  PageControl1.TabIndex := 0;
end;

procedure TMainForm.PannelCodeUpdate(Sender: TObject);
begin
  PannelCode.Checked := PageControl1.TabIndex = 0;
end;

procedure TMainForm.PannelGraphExecute(Sender: TObject);
begin
  PageControl1.TabIndex := 1;
end;

procedure TMainForm.PannelGraphUpdate(Sender: TObject);
begin
  PannelGraph.Checked := PageControl1.TabIndex = 1;
end;

procedure TMainForm.SynEditChange(Sender: TObject);
begin
  Modified := True;
end;

procedure TMainForm.SynEditMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  S: Integer;
begin
  if ssCtrl in Shift then
  begin
    S := SynEdit.Font.Size + Sign(WheelDelta);
    S := Min(90, Max(7, S));
    SynEdit.Font.Size := S;
  end;
end;

procedure TMainForm.SystemCheckExecute(Sender: TObject);
begin
  if Modified then FileSave.Execute;
  if Modified then Exit;
  FRuntime.Check(FFileName);
end;

procedure TMainForm.SystemFullRedrawExecute(Sender: TObject);
begin
  FSystem.ShowSystem;
end;

procedure TMainForm.SystemGoExecute(Sender: TObject);
begin
  if Modified then FileSave.Execute;
  if Modified then Exit;
  FSystem.Reset;
  FRuntime.Exec(FFileName);
  FSystem.ShowSystem;
end;

procedure TMainForm.SystemRedrawExecute(Sender: TObject);
begin
  FSystem.Graph.FullRender;
end;

procedure TMainForm.SystemResetExecute(Sender: TObject);
begin
  FSystem.Reset;
end;

procedure TMainForm.ViewLogLevelExecute(Sender: TObject);
begin
  if Sender is TComponent then
    TRadioLogger.Level := TRadioLogLevel((Sender as TComponent).Tag);
end;

procedure TMainForm.ViewLogLevelUpdate(Sender: TObject);
begin
  if Sender is TAction then
    with Sender as TAction do
      Checked := TRadioLogger.Level = TRadioLogLevel(Tag);
end;

procedure TMainForm.FMSys;
begin
  FSystem.AddModule('s', 'Spectrum');
  FSystem.AddModule('s2', 'Spectrum');
  FSystem.AddModule('iqcor', 'IQCorrecter');
  FSystem.AddModule('a', 'AudioIn');
  FSystem.AddModule('u', 'AudioOut');
  FSystem.AddModule('mixer1', 'FreqMixer');
  FSystem.AddModule('mixer2', 'FreqMixer');
  FSystem.AddModule('f1', 'Filter');
  FSystem.AddModule('f2', 'Filter');
  FSystem.AddModule('r', 'DumpPlayer');
  FSystem.AddModule('src', 'Rtl');
  FSystem.AddModule('dump', 'Dump');
  FSystem.AddModule('fd1', 'FreqDiscriminator');
  FSystem.AddModule('fm1', 'FMReceiver');
  FSystem.AddModule('re1', 'Resampling');
  FSystem.AddModule('re2', 'Resampling');
  FSystem.AddModule('aumixer', 'AudioMixer');
  FSystem.AddModule('rds', 'RDSDecoder');
  FSystem.AddModule('scope', 'Oscilloscope');

  FSystem.ConnectBoth('src', 'iqcor');
  FSystem.ConnectBoth('iqcor', 'f1');
  FSystem.ConnectBoth('f1', 'mixer1');
  FSystem.ConnectBoth('mixer1', 're1');
  FSystem.ConnectBoth('re1', 'fd1');
{
  FSystem.ConnectBoth('fd1', 'f1');
  FSystem.ConnectBoth('f1', 'mixer2');
  FSystem.ConnectBoth('mixer2', 're2');
  FSystem.ConnectBoth('re2', 'f2');
  FSystem.ConnectBoth('f2', 'rds');

  FSystem.ConnectBoth('mixer2', 's');
  FSystem.ConnectBoth('f2', 's2');

  FSystem.ConnectBoth('f2', 'scope');
  }

  FSystem.ConnectBoth('mixer1', 's2');
  FSystem.ConnectBoth('iqcor', 's');
  FSystem.ConnectFeature('s', 'mixer1');
  FSystem.ConnectFeature('s', 'f1');

  FSystem.ConnectBoth('fd1', 'fm1');
  FSystem.ConnectBoth('fm1', 'aumixer');
  FSystem.ConnectBoth('aumixer', 'u');

{
  FSystem.ConnectBoth('src', 'mixer2');
  FSystem.ConnectBoth('mixer2', 're2');
  FSystem.ConnectBoth('re2', 'fm2');
  FSystem.ConnectBoth('fm2', 'f2');
  FSystem.ConnectData('f2', 'aumixer', 1);
}

  // set-up channel 1
  RadioPostMessage(RM_RESAMPLING_CFG, 200000, 90000, 're1');

  // set-up rds
  RadioPostMessage(RM_RESAMPLING_CFG, 9500, 2500, 're2');
  RadioPostMessage(RM_FREQMIXER_SET_FREQ, 57000, 0, 'mixer2');
  RadioPostMessage(RM_SET_FEATURE, RM_FEATURE_SAMPLE_RATE, 200000, 'f1');
  RadioPostMessage(RM_FILTER_CONFIG, FILTER_TAPS, 400, 'f1');
  RadioPostMessage(RM_SPECTRUM_BAND_SELECT_1, 57000 - 4500, 57000 + 4500, 'f1');
  RadioPostMessage(RM_SET_FEATURE, RM_FEATURE_SAMPLE_RATE, 9500, 'f2');
  RadioPostMessage(RM_FILTER_CONFIG, FILTER_COEFF_DOMAIN, FILTER_COEFF_DOMAIN_REAL, 'f2');
  RadioPostMessage(RM_FILTER_CONFIG, FILTER_TYPE, Ord(ftLPF), 'f2');
  RadioPostMessage(RM_FILTER_CONFIG, FILTER_OMEGA, 2500, 'f2');
  RadioPostMessage(RM_FILTER_CONFIG, FILTER_TAPS, 64, 'f2');
  RadioPostMessage(RM_FILTER_REDESIGN, 0, 0, 'f2');

  //FSystem.ConnectBoth('src', 'dump');

 // FSystem.ConnectBoth('f', 's2');
  //FSystem.ConnectBoth('f', 'u');
 // FSystem.ConnectBoth('dump', 'u');

  RadioPostMessage(RM_SPECTRUM_CFG, SET_FFT_SIZE, 32768, 's');
//  RadioPostMessage(RM_SPECTRUM_CFG, SET_Y_RANGE, 10, 's');
  RadioPostMessage(RM_SPECTRUM_CFG, SET_SPAN, 100000, 's2');
  RadioPostMessage(RM_SPECTRUM_CFG, SET_CENTER_FREQ, 50000, 's2');
  //RadioPostMessage(RM_SPECTRUM_CFG, SET_DATA_DOMAIN, SPECTRUM_DATA_DOMAIN_REAL, 's2');
//  RadioPostMessage(RM_FILTER_CONFIG, FILTER_COEFF_DOMAIN, FILTER_COEFF_DOMAIN_REAL, 'f2');
  //RadioPostMessage(RM_SPECTRUM_CFG, SET_SPAN, 0, 's');

  RadioPostMessage(RM_AUDIOMIXER_CFG, AUDIOMIXER_STREAM_NUM, 4, 'aumixer');
  RadioPostMessage(RM_AUDIO_OUT_FMT, AUDIO_OUT_FMT_STEREO_IQ, 0, 'u');

 // FSystem.ConfigModule('a');
 // FSystem.ConfigModule('r');
  // RadioPostMessage(RM_DUMP_PLAYER_START, PtrUInt(TFileStream.Create('D:\baiduyundownload\90.0MHz.dump', fmOpenRead)), 0, 'src');
  //  RadioPostMessage(RM_DUMP_PLAYER_START, PtrUInt(TFileStream.Create('e:\90.0MHz.dump', fmOpenRead)), 0, 'src');
  //FSystem.ConfigModule('src');
  FSystem.ShowSystem;
end;

procedure TMainForm.AMSys;
begin
  FSystem.AddModule('s', 'Spectrum');
  FSystem.AddModule('s2', 'Spectrum');
  FSystem.AddModule('iqcor', 'IQCorrecter');
  FSystem.AddModule('a', 'AudioIn');
  FSystem.AddModule('u', 'AudioOut');
  FSystem.AddModule('mixer1', 'FreqMixer');
  FSystem.AddModule('mixer2', 'FreqMixer');
  FSystem.AddModule('f1', 'Filter');
  FSystem.AddModule('f2', 'Filter');
  FSystem.AddModule('r', 'DumpPlayer');
  FSystem.AddModule('src', 'Rtl');
  FSystem.AddModule('dump', 'Dump');
  FSystem.AddModule('fd1', 'FreqDiscriminator');
  FSystem.AddModule('fm1', 'FMReceiver');
  FSystem.AddModule('re1', 'Resampling');
  FSystem.AddModule('re2', 'Resampling');
  FSystem.AddModule('aumixer', 'AudioMixer');
  FSystem.AddModule('rds', 'RDSDecoder');
  FSystem.AddModule('scope', 'Oscilloscope');

  FSystem.ConnectBoth('src', 'iqcor');
  FSystem.ConnectBoth('iqcor', 'f1');
  FSystem.ConnectBoth('f1', 'mixer1');
  FSystem.ConnectBoth('mixer1', 're1');
  FSystem.ConnectBoth('re1', 'fd1');


  FSystem.ConnectBoth('mixer1', 's2');
  FSystem.ConnectBoth('iqcor', 's');
  FSystem.ConnectFeature('s', 'mixer1');
  FSystem.ConnectFeature('s', 'f1');

  FSystem.ConnectBoth('fm1', 'aumixer');
  FSystem.ConnectBoth('aumixer', 'u');

{
  FSystem.ConnectBoth('src', 'mixer2');
  FSystem.ConnectBoth('mixer2', 're2');
  FSystem.ConnectBoth('re2', 'fm2');
  FSystem.ConnectBoth('fm2', 'f2');
  FSystem.ConnectData('f2', 'aumixer', 1);
}

  // set-up channel 1
  RadioPostMessage(RM_RESAMPLING_CFG, 200000, 90000, 're1');

  // set-up rds
  RadioPostMessage(RM_RESAMPLING_CFG, 9500, 2500, 're2');
  RadioPostMessage(RM_FREQMIXER_SET_FREQ, 57000, 0, 'mixer2');
  RadioPostMessage(RM_SET_FEATURE, RM_FEATURE_SAMPLE_RATE, 200000, 'f1');
  RadioPostMessage(RM_FILTER_CONFIG, FILTER_TAPS, 400, 'f1');
  RadioPostMessage(RM_SPECTRUM_BAND_SELECT_1, 57000 - 4500, 57000 + 4500, 'f1');
  RadioPostMessage(RM_SET_FEATURE, RM_FEATURE_SAMPLE_RATE, 9500, 'f2');
  RadioPostMessage(RM_FILTER_CONFIG, FILTER_COEFF_DOMAIN, FILTER_COEFF_DOMAIN_REAL, 'f2');
  RadioPostMessage(RM_FILTER_CONFIG, FILTER_TYPE, Ord(ftLPF), 'f2');
  RadioPostMessage(RM_FILTER_CONFIG, FILTER_OMEGA, 2500, 'f2');
  RadioPostMessage(RM_FILTER_CONFIG, FILTER_TAPS, 64, 'f2');
  RadioPostMessage(RM_FILTER_REDESIGN, 0, 0, 'f2');

  //FSystem.ConnectBoth('src', 'dump');

 // FSystem.ConnectBoth('f', 's2');
  //FSystem.ConnectBoth('f', 'u');
 // FSystem.ConnectBoth('dump', 'u');

  RadioPostMessage(RM_SPECTRUM_CFG, SET_FFT_SIZE, 32768, 's');
//  RadioPostMessage(RM_SPECTRUM_CFG, SET_Y_RANGE, 10, 's');
  RadioPostMessage(RM_SPECTRUM_CFG, SET_SPAN, 100000, 's2');
  RadioPostMessage(RM_SPECTRUM_CFG, SET_CENTER_FREQ, 50000, 's2');
  //RadioPostMessage(RM_SPECTRUM_CFG, SET_DATA_DOMAIN, SPECTRUM_DATA_DOMAIN_REAL, 's2');
//  RadioPostMessage(RM_FILTER_CONFIG, FILTER_COEFF_DOMAIN, FILTER_COEFF_DOMAIN_REAL, 'f2');
  //RadioPostMessage(RM_SPECTRUM_CFG, SET_SPAN, 0, 's');

  RadioPostMessage(RM_AUDIOMIXER_CFG, AUDIOMIXER_STREAM_NUM, 4, 'aumixer');
  RadioPostMessage(RM_AUDIO_OUT_FMT, AUDIO_OUT_FMT_STEREO_IQ, 0, 'u');

  FSystem.ShowSystem;
end;

procedure TMainForm.RDSSys;
begin
  FSystem.AddModule('src', 'DumpPlayer');
  FSystem.AddModule('iqcor', 'IQCorrecter');
  FSystem.AddModule('s', 'Spectrum');
  FSystem.AddModule('s2', 'Spectrum');
  FSystem.AddModule('mixer1', 'FreqMixer');
  FSystem.AddModule('mixer2', 'FreqMixer');
  FSystem.AddModule('f1', 'Filter');
  FSystem.AddModule('f2', 'Filter');
  // FSystem.AddModule('f3', 'Filter');
  FSystem.AddModule('fd1', 'FreqDiscriminator');
  FSystem.AddModule('fm1', 'FMReceiver');
  FSystem.AddModule('re1', 'Resampling');
  FSystem.AddModule('re2', 'Resampling');
  FSystem.AddModule('rds', 'RDSDecoder');
  FSystem.AddModule('scope', 'Oscilloscope');

  FSystem.ConnectBoth('src', 'iqcor');
  FSystem.ConnectBoth('iqcor', 'f1');
  FSystem.ConnectBoth('f1', 'mixer1');
  FSystem.ConnectBoth('mixer1', 're1');
  FSystem.ConnectBoth('re1', 'fd1');
  FSystem.ConnectBoth('fd1', 'f2');
  FSystem.ConnectBoth('f2', 'mixer2');
  FSystem.ConnectBoth('mixer2', 're2');
  FSystem.ConnectBoth('re2', 'rds');

  FSystem.ConnectBoth('mixer1', 's');
  FSystem.ConnectBoth('re2', 's2');
  FSystem.ConnectBoth('re2', 'scope');

  // set-up channel 1
  RadioPostMessage(RM_RESAMPLING_CFG, 200000, 90000, 're1');
  RadioPostMessage(RM_RESAMPLING_CFG, 9500, 2500, 're2');

  RadioPostMessage(RM_FREQMIXER_SET_FREQ, 57000, 0, 'mixer2');
  RadioPostMessage(RM_SET_FEATURE, RM_FEATURE_SAMPLE_RATE, 2048000, 'f1');
  RadioPostMessage(RM_FILTER_CONFIG, FILTER_TAPS, 200, 'f1');
  RadioPostMessage(RM_SPECTRUM_BAND_SELECT_1, -90000, 90000, 'f1');
  RadioPostMessage(RM_SET_FEATURE, RM_FEATURE_SAMPLE_RATE, 200000, 'f2');
  RadioPostMessage(RM_FILTER_CONFIG, FILTER_TAPS, 200, 'f2');
  RadioPostMessage(RM_SPECTRUM_BAND_SELECT_1, 57000 - 2500, 57000 + 2500, 'f2');
  {
  RadioPostMessage(RM_SET_FEATURE, RM_FEATURE_SAMPLE_RATE, 9500, 'f3');
  RadioPostMessage(RM_FILTER_CONFIG, FILTER_TYPE, Ord(ftBPF), 'f3');
  RadioPostMessage(RM_FILTER_CONFIG, FILTER_OMEGA, 2500, 'f3');
  RadioPostMessage(RM_FILTER_CONFIG, FILTER_TAPS, 64, 'f3');
  RadioPostMessage(RM_FILTER_REDESIGN, 0, 0, 'f3');
  }
  //FSystem.ConnectBoth('src', 'dump');

 // FSystem.ConnectBoth('f', 's2');
  //FSystem.ConnectBoth('f', 'u');
 // FSystem.ConnectBoth('dump', 'u');

  RadioPostMessage(RM_SPECTRUM_CFG, SET_FFT_SIZE, 32768, 's');
//  RadioPostMessage(RM_SPECTRUM_CFG, SET_Y_RANGE, 10, 's');
  RadioPostMessage(RM_SPECTRUM_CFG, SET_SPAN, 100000, 's2');
  RadioPostMessage(RM_SPECTRUM_CFG, SET_CENTER_FREQ, 50000, 's2');
  //RadioPostMessage(RM_SPECTRUM_CFG, SET_DATA_DOMAIN, SPECTRUM_DATA_DOMAIN_REAL, 's2');
//  RadioPostMessage(RM_FILTER_CONFIG, FILTER_COEFF_DOMAIN, FILTER_COEFF_DOMAIN_REAL, 'f2');
  //RadioPostMessage(RM_SPECTRUM_CFG, SET_SPAN, 0, 's');

 // FSystem.ConfigModule('a');
 // FSystem.ConfigModule('r');
  //FSystem.ConfigModule('src');

  FSystem.ShowSystem;

 // RadioPostMessage(RM_DUMP_PLAYER_START, PtrUInt(TFileStream.Create('D:\baiduyundownload\90.0MHz.dump', fmOpenRead)), 0, 'src');
  //RadioPostMessage(RM_DUMP_PLAYER_START, PtrUInt(TFileStream.Create('e:\90.0MHz.dump', fmOpenRead)), 0, 'src');
end;

procedure TMainForm.SetModified(AValue: Boolean);
begin
  if FModified = AValue then Exit;
  FModified := AValue;
  UpdateCaption;
end;

procedure TMainForm.SetSystemName(AValue: string);
begin
  if FSystemName = AValue then Exit;
  FSystemName := AValue;
  UpdateCaption;
end;

procedure TMainForm.UpdateCaption;
var
  S: string;
begin
  S := '(unnamed)';
  if FSystemName <> '' then S := FSystemName;
  if FModified then
    Caption := Format('%s - %s* - LazRadio', [S, FFileName])
  else
    Caption := Format('%s - %s - LazRadio',  [S, FFileName]);
  Application.Title := Caption;
end;

procedure TMainForm.OpenProject(const Fn: string);
begin
  FSystem.Reset;
  FFileName := Fn;
  SynEdit.Lines.LoadFromFile(FFileName);
  SystemName := '';
  UpdateCaption;
end;

procedure TMainForm.CloseProject;
begin
  FSystem.Reset;
end;

function TMainForm.AskForClose: Boolean;
label
  SAVE, NOT_SAVE;
begin
  Result := False;
  if Modified then
  begin
    case MessageDlg('Modified', 'File modified. Save [Yes] or not [No] or Cancel?',
     mtConfirmation, mbYesNoCancel, '') of
      mrNo: goto NOT_SAVE;
      mrCancel: Exit;
    end;
  end
  else
    goto NOT_SAVE;

SAVE:
  FileSave.Execute;

  if Modified then
  begin
    if MessageDlg('Disard?', 'File not saved. Are you sure you want to discard it?',
     mtWarning, mbYesNo, '') = mrNo then
     goto SAVE;
  end;

NOT_SAVE:

  FSystem.Reset;
  SynEdit.ClearAll;
  FFileName := '';
  SystemName := '';
  Modified := False;
  Result := True;
end;

end.

