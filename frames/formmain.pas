unit formmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, TreeFilterEdit, ListFilterEdit, Forms,
  Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, ActnList, ComCtrls,
  Buttons, StdActns, RadioSystem, RadioModule;

type

  { TMainForm }

  TMainForm = class(TForm)
    ModuleImgs: TImageList;
    MenuItem2: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    ViewLogError: TAction;
    ViewLogWarn: TAction;
    ViewLogInfo: TAction;
    ViewLogVerbose: TAction;
    SystemPause: TAction;
    SystemReset: TAction;
    SystemStop: TAction;
    SystemStart: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit2: TEdit;
    FileExit1: TFileExit;
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    HelpOnHelp1: THelpOnHelp;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PageControl1: TPageControl;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    SynEdit1: TSynEdit;
    SheetCode: TTabSheet;
    SheetGraph: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    TreeFilterEdit1: TTreeFilterEdit;
    TreeView1: TTreeView;
    ModuleTree: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ViewLogLevelExecute(Sender: TObject);
    procedure ViewLogLevelUpdate(Sender: TObject);
  private
    FSystem: TRadioSystem;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  Genfft, UComplex, SignalBasic, logger_treeview, radiomessage, rm_fm;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
begin
  FSystem.Graph.PaintBox := PaintBox1;
  FSystem.AddModule('s', 'Spectrum');
  FSystem.AddModule('s2', 'Spectrum');
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

  FSystem.ConnectBoth('src', 's');
  FSystem.ConnectFeature('s', 'mixer1');
  FSystem.ConnectFeature('s', 're1');
  FSystem.ConnectFeature('s', 'mixer2');
  FSystem.ConnectFeature('s', 're2');

  FSystem.ConnectBoth('src', 'mixer1');
  FSystem.ConnectBoth('mixer1', 're1');
  FSystem.ConnectBoth('re1', 'fd1');
  FSystem.ConnectBoth('fd1', 'fm1');
  FSystem.ConnectBoth('fm1', 'aumixer', 0);

  FSystem.ConnectBoth('fm1', 'rds', 0, 1);

  //FSystem.ConnectBoth('fm1', 's2');

{
  FSystem.ConnectBoth('src', 'mixer2');
  FSystem.ConnectBoth('mixer2', 're2');
  FSystem.ConnectBoth('re2', 'fm2');
  FSystem.ConnectBoth('fm2', 'f2');
  FSystem.ConnectData('f2', 'aumixer', 1);
}

  FSystem.ConnectBoth('aumixer', 'u');

  // set-up channel 1
  RadioPostMessage(RM_RESAMPLING_CFG, 200000, 80000, 're1');
  RadioPostMessage(RM_SET_FEATURE, RM_FEATURE_FREQ, 0, 'f1');
  RadioPostMessage(RM_SET_FEATURE, RM_FEATURE_SAMPLE_RATE, 8000, 'f1');
  RadioPostMessage(RM_SPECTRUM_BAND_SELECT_1, 0, 15000, 'f1');

  //FSystem.ConnectBoth('src', 'dump');

 // FSystem.ConnectBoth('f', 's2');
  //FSystem.ConnectBoth('f', 'u');
 // FSystem.ConnectBoth('dump', 'u');

  RadioPostMessage(RM_SPECTRUM_CFG, SET_FFT_SIZE, 32768, 's');
//  RadioPostMessage(RM_SPECTRUM_CFG, SET_Y_RANGE, 10, 's');
  RadioPostMessage(RM_SPECTRUM_CFG, SET_SPAN, 100000, 's2');
  RadioPostMessage(RM_SPECTRUM_CFG, SET_CENTER_FREQ, 50000, 's2');
 // RadioPostMessage(RM_SPECTRUM_CFG, SET_DATA_DOMAIN, SPECTRUM_DATA_DOMAIN_REAL, 's2');
//  RadioPostMessage(RM_FILTER_CONFIG, FILTER_COEFF_DOMAIN, FILTER_COEFF_DOMAIN_REAL, 'f2');
  //RadioPostMessage(RM_SPECTRUM_CFG, SET_SPAN, 0, 's');

  RadioPostMessage(RM_AUDIOMIXER_CFG, AUDIOMIXER_STREAM_NUM, 4, 'aumixer');
  RadioPostMessage(RM_AUDIO_OUT_FMT, AUDIO_OUT_FMT_STEREO_IQ, 0, 'u');

 // FSystem.ConfigModule('a');
 // FSystem.ConfigModule('r');
 //  RadioPostMessage(RM_DUMP_PLAYER_START, PtrUInt(TFileStream.Create('D:\baiduyundownload\90.0MHz.dump', fmOpenRead)), 0, 'src');
  //  RadioPostMessage(RM_DUMP_PLAYER_START, PtrUInt(TFileStream.Create('e:\90.0MHz.dump', fmOpenRead)), 0, 'src');
  //FSystem.ConfigModule('src');
  FSystem.ShowSystem;
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
  FSystem.Graph.FullRender;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FSystem.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FSystem := TRadioSystem.Create;
  FSystem.ShowModules(ModuleTree);
  TRadioLogger.Level := llError;
  TTreeViewLogger.Start;
  with TRadioLogger.GetInstance as TTreeViewLogger do
  begin
    MessageTree := TreeView1;
  end;
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

end.

