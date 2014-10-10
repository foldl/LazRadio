unit formmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, SynMemo,
  SynHighlighterAny, SynCompletion, TreeFilterEdit, ListFilterEdit, Forms,
  Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, ActnList, ComCtrls,
  Buttons, StdActns, RadioSystem, RadioModule, RadioLang, types, Math, LCLType;

type

  { TMainForm }

  TMainForm = class(TForm)
    MenuItem25: TMenuItem;
    ViewLogSystem: TAction;
    EditComplete: TAction;
    MenuItem11: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem24: TMenuItem;
    SystemCheck: TAction;
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
    PanelGraph: TAction;
    PanelCode: TAction;
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
    procedure EditCompleteExecute(Sender: TObject);
    procedure FileCloseExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpenAccept(Sender: TObject);
    procedure FileSaveAsAccept(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PanelCodeExecute(Sender: TObject);
    procedure PanelCodeUpdate(Sender: TObject);
    procedure PanelGraphExecute(Sender: TObject);
    procedure PanelGraphUpdate(Sender: TObject);
    procedure SynCompletionKeyNextChar(Sender: TObject);
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
    procedure ProjNameChanged(Sender: TObject);
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
  Genfft, UComplex, SignalBasic, logger_treeview, logger, radiomessage, rm_fm,
  formwait;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.EditCompleteExecute(Sender: TObject);
begin
  //SynCompletion.Execute();
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
  UpdateCaption;
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
var
  L: TStringList;
begin
  TRadioLogger.Level := llError;
  //TTextLogger.Start;
  TTreeViewLogger.Start;
  if TRadioLogger.GetInstance is TTreeViewLogger then
    with TRadioLogger.GetInstance as TTreeViewLogger do
    begin
      MessageTree := TreeView1;
    end;

  L := TStringList.Create;

  FSystem := TRadioSystem.Create;
  FSystem.ShowModules(ModuleTree);
  FSystem.GetModList(SynAnySyn.Objects, False);
  FSystem.GetModList(L, True);

  FRuntime := TRadioLangRT.Create;
  FRuntime.OnProjNameChanged := @ProjNameChanged;
  FSystem.GetModList(FRuntime.ModuleTypes, False);
  FSystem.Graph.PaintBox := PaintBox1;
  FRuntime.GetRTSymbols(L);

  L.Sort;
  SynCompletion.ItemList.Assign(L);
  L.Free;
  UpdateCaption;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  OpenProject('.\examples\rds-debug.lzr');
end;

procedure TMainForm.PanelCodeExecute(Sender: TObject);
begin
  PageControl1.TabIndex := 0;
end;

procedure TMainForm.PanelCodeUpdate(Sender: TObject);
begin
  PanelCode.Checked := PageControl1.TabIndex = 0;
end;

procedure TMainForm.PanelGraphExecute(Sender: TObject);
begin
  PageControl1.TabIndex := 1;
end;

procedure TMainForm.PanelGraphUpdate(Sender: TObject);
begin
  PanelGraph.Checked := PageControl1.TabIndex = 1;
end;

procedure TMainForm.SynCompletionKeyNextChar(Sender: TObject);
begin

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
  WaitForm.ShowWaitMessage('Executing...');
  WaitForm.Update;
  FSystem.Reset;
  if FRuntime.Exec(FFileName) then
  begin
    FSystem.ShowSystem;
    PanelGraph.Execute;
  end;
  WaitForm.Hide;
end;

procedure TMainForm.SystemRedrawExecute(Sender: TObject);
begin
  FSystem.Graph.FullRender;
end;

procedure TMainForm.SystemResetExecute(Sender: TObject);
begin
  FSystem.Reset;
  SystemFullRedraw.Execute;
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


procedure TMainForm.ProjNameChanged(Sender: TObject);
begin
  SystemName := FRuntime.ProjName;
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

