unit formmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, StdCtrls, ActnList, ComCtrls, RadioSystem, RadioModule;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    SynEdit1: TSynEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FSystem: TRadioSystem;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  kissfft, UComplex, SignalBasic, rm_spectrum, rm_oscillator, logger;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
begin
  TTextLogger.Start;
  if not Assigned(FSystem) then
  begin
    FSystem := TRadioSystem.Create;
    FSystem.AddModule('s', 'Spectrum');
    FSystem.AddModule('a', 'AudioIn');
    FSystem.AddModule('u', 'AudioOut');
    FSystem.AddModule('o', 'Oscillator');
    FSystem.AddModule('dump', 'Dump');
  end;
  //FSystem.ConfigModule('rtl');
  FSystem.ConnectModuel('o', 's');
  FSystem.ConnectModuel('o', 'u');
  //FSystem.ConnectModuel('a', 'dump');

  RadioPostMessage(RM_SPECTRUM_CFG, SET_FFT_SIZE, 44100 div 4, 's');
  RadioPostMessage(RM_SPECTRUM_CFG, SET_Y_RANGE, 10, 's');
  RadioPostMessage(RM_SPECTRUM_CFG, SET_SPAN, 6000, 's');
  RadioPostMessage(RM_SPECTRUM_CFG, SET_CENTER_FREQ, 3000, 's');
  //RadioPostMessage(RM_SPECTRUM_CFG, SET_SPAN, 0, 's');

  FSystem.ConfigModule('o');
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  Button1Click(Sender);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  RadioPostMessage(RM_SET_FEATURE, RM_FEATURE_FREQ, 5000, 'o');
end;

end.

