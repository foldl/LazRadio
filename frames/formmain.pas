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
  private
    FSystem: TRadioSystem;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  kissfft, UComplex, SignalBasic, rm_spectrum;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
begin
  if not Assigned(FSystem) then
  begin
    FSystem := TRadioSystem.Create;
    FSystem.AddModule('s', 'Spectrum');
    FSystem.AddModule('o', 'Oscillator');
  end;
  //FSystem.ConfigModule('rtl');
  FSystem.ConnectModuel('o', 's');
  RadioPostMessage(RM_CONTROL, RM_CONTROL_RUN, 0, 's');
  RadioPostMessage(RM_CONTROL, RM_CONTROL_RUN, 0, 'o');

  RadioPostMessage(RM_SET_FEATURE, RM_FEATURE_FREQ, 500, 'o');
  RadioPostMessage(RM_SET_FEATURE, RM_FEATURE_SAMPLE_RATE, 20000, 'o');
  RadioPostMessage(RM_SPECTRUM_CFG, SET_FFT_SIZE, 1024 * 5, 's');
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  Button1Click(Sender);
end;

end.

