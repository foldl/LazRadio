unit formmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, StdCtrls, ActnList, ComCtrls, RadioSystem;

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
  kissfft, UComplex, SignalBasic;

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
  FSystem.ConfigModule('rtl');
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  P: PFFTPlan;
  I, O: array [0..13 -1] of Complex;
  K: Integer;
begin
  for K := 0 to High(I) do
  begin
    Memo1.Lines.Add(Format('I0(%f) = %.9f', [K * 1.0, BesselI0(K)]));
  end;

end;

end.

