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
  private
    FSystem: TRadioSystem;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
begin
  if not Assigned(FSystem) then
  begin
    FSystem := TRadioSystem.Create;
    FSystem.AddModule('rtl', 'RtlModule');
  end;
  FSystem.ConfigModule('rtl');
end;

end.

