unit formgui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TGUIForm }

  TGUIForm = class(TForm)
    PaintBox1: TPaintBox;
    ScrollBox1: TScrollBox;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  GUIForm: TGUIForm;

implementation

{$R *.lfm}

{ TGUIForm }

procedure TGUIForm.FormCreate(Sender: TObject);
begin

end;

end.

