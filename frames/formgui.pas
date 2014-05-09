unit formgui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TGUIForm }

  TGUIForm = class(TForm)
    PaintBox1: TPaintBox;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  GUIForm: TGUIForm;

implementation

{$R *.lfm}

end.

