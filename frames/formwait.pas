unit formwait;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TWaitForm }

  TWaitForm = class(TForm)
    Label1: TLabel;
  private
    { private declarations }
  public
    procedure ShowWaitMessage(const Msg: string);
  end;

var
  WaitForm: TWaitForm;

implementation

{$R *.lfm}

{ TWaitForm }

procedure TWaitForm.ShowWaitMessage(const Msg: string);
begin
  Label1.Caption := Msg;
  Visible := True;
end;

end.

