unit formaudiomixer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls;

type

  { TAudioMixerForm }

  TAudioMixerForm = class(TForm)
    ScrollBox1: TScrollBox;
  private
    { private declarations }
  public
    { public declarations }
  end;

  {
  Off
IQ -> L/R
QI -> L/R
I -> L
I -> R
Q -> L
Q -> R
}

var
  AudioMixerForm: TAudioMixerForm;

implementation

{$R *.lfm}

end.

