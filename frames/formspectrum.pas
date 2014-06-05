unit formspectrum;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TSpectrumForm }

  TSpectrumForm = class(TForm)
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    Splitter1: TSplitter;
  private
    { private declarations }

  public
    { public declarations }
  end;

var
  SpectrumForm: TSpectrumForm;

implementation

{$R *.lfm}

{ TSpectrumForm }


end.

