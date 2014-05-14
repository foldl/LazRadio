unit formspectrum;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TSpectrumForm }

  TSpectrumForm = class(TForm)
    Image1: TImage;
    Image2: TImage;
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

end.

