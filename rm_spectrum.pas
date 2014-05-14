unit rm_spectrum;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioModule;

type

  TSpecturmWindowFunc = (wfRect, wfKaiser, wfBatterly);

  TRadioSpectrum = class(TRadioModule)
  private
    FFreq: Cardinal;
    FSampleRate: Cardinal;
    FWindow: TSpecturmWindowFunc;
    FOverlapPercent: Integer;
  protected
    //;
  end;

implementation

end.

