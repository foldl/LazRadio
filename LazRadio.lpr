program LazRadio;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, formmain, radiomodule, rtlsdr, formrtl, rm_rtl, async, rm_oscillator,
  radiosystem, superobject, rm_spectrum, rm_timer,
  rm_audio, rm_filter, formspectrum, kissfft, signalbasic, formoscillator,
  formauin, utils, rm_dump, logger, tachartlazaruspkg, formfilter, rm_fm, 
rm_pll, fft2, genfft, fftw, gen_graph, radiomessage;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TRTLForm, RTLForm);
  Application.CreateForm(TSpectrumForm, SpectrumForm);
  Application.CreateForm(TOscillatorForm, OscillatorForm);
  Application.CreateForm(TAudioInForm, AudioInForm);
  Application.CreateForm(TFilterForm, FilterForm);
  Application.Run;
end.

