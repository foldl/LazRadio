program LazRadio;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, formmain, radiomodule, rtlsdr, formrtl, rm_rtl, async, rm_oscillator,
  radiosystem, superobject, rm_spectrum, rm_timer, formgui, rm_audio_out, 
rm_audio_in, rm_filter;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TRTLForm, RTLForm);
  Application.CreateForm(TGUIForm, GUIForm);
  Application.Run;
end.

