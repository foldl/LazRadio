unit formoscillator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ButtonPanel, Buttons, StdCtrls, RadioModule, RadioSystem, LCLType;

type

  { TOscillatorForm }

  TOscillatorForm = class(TForm)
    Label1: TLabel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    RadioGroup1: TRadioGroup;
    procedure LabeledEdit1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabeledEdit2KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabeledEdit3KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RadioGroup1SelectionChanged(Sender: TObject);
  private
    FModule: TRadioModule;
    procedure SetModule(AValue: TRadioModule);

  public
    property Module: TRadioModule read FModule write SetModule;
  end;

var
  OscillatorForm: TOscillatorForm;

implementation

uses
  rm_oscillator;

{$R *.lfm}

{ TOscillatorForm }

procedure TOscillatorForm.RadioGroup1SelectionChanged(Sender: TObject);
begin
  if not Assigned(FModule) then Exit;
  RadioPostMessage(RM_OSC_WAVE, RadioGroup1.ItemIndex, StrToIntDef(LabeledEdit1.Text, 50), FModule);
end;

procedure TOscillatorForm.LabeledEdit2KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not Assigned(FModule) then Exit;
  if Key <> VK_RETURN then Exit;
  RadioPostMessage(RM_SET_FEATURE, RM_FEATURE_FREQ,
                              StrToIntDef(LabeledEdit2.Text, 1000), FModule);
end;

procedure TOscillatorForm.LabeledEdit3KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not Assigned(FModule) then Exit;
  if Key <> VK_RETURN then Exit;
  RadioPostMessage(RM_SET_FEATURE, RM_FEATURE_SAMPLE_RATE,
                              StrToIntDef(LabeledEdit3.Text, 20000), FModule);
end;

procedure TOscillatorForm.LabeledEdit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not Assigned(FModule) then Exit;
  if RadioGroup1.ItemIndex <= 0 then Exit;
  if Key <> VK_RETURN then Exit;
  RadioPostMessage(RM_OSC_WAVE, RadioGroup1.ItemIndex, StrToIntDef(LabeledEdit1.Text, 50), FModule);
end;

procedure TOscillatorForm.SetModule(AValue: TRadioModule);
begin
  if FModule = AValue then Exit;
  FModule := AValue;
end;

end.

