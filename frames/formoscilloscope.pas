unit formoscilloscope;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, RadioModule, RadioSystem, LCLType;

type

  { TOscilloscopeForm }

  TOscilloscopeForm = class(TForm)
    Button2: TButton;
    Button3: TButton;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    LabeledEdit5: TLabeledEdit;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    RadioGroup1: TRadioGroup;
    ToggleBox1: TToggleBox;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure LabeledEdit1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabeledEdit2KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabeledEdit4KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabeledEdit5KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ToggleBox1Change(Sender: TObject);
  private
    FModule: TRadioModule;
    procedure SetModule(AValue: TRadioModule);
    { private declarations }
  public
    property Module: TRadioModule read FModule write SetModule;
  end;

var
  OscilloscopeForm: TOscilloscopeForm;

implementation

uses
  util_math, RadioMessage;

{$R *.lfm}

{ TOscilloscopeForm }

procedure TOscilloscopeForm.ToggleBox1Change(Sender: TObject);
begin
  RadioPostMessage(RM_OSCILLOSCOPE_CFG, OSCILLOSCOPE_SET_Y_AUTO,
                              Ord(ToggleBox1.Checked), FModule);
end;

procedure TOscilloscopeForm.LabeledEdit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key <> VK_RETURN then Exit;
  RadioPostMessage(RM_OSCILLOSCOPE_CFG, OSCILLOSCOPE_SET_Y_MAX,
                              FloatAsInt(StrToFloatDef(LabeledEdit1.Text, 1)), FModule);
end;

procedure TOscilloscopeForm.Button2Click(Sender: TObject);
begin
  RadioPostMessage(RM_OSCILLOSCOPE_CFG, OSCILLOSCOPE_SET_RUN_MODE,
                              OSCILLOSCOPE_RUN_SINGLE, FModule);
end;

procedure TOscilloscopeForm.Button3Click(Sender: TObject);
begin
  RadioPostMessage(RM_OSCILLOSCOPE_CFG, OSCILLOSCOPE_SET_RUN_MODE,
                              OSCILLOSCOPE_RUN_CONTINOUS, FModule);
end;

procedure TOscilloscopeForm.ComboBox4Change(Sender: TObject);
begin
  RadioPostMessage(RM_OSCILLOSCOPE_CFG, OSCILLOSCOPE_SET_DRAW_MIN_INTERVAL,
                              1000 div StrToInt(ComboBox4.Text), FModule);
end;

procedure TOscilloscopeForm.ComboBox5Change(Sender: TObject);
begin
  RadioPostMessage(RM_OSCILLOSCOPE_CFG, OSCILLOSCOPE_SET_CHANNEL,
                              ComboBox5.ItemIndex, FModule);
end;

procedure TOscilloscopeForm.ComboBox6Change(Sender: TObject);
begin
  RadioPostMessage(RM_OSCILLOSCOPE_CFG, OSCILLOSCOPE_ARITH_FOR_DUAL,
                              ComboBox6.ItemIndex, FModule);
end;

procedure TOscilloscopeForm.LabeledEdit2KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key <> VK_RETURN then Exit;
  RadioPostMessage(RM_OSCILLOSCOPE_CFG, OSCILLOSCOPE_SET_Y_MIN,
                              FloatAsInt(StrToFloatDef(LabeledEdit2.Text, -1)), FModule);
end;

procedure TOscilloscopeForm.LabeledEdit4KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key <> VK_RETURN then Exit;
  RadioPostMessage(RM_OSCILLOSCOPE_CFG, OSCILLOSCOPE_SET_SWEEP_SPEED,
                              FloatAsInt(StrToFloatDef(LabeledEdit4.Text, 1)), FModule);
end;

procedure TOscilloscopeForm.LabeledEdit5KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key <> VK_RETURN then Exit;
  RadioPostMessage(RM_OSCILLOSCOPE_CFG, OSCILLOSCOPE_SET_SAMPLE_GRID,
                              StrToIntDef(LabeledEdit5.Text, 0), FModule);
end;

procedure TOscilloscopeForm.SetModule(AValue: TRadioModule);
begin
  if FModule = AValue then Exit;
  FModule := AValue;
end;

end.

