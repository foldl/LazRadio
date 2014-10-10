unit formspectrum;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, RadioModule, RadioSystem, Math, radiomessage;

type

  { TSpectrumForm }

  TSpectrumForm = class(TForm)
    Button2: TButton;
    Button3: TButton;
    PickerGroup: TCheckGroup;
    ComboBox1: TComboBox;
    FFTCombo: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    ToggleBox1: TToggleBox;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FFTComboChange(Sender: TObject);
    procedure LabeledEdit1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabeledEdit2KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabeledEdit3KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabeledEdit4KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseLeave(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PickerGroupItemClick(Sender: TObject; Index: integer);
    procedure ToggleBox1Change(Sender: TObject);
  private
    FModule: TRadioModule;
    procedure SetModule(AValue: TRadioModule);
    { private declarations }

  public
    property Module: TRadioModule read FModule write SetModule;
  end;

var
  SpectrumForm: TSpectrumForm;

implementation

uses
  rm_spectrum;

{$R *.lfm}

{ TSpectrumForm }

procedure TSpectrumForm.LabeledEdit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  M: Integer;
begin
  if not Assigned(FModule) then Exit;
  if Key <> VK_RETURN then Exit;
  M := StrToIntDef(LabeledEdit1.Text, 0);
  RadioPostMessage(RM_SPECTRUM_CFG, SET_Y_MAX, M, FModule);
end;

procedure TSpectrumForm.LabeledEdit2KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  M: Integer;
begin
  if not Assigned(FModule) then Exit;
  if Key <> VK_RETURN then Exit;
  M := StrToIntDef(LabeledEdit2.Text, 0);
  RadioPostMessage(RM_SPECTRUM_CFG, SET_Y_RANGE, M, FModule);
end;

procedure TSpectrumForm.LabeledEdit3KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not Assigned(FModule) then Exit;
  if Key <> VK_RETURN then Exit;
  RadioPostMessage(RM_SPECTRUM_CFG, SET_SPAN, StrToIntDef(LabeledEdit3.Text, -1), FModule);
end;

procedure TSpectrumForm.LabeledEdit4KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not Assigned(FModule) then Exit;
  if Key <> VK_RETURN then Exit;
  RadioPostMessage(RM_SPECTRUM_CFG, SET_CENTER_FREQ, StrToIntDef(LabeledEdit4.Text, 0), FModule);
end;

procedure TSpectrumForm.PaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(FModule) then Exit;
  RadioPostMessage(PRIV_RM_SPECTRUM_MOUSE_DOWN, Ord(Button), (X shl 16) or Y, FModule);
end;

procedure TSpectrumForm.PaintBox1MouseLeave(Sender: TObject);
begin
  if not Assigned(FModule) then Exit;
  RadioPostMessage(PRIV_RM_SPECTRUM_MOUSE_LEAVE, 0, 0, FModule);
end;

procedure TSpectrumForm.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if not Assigned(FModule) then Exit;
  RadioPostMessage(PRIV_RM_SPECTRUM_MOUSE_MOVE, 0, (X shl 16) or Y, FModule);
end;

procedure TSpectrumForm.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(FModule) then Exit;
  RadioPostMessage(PRIV_RM_SPECTRUM_MOUSE_UP, Ord(Button), (X shl 16) or Y, FModule);
end;

procedure TSpectrumForm.PickerGroupItemClick(Sender: TObject; Index: integer);
begin
  if not Assigned(FModule) then Exit;
  RadioPostMessage(PRIV_RM_SPECTRUM_ENABLE_PICKER, Index, IfThen(PickerGroup.Checked[Index], 1, 0), FModule);
end;

procedure TSpectrumForm.ToggleBox1Change(Sender: TObject);
begin
  if not Assigned(FModule) then Exit;
  RadioPostMessage(RM_SPECTRUM_CFG, SET_AUTO_Y, IfThen(ToggleBox1.Checked, 1, 0), FModule);
  if ToggleBox1.Checked then LabeledEdit2.Text := '';
end;

procedure TSpectrumForm.Button2Click(Sender: TObject);
begin
  if not Assigned(FModule) then Exit;
  RadioPostMessage(RM_SPECTRUM_CFG, SET_SPAN, PtrUInt(-1), FModule);
  LabeledEdit3.Text := '';
end;

procedure TSpectrumForm.Button3Click(Sender: TObject);
begin
  if not Assigned(FModule) then Exit;
  RadioPostMessage(RM_SPECTRUM_CFG, SET_SPAN, 0, FModule);
  LabeledEdit3.Text := '0';
end;

procedure TSpectrumForm.ComboBox1Change(Sender: TObject);
begin
  if not Assigned(FModule) then Exit;
  RadioPostMessage(RM_SPECTRUM_CFG, SET_DRAW_MIN_INTERVAL, Round(1000 / StrToInt(ComboBox1.Text)), FModule);
end;

procedure TSpectrumForm.FFTComboChange(Sender: TObject);
begin
  if not Assigned(FModule) then Exit;
  RadioPostMessage(RM_SPECTRUM_CFG, SET_FFT_SIZE, StrToInt(FFTCombo.Text), FModule);
end;

procedure TSpectrumForm.SetModule(AValue: TRadioModule);
begin
  if FModule = AValue then Exit;
  FModule := AValue;
end;

end.

