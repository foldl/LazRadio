unit formspectrum;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, RadioModule, RadioSystem;

type

  { TSpectrumForm }

  TSpectrumForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure LabeledEdit1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabeledEdit2KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabeledEdit3KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabeledEdit4KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
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

procedure TSpectrumForm.Button1Click(Sender: TObject);
begin
  if not Assigned(FModule) then Exit;
  RadioPostMessage(RM_SPECTRUM_CFG, SET_AUTO_Y, 0, FModule);
  LabeledEdit2.Text := '';
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

procedure TSpectrumForm.SetModule(AValue: TRadioModule);
begin
  if FModule = AValue then Exit;
  FModule := AValue;
end;

end.

