unit formrtl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, Spin;

type

  { TRTLForm }

  TRTLForm = class(TForm)
    BtnStop: TBitBtn;
    BtnGo: TBitBtn;
    BtnRefresh: TBitBtn;
    DevList: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    SamplingRateList: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    AGCMode: TRadioGroup;
    ManualGain: TTrackBar;
    FreqCorrectionSpin: TSpinEdit;
    SamplingRateList1: TComboBox;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  RTLForm: TRTLForm;

implementation

{$R *.lfm}

end.

