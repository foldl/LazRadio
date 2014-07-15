unit rm_fm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex, RadioModule, RadioSystem, Math;

type

  { TRadioFMDemod }

  TRadioFMDemod = class(TRadioModule)
  private
    FRegulator: TStreamRegulator;
    FLastValue: Complex;
    FBandIndex: Integer;
    FCarrierFreq: Cardinal;
    FSampleRate: Cardinal;
    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: Integer); override;
    procedure ReceiveRegulatedData(const P: PComplex; const Len: Integer);
  protected
    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer; override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

implementation

{ TRadioFMDemod }

procedure TRadioFMDemod.ProccessMessage(const Msg: TRadioMessage;
  var Ret: Integer);
begin
  if Msg.Id = RM_SPECTRUM_BAND_SELECT_1 + FBandIndex then
  begin
    FCarrierFreq := Msg.ParamH;
    Exit;
  end;

  inherited;
end;

procedure TRadioFMDemod.ReceiveRegulatedData(const P: PComplex;
  const Len: Integer);
var
  I: Integer;
  J: Integer;
  O: PComplex;
  T: Complex;
  X: Complex;
  F: Double;
begin
  if FSampleRate = 0 then Exit;
  F := 2 * Pi * FCarrierFreq / FSampleRate;
  O := AllocWait(I);
  T := FLastValue;
  for J := 0 to Len - 1 do
  begin
    X := P[J] * cong(T);
    T := P[J];
    O[J].re := arctan2(X.im, X.re) - F;
    O[J].im := 0;
  end;
  FLastValue := T;
end;

function TRadioFMDemod.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  FSampleRate := Rate;
  Result := 0;
end;

constructor TRadioFMDemod.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  FRegulator := TStreamRegulator.Create;
  FRegulator.Size := DefOutput.BufferSize;
  FRegulator.OnRegulatedData := @ReceiveRegulatedData;
end;

destructor TRadioFMDemod.Destroy;
begin
  FRegulator.Free;
  inherited Destroy;
end;

procedure TRadioFMDemod.ReceiveData(const P: PComplex; const Len: Integer);
begin
  FRegulator.ReceiveData(P, Len);
end;

end.

