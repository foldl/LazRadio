unit rm_fm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex, RadioModule, RadioSystem;

type

  { TRadioFMDemod }

  TRadioFMDemod = class(TRadioModule)
  private
    FRegulator: TStreamRegulator;
    FLastValue: Complex;
    procedure ReceiveRegulatedData(const P: PComplex; const Len: Integer);
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

implementation

{ TRadioFMDemod }

procedure TRadioFMDemod.ReceiveRegulatedData(const P: PComplex;
  const Len: Integer);
var
  I: Integer;
  J: Integer;
  O: PComplex;
  T: Complex;
begin
  O := AllocWait(I);
  T := FLastValue;
  for J := 0 to Len - 1 do
  begin

  end;
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

