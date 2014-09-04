unit rm_am;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex, RadioMessage, RadioModule, RadioSystem, Math;

type

  { TRadioAMDetector }

  TRadioAMDetector = class(TRadioModule)
  private
    FRegulator: TStreamRegulator;
    procedure ReceiveRegulatedData(const P: PComplex; const Len: Integer);
  protected
    function RMSetFrequency(const Msg: TRadioMessage; const Freq: Cardinal): Integer; override;
    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer; override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

implementation

uses
  SignalBasic;

{ TRadioAMDetector }

procedure TRadioAMDetector.ReceiveRegulatedData(const P: PComplex;
  const Len: Integer);
var
  I: Integer;
  J: Integer;
  O: PComplex;
  T: Complex;
  X: Complex;
  F: Double;
begin
  O := Alloc(DefOutput, I);
  if not Assigned(O) then
  begin
    TRadioLogger.Report(llWarn, 'TRadioAMDetector.ReceiveRegulatedData: data lost');
    Exit;
  end;
  for J := 0 to Len - 1 do
  begin
    O[J].im := 0;
    O[J].re := cmod(P[I]);
  end;
  CancelDC(O, Len);
  DefOutput.Broadcast(I, FDataListeners);
end;

function TRadioAMDetector.RMSetFrequency(const Msg: TRadioMessage;
  const Freq: Cardinal): Integer;
begin
  Result := 0;
end;

function TRadioAMDetector.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  Result := inherited;
  Broadcast(RM_SET_FEATURE, RM_FEATURE_FREQ, 0);
end;

constructor TRadioAMDetector.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  FRegulator := TStreamRegulator.Create;
  FRegulator.Size := DefOutput.BufferSize;
  FRegulator.OnRegulatedData := @ReceiveRegulatedData;
end;

destructor TRadioAMDetector.Destroy;
begin
  FRegulator.Free;
  inherited Destroy;
end;

procedure TRadioAMDetector.ReceiveData(const P: PComplex; const Len: Integer);
begin
  FRegulator.ReceiveData(P, Len);
end;

initialization

  RegisterModule(TRadioModuleClass(TRadioAMDetector.ClassType));

end.

