unit rm_lte;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioMessage, RadioModule, RadioSystem, RadioNode,
  Math, UComplex;

type

  { TLteSync }

  TLteSync = class(TRadioModule)
  private
    FRate: Integer;
    FFilters: array [0..2] of TFIRNode;
    procedure ReceiveCorrelationData(const Index: Integer; const P: PComplex; const Len: Integer);
    procedure ReceiveCorrelationData0(const P: PComplex; const Len: Integer);
    procedure ReceiveCorrelationData1(const P: PComplex; const Len: Integer);
    procedure ReceiveCorrelationData2(const P: PComplex; const Len: Integer);
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
  LteBasic, SignalBasic;

{ TLteSync }

procedure TLteSync.ReceiveCorrelationData(const Index: Integer;
  const P: PComplex; const Len: Integer);
var
  I, J: Integer;
  O: PComplex;
begin
  if Index <> 0 then Exit;
  DefOutput.BufferSize := Len;
  O := Alloc(DefOutput, I);
  if not Assigned(O) then Exit;
  for J := 0 to Len - 1 do
  begin
    O[I].re := cmod(P[I]);
    O[I].im := 0;
  end;
  //Move(P^, O^, Len * SizeOf(Complex));
  DefOutput.Broadcast(I, FDataListeners);
end;

procedure TLteSync.ReceiveCorrelationData0(const P: PComplex; const Len: Integer
  );
begin
  ReceiveCorrelationData(0, P, Len);
end;

procedure TLteSync.ReceiveCorrelationData1(const P: PComplex; const Len: Integer
  );
begin
  ReceiveCorrelationData(1, P, Len);

end;

procedure TLteSync.ReceiveCorrelationData2(const P: PComplex; const Len: Integer
  );
begin
  ReceiveCorrelationData(2, P, Len);
end;

function TLteSync.RMSetFrequency(const Msg: TRadioMessage; const Freq: Cardinal
  ): Integer;
begin
  Result := inherited;
end;

function TLteSync.RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal
  ): Integer;
var
  I, D: Integer;
  DecimM, InterpL: Integer;
  A: array [0..PSS_RB_NUM * SUB_CARRIER_PER_RB - 1] of Complex;
  S: array [0..2047] of Complex;
  Y: array [0..2047] of Complex;
begin
  if FRate = Rate then Exit;
  if (Rate < 100) or (Rate > LteBasic.FS) then Exit;
  FRate := Rate;
  D := Int64(Length(S)) * FRate div LteBasic.FS;
  for I := 0 to High(LteBasic.PSS_SEQ) do
  begin
    FillChar(A[0], Length(A) * SizeOf(A[0]), 0);
    MapPSSToA(I, @A[0], PSS_RB_NUM, SUB_CARRIER_PER_RB);
    OFDMGenerate(A, PSS_RB_NUM, SUB_CARRIER_PER_RB, @S[0]);
    Reverse(@S[0], Length(S));
    Conjugate(@S[0], Length(S));

    Xpolate(PComplex(@S[0]), @Y[0], Length(S), D);
    DumpData(PComplex(@Y[0]), D, Format('e:\%d.txt', [I]));

    FFilters[I].SetFIR(PComplex(@Y[0]), D);
  end;
  Result := inherited;
end;

constructor TLteSync.Create(RunQueue: TRadioRunQueue);
var
  I: Integer;
begin
  inherited Create(RunQueue);
  for I in [0,1,2] do
  begin
    FFilters[I] := TFIRNode.Create;
  end;
  FFilters[0].OnSendToNext := @ReceiveCorrelationData0;
  FFilters[1].OnSendToNext := @ReceiveCorrelationData1;
  FFilters[2].OnSendToNext := @ReceiveCorrelationData2;
end;

destructor TLteSync.Destroy;
var
  I: Integer;
begin
  inherited Destroy;
  for I in [0,1,2] do FFilters[I].Free;
end;

procedure TLteSync.ReceiveData(const P: PComplex; const Len: Integer);
var
  F: TFIRNode;
begin
  if FRate < 0 then Exit;
  for F in FFilters do
    F.ReceiveData(P, Len);
end;

initialization

  RegisterModule(TRadioModuleClass(TLteSync.ClassType));

end.

