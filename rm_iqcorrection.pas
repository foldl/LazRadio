unit rm_iqcorrection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex, Math, RadioMessage, RadioModule, RadioSystem;

{
  THIS IS EXPERIMENTAL!

  correct signal is s_i, s_q, then desorted signal u_i, u_q are:

  [ u_i ]    [      1             0         ]  [ s_i ]
  |     |  = |                              |  |     |
  [ u_q ]    [ -g sin(phi)      g cos(phi)  ]  [ s_q ]

  u_i = s_i
  u_q = -g sin(phi) s_i + g cos(phi) s_q

  assume phi is small, sin(phi) << 1, g = sqrt((sum u_q ^2) / (sum u_i ^2))

  assume g = 1, then u_i * u_q = -sin(phi) s_i^2 + cos(phi) s_i s_q,
    if E[s_i s_q] = 0, then sin(phi) can be estimated as:
      phi = -arcsin(sum(u_i * u_q)/sum(s_i ^2))
}

type

  { TRadioIQCorrecter }

  TRadioIQCorrecter = class(TRadioModule)
  private
    FAlpha: Double;
    FRegulator: TStreamRegulator;
    FGain: Double;
    FPhi: Double;
    procedure ReceiveRegulatedData(const P: PComplex; const Len: Integer);
  protected
    procedure Describe(Strs: TStrings); override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

implementation

uses
  SignalBasic;

{ TRadioIQCorrecter }

procedure TRadioIQCorrecter.ReceiveRegulatedData(const P: PComplex;
  const Len: Integer);
var
  CI: Double = 0.0;
  CQ: Double = 0.0;
  I: Integer;
  O: PComplex;
  CosPhiRep, TanPhi: Double;
begin
  CancelDC(P, Len);
  for I := 0 to Len - 1 do
  begin
    CI := CI + P[I].re * P[I].re;
    CQ := CQ + P[I].im * P[I].im;
  end;
  if CQ > 0 then FGain := FAlpha * (CI / CQ) + (1 - FAlpha) * FGain;
  CQ := 0;
  for I := 0 to Len - 1 do
  begin
    P[I].im := FGain * P[I].im;
    CQ := CQ + P[I].re * P[I].im;
  end;

  FPhi := FAlpha * (-arcsin(EnsureRange(CQ / CI, -0.99999, 0.99999))) + (1 - FAlpha) * FPhi;
  CosPhiRep := 1 / Cos(FPhi);
  TanPhi := Tan(FPhi);
  for I := 0 to Len - 1 do
    P[I].im := TanPhi * P[I].re + CosPhiRep * P[I].Im;

  O := Alloc(DefOutput, I);
  if not Assigned(O) then
  begin
    TRadioLogger.Report(llWarn, 'TRadioIQCorrecter.ReceiveRegulatedData: data lost');
    Exit;
  end;
  Move(P^, O^, Len * SizeOf(P^));
  DefOutput.Broadcast(I, FDataListeners);
end;

procedure TRadioIQCorrecter.Describe(Strs: TStrings);
begin
  Strs.Add(Format('^bGain: ^n%.3f', [FGain]));
  Strs.Add(Format('^bPhi : ^n%.3f', [FPhi]));
end;

constructor TRadioIQCorrecter.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  FGain := 1.0;
  FAlpha := 0.1;
  FRegulator := TStreamRegulator.Create;
  FRegulator.Size := DefOutput.BufferSize;
  FRegulator.OnRegulatedData := @ReceiveRegulatedData;
end;

destructor TRadioIQCorrecter.Destroy;
begin
  FRegulator.Free;
  inherited Destroy;
end;

procedure TRadioIQCorrecter.ReceiveData(const P: PComplex; const Len: Integer);
begin
  FRegulator.ReceiveData(P, Len);
end;

initialization

  RegisterModule(TRadioModuleClass(TRadioIQCorrecter.ClassType));

end.

