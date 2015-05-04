unit LteBasic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex, GenFFT, Math;

const
  TTI_PER_FRAME  = 10;
  SUB_CARRIER_PER_RB = 12;
  RE_PER_SLOT_NORMAL = 7;
  RE_PER_SLOT_EXT    = 6;
  PSS_RB_NUM = 6;
  FS = 15000 * 2048;

type

  TFrameType = (ftFDD, ftTDD);

  TSubframeAlloc = (sa0, sa1, sa2, sa3, sa4, sa5, sa6);

  TSpecialSubframeConfig = (ssc0, ssc1, ssc2, ssc3, ssc4);

  TCPMode = (cmNormal, cmExtended);

procedure MapPSSToA(const NID: Integer; A: PComplex; const DLRB, RBSC: Integer);

procedure ZadoffChu(const Root, N, Len: Integer; Seq: PComplex; const N0: Integer = 0);

procedure OFDMGenerate(A: PComplex; const DLRB, RBSC: Integer; S: PComplex);

procedure Init;

var
  PSS_SEQ: array [0..2] of array [0..61] of Complex;

implementation

var
  Initialized: Boolean = False;
  IFFT2048, IFFT4096: GenFFT.PFFTPlan;

procedure ZadoffChu(const Root, N, Len: Integer; Seq: PComplex;
  const N0: Integer);
var
  I: Integer;
  C: Complex;
begin
  C.re := 0;
  for I := 0 to Len - 1 do
  begin
    C.im := -Pi * Root * (N0 + I) * (N0 + I + 1) / N;
    Seq[I] := cexp(C);
  end;
end;

procedure MapPSSToA(const NID: Integer; A: PComplex; const DLRB, RBSC: Integer);
var
  P: PComplex;
  I, J: Integer;
begin
  if DLRB < 6 then Exit;
  P := @PSS_SEQ[NID][0];
  J := DLRB * RBSC div 2;
  for I := 0 to 30 do
  begin
    A[J - 31 + I] := P[I];
    A[J + I] := P[I + 31];
  end;
end;

procedure InitSyncCodes;
  procedure Init(const I, Root: Integer);
  begin
    ZadoffChu(Root, 63, 31, @PSS_SEQ[I][0], 0);
    ZadoffChu(Root, 63, 31, @PSS_SEQ[I][31], 32);
  end;

begin
  Init(0, 25);
  Init(1, 29);
  Init(2, 34);
end;

procedure OFDMGenerate(A: PComplex; const DLRB, RBSC: Integer; S: PComplex);
var
  X: array [0..4095] of Complex;
  N: Integer;
  I, J, K: Integer;
begin
  N := IfThen(RBSC = 12, 2048, 4096);
  FillChar(X[0], N * SizeOf(Complex), 0);
  I := DLRB * RBSC div 2;
  Move(A[0], X[N div 2 - I], I * SizeOf(Complex));
  Move(A[I], X[N div 2 + 1], I * SizeOf(Complex));
  if N = 2048 then
    GenFFT.FFT(IFFT2048, X, S)
  else
    GenFFT.FFT(IFFT4096, X, S);
  for J := 0 to N - 1 do
    if Odd(J) then S[J] := -S[J];
end;

procedure Init;
begin
  if Initialized then Exit;
  InitSyncCodes;
  IFFT2048 := GenFFT.BuildFFTPlan(2048, True);
  IFFT4096 := GenFFT.BuildFFTPlan(4096, True);
  Initialized := True;
end;

initialization

Init;

finalization

  if Initialized then
  begin
    GenFFT.FinalizePlan(IFFT2048);
    GenFFT.FinalizePlan(IFFT4096);
  end;
end.

