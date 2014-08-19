unit GenFFT;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex;

type

  TFFTProvider = (fpKissFFT, fpFFT2, fpFFTW);

  PFFTPlan = Pointer;

procedure SelectFFTProvider(const AProvider: TFFTProvider);

function BuildFFTPlan(const N: Integer;
                      const Inverse: Boolean): PFFTPlan;
procedure ChangePlan(Plan: PFFTPlan; const N: Integer;
                      const Inverse: Boolean);
procedure FinalizePlan(P: PFFTPlan);

procedure FFT(Plan: PFFTPlan;
              Input: PComplex;
              Output: PComplex);

function NextFastSize(N: Integer): Integer;

implementation

uses
  KissFFT, FFT2, FFTW;

type

  TBuildFFTPlan = function (const N: Integer;
                     const Inverse: Boolean): PFFTPlan;
  TChangePlan = procedure (Plan: PFFTPlan; const N: Integer;
                     const Inverse: Boolean);
  TFinalizePlan = procedure (P: PFFTPlan);

  TFFT = procedure (Plan: PFFTPlan;
             Input: PComplex;
             Output: PComplex);

  TNextFastSize = function (N: Integer): Integer;

  TFFTFuncs = record
    BuildFFTPlan: TBuildFFTPlan;
    ChangePlan: TChangePlan;
    FinalizePlan: TFinalizePlan;
    FFT: TFFT;
    NextFastSize: TNextFastSize;
  end;

var
  FFTProvider: TFFTFuncs;
  PlanCS: TRTLCriticalSection;

procedure SelectFFTProvider(const AProvider: TFFTProvider);
begin
  case AProvider of
    fpKissFFT:
      with FFTProvider do
      begin
        BuildFFTPlan := TBuildFFTPlan(@KissFFT.BuildFFTPlan);
        ChangePlan   := TChangePlan(@KissFFT.ChangePlan);
        FinalizePlan := TFinalizePlan(@KissFFT.FinalizePlan);
        FFT          := TFFT(@KissFFT.FFT);
        NextFastSize := @KissFFT.NextFastSize;
      end;
    fpFFT2:
      with FFTProvider do
      begin
        BuildFFTPlan := TBuildFFTPlan(@FFT2.BuildFFTPlan);
        ChangePlan   := TChangePlan(@FFT2.ChangePlan);
        FinalizePlan := TFinalizePlan(@FFT2.FinalizePlan);
        FFT          := TFFT(@FFT2.FFT);
        NextFastSize := @FFT2.NextFastSize;
      end;
    fpFFTW:
      with FFTProvider do
      begin
        BuildFFTPlan := TBuildFFTPlan(@FFTW.BuildFFTPlan);
        ChangePlan   := TChangePlan(@FFTW.ChangePlan);
        FinalizePlan := TFinalizePlan(@FFTW.FinalizePlan);
        FFT          := TFFT(@FFTW.FFT);
        NextFastSize := @FFTW.NextFastSize;
      end;
  end;
end;

function BuildFFTPlan(const N: Integer; const Inverse: Boolean): PFFTPlan;
begin
  EnterCriticalsection(PlanCS);
  Result := FFTProvider.BuildFFTPlan(N, Inverse);
  LeaveCriticalsection(PlanCS);
end;

procedure ChangePlan(Plan: PFFTPlan; const N: Integer; const Inverse: Boolean);
begin
  EnterCriticalsection(PlanCS);
  FFTProvider.ChangePlan(Plan, N, Inverse);
  LeaveCriticalsection(PlanCS);
end;

procedure FinalizePlan(P: PFFTPlan);
begin
  EnterCriticalsection(PlanCS);
  FFTProvider.FinalizePlan(P);
  LeaveCriticalsection(PlanCS);
end;

procedure FFT(Plan: PFFTPlan; Input: PComplex; Output: PComplex);
begin
  FFTProvider.FFT(Plan, Input, Output);
end;

function NextFastSize(N: Integer): Integer;
begin
  Result := FFTProvider.NextFastSize(N);
end;

initialization
  SelectFFTProvider(fpFFTW);
  InitCriticalSection(PlanCS);

finalization
  DoneCriticalsection(PlanCS);
end.

