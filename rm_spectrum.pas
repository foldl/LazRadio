unit rm_spectrum;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioModule, UComplex, SignalBasic, KissFFT;

const

  RM_SPECTRUM_CFG = RM_USER;
                  SET_WND_FUNC      = 0;
                  SET_OVERLAP_PER   = 1;
                  SET_FFT_SIZE      = 2;
                  SET_FULL_REDRAW   = 3;   // GUI resized

type

  { TRadioSpectrum }

  TRadioSpectrum = class(TRadioModule)
  private
    FFreq: Cardinal;
    FSampleRate: Cardinal;
    FFlow: TDataFlowNode;
    FWndNode: TWindowNode;
    FWindow: TWindowFunction;
    FFFTSize: Integer;
    FFFTPlan: PFFTPlan;
  protected
    procedure RedrawFull;
    procedure ConfigWndNode(const W: TWindowFunction; const L: Integer; const Ov: Double);
    procedure SetWindowFunc(const FuncIndex: Integer);
    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: Integer); override;
    procedure ReceiveWindowedData(const P: PComplex; const Len: Integer);
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

implementation

{ TRadioSpectrum }

procedure TRadioSpectrum.RedrawFull;
begin

end;

procedure TRadioSpectrum.ConfigWndNode(const W: TWindowFunction;
  const L: Integer; const Ov: Double);
var
  W: array of Double;
  I: Integer;
  D: Double;
begin
  SetLength(W, L);
  CreateWindowFunction(@W[0], L, W);
  FWndNode.SetWindow(@W[0], L);
  I := Round(L * Ov);
  if (I >= 0) and (I < L) then FWndNode.Overlap := I;
end;

procedure TRadioSpectrum.SetWindowFunc(const FuncIndex: Integer);
var
  W: array of Double;
begin
  if (FuncIndex >= Ord(Low(TWindowFunction))) and (FuncIndex <= Ord(High(TWindowFunction))) then
  begin
    FWindow := TWindowFunction(FuncIndex);
    ConfigWndNode(FWindow, FFFTSize, FWndNode.Overlap / FFFTSize);
  end;
end;

procedure TRadioSpectrum.ProccessMessage(const Msg: TRadioMessage;
  var Ret: Integer);
var
  I: Integer;
  D: Double;
begin
  case Msg.Id of
    RM_SPECTRUM_CFG:
      case Msg.ParamH of
        SET_WND_FUNC: SetWindowFunc(Integer(Msg.ParamL));
        SET_OVERLAP_PER:
          begin
            I := Round(FFFTSize * Integer(Msg.ParamL) / 100);
            if (I >= 0) and (I < FFFTSize) then FWndNode.Overlap := I;
          end;
        SET_FFT_SIZE:
          begin
            D := FWndNode.Overlap / FFFTSize;
            FFFTSize := Integer(Msg.ParamL);
            ChangePlan(FFFTPlan, FFFTSize, False);
            ConfigWndNode(FWindow, FFFTSize, D);
          end;
        SET_FULL_REDRAW:
          RedrawFull;
      end;
    else
      inherited ProccessMessage(Msg, Ret);
  end;
end;

procedure TRadioSpectrum.ReceiveWindowedData(const P: PComplex;
  const Len: Integer);
begin

end;

constructor TRadioSpectrum.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  FFFTSize := 1024 * 16;
  FWindow := wfRect;
  FFFTPlan := BuildFFTPlan(FFFTSize, False);

  FFlow := TWindowNode.Create;
  FFlow.LastNode.OnSendToNext := @ReceiveWindowedData;
  FWndNode := FFlow as TWindowNode;

  ConfigWndNode(FWindow, FFFTSize, 0);
end;

destructor TRadioSpectrum.Destroy;
begin
  FFlow.Free;
  FinalizePlan(FFFTPlan);
  inherited Destroy;
end;

procedure TRadioSpectrum.ReceiveData(const P: PComplex; const Len: Integer);
begin
  FFlow.ReceiveData(P, Len);
end;

initialization

  RegisterModule('Spectrum', TRadioModuleClass(TRadioSpectrum.ClassType));

end.

