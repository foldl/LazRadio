unit rm_spectrum;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Graphics, RadioModule, UComplex, SignalBasic,
  KissFFT, formspectrum, Forms, Math;

const

  RM_SPECTRUM_CFG = RM_USER;
                  SET_WND_FUNC      = 0;
                  SET_OVERLAP_PER   = 1;
                  SET_FFT_SIZE      = 2;
                  GUI_RESET         = 3;  // gui resize

type

  { TDoubleBuffer }

  TDoubleBuffer = class
  private
    FDrawBuffer: TBitmap;
    FPaintBox: TPaintBox;
    FPaintBuffer: TBitmap;
    procedure OnPaint(Sender: TObject);
    procedure SetPaintBox(AValue: TPaintBox);
    procedure Draw2Paint2;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Draw2Paint;

    procedure SetSize(const W, H: Integer); virtual;
    property DrawBuffer: TBitmap read FDrawBuffer;
    property PaintBuffer: TBitmap read FPaintBuffer;
    property PaintBox: TPaintBox read FPaintBox write SetPaintBox;
  end;

  { TTripleBuffer }

  TTripleBuffer = class(TDoubleBuffer)
  private
    FBackground: TBitmap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetSize(const W, H: Integer); override;
    property Background: TBitmap read FBackground;
  end;

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
    FF: array of Complex;
    FRt: TTripleBuffer;
    FWf: TDoubleBuffer;
    FForm: TSpectrumForm;
    FPower: array of Double;
    FLine: array of Double;
    FFrames: Integer;
    procedure ImageResize(Sender: TObject);
    procedure SetRealtime(AValue: TPaintBox);
    procedure SetWaterfall(AValue: TPaintBox);
  protected
    procedure DrawFrame;
    procedure DrawRealtime;
    procedure DrawWaterfall;
    procedure PaintRealtime;
    procedure PaintWaterfall;
    procedure SyncPaintRealtime(Sender: TObject);
    procedure SyncPaintWaterfall(Sender: TObject);
    procedure SyncPaint;
    procedure RedrawFull;
    procedure SyncRedrawFull;
    procedure ConfigWndNode(const Wt: TWindowFunction; const L: Integer; const Ov: Double);
    procedure SetWindowFunc(const FuncIndex: Integer);
    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: Integer); override;
    procedure ReceiveWindowedData(const P: PComplex; const Len: Integer);
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

implementation

uses
  RadioSystem;

const
  FRAME_MARGIN_LEFT   = 20;
  FRAME_MARGIN_RIGHT  = 0;
  FRAME_MARGIN_TOP    = 0;
  FRAME_MARGIN_BOTTOM = 10;

{ TTripleBuffer }

constructor TTripleBuffer.Create;
begin
  inherited;
  FBackground := TBitmap.Create;
end;

destructor TTripleBuffer.Destroy;
begin
  FBackground.Free;
  inherited Destroy;
end;

procedure TTripleBuffer.SetSize(const W, H: Integer);
begin
  FBackground.SetSize(W, H);
  inherited;
end;

{ TDoubleBuffer }

procedure TDoubleBuffer.OnPaint(Sender: TObject);
begin
  FPaintBox.Canvas.Draw(0, 0, FPaintBuffer);
end;

procedure TDoubleBuffer.SetPaintBox(AValue: TPaintBox);
begin
  if FPaintBox = AValue then Exit;
  FPaintBox := AValue;
  FPaintBox.OnPaint := @OnPaint;
end;

procedure TDoubleBuffer.Draw2Paint2;
begin
  FPaintBuffer.Canvas.Draw(0, 0, FDrawBuffer);
  FPaintBox.Canvas.Draw(0, 0, FPaintBuffer);
end;

constructor TDoubleBuffer.Create;
begin
  FDrawBuffer := TBitmap.Create;
  FPaintBuffer := TBitmap.Create;
end;

destructor TDoubleBuffer.Destroy;
begin
  FDrawBuffer.Free;
  FPaintBuffer.Free;
end;

procedure TDoubleBuffer.Draw2Paint;
begin
  TThread.Synchronize(nil, @Draw2Paint2);
end;

procedure TDoubleBuffer.SetSize(const W, H: Integer);
begin
  FDrawBuffer.SetSize(W, H);
  FPaintBuffer.SetSize(W, H);
end;

{ TRadioSpectrum }

procedure TRadioSpectrum.ImageResize(Sender: TObject);
begin
  PostMessage(RM_SPECTRUM_CFG, GUI_RESET, 0);
end;

procedure TRadioSpectrum.SetRealtime(AValue: TPaintBox);
begin
  FRt.PaintBox := AValue;
end;

procedure TRadioSpectrum.SetWaterfall(AValue: TPaintBox);
begin
  FWf.PaintBox := AValue;
end;

procedure TRadioSpectrum.DrawFrame;
begin
  with FRt.Background.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBlack;
    FillRect(0, 0, Width, Height);

    Pen.Style := psSolid;
    Pen.Color := clWhite;
    Rectangle(FRAME_MARGIN_LEFT, FRAME_MARGIN_TOP, Width - FRAME_MARGIN_RIGHT, Height - FRAME_MARGIN_BOTTOM);
    Font.Color := clYellow;
    TextOut(20, 20, Format('%d, %d', [Width, Height]));
  end;
  with FRt.DrawBuffer.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBlack;
    Pen.Color := clWhite;
    Font.Color := clWhite;
  end;
end;

procedure TRadioSpectrum.DrawRealtime;
var
  I: Integer;
  M: Double = MinDouble;
  N: Double = MaxDouble;
  X: Double;
  CurvehHeight: Integer;
  Pts: array of TPoint;
  function VtoY(const V: Double): Integer;
  begin
    Result := FRAME_MARGIN_TOP + CurvehHeight - Round(CurvehHeight * (V - N) / (M - N));
  end;

begin
  CurvehHeight := FRt.DrawBuffer.Height - FRAME_MARGIN_TOP - FRAME_MARGIN_BOTTOM;

  for X in FLine do
  begin
    M := Max(M, X);
    N := Min(N, X);
  end;
  M := Max(M, N + 1e-6);
  SetLength(Pts, High(FLine) + 1);
  for I := 0 to High(FLine) do
  begin
    Pts[I].x := FRAME_MARGIN_LEFT + I;
    Pts[I].y := VtoY(FLine[I]);
  end;

  FRt.DrawBuffer.Canvas.Draw(0, 0, FRt.Background);
  Inc(FFrames);
  with FRt.DrawBuffer.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBlack;

    Pen.Color := clYellow;
    Polyline(Pts);

    TextOut(20, 40, Format('%d', [FFrames]));
  end;
  FRt.Draw2Paint;
end;

procedure TRadioSpectrum.DrawWaterfall;
begin

end;

procedure TRadioSpectrum.PaintRealtime;
begin

end;

procedure TRadioSpectrum.PaintWaterfall;
begin

end;

procedure TRadioSpectrum.SyncPaintRealtime(Sender: TObject);
begin

end;

procedure TRadioSpectrum.SyncPaintWaterfall(Sender: TObject);
begin

end;

procedure TRadioSpectrum.SyncPaint;
begin

end;

procedure TRadioSpectrum.RedrawFull;
begin
  SetLength(FLine, FRt.PaintBox.Width - FRAME_MARGIN_LEFT - FRAME_MARGIN_RIGHT);
  FRt.SetSize(FRt.PaintBox.Width, FRt.PaintBox.Height);
  FWf.SetSize(FWf.PaintBox.Width, FWf.PaintBox.Height);
  DrawFrame;
  FRt.DrawBuffer.Canvas.Draw(0, 0, FRt.Background);
  FRt.Draw2Paint;
end;

procedure TRadioSpectrum.SyncRedrawFull;
begin
  TThread.Synchronize(nil, @RedrawFull);
end;

procedure TRadioSpectrum.ConfigWndNode(const Wt: TWindowFunction;
  const L: Integer; const Ov: Double);
var
  W: array of Double;
  I: Integer;
  D: Double;
begin
  SetLength(W, L);
  CreateWindowFunction(@W[0], L, Wt);
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
            FWndNode.Hold;
            D := FWndNode.Overlap / FFFTSize;
            FFFTSize := Integer(Msg.ParamL);
            SetLength(FF, FFFTSize);
            SetLength(FPower, FFFTSize div 2);
            ChangePlan(FFFTPlan, FFFTSize, False);
            ConfigWndNode(FWindow, FFFTSize, D);
            FWndNode.ReleaseHold;
          end;
        GUI_RESET:
          SyncRedrawFull;
      end;
    else
      inherited ProccessMessage(Msg, Ret);
  end;
end;

procedure TRadioSpectrum.ReceiveWindowedData(const P: PComplex;
  const Len: Integer);
var
  I: Integer;
begin
  FFT(FFFTPlan, P, @FF[0]);
  ModArg(@FF[0], FFFTSize);
  for I := 0 to High(FPower) do
  begin
    FPower[I] := Sqr(FF[I].re);       //
  end;
  Xpolate(@FPower[0], @FLine[0], High(FPower) + 1, High(FLine) + 1);
  DrawRealtime;
end;

constructor TRadioSpectrum.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  FRt := TTripleBuffer.Create;
  FWf := TDoubleBuffer.Create;

  FFFTSize := 1024 * 16;
  FWindow := wfRect;
  FFFTPlan := BuildFFTPlan(FFFTSize, False);

  FFlow := TWindowNode.Create;
  FFlow.LastNode.OnSendToNext := @ReceiveWindowedData;
  FWndNode := FFlow as TWindowNode;

  ConfigWndNode(FWindow, FFFTSize, 0);
  FForm := TSpectrumForm.Create(Application);
  FRt.PaintBox := FForm.PaintBox1;
  FWf.PaintBox := FForm.PaintBox2;
  FForm.Show;
  RedrawFull;
  FForm.PaintBox1.OnResize := @ImageResize;
  FForm.PaintBox2.OnResize := @ImageResize;
end;

destructor TRadioSpectrum.Destroy;
begin
  FForm.Free;
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

