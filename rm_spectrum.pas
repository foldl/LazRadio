unit rm_spectrum;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, ExtCtrls, Graphics, RadioModule, UComplex, SignalBasic,
  KissFFT, formspectrum, Forms, Math;

const

  RM_SPECTRUM_CFG = RM_USER;
                  SET_WND_FUNC      = 0;
                  SET_OVERLAP_PER   = 1;
                  SET_FFT_SIZE      = 2;
                  GUI_RESET         = 3;  // gui resize
                  SET_Y_RANGE       = 4;  // y range in dB
                  SET_AUTO_Y        = 5;  // set y range automatically for one-shot
                  SET_SPAN          = 6;  // set x span in Hz (full span: sample rate / 2 (-1))
                  SET_CENTER_FREQ   = 7;  // set center frequency in x span (freq of the input data stream)
                  SET_Y_MAX         = 8;


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
    FCenterFreq: Integer;
    FSpan: Integer;
    FStartFreq: Cardinal;
    FEndFreq: Cardinal;
    FHzPerPix: Double;
    FAutoY: Boolean;
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
    FYRange: Integer;
    FYMax: Double;
    FThisMax: Double;
    FX:boolean;
    procedure ImageResize(Sender: TObject);
    procedure SetRealtime(AValue: TPaintBox);
    procedure SetWaterfall(AValue: TPaintBox);
  protected
    function RMSetFrequency(const Msg: TRadioMessage; const Freq: Cardinal): Integer; override;
    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer; override;

    procedure DrawFrame;
    procedure DrawRealtimeFrame;
    procedure DrawWaterfallFrame;
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
  FRAME_MARGIN_LEFT   = 41;
  FRAME_MARGIN_RIGHT  = 1;
  FRAME_MARGIN_TOP    = 1;
  FRAME_MARGIN_BOTTOM = 16;

procedure HSL2RGB(Hue, Saturation, Lightness: Double; out R, G, B: Byte);
var
  P, Q: Double;

  function hue2rgb(P, Q, T: Double): Double;
  begin
    if T < 0 then T := T + 1;
    if T > 1 then T := T - 1;
    if T < 1/6 then Result := P + (Q - P) * 6 * T
    else if T < 1/2 then Result := Q
    else if T < 2/3 then Result := P + (Q - P) * (2/3 - T) * 6
    else Result := P;
  end;
begin
  if Saturation = 0 then
  begin
    R := 255; G := 255; B := 255;
    Exit;
  end;
  q := IfThen(Lightness < 0.5, Lightness * (1 + Saturation), Lightness + Saturation - Lightness * Saturation);
  p := 2 * Lightness - q;
  R := Trunc(255 * hue2rgb(p, q, Hue + 1/3));
  G := Trunc(255 * hue2rgb(p, q, Hue));
  B := Trunc(255 * hue2rgb(p, q, Hue - 1/3));
end;

procedure BeatifulTick(const ScreenWidth, MinTickWidth: Integer; const V0, V1: Double; out Start, Step: Double);
var
  I: Integer;

  function Simplify(V: Double): Double;
  var
    S: string;
    K, J: Integer;
    T: Integer = 0;
    F: Boolean = False;
    procedure ClearS(const Start: Integer);
    var
      L: Integer;
    begin
      for L := Start to Length(S) do
        if S[L] in ['0'..'9'] then S[L] := '0';
    end;

  begin
    S := FloatToStr(V);
    for K := 1 to Length(S) do
    begin
      if S[K] in ['1'..'9'] then
      begin
        if I < Length(S) then
          if StrToIntDef(S[K + 1], 0) >= 5 then T := 1;
        Inc(T, StrToInt(S[K]));
        case T of
          3, 7, 9:
            Inc(T);
        end;
        if T < 10 then
        begin
          S[K] := IntToStr(T)[1];
          ClearS(K + 1);
        end
        else begin
          if K > 1 then
          begin
            S[K - 1] := '1';
            ClearS(K);
          end
          else begin
            ClearS(1);
            S := '1' + S;
          end;
        end;
      end;
    end;
    Result := StrToFloat(S);
  end;

begin
  I := Trunc(ScreenWidth / MinTickWidth);
  Start := V0;
  Step := V1 - V0;
  if I <= 0 then Exit;
  if Step <= 0 then Exit;

  Step := Simplify((V1 - V0) / I);
  I := Round(V0 / Step);
  Start := I * Step;
  if Start > V0 then Start := Start - Step;
end;

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

function TRadioSpectrum.RMSetFrequency(const Msg: TRadioMessage;
  const Freq: Cardinal): Integer;
begin
  FFreq := Freq;
  DrawRealtimeFrame;
end;

function TRadioSpectrum.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  FSampleRate := Rate;
  DrawRealtimeFrame;
end;

procedure TRadioSpectrum.DrawFrame;
begin
  DrawRealtimeFrame;
  DrawWaterfallFrame;
end;

procedure TRadioSpectrum.DrawRealtimeFrame;
var
  Start, Step: Double;
  E: TSize;
  T: Integer;
  C: Integer;
  I: Integer;
  XStep: Integer;
  Style: TTextStyle;
  R: TRect;
begin
  with Style do
  begin
    Alignment := taRightJustify;
    Layout    := tlCenter;
    SingleLine:= True;
    Clipping  := True;
    ExpandTabs:= False;
    ShowPrefix:= False;
    Wordbreak := False;
    Opaque    := False;
    SystemFont:= False;
    RightToLeft:= False;
    EndEllipsis:= False;
  end;

  if FSpan < 0 then
  begin
    // full span
    FStartFreq := 0;
    FEndFreq := FSampleRate div 2;
  end
  else if FSpan > 0 then
  begin
    FStartFreq := Max(0, FCenterFreq - (FSpan div 2));
    FEndFreq := Min(FSampleRate div 2, FStartFreq + FSpan);
  end;

  with FRt.Background.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBlack;
    FillRect(0, 0, Width, Height);

    Pen.Style := psSolid;
    Pen.Color := clWhite;
    Rectangle(FRAME_MARGIN_LEFT - 1, FRAME_MARGIN_TOP - 1, Width - FRAME_MARGIN_RIGHT + 1, Height - FRAME_MARGIN_BOTTOM + 1);

    // y-axis
    Font.Color := clWhite;
    Font.Size  := 12;
    E := TextExtent(IntToStr(-FYRange));
    BeatifulTick(Height - FRAME_MARGIN_BOTTOM - FRAME_MARGIN_TOP, Max(50, E.cy), 0, FYRange, Start, Step);
    XStep := Max(1, Round(Step));

    with R do
    begin
      Left := 1;
      Right := Left + E.cx;
      Top := 0;
      Bottom := Top + E.cy;
    end;

    T := Round((Height - FRAME_MARGIN_BOTTOM - FRAME_MARGIN_TOP) / (FYRange / XStep));
    Pen.Color := TColor($404040);
    C := (Height - FRAME_MARGIN_BOTTOM - FRAME_MARGIN_TOP) div T;
    TextRect(R, 0, 0, '0', Style);
    Dec(R.Top, E.cy div 2); Dec(R.Bottom, E.cy div 2);
    for I := 1 to C do
    begin
      Line(FRAME_MARGIN_LEFT, FRAME_MARGIN_TOP + I * T, Width - FRAME_MARGIN_RIGHT, FRAME_MARGIN_TOP + I * T);
      Inc(R.Top, T); Inc(R.Bottom, T);
      TextRect(R, 0, 0, IntToStr(- I * XStep), Style);
    end;

    // x-axis
    FHzPerPix := 1;
    if FSpan = 0 then Exit;
    if FEndFreq = FStartFreq then Exit;

    Font.Size  := 9;
    E := TextExtent(FormatFreq(FEndFreq + FFreq));
    E.cx := E.cx * 2;
    BeatifulTick(Width - FRAME_MARGIN_RIGHT - FRAME_MARGIN_LEFT, Max(100, E.cx), FStartFreq + FFreq, FEndFreq + FFreq, Start, Step);
    XStep := Max(1, Round(Step));

    with R do
    begin
      Left := FRAME_MARGIN_LEFT - (E.cx div 2);
      Right := Left + E.cx;
      Top := Height - E.cy - 1;
      Bottom := Height - 1;
    end;

    Style.Alignment := taCenter;

    T := Trunc((Width - FRAME_MARGIN_RIGHT - FRAME_MARGIN_LEFT) / ((FEndFreq - FStartFreq) / XStep));
    C := (Width - FRAME_MARGIN_RIGHT - FRAME_MARGIN_LEFT) div T;
    TextRect(R, 0, 0, FloatToStr(Start), Style);
    FStartFreq := Round(Start);
    FEndFreq := Round((Width - FRAME_MARGIN_RIGHT - FRAME_MARGIN_LEFT) / T * XStep + FStartFreq);
    FHzPerPix := XStep / T;
    for I := 1 to C do
    begin
      Line(FRAME_MARGIN_LEFT + I * T, FRAME_MARGIN_TOP, FRAME_MARGIN_LEFT + I * T, Height - FRAME_MARGIN_BOTTOM);
      Inc(R.Left, T); Inc(R.Right, T); Start := Start + XStep;
      TextRect(R, 0, 0, FormatFreq(Round(Start)), Style);
    end;
  end;
end;

procedure TRadioSpectrum.DrawWaterfallFrame;
begin
  with Fwf.DrawBuffer.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBlack;
    FillRect(0, 0, Width, Height);

    Pen.Color := clWhite;
    Font.Color := clYellow;
    Font.Size  := 9;

    Line(FRAME_MARGIN_LEFT - 1, 0, FRAME_MARGIN_LEFT - 1, Height);
    Line(Width - FRAME_MARGIN_RIGHT + 1, 0, Width - FRAME_MARGIN_RIGHT + 1, Height);
  end;
end;

procedure TRadioSpectrum.DrawRealtime;
var
  I: Integer;
  M: Double = -MaxDouble;
  N: Double = MaxDouble;
  X: Double;
  CurvehHeight: Integer;
  Pts: array of TPoint;
  function VtoY(const V: Double): Integer;
  var
    T: Double;
  begin
    T := EnsureRange(V, N, M);
    Result := FRAME_MARGIN_TOP + CurvehHeight - Round(CurvehHeight * (T - N) / (M - N));
  end;

begin
  CurvehHeight := FRt.DrawBuffer.Height - FRAME_MARGIN_TOP - FRAME_MARGIN_BOTTOM;

  M := FYMax;
  N := M - FYRange;
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
var
  SrcRect, TarRect: TRect;
  P: PByte;
  I: Integer;
  R, G, B: Byte;
  S: Double;
begin
  with SrcRect do
  begin
    Left := FRAME_MARGIN_LEFT;
    Right := FWf.DrawBuffer.Canvas.Width - FRAME_MARGIN_RIGHT;
    Top := 0;
    Bottom := FWf.DrawBuffer.Canvas.Height - 1;
  end;
  TarRect := SrcRect;
  TarRect.Top := 1;
  Inc(TarRect.Bottom);
  FWf.DrawBuffer.Canvas.CopyRect(TarRect, FWf.DrawBuffer.Canvas, SrcRect);
  if SrcRect.Right - SrcRect.Left < High(FLine) then Exit;
  with FWf.DrawBuffer do
  begin
    BeginUpdate();
    P := @PByte(FWf.DrawBuffer.ScanLine[0])[FRAME_MARGIN_LEFT * 3];
    for I := 0 to High(FLine) do
    begin
      S := EnsureRange((FYMax - FLine[I]) / FYRange, 0, 1);
      HSL2RGB(0.7 * S, 1, 0.5 - 0.3 * S, R, G, B);
      P[3 * I + 0] := B;
      P[3 * I + 1] := G;
      P[3 * I + 2] := R;
    end;
    EndUpdate();
  end;
  FWf.Draw2Paint;
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
  FWf.Draw2Paint;
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
        SET_Y_RANGE:
          begin
            FYRange := Max(2, Integer(Msg.ParamL));
            DrawRealtimeFrame;
          end;
        SET_Y_MAX:
          begin
            FYMax := Integer(Msg.ParamL);
            DrawRealtimeFrame;
          end;
        SET_AUTO_Y:
          FAutoY := True;
        SET_SPAN:
          begin
            FSpan := Integer(Msg.ParamL);
            if FSpan < 0 then FSpan := FSampleRate div 2;
            DrawRealtimeFrame;
          end;
        SET_CENTER_FREQ:
          begin
            FCenterFreq := Integer(Msg.ParamL);
            DrawRealtimeFrame;
          end;
      end;
    else
      inherited ProccessMessage(Msg, Ret);
  end;
end;

procedure TRadioSpectrum.ReceiveWindowedData(const P: PComplex;
  const Len: Integer);
var
  I, J: Integer;
  Li, Lj: Integer;
  F0: Double;
  Ma, Mi: Double;
begin
  if FSpan = 0 then
  begin
    // TODO: zero span
    for I := 0 to Min(Len - 1, High(FLine)) do
    begin
      FLine[I] := log10(P[I].re * P[I].re + P[I].im * P[I].im + MinDouble);
    end;
    DrawRealtime;
    Exit;
  end;

  if FEndFreq < FStartFreq then Exit;

  FFT(FFFTPlan, P, @FF[0]);
  PowArg(@FF[0], FFFTSize);
  for I := 0 to High(FPower) do
    FPower[I] := log10(FF[I].re + MinDouble);
  if FAutoY then
  begin
    FAutoY := False;
    Ma := FPower[0]; Mi := FPower[0];
    if High(FPower) >= 1 then
    begin
      Ma := FPower[1]; Mi := FPower[1];
    end
    else;
    for I := 1 to High(FPower) do
    begin
      Ma := Max(Ma, FPower[I]);
      Mi := Min(Mi, FPower[I]);
    end;
    FYMax := Ma;;
    FYRange := Max(1, Min(50, Round(Ma - Mi)));
    DrawRealtimeFrame;
  end;
  for I := 0 to High(FLine) do
    FLine[I] := -MaxDouble;

  F0 := 1;
  if FSampleRate > 0 then
    F0 := FSampleRate / FFFTSize;

  Li := 0; Lj := High(FLine);
  I := Round(FStartFreq / F0);
  J := Round(FEndFreq / F0);
  if I < 0 then
  begin
    Li := Round(-FStartFreq / FHzPerPix);
    I := 0;
  end;
  if J > High(FPower) then
  begin
    Dec(Lj, Round((J - High(FPower)) * F0 / FHzPerPix));
    J := High(FPower);
  end;
  Xpolate(@FPower[I], @FLine[Li], J - I + 1, Lj - Li + 1);
  DrawRealtime;
  DrawWaterfall;
end;

constructor TRadioSpectrum.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  FRt := TTripleBuffer.Create;
  FWf := TDoubleBuffer.Create;
  FWf.DrawBuffer.PixelFormat := pf24bit;

  FYRange := 30;
  FFFTSize := 1024 * 16;
  FWindow := wfRect;
  FFFTPlan := BuildFFTPlan(FFFTSize, False);
  FSpan := -1;

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
  FForm.Module := Self;
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

