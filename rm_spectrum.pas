unit rm_spectrum;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, ExtCtrls, FPImage, RadioModule, RadioNode, UComplex, SignalBasic,
  GenFFT, formspectrum, Forms, Math, Controls, radiomessage, Utils;

const

  PRIV_RM_SPECTRUM_ENABLE_PICKER = RM_USER + 10; // ParamH: Picker index (0..); ParamL: enable(1) or disable(0)
  PRIV_RM_SPECTRUM_MOUSE_DOWN    = RM_USER + 11; // ParamH: Button, ParamL: (X shl 16) or Y
  PRIV_RM_SPECTRUM_MOUSE_UP      = RM_USER + 12; // ParamH: Button, ParamL: (X shl 16) or Y
  PRIV_RM_SPECTRUM_MOUSE_MOVE    = RM_USER + 13; // ParamH: Button, ParamL: (X shl 16) or Y
  PRIV_RM_SPECTRUM_MOUSE_LEAVE   = RM_USER + 14; // ParamH/L ignore

type

  TBandInfo = record
    Freq: Integer;
    Bandwidth: Integer;
    Enable: Boolean;
  end;

  TSpectrumMouse = (smCenter, smBandwidth);

  { TRadioSpectrum }

  TRadioSpectrum = class(TRadioModule)
  private
    FPassbandFreq: Cardinal;
    FFreq: Integer;
    FSampleRate: Cardinal;
    FCenterFreq: Integer;
    FStartFreq: Integer;
    FEndFreq: Integer;
    FHzPerPixel: Double;
    FAutoY: Boolean;
    FSpan: Integer;
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
    FYRange: Double;
    FYMax: Double;
    FWaterfallTickInterval: Integer;
    FLastWaterfallTick: Integer;
    FMinInterval: TDateTime;
    FLastTime: TDateTime;
    FBandPicker: array [0..3] of TBandInfo;
    FSelectedBand: Integer;
    FLastSelected: Integer;
    FMouseTool: TSpectrumMouse;
    FDomain: Integer;
    function ScreenToPassBandFreq(const X: Integer): Integer;
    function PassBandFreqToScreen(const F: Integer): Integer;

    procedure ImageResize(Sender: TObject);
    procedure SetFFTSize(const ASize: Integer);
    procedure SetRealtime(AValue: TPaintBox);
    procedure SetWaterfall(AValue: TPaintBox);

    procedure PickerEnable(const Index: Integer; const Enable: Integer);
    procedure MouseDown(Button: TMouseButton; X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; X, Y: Integer);
    procedure MouseMove(X, Y: Integer);
    procedure MouseLeave;

  protected
    function RMSetFrequency(const Msg: TRadioMessage; const Freq: Cardinal): Integer; override;
    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer; override;

    procedure DrawFrame;
    procedure DrawRealtimeFrame;
    procedure DrawWaterfallFrame;
    procedure DrawRealtime;
    procedure DrawWaterfall;
    procedure DrawPickers(const X, Y: Integer);
    procedure RedrawFull;
    procedure SyncRedrawFull;
    procedure ConfigWndNode(const Wt: TWindowFunction; const L: Integer; const Ov: Double);
    procedure SetWindowFunc(const FuncIndex: Integer);
    procedure ProccessCustomMessage(const Msg: TRadioMessage; var Ret: Integer); override;
    procedure ReceiveWindowedData(const P: PComplex; const Len: Integer);

    procedure DoShowGUI; override;
    procedure Describe(Strs: TStrings); override;
    procedure DoSyncDestroy; override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

implementation

uses
  RadioSystem, util_math;

const
  FRAME_MARGIN_LEFT   = 45;
  FRAME_MARGIN_RIGHT  = 1;
  FRAME_MARGIN_TOP    = 1;
  FRAME_MARGIN_BOTTOM = 16;

{ TRadioSpectrum }

function TRadioSpectrum.ScreenToPassBandFreq(const X: Integer): Integer;
begin
  Result := 0;
  if FStartFreq >= FEndFreq then Exit;
  Result := FStartFreq + Round((X - FRAME_MARGIN_LEFT) * FHzPerPixel);
end;

function TRadioSpectrum.PassBandFreqToScreen(const F: Integer): Integer;
begin
  Result := 0;
  if FStartFreq >= FEndFreq then Exit;
  Result := FRAME_MARGIN_LEFT + Round((F - FStartFreq) / FHzPerPixel);
end;

procedure TRadioSpectrum.ImageResize(Sender: TObject);
begin
  PostMessage(RM_SPECTRUM_CFG, GUI_RESET, 0);
end;

procedure TRadioSpectrum.SetFFTSize(const ASize: Integer);
var
  D: Double;
begin
  FWndNode.Hold;
  D := FWndNode.Overlap / FFFTSize;
  FFFTSize := Integer(ASize);
  SetLength(FF, FFFTSize);
  SetLength(FPower, FFFTSize);
  ChangePlan(FFFTPlan, FFFTSize, False);
  ConfigWndNode(FWindow, FFFTSize, D);
  FWndNode.ReleaseHold;
end;

procedure TRadioSpectrum.SetRealtime(AValue: TPaintBox);
begin
  FRt.PaintBox := AValue;
end;

procedure TRadioSpectrum.SetWaterfall(AValue: TPaintBox);
begin
  FWf.PaintBox := AValue;
end;

procedure TRadioSpectrum.PickerEnable(const Index: Integer;
  const Enable: Integer);
var
  B: Boolean;
  S: Integer;
begin
  if (Index < 0) or (Index > High(FBandPicker)) then Exit;
  B := Enable <> 0;
  if FBandPicker[Index].Enable = B then Exit;
  FBandPicker[Index].Enable := B;
  S := (FEndFreq - FStartFreq) div 4;
  FBandPicker[Index].Freq := FStartFreq + Index * S + S div 2;
  FBandPicker[Index].Bandwidth := S div 2;
  FRt.Draw2Paint;
  DrawPickers(0, 0);
  FRt.Paint;
end;

procedure TRadioSpectrum.MouseDown(Button: TMouseButton; X, Y: Integer);
const
  CAPTURE = 3;
var
  I: Integer;
  AX: Integer;
begin
  FSelectedBand := -1;
  for I := 0 to High(FBandPicker) do
  begin
    if not FBandPicker[I].Enable then Continue;
    AX := PassBandFreqToScreen(FBandPicker[I].Freq - FBandPicker[I].Bandwidth div 2);
    if Abs(AX - X) <= CAPTURE then
    begin
      FSelectedBand := I;
      FMouseTool := smBandwidth;
      Break;
    end;
    AX := PassBandFreqToScreen(FBandPicker[I].Freq + FBandPicker[I].Bandwidth div 2);
    if Abs(AX - X) <= CAPTURE then
    begin
      FSelectedBand := I;
      FMouseTool := smBandwidth;
      Break;
    end;
    AX := PassBandFreqToScreen(FBandPicker[I].Freq);
    if Abs(AX - X) <= CAPTURE then
    begin
      FSelectedBand := I;
      FMouseTool := smCenter;
      Break;
    end;
  end;
end;

procedure TRadioSpectrum.MouseUp(Button: TMouseButton; X, Y: Integer);
var
  Z: Int64;
  F: Integer;
begin
  if FSelectedBand < 0 then
  begin
    if (FLastSelected >= 0) and FBandPicker[FLastSelected].Enable then
    begin
      F := ScreenToPassBandFreq(X);
      FBandPicker[FLastSelected].Freq := F;
      FRt.Draw2Paint;
      DrawPickers(X, Y);
      FRt.Paint;
      FSelectedBand := FLastSelected;
    end
    else
      Exit;
  end;

  // convert to baseband freq
  Z := FBandPicker[FSelectedBand].Freq - FPassbandFreq;

  // send out notification message
  Broadcast(RM_SPECTRUM_BAND_SELECT_1 + FSelectedBand,
            EnsureRange(Z - FBandPicker[FSelectedBand].Bandwidth div 2, - FSampleRate div 2, FSampleRate div 2),
            EnsureRange(Z + FBandPicker[FSelectedBand].Bandwidth div 2, - FSampleRate div 2, FSampleRate div 2));

  FLastSelected := FSelectedBand;
  FSelectedBand := -1;
end;

procedure TRadioSpectrum.MouseMove(X, Y: Integer);
var
  F: Integer;
begin
  if FSelectedBand < 0 then Exit;
  F := ScreenToPassBandFreq(X);
  case FMouseTool of
    smCenter:
      FBandPicker[FSelectedBand].Freq := F;
    smBandwidth:
      FBandPicker[FSelectedBand].Bandwidth := 2 * Round(Abs(F - FBandPicker[FSelectedBand].Freq));
  end;
  FRt.Draw2Paint;
  DrawPickers(X, Y);
  FRt.Paint;
end;

procedure TRadioSpectrum.MouseLeave;
begin
  FSelectedBand := -1;
end;

function TRadioSpectrum.RMSetFrequency(const Msg: TRadioMessage;
  const Freq: Cardinal): Integer;
begin
  if FPassbandFreq = Freq then Exit;
  FPassbandFreq := Freq;
  FFreq := FPassbandFreq - FSampleRate div 2;
  DrawRealtimeFrame;
  Result := 0;
  GraphInvalidate;
end;

function TRadioSpectrum.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  if FSampleRate = Rate then Exit;
  FSampleRate := Rate;
  FFreq := FPassbandFreq - FSampleRate div 2;
  DrawRealtimeFrame;
  Result := 0;
  GraphInvalidate;
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
  S: string;
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
    if FDomain = SPECTRUM_DATA_DOMAIN_REAL then
    begin
      FStartFreq := Max(0, FFreq + FSampleRate div 2);
      FEndFreq := FFreq + FSampleRate;
    end
    else begin
      FStartFreq := FFreq;
      FEndFreq := FFreq + FSampleRate;
    end;
  end
  else if FSpan > 0 then
  begin
    FStartFreq := Max(FFreq, FCenterFreq - (FSpan div 2));
    if FDomain = SPECTRUM_DATA_DOMAIN_REAL then
    begin
      FStartFreq := Max(0, FStartFreq);
      FEndFreq := Min(FCenterFreq + (FSpan div 2), FFreq + FSampleRate div 2);
    end
    else begin
      FEndFreq := Min(FCenterFreq + (FSpan div 2), FFreq + FSampleRate);
    end;
  end;

  with FRt.Background.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBlack;
    FillRect(0, 0, Width, Height);
    Brush.Style := bsClear;

    Pen.Style := psSolid;
    Pen.Color := clWhite;
    Rectangle(FRAME_MARGIN_LEFT - 1, FRAME_MARGIN_TOP - 1, Width - FRAME_MARGIN_RIGHT + 1, Height - FRAME_MARGIN_BOTTOM + 1);

    // y-axis
    Font.Color := clWhite;
    Font.Size  := 10;
    E := TextExtent('0');
    BeatifulTick(Height - FRAME_MARGIN_BOTTOM - FRAME_MARGIN_TOP, 50, FYMax - FYRange, FYMax, Start, Step);
    Step := Max(1, Step);

    with R do
    begin
      Left := 1;
      Right := Left + FRAME_MARGIN_LEFT - 2;
      Top := 0;
      Bottom := Top + E.cy;
    end;

    T := Round((Height - FRAME_MARGIN_BOTTOM - FRAME_MARGIN_TOP) / (FYRange / Step));
    Pen.Color := TColor($404040);
    C := (Height - FRAME_MARGIN_BOTTOM - FRAME_MARGIN_TOP) div T;
    TextRect(R, 0, 0, FloatToStr(Start + (C + 1) * Step), Style);
    Dec(R.Top, E.cy div 2); Dec(R.Bottom, E.cy div 2);
    for I := 1 to C do
    begin
      Line(FRAME_MARGIN_LEFT, FRAME_MARGIN_TOP + I * T, Width - FRAME_MARGIN_RIGHT, FRAME_MARGIN_TOP + I * T);
      Inc(R.Top, T); Inc(R.Bottom, T);
      TextRect(R, 0, 0, FloatToStr(Start + (C - I + 1) * Step), Style);
    end;

    // x-axis
    FHzPerPixel := 1;
    if FSpan = 0 then Exit;
    if FEndFreq <= FStartFreq then Exit;

    Font.Height := FRAME_MARGIN_BOTTOM - 1;
    E := TextExtent(FormatFreq(FEndFreq));
    E.cx := E.cx * 2;
    BeatifulTick(Width - FRAME_MARGIN_RIGHT - FRAME_MARGIN_LEFT, Max(100, E.cx), FStartFreq, FEndFreq, Start, Step);
    XStep := Max(1, Round(Step));
    FHzPerPixel := (FEndFreq - FStartFreq) / (Width - FRAME_MARGIN_RIGHT - FRAME_MARGIN_LEFT);

    with R do
    begin
      Left := FRAME_MARGIN_LEFT - (E.cx div 2);
      Right := Left + E.cx;
      Top := Height - E.cy - 1;
      Bottom := Height - 1;
    end;

    Style.Alignment := taCenter;
    T := (FStartFreq div XStep) * XStep;
    if T < FStartFreq then Inc(T, XStep);
    while T <= FEndFreq do
    begin
      I := Round((T - FStartFreq) / (FEndFreq - FStartFreq) * (Width - FRAME_MARGIN_RIGHT - FRAME_MARGIN_LEFT)) + FRAME_MARGIN_LEFT;
      Line(I, FRAME_MARGIN_TOP, I, Height - FRAME_MARGIN_BOTTOM);
      S := FormatFreq(T);
      E := TextExtent(S);
      TextOut(I - E.cx div 2, R.Top, S);
      Inc(T, XStep);
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
    Font.Color := clWhite;
    Font.Size  := 8;

    Line(FRAME_MARGIN_LEFT - 1, 0, FRAME_MARGIN_LEFT - 1, Height);
    Line(Width - FRAME_MARGIN_RIGHT + 1, 0, Width - FRAME_MARGIN_RIGHT + 1, Height);
  end;
end;

procedure TRadioSpectrum.DrawRealtime;
var
  I: Integer;
  M: Double = -MaxDouble;
  N: Double = MaxDouble;
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
  end;
  FRt.Draw2Paint;
  DrawPickers(0, 0);
  FRt.Paint;
end;

procedure TRadioSpectrum.DrawWaterfall;
var
  SrcRect, TarRect: TRect;
  P: PByte;
  I: Integer;
  R, G, B: Byte;
  S: Double;
  procedure DrawTick;
  var
    H, M, S, MS: Word;
    K: Integer;
  begin
    DecodeTime(FLastTime, H, M, S, MS);
    K := ((H * 60 + M) * 60 + S);
    if (K = FLastWaterfallTick) or (K mod FWaterfallTickInterval <> 0) then Exit;
    with FWf.DrawBuffer do
    begin
      Canvas.TextOut(0, 0, Format('%.2d:%.2d:%.2d', [H, M, S]));
    end;
    FLastWaterfallTick := K;
  end;

begin
  with SrcRect do
  begin
    Left := 0;
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
    P := @PByte(FWf.DrawBuffer.ScanLine[0])[(FRAME_MARGIN_LEFT - 1)* 3];
    P[0] := 255; P[1] := 255; P[2] := 255;

    P := @PByte(FWf.DrawBuffer.ScanLine[0])[FRAME_MARGIN_LEFT * 3];
    for I := 0 to High(FLine) do
    begin
      S := EnsureRange((FYMax - FLine[I]) / FYRange, 0, 1);
      HSL2RGB(0.7 * S, 1, 0.5 - 0.4 * S, R, G, B);
      P[3 * I + 0] := B;
      P[3 * I + 1] := G;
      P[3 * I + 2] := R;
    end;
    EndUpdate();
  end;

  // time ticker
  if FWaterfallTickInterval > 0 then DrawTick;

  FWf.Draw2Paint;
  FWf.Paint;
end;

procedure TRadioSpectrum.DrawPickers(const X, Y: Integer);
var
  I: Integer;
  X1, X2: Integer;

  procedure VLine(const V: Integer);
  begin
    if V <= FRAME_MARGIN_LEFT then Exit;
    if V >= FRt.PaintBuffer.Width - FRAME_MARGIN_RIGHT then Exit;
    with FRt.PaintBuffer.Canvas do
      Line(V, FRAME_MARGIN_TOP, V, Height - FRAME_MARGIN_BOTTOM);
  end;

begin
  for I := 0 to High(FBandPicker) do
  begin
    if not FBandPicker[I].Enable then Continue;
    with FRt.PaintBuffer.Canvas do
    begin
      Pen.Color := clWhite;
      Pen.Width := 1;
      Pen.Style := psSolid;
      X1 := PassBandFreqToScreen(FBandPicker[I].Freq - FBandPicker[I].Bandwidth div 2);
      X2 := PassBandFreqToScreen(FBandPicker[I].Freq);
      VLine(X1);
      VLine(X2 + (X2 - X1));
      Pen.Color := clRed;
      Pen.Width := 2;
      Pen.Style := psDashDot;
      VLine(X2);
      Brush.Color := clWhite;
      Brush.Style := bsSolid;
      Font.Color := clBlack;
      TextOut(X2 - TextWidth('0') div 2, FRAME_MARGIN_TOP, IntToStr(I + 1));
      if FSelectedBand = I then
      begin
        Brush.Style := bsClear;
        Font.Color := clCream;
        TextOut(X2 + 2, Height div 2,
                Format('Freq = %s',
                       [FormatFreq(FBandPicker[I].Freq)]));
        TextOut(X2 + 2, Height div 2 + TextHeight('F'),
                Format('Bw = %s',
                       [FormatFreq(FBandPicker[I].Bandwidth)]));
      end;
    end;
  end;
end;

procedure TRadioSpectrum.RedrawFull;
var
  W, H: Integer;
begin
  W := FRt.PaintBox.Width;
  H := FRt.PaintBox.Height;
  if W < FRAME_MARGIN_LEFT + FRAME_MARGIN_RIGHT then Exit;
  SetLength(FLine, W - FRAME_MARGIN_LEFT - FRAME_MARGIN_RIGHT);
  FRt.SetSize(W, H);
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
begin
  SetLength(W, L);
  CreateWindowFunction(@W[0], L, Wt);
  FWndNode.SetWindow(@W[0], L);
  I := Round(L * Ov);
  if (I >= 0) and (I < L) then FWndNode.Overlap := I;
end;

procedure TRadioSpectrum.SetWindowFunc(const FuncIndex: Integer);
begin
  if (FuncIndex >= Ord(Low(TWindowFunction))) and (FuncIndex <= Ord(High(TWindowFunction))) then
  begin
    FWindow := TWindowFunction(FuncIndex);
    ConfigWndNode(FWindow, FFFTSize, FWndNode.Overlap / FFFTSize);
  end;
end;

procedure TRadioSpectrum.ProccessCustomMessage(const Msg: TRadioMessage;
  var Ret: Integer);
var
  I: Integer;
begin
  case Msg.Id of
    PRIV_RM_SPECTRUM_ENABLE_PICKER:
      PickerEnable(Msg.ParamH, Msg.ParamL);
    PRIV_RM_SPECTRUM_MOUSE_DOWN:
      MouseDown(TMouseButton(Msg.ParamH), Msg.ParamL shr 16, Msg.ParamL and $FFFF);
    PRIV_RM_SPECTRUM_MOUSE_UP:
      MouseUp(TMouseButton(Msg.ParamH), Msg.ParamL shr 16, Msg.ParamL and $FFFF);
    PRIV_RM_SPECTRUM_MOUSE_LEAVE:
      MouseLeave;
    PRIV_RM_SPECTRUM_MOUSE_MOVE:
      MouseMove(Msg.ParamL shr 16, Msg.ParamL and $FFFF);
    RM_SPECTRUM_CFG:
      begin
        case Msg.ParamH of
          SET_WND_FUNC: SetWindowFunc(Integer(Msg.ParamL));
          SET_OVERLAP_PER:
            begin
              I := Round(FFFTSize * Integer(Msg.ParamL) / 100);
              if (I >= 0) and (I < FFFTSize) then FWndNode.Overlap := I;
            end;
          SET_FFT_SIZE:
             SetFFTSize(Msg.ParamL);
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
            FAutoY := Msg.ParamL <> 0;
          SET_SPAN:
            begin
              FSpan := Integer(Msg.ParamL);
              DrawRealtimeFrame;
            end;
          SET_CENTER_FREQ:
            begin
              FCenterFreq := Integer(Msg.ParamL);
              DrawRealtimeFrame;
            end;
          SET_DRAW_MIN_INTERVAL:
            FMinInterval := Msg.ParamL / MSecsPerDay;
          SET_DATA_DOMAIN:
            begin
              FDomain := Msg.ParamL;
              DrawRealtimeFrame;
            end;
        end;
        GraphInvalidate;
      end
    else
      inherited;
  end;
end;

procedure TRadioSpectrum.ReceiveWindowedData(const P: PComplex;
  const Len: Integer);
var
  I, J: Integer;
  Li, Lj: Integer;
  F0, T: Double;
  Ma, Mi: Double;
  ZeroFreq: Integer;
  BStart, BEnd: Integer;

  function CheckTime: Boolean;
  var
    T: TDateTime;
  begin
    T := Now;
    Result := T - FLastTime > FMinInterval;
    if Result then FLastTime := T;
  end;

begin
  if not CheckTime then Exit;
  if not FForm.Visible then Exit;
  if FSpan = 0 then
  begin
    J := Min(Len - 1, High(FLine));
    Ma := 0;
    Mi := MaxDouble;
    for I := 0 to J do
    begin
      FLine[I] := P[I].re * P[I].re + P[I].im * P[I].im;
      Ma := Max(Ma, FLine[I]);
      Mi := Min(Mi, FLine[I]);
    end;
    for I := J + 1 to High(FLine) do
      FLine[I] := 0;
    T := FYRange;
    F0 := FYMax;
    FYMax := Ma;
    FYRange := Round(Ma - Mi);
    DrawRealtime;
    FYRange := T; // restore
    FYMax   := F0;
    Exit;
  end;

  if FEndFreq < FStartFreq then Exit;

  FillByte(FLine[0], (High(FLine) + 1) * SizeOf(FLine[0]), 0);

  FFT(FFFTPlan, P, @FF[0]);
  SpectrumPower(@FF[0], @FPower[0], FFFTSize);

  F0 := 1;
  if FSampleRate > 0 then
    F0 := FSampleRate / FFFTSize;

  ZeroFreq := FFreq;
  BStart := FStartFreq - ZeroFreq;
  BEnd   := FEndFreq - ZeroFreq;

  Li := 0; Lj := High(FLine);
  I := Round(BStart / F0);
  J := Round(BEnd / F0);
  if I < 0 then
  begin
    Li := Round(-BStart / FHzPerPixel);
    I := 0;
  end;
  if J > High(FPower) then
  begin
    Dec(Lj, Round((J - High(FPower)) * F0 / FHzPerPixel));
    J := High(FPower);
  end;

  Xpolate(PDouble(@FPower[I]), @FLine[Li], J - I + 1, Lj - Li + 1);

  if FAutoY then
  begin
    //FAutoY := False;
    Ma := 0.0;
    Mi := 0.0;
    for J := Li to Lj do
    begin
      if FLine[J] > 0 then
      begin
        Ma := FLine[J];
        Mi := FLine[J];
        Break;
      end;
    end;
    for I := J + 1 to Lj do
    begin
      Ma := Max(Ma, FLine[I]);
      Mi := Min(Mi, FLine[I]);
    end;
    Ma := 10 * log10(Ma + MinDouble);
    Mi := 10 * log10(Mi + MinDouble);

    FYMax := Ma;
    FYRange := Max(1, Min(50, Round(Ma - Mi)));
    DrawRealtimeFrame;
  end;

  for I := 0 to High(FLine) do
    FLine[I] := 10 * log10(FLine[I] + MinDouble);

  DrawRealtime;
  DrawWaterfall;
end;

procedure TRadioSpectrum.DoShowGUI;
begin
  FForm.Caption := Format('Spectrum (%s)', [Name]);
  FForm.Show;
end;

procedure TRadioSpectrum.Describe(Strs: TStrings);
begin
  with Strs do
  begin
    Add(Format('^bCenter Freq: ^n%s', [FormatFreq(FCenterFreq)]));
    Add(Format('^bSample Rate: ^n%d', [FSampleRate]));
  end;
end;

procedure TRadioSpectrum.DoSyncDestroy;
begin
  FRt.Free;
  FWf.Free;
  FForm.Free;
  FFlow.Free;
  inherited DoSyncDestroy;
end;

constructor TRadioSpectrum.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
  FHasGUI := True;
  FHasConfig := False;
  FRt := TTripleBuffer.Create;
  FWf := TDoubleBuffer.Create;
  FWf.DrawBuffer.PixelFormat := pf24bit;

  FMinInterval := 20 / MSecsPerDay;
  FYRange := 30;
  FFFTSize := 1024 * 16;
  FWindow := wfRect;
  FFFTPlan := BuildFFTPlan(FFFTSize, False);
  FSpan := -1;
  FAutoY := True;
  FWaterfallTickInterval := 5;

  FSelectedBand := -1;

  FFlow := TWindowNode.Create;
  FFlow.LastNode.OnSendToNext := @ReceiveWindowedData;
  FWndNode := FFlow as TWindowNode;

  ConfigWndNode(FWindow, FFFTSize, 0);
  FForm := TSpectrumForm.Create(Application);
  FRt.PaintBox := FForm.PaintBox1;
  FWf.PaintBox := FForm.PaintBox2;
  //FForm.Show;
  RedrawFull;
  FForm.PaintBox1.OnResize := @ImageResize;
  FForm.PaintBox2.OnResize := @ImageResize;
  FForm.Module := Self;

  SetFFTSize(FFFTSize);
end;

destructor TRadioSpectrum.Destroy;
var
  P: PFFTPlan;
begin
  P := FFFTPlan;
  FFFTPlan := nil;
  FinalizePlan(P);
  inherited Destroy;
end;

procedure TRadioSpectrum.ReceiveData(const P: PComplex; const Len: Integer);
begin
  FFlow.ReceiveData(P, Len);
end;

initialization

  RegisterModule(TRadioModuleClass(TRadioSpectrum.ClassType));

end.

