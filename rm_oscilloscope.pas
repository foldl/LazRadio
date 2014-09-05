unit rm_oscilloscope;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, ExtCtrls, FPImage, RadioModule, RadioSystem,
  UComplex, SignalBasic,
  formoscilloscope, Forms, Math, Controls, radiomessage, Utils;

type

  { TRadioOscilloscope }

  TRadioOscilloscope = class(TRadioModule)
  private
    FRegulator: TStreamRegulator;
    FSweep: Double;
    FRate: Cardinal;
    FYMax: Double;
    FYMin: Double;
    FAutoY: Boolean;
    FChMode: Integer;
    FChArith: Integer;
    FRunMode: Integer;
    FSampleGrid: Integer;
    FCoupling: Integer;
    FMinInterval: Double;
    FLastFrameTime: Double;
    FGraphBox: TTripleBuffer;
    FForm: TOscilloscopeForm;
    FUIData: array of Complex;
    FPts: array of TPoint;
    procedure ImageResize(Sender: TObject);
    procedure CfgRegulator;
  protected
    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer; override;

    procedure DrawFrame;
    procedure DrawFrameNormal;
    procedure DrawFrameXY;
    procedure DrawData(const P: PComplex; const Len: Integer);
    procedure DrawDataXY(const P: PComplex; const Len: Integer);
    procedure RedrawFull;
    procedure SyncRedrawFull;

    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: Integer); override;
    procedure ReceiveWindowedData(const P: PComplex; const Len: Integer);

    procedure DoShowGUI; override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

implementation

uses
  util_math;

const
  FRAME_MARGIN_LEFT   = 45;
  FRAME_MARGIN_RIGHT  = 1;
  FRAME_MARGIN_TOP    = 1;
  FRAME_MARGIN_BOTTOM = 16;

  THEME: array [0..8] of TColor =
    (482559, 654847, 505739, 6450981, 15631106, 8199463, 9847672, 8192996, 2164712);

{ TRadioOscilloscope }

procedure TRadioOscilloscope.ImageResize(Sender: TObject);
begin
  PostMessage(RM_OSCILLOSCOPE_CFG, OSCILLOSCOPE_GUI_RESET, 0);
end;

procedure TRadioOscilloscope.CfgRegulator;
begin
  FRegulator.Size := EnsureRange(Round(FSweep * FRate), 10, 20 * 1024);
end;

function TRadioOscilloscope.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  FRate := Rate;
  CfgRegulator;
  Result := inherited;
end;

procedure TRadioOscilloscope.DrawFrame;
begin
  if FChMode = OSCILLOSCOPE_CHANNEL_XY then
    DrawFrameXY
  else
    DrawFrameNormal;
end;

procedure TRadioOscilloscope.DrawFrameNormal;
var
  Start, Step: Double;
  E: TSize;
  T: Double;
  I: Integer;
  Style: TTextStyle;
  R: TRect;
  S: string;
  TimeScale: Integer;
  ScaledSweep: Double;
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

  with FGraphBox.Background.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBlack;
    FillRect(0, 0, Width, Height);
    Brush.Style := bsClear;

    Pen.Style := psSolid;
    Pen.Color := clWhite;
    Rectangle(FRAME_MARGIN_LEFT - 1, FRAME_MARGIN_TOP - 1, Width - FRAME_MARGIN_RIGHT + 1, Height - FRAME_MARGIN_BOTTOM + 1);

    if FYMax <= FYMin then Exit;
    if FSweep <= 0.0 then Exit;

    // y-axis
    Font.Color := clWhite;
    Font.Size  := 10;
    E := TextExtent('0');
    BeatifulTick(Height - FRAME_MARGIN_BOTTOM - FRAME_MARGIN_TOP, 50, FYMin, FYMax, Start, Step);
    if Step <= 0.0 then Step := FYMax - FYMin;

    with R do
    begin
      Left := 1;
      Right := Left + FRAME_MARGIN_LEFT - 2;
      Top := 0;
      Bottom := Top + E.cy;
    end;

    T := Start;
    Pen.Color := TColor($404040);
    while T <= FYMax do
    begin
      I := Round(T / (FYMax - FYMin) * (Height - FRAME_MARGIN_BOTTOM - FRAME_MARGIN_TOP));
      Line(FRAME_MARGIN_LEFT, FRAME_MARGIN_TOP + I, Width - FRAME_MARGIN_RIGHT, FRAME_MARGIN_TOP + I);
      R.Top := I - E.cy div 2;
      R.Top := I + E.cy div 2;
      TextRect(R, 0, 0, FloatToStr(T), Style);
      T := T + Step;
    end;

    // x-axis
    if FSweep < 1e-3 then
    begin
      TimeScale := 1000000;
      TextOut(2, Height - Abs(Font.Height) - 2, 'us');
    end
    else if FSweep < 1 then
    begin
      TimeScale := 1000;
      TextOut(2, Height - Abs(Font.Height) - 2, 'ms');
    end
    else begin
      TimeScale := 1;
      TextOut(2, Height - Abs(Font.Height) - 2, 's');
    end;
    ScaledSweep := TimeScale * FSweep;

    Font.Height := FRAME_MARGIN_BOTTOM - 1;
    E := TextExtent(FloatToStr(ScaledSweep));
    E.cx := E.cx * 2;
    BeatifulTick(Width - FRAME_MARGIN_RIGHT - FRAME_MARGIN_LEFT, Max(100, E.cx), 0, ScaledSweep, Start, Step);
    if Step <= 0.0 then Step := ScaledSweep;

    with R do
    begin
      Left := FRAME_MARGIN_LEFT - (E.cx div 2);
      Right := Left + E.cx;
      Top := Height - E.cy - 1;
      Bottom := Height - 1;
    end;

    Style.Alignment := taCenter;
    T := Start;
    while T <= ScaledSweep do
    begin
      I := Round(T / ScaledSweep * (Width - FRAME_MARGIN_RIGHT - FRAME_MARGIN_LEFT)) + FRAME_MARGIN_LEFT;
      Line(I, FRAME_MARGIN_TOP, I, Height - FRAME_MARGIN_BOTTOM);
      S := FloatToStr(T);
      E := TextExtent(S);
      TextOut(I - E.cx div 2, R.Top, S);
      T := T + Step;
    end;
  end;
end;

procedure TRadioOscilloscope.DrawFrameXY;
var
  Start, Step: Double;
  E: TSize;
  T: Double;
  I: Integer;
  Style: TTextStyle;
  R: TRect;
  S: string;
  TimeScale: Integer;
  ScaledSweep: Double;
  Range: Double;
  Mid: Integer;
  PR: Integer;
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

  with FGraphBox.Background.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBlack;
    FillRect(0, 0, Width, Height);
    Brush.Style := bsClear;

    Pen.Style := psSolid;
    Pen.Color := clWhite;
    Rectangle(FRAME_MARGIN_LEFT - 1, FRAME_MARGIN_TOP - 1, Width - FRAME_MARGIN_RIGHT + 1, Height - FRAME_MARGIN_BOTTOM + 1);

    Range := Abs(FYMax);
    if Range <= 0.0 then Exit;

    // y-axis
    PR := (Height - FRAME_MARGIN_BOTTOM - FRAME_MARGIN_TOP) div 2;
    Mid := FRAME_MARGIN_TOP + PR;
    Font.Color := clWhite;
    Font.Size  := 10;
    E := TextExtent('0');
    BeatifulTick(PR, 50, 0, Range, Start, Step);
    if Step <= 0.0 then Step := Range;

    with R do
    begin
      Left := 1;
      Right := Left + FRAME_MARGIN_LEFT - 2;
      Top := 0;
      Bottom := Top + E.cy;
    end;

    T := Start;
    Pen.Color := TColor($404040);
    while T <= Range do
    begin
      I := Round(T / Range * PR);
      Line(FRAME_MARGIN_LEFT, Mid + I, Width - FRAME_MARGIN_RIGHT, Mid + I);
      Line(FRAME_MARGIN_LEFT, Mid - I, Width - FRAME_MARGIN_RIGHT, Mid - I);
      R.Top := Mid + I - E.cy div 2;
      R.Top := Mid + I + E.cy div 2;
      TextRect(R, 0, 0, FloatToStr(T), Style);
      R.Top := Mid - I - E.cy div 2;
      R.Top := Mid - I + E.cy div 2;
      TextRect(R, 0, 0, FloatToStr(-T), Style);
      T := T + Step;
    end;

    // x-axis
    PR := (Width - FRAME_MARGIN_RIGHT - FRAME_MARGIN_LEFT) div 2;
    Mid := FRAME_MARGIN_LEFT + PR;
    with R do
    begin
      Left := Mid - (E.cx div 2);
      Right := Left + E.cx;
      Top := (Height - FRAME_MARGIN_BOTTOM - FRAME_MARGIN_TOP) div 2 - E.cy - 1;
      Bottom := Height - 1;
    end;

    T := Start;
    while T <= Range do
    begin
      I := Round(T / Range * PR);
      Line(Mid + I, FRAME_MARGIN_TOP, Mid + I, Height - FRAME_MARGIN_BOTTOM);
      Line(Mid - I, FRAME_MARGIN_TOP, Mid - I, Height - FRAME_MARGIN_BOTTOM);
      S := FloatToStr(T);
      E := TextExtent(S);
      TextOut(Mid + I - E.cx div 2, R.Top, S);
      S := FloatToStr(-T);
      E := TextExtent(S);
      TextOut(Mid - I - E.cx div 2, R.Top, S);
      T := T + Step;
    end;
  end;
end;

procedure TRadioOscilloscope.DrawData(const P: PComplex; const Len: Integer);
label
  PAINT;
var
  I: Integer;
  Dual: Boolean = False;
  MI, MA: Double;
  Y2Pixes: Double;
  YRange: Integer;
begin
  if FChMode = OSCILLOSCOPE_CHANNEL_XY then
  begin
    DrawDataXY(P, Len);
    Exit;
  end;

  SetLength(FUIData, FGraphBox.Background.Canvas.Width - FRAME_MARGIN_RIGHT - FRAME_MARGIN_LEFT);
  SetLength(FPts, Length(FUIData));
  if Length(FUIData) < 1 then Exit;

  case FChArith of
    OSCILLOSCOPE_ARITH_I_PLUS_Q:
      begin
        for I := 0 to Len - 1 do P[I].re := P[I].re + P[I].im;
      end;
    OSCILLOSCOPE_ARITH_I_SUBS_Q:
      begin
        for I := 0 to Len - 1 do P[I].re := P[I].re - P[I].im;
      end;
    OSCILLOSCOPE_ARITH_Q_SUBS_I:
      begin
        for I := 0 to Len - 1 do P[I].re := P[I].im - P[I].re;
      end;
    OSCILLOSCOPE_ARITH_I_MULT_Q:
      begin
        for I := 0 to Len - 1 do P[I].re := P[I].re * P[I].im;
      end;
    OSCILLOSCOPE_ARITH_I_DIV_Q:
      begin
        for I := 0 to Len - 1 do
          if P[I].im <> 0.0 then P[I].re :=  P[I].re / P[I].im;
      end;
    OSCILLOSCOPE_ARITH_Q_DIV_I:
      begin
        for I := 0 to Len - 1 do
          if P[I].re <> 0.0 then P[I].re :=  P[I].im / P[I].re;
      end;
    OSCILLOSCOPE_ARITH_ARG:
      begin
        for I := 0 to Len - 1 do P[I].re := arctan2(P[I].im, P[I].re);
      end;
  else
    case FChMode of
      OSCILLOSCOPE_CHANNEL_I: ;
      OSCILLOSCOPE_CHANNEL_Q: for I := 0 to Len - 1 do P[I].re := P[I].im;
      OSCILLOSCOPE_CHANNEL_DUAL: Dual := True;
    end;
  end;

  Xpolate(P, @FUIData[0], Len, Length(FUIData));

  if FAutoY then
  begin
    MI := FUIData[0].re;
    MA := FUIData[0].re;
    for I := 1 to High(FUIData) do
    begin
      MI := Min(MI, FUIData[I].re);
      MA := Max(MA, FUIData[I].re);
    end;
    if Dual then
    begin
      for I := 0 to High(FUIData) do
      begin
        MI := Min(MI, FUIData[I].re);
        MA := Max(MA, FUIData[I].re);
      end;
    end;
    if MA = MI then MA := MI + 0.1;
    FYMin := MI;
    FYMax := MA;
    DrawFrameNormal;
  end;

  if FYMax <= FYMin then goto PAINT;

  YRange := FGraphBox.DrawBuffer.Canvas.Width - FRAME_MARGIN_TOP - FRAME_MARGIN_BOTTOM;
  Y2Pixes := YRange / (FYMax - FYMin);
  FGraphBox.Bg2Draw;

  // finally, draw it
  for I := 0 to High(FUIData) do
  begin
    FPts[I].x := FRAME_MARGIN_LEFT + I;
    FPts[I].y := FRAME_MARGIN_TOP + YRange - Round((FUIData[I].re - FYMin) * Y2Pixes);
  end;
  with FGraphBox.DrawBuffer.Canvas do
  begin
    Pen.Width := 2;
    Pen.Style := psSolid;
    Pen.Color := THEME[0];
    Polyline(FPts);
  end;
  if not Dual then goto PAINT;
  for I := 0 to High(FUIData) do
    FPts[I].y := FRAME_MARGIN_TOP + YRange - Round((FUIData[I].im - FYMin) * Y2Pixes);
  with FGraphBox.DrawBuffer.Canvas do
  begin
    Pen.Color := THEME[1];
    Polyline(FPts);
  end;

PAINT:
  FGraphBox.Draw2Paint;
  FGraphBox.Paint;
end;

procedure TRadioOscilloscope.DrawDataXY(const P: PComplex; const Len: Integer);
begin

end;

procedure TRadioOscilloscope.RedrawFull;
begin
  FGraphBox.SetSize(FGraphBox.PaintBox.Width, FGraphBox.PaintBox.Height);
  DrawFrame;
  FGraphBox.Bg2Draw;
  FGraphBox.Draw2Paint;
end;

procedure TRadioOscilloscope.SyncRedrawFull;
begin
  TThread.Synchronize(nil, @RedrawFull);
end;

procedure TRadioOscilloscope.ProccessMessage(const Msg: TRadioMessage;
  var Ret: Integer);
begin
  case Msg.Id of
    RM_OSCILLOSCOPE_CFG:
      begin
        case Msg.ParamH of
          OSCILLOSCOPE_SET_CHANNEL:
            begin
              FChMode := Integer(Msg.ParamL);
              DrawFrame;
            end;
          OSCILLOSCOPE_ARITH_FOR_DUAL: FChArith := Integer(Msg.ParamL);
          OSCILLOSCOPE_SET_Y_MAX:
            begin
              FYMax    := IntAsFloat(Integer(Msg.ParamL));
              DrawFrame;
            end;
          OSCILLOSCOPE_SET_Y_MIN:
            begin
              FYMin    := IntAsFloat(Integer(Msg.ParamL));
              DrawFrame;
            end;
          OSCILLOSCOPE_SET_Y_AUTO:     FAutoY   := Msg.ParamL <> 0;
          OSCILLOSCOPE_SET_SWEEP_SPEED:
            begin
              FSweep   := IntAsFloat(Integer(Msg.ParamL));
              CfgRegulator;
              DrawFrame;
            end;
          OSCILLOSCOPE_SET_RUN_MODE:   FRunMode := Integer(Msg.ParamL);
          OSCILLOSCOPE_SET_DRAW_MIN_INTERVAL:  FMinInterval := Msg.ParamL / MSecsPerDay;
          OSCILLOSCOPE_SET_SAMPLE_GRID:
            begin
              FSampleGrid := Integer(Msg.ParamL);
              DrawFrame;
            end;
          OSCILLOSCOPE_GUI_RESET: SyncRedrawFull;
          OSCILLOSCOPE_SET_COUPLING: FCoupling := Integer(Msg.ParamL);
        end;
        ;
      end
    else
      inherited ProccessMessage(Msg, Ret);
  end;
end;

procedure TRadioOscilloscope.ReceiveWindowedData(const P: PComplex;
  const Len: Integer);
var
  T: TDateTime;
begin
  T := Now;
  if T - FLastFrameTime < FMinInterval then Exit;
  FLastFrameTime := T;
  if not FForm.Visible then Exit;

  DrawData(P, Len);
end;

procedure TRadioOscilloscope.DoShowGUI;
begin
  FForm.Show;
end;

constructor TRadioOscilloscope.Create(RunQueue: TRadioRunQueue);
begin
  inherited;
  FHasGUI := True;
  FHasConfig := False;
  FRegulator := TStreamRegulator.Create;
  FRegulator.OnRegulatedData := @ReceiveWindowedData;

  FMinInterval := 50 / MSecsPerDay;
  FAutoY := True;
  FSweep := 0.1;

  FGraphBox := TTripleBuffer.Create;
  FForm := TOscilloscopeForm.Create(Application);
  FGraphBox.PaintBox := FForm.PaintBox1;
  RedrawFull;
  FForm.PaintBox1.OnResize := @ImageResize;
  FForm.Module := Self;
end;

destructor TRadioOscilloscope.Destroy;
begin
  FGraphBox.Free;
  FForm.Free;
  inherited Destroy;
end;

procedure TRadioOscilloscope.ReceiveData(const P: PComplex; const Len: Integer);
begin
  inherited ReceiveData(P, Len);
end;

initialization

  RegisterModule(TRadioModuleClass(TRadioOscilloscope.ClassType));

end.

