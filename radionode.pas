unit RadioNode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex, Math, RadioModule, GenFFT;

type

    { TDataFlowNode }

  TDataFlowNode = class
  private
    FNext: TDataFlowNode;
    FOnSendToNext: TReceiveData;
    FCache: array of Complex;
    FHoldCount: Integer;
  protected
    procedure DoReceiveData(const P: PComplex; const Len: Integer); virtual;
  public
    destructor Destroy; override;

    procedure SendToNext(const P: PComplex; const Len: Integer);
    procedure ReceiveData(const P: PComplex; const Len: Integer);

    procedure Hold;
    procedure ReleaseHold;

    function  Connect(ANext: TDataFlowNode): TDataFlowNode;
    function  LastNode: TDataFlowNode;

    property Next: TDataFlowNode read FNext;
    property OnSendToNext: TReceiveData read FOnSendToNext write FOnSendToNext;
  end;

  { TRegulatorNode }

  TRegulatorNode = class(TDataFlowNode)
  private
    FRegulator: TStreamRegulator;
  protected
    procedure DoReceiveData(const P: PComplex; const Len: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;
    property Regulator: TStreamRegulator read FRegulator;
  end;

  { TWindowNode }

  TWindowNode = class(TDataFlowNode)
  private
    FWnd: array of Double;
    FRegulator: TStreamRegulator;
    function GetOverlap: Integer;
    function GetWindowLen: Integer;
    procedure RegulatedData(const P: PComplex; const Len: Integer);
    procedure SetOverlap(AValue: Integer);
  protected
    procedure DoReceiveData(const P: PComplex; const Len: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetWindow(const P: PDouble; const Len: Integer);

    property WindowLen: Integer read GetWindowLen;
    property Overlap: Integer read GetOverlap write SetOverlap;
  end;

  { TFIRNode }

  TFIRNode = class(TDataFlowNode)
  private
    FHFIR: array of Complex;
    FBuf: array of Complex;
    FRes: array of Complex;
    FFPlan: PFFTPlan;
    FIPlan: PFFTPlan;
    FRegulator: TStreamRegulator;
    FTaps: Integer;
    function GetProcessingSize: Integer;
    procedure SetTimeDomainFIR(const P: PComplex; const Len: Integer); virtual;
    procedure RegulatedData(const P: PComplex; const Len: Integer); virtual;
  protected
    procedure DoReceiveData(const P: PComplex; const Len: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetFIR(const P: PComplex; const Len: Integer; const FreqDomain: Boolean = False); overload;
    procedure SetFIR(const P: PDouble; const Len: Integer);

    property ProcessingSize: Integer read GetProcessingSize;
  end;

  { TRealFIRNode }

  TRealFIRNode = class(TFIRNode)
  private
    FMono: Boolean;
    procedure RegulatedData(const P: PComplex; const Len: Integer); override;
    procedure SetMono(AValue: Boolean);
  public
    property Mono: Boolean read FMono write SetMono;
  end;

  { TResampleNode }

  TResampleNode = class(TDataFlowNode)
  private
    FBuf: array [0..2048 - 1] of Complex;
    FCursor: Integer;
    FLastInput: Complex;
    FLastScaledIndex: Double;
    FInputRate: Cardinal;
    FOutputRate: Cardinal;
    procedure SetInputRate(AValue: Cardinal);
    procedure SetOutputRate(AValue: Cardinal);
  protected
    procedure DoReceiveData(const P: PComplex; const Len: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;

    property InputRate: Cardinal read FInputRate write SetInputRate ;
    property OutputRate: Cardinal read FOutputRate write SetOutputRate;
  end;

implementation

uses
  SignalBasic;
{ TRealFIRNode }

procedure TRealFIRNode.RegulatedData(const P: PComplex; const Len: Integer);
var
  I: Integer;
begin
  if Assigned(FFPlan) then
  begin
    if Len <> High(FBuf) + 1 then
    begin
      TRadioLogger.Report(llWarn, 'TRealFIRNode.RegulatedData: Len <> High(FBuf) + 1');
      Exit;
    end;
  end
  else begin
    TRadioLogger.Report(llWarn, 'TRealFIRNode.RegulatedData: FPlan = nil');
    Exit;
  end;

  FillChar(FRes[0], Len * SizeOf(FRes[0]), 0);
  FillChar(FBuf[0], Len * SizeOf(FBuf[0]), 0);
  for I := 0 to Len - 1 do
    FBuf[I].re := P[I].re;
  Transform(FFPlan, @FBuf[0], @FBuf[0]);
  for I := 0 to Len - 1 do
    FBuf[I] := FBuf[I] * FHFIR[I];
  Transform(FIPlan, @FBuf[0], @FBuf[0]);
  for I := 0 to Len - 1 do
    FRes[I].re := FBuf[I].re;

  if not FMono then
  begin
    FillChar(FBuf[0], Len * SizeOf(FBuf[0]), 0);
    for I := 0 to Len - 1 do
      FBuf[I].re := P[I].im;
    Transform(FFPlan, @FBuf[0], @FBuf[0]);
    for I := 0 to Len - 1 do
      FBuf[I] := FBuf[I] * FHFIR[I];
    Transform(FIPlan, @FBuf[0], @FBuf[0]);
    for I := 0 to Len - 1 do
      FRes[I].im := FBuf[I].re;
  end;
  I := FTaps - 1;
  SendToNext(@FRes[I], Len - I);
end;

procedure TRealFIRNode.SetMono(AValue: Boolean);
begin
  if FMono = AValue then Exit;
  FMono := AValue;
end;

{ TResampleNode }

procedure TResampleNode.SetInputRate(AValue: Cardinal);
begin
  if FInputRate = AValue then Exit;
  FInputRate := Max(1, AValue);
  FLastScaledIndex := 0;
end;

procedure TResampleNode.SetOutputRate(AValue: Cardinal);
begin
  if FOutputRate = AValue then Exit;
  FOutputRate := Max(1, AValue);
  FLastScaledIndex := 0;
end;

procedure TResampleNode.DoReceiveData(const P: PComplex; const Len: Integer);
label
  again;
var
  I: Integer = 0;
  K: Double;
  V: Double;
begin
  if FInputRate = FOutputRate then
  begin
    if FCursor > 0 then
    begin
      SendToNext(@FBuf[0], FCursor);
      FCursor := 0;
    end;

    SendToNext(P, Len);
    Exit;
  end;

  V := FLastScaledIndex;
  K := FInputRate / FOutputRate;
again:
  while FCursor <= High(FBuf) do
  begin
    V := V + K;
    I := Trunc(V);
    if I >= Len - 1 then
    begin
      FLastScaledIndex := V - Len;
      FLastInput := P[Len - 1];
      Exit;
    end;
    if I >= 0 then
      FBuf[FCursor] := P[I] * (V - I) + P[I + 1] * (1 + I - V)
    else
      FBuf[FCursor] := FLastInput * (V + 1) + P[0] * (- V);
    Inc(FCursor);
  end;

  if FCursor = High(FBuf) + 1 then
  begin
    SendToNext(@FBuf[0], High(FBuf) + 1);
    FCursor := 0;
    goto again;
  end;
end;

constructor TResampleNode.Create;
begin
  FInputRate := 1;
  FOutputRate := 1;
end;

destructor TResampleNode.Destroy;
begin
  inherited Destroy;
end;

{ TWindowNode }

procedure TWindowNode.RegulatedData(const P: PComplex; const Len: Integer);
var
  I: Integer;
begin
  if Len = High(FWnd) + 1 then
    for I := 0 to Len - 1 do P[I] := P[I] * FWnd[I];
  SendToNext(P, Len);
end;

function TWindowNode.GetOverlap: Integer;
begin
  Result := FRegulator.Overlap;
end;

function TWindowNode.GetWindowLen: Integer;
begin
  Result := High(FWnd) + 1;
end;

procedure TWindowNode.SetOverlap(AValue: Integer);
begin
  FRegulator.Overlap := AValue;
end;

constructor TWindowNode.Create;
begin
  inherited;
  FRegulator := TStreamRegulator.Create;
  FRegulator.OnRegulatedData := @RegulatedData;
end;

destructor TWindowNode.Destroy;
begin
  FRegulator.Free;
  inherited Destroy;
end;

procedure TWindowNode.SetWindow(const P: PDouble; const Len: Integer);
begin
  SetLength(FWnd, Len);
  Move(P^, FWnd[0], Len * SizeOf(P^));
  FRegulator.Size := Len;
end;

procedure TWindowNode.DoReceiveData(const P: PComplex; const Len: Integer);
begin
  FRegulator.ReceiveData(P, Len);
end;

{ TFIRNode }

procedure TFIRNode.SetTimeDomainFIR(const P: PComplex; const Len: Integer);
var
  T: array of Complex;
  I: Integer;
begin
  if Assigned(FFPlan) then
  begin
    FinalizePlan(FFPlan);
    FinalizePlan(FIPlan);
  end;
  SetLength(T, Len);
  ModArg(P, @T[0], Len);
  I := NextFastSize(Max(20480, 2 * Len - 1));
  FRegulator.Size := I;
  SetLength(FHFIR, I);
  SetLength(FBuf, I);
  SetLength(FRes, I);
  FillChar(FHFIR[0], I * SizeOf(FHFIR[0]), 0);
  FillChar(FBuf[0], I * SizeOf(FHFIR[0]), 0);
  Move(P^, FBuf[0], Len * SizeOf(FHFIR[0]));
  FFPlan := BuildFFTPlan(I, False);
  Transform(FFPlan, @FBuf[0], @FHFIR[0]);
  FIPlan := BuildFFTPlan(I, True);
  FRegulator.Overlap := Len - 1;
  FTaps := Len;
end;

function TFIRNode.GetProcessingSize: Integer;
begin
  Result := Length(FBuf);
end;

procedure TFIRNode.RegulatedData(const P: PComplex; const Len: Integer);
var
  I: Integer;
begin
  if Assigned(FFPlan) then
  begin
    if Len <> High(FBuf) + 1 then
    begin
      TRadioLogger.Report(llWarn, 'TFIRNode.RegulatedData: Len <> High(FBuf) + 1');
      Exit;
    end;

    Transform(FFPlan, P, @FBuf[0]);
    for I := 0 to Len - 1 do
      FBuf[I] := FBuf[I] * FHFIR[I];
    Transform(FIPlan, @FBuf[0], @FRes[0]);
    I := FTaps - 1;
    SendToNext(@FRes[I], Len - I);
  end
  else
    TRadioLogger.Report(llWarn, 'TFIRNode.RegulatedData: FPlan = nil');
end;

constructor TFIRNode.Create;
begin
  inherited Create;
  FRegulator := TStreamRegulator.Create;
  FRegulator.OnRegulatedData := @RegulatedData;
end;

destructor TFIRNode.Destroy;
begin
  FRegulator.Free;
  inherited Destroy;
end;

procedure TFIRNode.SetFIR(const P: PComplex; const Len: Integer;
  const FreqDomain: Boolean);
var
  T: array of Complex;
  X: PFFTPlan;
begin
  if not FreqDomain then
    SetTimeDomainFIR(P, Len)
  else begin
    SetLength(T, Len);
    X := BuildFFTPlan(Len, True);
    Transform(X, P, @T[0]);
    FinalizePlan(X);
    SetTimeDomainFIR(@T[0], Len);
  end;
end;

procedure TFIRNode.SetFIR(const P: PDouble; const Len: Integer);
var
  X: array of Complex;
  I: Integer;
begin
  SetLength(X, Len);
  for I := 0 to Len - 1 do
  begin
    X[I].re := P[I];
    X[I].im := 0;
  end;
  SetTimeDomainFIR(PComplex(@X[0]), Len);
end;

procedure TFIRNode.DoReceiveData(const P: PComplex; const Len: Integer);
begin
  FRegulator.ReceiveData(P, Len);
end;

{ TRegulatorNode }

procedure TRegulatorNode.DoReceiveData(const P: PComplex; const Len: Integer);
begin
  FRegulator.ReceiveData(P, Len);
end;

constructor TRegulatorNode.Create;
begin
  inherited Create;
  FRegulator := TStreamRegulator.Create;
  FRegulator.OnRegulatedData := @SendToNext;
end;

destructor TRegulatorNode.Destroy;
begin
  FRegulator.Free;
  inherited Destroy;
end;

{ TDataFlowNode }

procedure TDataFlowNode.DoReceiveData(const P: PComplex; const Len: Integer);
begin

end;

destructor TDataFlowNode.Destroy;
begin
  if Assigned(FNext) then FNext.Free;
  inherited Destroy;
end;

procedure TDataFlowNode.SendToNext(const P: PComplex; const Len: Integer);
begin
  if Assigned(FOnSendToNext) then
    FOnSendToNext(P, Len)
  else if Assigned(FNext) then
    FNext.ReceiveData(P, Len);
end;

procedure TDataFlowNode.ReceiveData(const P: PComplex; const Len: Integer);
var
  I: Integer;
begin
  if FHoldCount <= 0 then
    DoReceiveData(P, Len)
  else begin
    I := High(FCache) + 1;
    SetLength(FCache, High(FCache) + Len + 1);
    Move(P^, FCache[I], Len * SizeOf(P^));
  end;
end;

procedure TDataFlowNode.Hold;
begin
  Inc(FHoldCount);
end;

procedure TDataFlowNode.ReleaseHold;
begin
  Dec(FHoldCount);
  if FHoldCount = 0 then
  begin
    if High(FCache) >= 0 then
    begin
      DoReceiveData(@FCache[0], High(FCache) + 1);
      SetLength(FCache, 0);
    end;
  end;
end;

function TDataFlowNode.Connect(ANext: TDataFlowNode): TDataFlowNode;
begin
  FNext := ANext;
  Result := ANext;
end;

function TDataFlowNode.LastNode: TDataFlowNode;
begin
  Result := Self;
  while Assigned(Result.FNext) do Result := Result.FNext;
end;

end.

