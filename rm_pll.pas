unit rm_pll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioModule, RadioSystem, UComplex, Math, radiomessage;

type

  TPLLState = (psCapture, psLocked);

  { TPLLNode }

  TPLLNode = class(TDataFlowNode)
  private
    FState: TPLLState;
    FDefaultFrequency: Cardinal;
    FOutputVoltage: Boolean;
    FSampleRate: Cardinal;
    FTolerance: Double;
    FLastPhase: Double;
    function GetLocked: Boolean;
    procedure SetDefaultFrequency(AValue: Cardinal);
    procedure SetSampleRate(AValue: Cardinal);
    procedure Reset;
    procedure SetTolerance(AValue: Double);
  protected
    procedure DoReceiveData(const P: PComplex; const Len: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;

    property OutputVoltage: Boolean read FOutputVoltage write FOutputVoltage;
    property Locked: Boolean read GetLocked;

    property DefaultFrequency: Cardinal read FDefaultFrequency write SetDefaultFrequency;
    property SampleRate: Cardinal read FSampleRate write SetSampleRate;

    property Tolerance: Double read FTolerance write SetTolerance;
  end;

implementation

{ TPLLNode }

procedure TPLLNode.SetDefaultFrequency(AValue: Cardinal);
begin
  if FDefaultFrequency = AValue then Exit;
  FDefaultFrequency := AValue;
  Reset;
end;

function TPLLNode.GetLocked: Boolean;
begin
  Result := FState = psLocked;
end;

procedure TPLLNode.SetSampleRate(AValue: Cardinal);
begin
  if FSampleRate = AValue then Exit;
  FSampleRate := AValue;
  Reset;
end;

procedure TPLLNode.Reset;
begin
  FState := psCapture;
  FLastPhase := 0.0;

end;

procedure TPLLNode.SetTolerance(AValue: Double);
begin
  if FTolerance = AValue then Exit;
  FTolerance := Max(1e-4, AValue);
end;

procedure TPLLNode.DoReceiveData(const P: PComplex; const Len: Integer);
begin
  inherited DoReceiveData(P, Len);
end;

constructor TPLLNode.Create;
begin
  inherited;
  FTolerance := 1e-2;
  FSampleRate := 1024;
  FDefaultFrequency := 10;
end;

destructor TPLLNode.Destroy;
begin
  inherited Destroy;
end;

end.

