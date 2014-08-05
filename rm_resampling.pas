unit rm_resampling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex, RadioModule, RadioSystem, Math;

type

  TResamplingMode = (rmDecimation, rmInterpolation, rmResampling);

  { TRadioResampling }

  TRadioResampling = class(TRadioModule)
  private
    FRateIn: Integer;
    FRateOut: Integer;
    FH: array of Double;
    FZ: array of Double;
    FPhase: Integer;
  private
    procedure Reconfig;
  protected
    function RMSetSampleRate(const Msg: TRadioMessage; const Rate: Cardinal): Integer; override;
    procedure ProccessMessage(const Msg: TRadioMessage; var Ret: Integer); override;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

implementation

{ TRadioResampling }

procedure TRadioResampling.Reconfig;
begin

end;

function TRadioResampling.RMSetSampleRate(const Msg: TRadioMessage;
  const Rate: Cardinal): Integer;
begin
  Result := inherited RMSetSampleRate(Msg, Rate);
end;

procedure TRadioResampling.ProccessMessage(const Msg: TRadioMessage;
  var Ret: Integer);
begin
  inherited ProccessMessage(Msg, Ret);
end;

constructor TRadioResampling.Create(RunQueue: TRadioRunQueue);
begin
  inherited Create(RunQueue);
end;

destructor TRadioResampling.Destroy;
begin
  inherited Destroy;
end;

procedure TRadioResampling.ReceiveData(const P: PComplex; const Len: Integer);
begin
  inherited ReceiveData(P, Len);
end;

end.

