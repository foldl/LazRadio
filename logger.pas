unit logger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioModule;

type

  { TTextLogger }

  TTextLogger = class(TRadioLogger)
  private
    FFile: TFileStream;
    constructor Create;
  protected
    procedure DoReport(const ALevel: TRadioLogLevel; const S: string); override;
  public
    destructor Destroy; override;
    class procedure Start;
  end;

implementation

var
  LogLevel2Str: array[TRadioLogLevel] of string = ('verbose', 'info', 'warn', 'error');

{ TTextLogger }

constructor TTextLogger.Create;
var
  P: string;
begin
  while True do
  begin
    try
      FFile := TFileStream.Create('log' + P + '.txt', fmCreate);
      P := FormatDateTime('-yyyy-mm-dd-hh:nn:ss.z', Now);
      Break;
    except
    end;
  end;
end;

procedure TTextLogger.DoReport(const ALevel: TRadioLogLevel; const S: string);
var
  T: string;
begin
  T := FormatDateTime('hh:nn:ss.z ', Now) + LogLevel2Str[ALevel] + ' ' + S + #13#10;
  FFile.Write(T[1], Length(T));
end;

destructor TTextLogger.Destroy;
begin
  FFile.Free;
  inherited Destroy;
end;

class procedure TTextLogger.Start;
begin
  FreeAndNil(FInstance);
  FInstance := TTextLogger.Create;
end;

end.

