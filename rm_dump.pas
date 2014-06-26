unit rm_dump;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex, RadioModule, RadioSystem;

type

  { TRadioDump }

  TRadioDump = class(TRadioModule)
  private
    FFile: TFileStream;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure ReceiveData(const P: PComplex; const Len: Integer); override;
  end;

implementation

uses
  KissFFT;

{ TRadioDump }

constructor TRadioDump.Create(RunQueue: TRadioRunQueue);
var
  I: Integer = 0;
begin
  inherited Create(RunQueue);
  while True do
  begin
    try
      FFile := TFileStream.Create('dump' + IntToStr(I), fmCreate);
      Break;
    except
    end;
  end;
end;

destructor TRadioDump.Destroy;
begin
  FFile.Free;
  FFile := nil;
  inherited Destroy;
end;

procedure TRadioDump.ReceiveData(const P: PComplex; const Len: Integer);
var
  S: string;
begin
  if Assigned(FFile) then
  begin
    S := KissFFT.ToString(P, Len);
    FFile.Write(S[1], Length(S));
    FFile.WriteByte(13); FFile.WriteByte(10);
  end;
end;

initialization

  RegisterModule('Dump', TRadioModuleClass(TRadioDump.ClassType));

end.

