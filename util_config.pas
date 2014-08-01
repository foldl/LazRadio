unit util_config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

function GetResFullName(const F: string): string;

function GetResStream(const F: string): TFileStream;

implementation

function GetResFullName(const F: string): string;
begin
  Result := ConcatPaths([ExtractFileDir(Application.ExeName), 'res', F]);
end;

function GetResStream(const F: string): TFileStream;
begin
  Result := nil;
  try
    Result := TFileStream.Create(GetResFullName(F), fmOpenRead);
  except
    Result := nil;
  end;
end;

end.

