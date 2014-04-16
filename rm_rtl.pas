unit rm_rtl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioModule, RtlSdr;

const
  // ParamH: PRtlSdrDev
  RM_RTL_SET_DEV = RM_USER + 1;

type

  { TRtlModule }

  TRtlModule = class(TBackgroundRadioModule)
  private
    FDev: PRtlSdrDev;
    FDevRunning: Boolean;
    function  DevOK: Boolean;
  public
    constructor Create(RunQueue: TRadioRunQueue); override;
    destructor Destroy; override;

    procedure Configure; override;
    property DevRunning: Boolean read FDevRunning;
  end;

implementation

uses
  FormRtl, RadioSystem;

{ TRtlModule }

function TRtlModule.DevOK: Boolean;
begin
  Result := Assigned(FDev);
end;

constructor TRtlModule.Create(RunQueue: TRadioRunQueue);
begin
  inherited;
end;

destructor TRtlModule.Destroy;
begin
  RtlSdrClose(FDev);
  inherited Destroy;
end;

procedure TRtlModule.Configure;
begin
  RTLForm.Show;
  RTLForm.RtlMod := Self;
end;

initialization

  RegisterModule('RtlModule', TRadioModuleClass(TRtlModule.ClassType));

end.

