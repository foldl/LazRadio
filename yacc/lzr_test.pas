program lzr_test;

{$mode objfpc} 

uses 
  SysUtils, Classes, Math, LexLib, YaccLib, superobject;

type
  TSendMessage = function (const Name: string; const V1, V2, V3: PtrUInt): Boolean of object;
  TConnectFeature = function (const Source, Target: string): Boolean of object;
  TConnectData = function (const Source, Target: string; const SourcePort, TargetPort: Integer): Boolean of object;
  TEmitMessage = procedure (const Line: string) of  object;
  TRegName = string;

var 
    filename: string = '';
    ObjTypes: ISuperObject = nil;
    RtOK: Boolean;
    SymTable: ISuperObject = nil;
    RegTable: ISuperObject = nil;
    RegIndex: Integer = 0;
    OnCreateModules: TNotifyEvent = nil;
    OnSendMessage: TSendMessage = nil;
    OnConnectFeature: TConnectFeature = nil;
    OnConnectData: TConnectData = nil;
    OnWriteLn: TEmitMessage = nil;

{$I lazradio.pas}

begin
  Interpret(ParamStr(1));
end.
