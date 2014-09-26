unit lzr_interpreter;

{$mode objfpc}

interface

uses
  SysUtils, Classes, Math, LexLib, YaccLib, superobject;

type
  TSendMessage = function (const Name: string; const V1, V2, V3: PtrUInt): Boolean of object;
  TConnectFeature = function (const Source, Target: string): Boolean of object;
  TConnectData = function (const Source, Target: string; const SourcePort, TargetPort: Integer): Boolean of object;
  TEmitMessage = procedure (const Line: string) of  object;

var
    ObjTypes: ISuperObject = nil;
    SymTable: ISuperObject = nil;
    OnCreateModules: TNotifyEvent = nil;
    OnSendMessage: TSendMessage = nil;
    OnConnectFeature: TConnectFeature = nil;
    OnConnectData: TConnectData = nil;
    OnWriteLn: TEmitMessage = nil;

function Interpret(const Fn: string): Boolean;

implementation

type
  TRegName = string;

var
    filename: string = '';
    RtOK: Boolean;
    RegTable: ISuperObject = nil;
    RegIndex: Integer = 0;

{$I yacc/lazradio.pas}

end.
