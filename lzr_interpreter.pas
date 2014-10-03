unit lzr_interpreter;

{$mode objfpc}

interface

uses
  SysUtils, Classes, Math, LexLib, YaccLib, superobject;

type
  TSendMessage = function (const Name: ansistring; const V1, V2, V3: PtrUInt): Boolean of object;
  TConnectFeature = function (const Source, Target: ansistring): Boolean of object;
  TConnectData = function (const Source, Target: ansistring; const SourcePort, TargetPort: Integer): Boolean of object;
  TEmitMessage = procedure (const Line: ansistring) of  object;

var
    ObjTypes: ISuperObject = nil;
    SymTable: ISuperObject = nil;
    RegTable: ISuperObject = nil;
    OnCreateModules: TNotifyEvent = nil;
    OnSendMessage: TSendMessage = nil;
    OnConnectFeature: TConnectFeature = nil;
    OnConnectData: TConnectData = nil;
    OnWriteLn: TEmitMessage = nil;
    OnProjName: TEmitMessage = nil;

function Interpret(const Fn: string): Boolean;
procedure PredefineInt(const S: string; const V: Integer);

implementation

type
  TRegName = string;

var
    filename: string = '';
    RtOK: Boolean;
    RegIndex: Integer = 0;

{$I yacc/lazradio.pas}

end.
