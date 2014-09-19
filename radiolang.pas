unit RadioLang;

{$mode objfpc}{$H+}

{
a LazRadio project is defined by a .lar file.

lazradio SYS-NAME;

var
  VAR-NAME: ModuleName;
  ...

begin

  // connect feature
  VAR-NAME :> VAR-NAME [ :> VAR-NAME];

  // connect data port 0
  VAR-NAME -> VAR-NAME [ -> VAR-NAME];

  // connect data port, soure port i, target port j
  VAR-NAME[I] -> VAR-NAME[J];

  // connect feature and data port 0
  VAR-NAME => VAR-NAME [ => VAR-NAME];

  // post a message
  VAR-NAME ! {M-ID, PRAMH, PRAML};
end.
}

interface

uses
  Classes, SysUtils, superobject, RadioModule;

type

  { TRadioLangRT }

  TRadioLangRT = class
  private
    FTable: ISuperObject;
    procedure LoadMsgConsts;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    function DefVar(Identifier: string; const T: string): Boolean;

    function Exec(const Fn: string): Boolean;
  end;

implementation

uses
  util_config;



{ TRadioLangRT }

procedure TRadioLangRT.LoadMsgConsts;
var
  O: ISuperObject;
  procedure LoadConsts(Obj: ISuperObject);
  var
    I: Integer;
    A: TSuperArray;
    N: string;
    V: Integer;
  begin
    if not Assigned(Obj) then Exit;
    if Obj.IsType(stObject) then
    begin
      N := Obj.S['name'];
      TRadioLogger.Report(llError, N);
      if N <> '' then
        FTable.I[N] := Obj.I['id'];
      LoadConsts(Obj.O['paramh.vals']);
      LoadConsts(Obj.O['paraml.vals']);
    end;
    if not Obj.IsType(stArray) then Exit;
    A := Obj.AsArray;
    for I := 0 to A.Length - 1 do
    begin
      LoadConsts(A.O[I]);
    end;
  end;
begin
  O := TSuperObject.ParseFile(GetResFullName('messages.json'), False);
  LoadConsts(O);
end;

constructor TRadioLangRT.Create;
begin
  FTable := TSuperObject.Create(stObject);
  Reset;
end;

destructor TRadioLangRT.Destroy;
begin
  inherited Destroy;
end;

procedure TRadioLangRT.Reset;
begin
  FTable.Clear(True);
  LoadMsgConsts;
end;

function TRadioLangRT.DefVar(Identifier: string; const T: string): Boolean;
begin

end;

function TRadioLangRT.Exec(const Fn: string): Boolean;
begin

end;

end.

