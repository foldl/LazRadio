
(* Yacc parser template (TP Yacc V3.0), V1.2 6-17-91 AG *)

(* global definitions: *)

{$mode objfpc} 
uses 
  SysUtils, Classes, Math, LexLib, YaccLib, superobject;

type
  TCreateModules = function (ATable: ISuperObject): Boolean of object;
  TSendMessage = function (const Name: string; const V1, V2, V3: PtrUInt): Boolean of object;
  TConnectFeature = function (const Source, Target: string): Boolean of object;
  TConnectData = function (const Source, Target: string; const SourcePort, TargetPort: Integer): Boolean of object;
  
  TRegName = string;

var 
    filename : String;
    ObjTypes: ISuperObject = nil;
    yywrap: Boolean = True;
    RtOK: Boolean;
    SymTable: ISuperObject = nil;
    RegTable: ISuperObject = nil;
    RegIndex: Integer = 0;
    OnCreateModules: TCreateModules = nil;
    OnSendMessage: TSendMessage = nil;
    OnConnectFeature: TConnectFeature = nil;
    OnConnectData: TConnectData = nil;
    TempS: string;

procedure yyerror(msg : string);
begin
  writeln(filename, ': ', yylineno, ': ', msg, ' at or before `', yytext, '`.');
  yyabort;
end;

function IsDefined(const S: string): Boolean;
begin
  IsDefined := Assigned(SymTable.O[UpperCase(S)]);
end;

function RegAlloc(const T: string): TRegName;
begin
  RegAlloc := IntToStr(RegIndex);
  RegTable.O[RegAlloc] := SO(Format('{type: "%s"}', [T]));
  Inc(RegIndex);
end;

function RegWrite(const R: TRegName; const V: Integer): Boolean; overload;
begin
  RegWrite := RegTable.S[R + '.type'] = 'int';
  if RegWrite then 
    RegTable.I[R + '.value'] := V;
end;

function RegWrite(const R: TRegName; const V: Boolean): Boolean; 
begin
  RegWrite := RegTable.S[R + '.type'] = 'boolean';
  if RegWrite then 
    RegTable.B[R + '.value'] := V;
end;

function RegWrite(const R: TRegName; const V: Real): Boolean; 
begin
  RegWrite := RegTable.S[R + '.type'] = 'real';
  if RegWrite then 
    RegTable.D[R + '.value'] := V;
end;

function RegWrite(const R: TRegName; const V: string): Boolean;
begin
  RegWrite := RegTable.S[R + '.type'] = 'string';
  if RegWrite then 
    RegTable.S[R + '.value'] := V;
end;

function RegWriteSym(const R: TRegName; const V: string): Boolean;
begin
  RegWriteSym := RegTable.S[R + '.type'] = 'symbol';
  if RegWriteSym then 
    RegTable.S[R + '.name'] := V;
end;

procedure PredefineInt(const S: string; const V: Integer);
var
  T: string;
begin
  if IsDefined(S) then 
  begin
    yyerror(Format('"%s" already defined', [S]));
    Exit;
  end;

  T := RegAlloc('int');
  RegWrite(T, V);
  SymTable.O[UpperCase(S)] := SO(Format('{type: "int", disp: "%s", reg: "%s"}', [S, T]));
  writeln(SymTable.asjson(true));
end;

// for built-in types
function DefVars(const S: string; const T: string): Boolean;
var
  L: TStringList;
  V: string;
begin
  writeln('defvars ', S, ' ', T);
  L := TStringList.Create;
  L.Delimiter := ' ';
  L.StrictDelimiter := True;
  L.DelimitedText := S;
  DefVars := True;
  for V in L do
  begin
    if IsDefined(V) then 
    begin
      DefVars := False;
      yyerror(Format('"%s" is already defined.', [V]));
      Break;
    end;
    SymTable.O[UpperCase(V)] := SO(Format('{type: "%s", disp: "%s", reg: "%s"}', [T, V, RegAlloc(T)]));
  end;
  L.Free;
end;

function DefObjs(const S: string; const T: string): Boolean;
var
  L: TStringList;
  V: string;
  R: string;
begin
  writeln('DefObjs ', S, ' ', T);
  L := TStringList.Create;
  L.Delimiter := ' ';
  L.StrictDelimiter := True;
  L.DelimitedText := S;
  DefObjs := True;
  for V in L do
  begin
    if IsDefined(V) then 
    begin
      DefObjs := False;  
      yyerror(Format('"%s" is already defined.', [V]));
      Break;
    end;  
    R := UpperCase(T);
    if not Assigned(ObjTypes.O[R]) then 
    begin
      DefObjs := False;
      yyerror(Format('type "%s" is unknown.', [T]));
      Break;
    end; 
    SymTable.O[UpperCase(V)] := SO(Format('{type: "%s", disp: "%s"}', [R, V]));
  end;
  L.Free;
end;

function RegPlus(const R1, R2: TRegName; var bOK: Boolean): string;
begin
  case RegTable.S[R1 + '.type'] of
    'string':
      begin
        bOK := RegTable.S[R2 + '.type'] = 'string';
        if bOK then
        begin
          RegPlus := RegAlloc('string');
          RegTable.S[RegPlus + '.value'] := RegTable.S[R1 + '.value'] + RegTable.S[R2 + '.value'];
        end
        else
          yyerror('type error: string type required');
      end;
    'int':
      begin
        bOK := True;
        if RegTable.S[R2 + '.type'] = 'int' then
        begin
          RegPlus := RegAlloc('int');
          RegTable.I[RegPlus + '.value'] := RegTable.I[R1 + '.value'] + RegTable.I[R2 + '.value'];
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegPlus := RegAlloc('real');
          RegTable.D[RegPlus + '.value'] := RegTable.I[R1 + '.value'] + RegTable.D[R2 + '.value'];
        end
        else begin
          bOK := False;
          yyerror('type error: real or integer type required');
        end;
      end;
    'real':
      begin
        bOK := True;
        if RegTable.S[R2 + '.type'] = 'int' then
        begin
          RegPlus := RegAlloc('real');
          RegTable.D[RegPlus + '.value'] := RegTable.D[R1 + '.value'] + RegTable.I[R2 + '.value'];
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegPlus := RegAlloc('real');
          RegTable.D[RegPlus + '.value'] := RegTable.D[R1 + '.value'] + RegTable.D[R2 + '.value'];
        end
        else begin
          bOK := False;
          yyerror('type error: real or integer type required');
        end;
      end;
  else
    bOK := False;
    yyerror('type for + error');
  end;
end;

function RegMinus(const R1, R2: TRegName; var bOK: Boolean): string;
begin
  case RegTable.S[R1 + '.type'] of
    'int':
      begin
        bOK := True;
        if RegTable.S[R2 + '.type'] = 'int' then
        begin
          RegMinus := RegAlloc('int');
          RegTable.I[RegMinus + '.value'] := RegTable.I[R1 + '.value'] - RegTable.I[R2 + '.value'];
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegMinus := RegAlloc('real');
          RegTable.D[RegMinus + '.value'] := RegTable.I[R1 + '.value'] - RegTable.D[R2 + '.value'];
        end
        else begin
          bOK := False;
          yyerror('type error: real or integer type required');
        end;
      end;
    'real':
      begin
        bOK := True;
        if RegTable.S[R2 + '.type'] = 'int' then
        begin
          RegMinus := RegAlloc('real');
          RegTable.D[RegMinus + '.value'] := RegTable.D[R1 + '.value'] - RegTable.I[R2 + '.value'];
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegMinus := RegAlloc('real');
          RegTable.D[RegMinus + '.value'] := RegTable.D[R1 + '.value'] - RegTable.D[R2 + '.value'];
        end
        else begin
          bOK := False;
          yyerror('type error: real or integer type required');
        end;
      end;
  else
    bOK := False;
    yyerror('type for - error');
  end;
end;

function RegTimes(const R1, R2: TRegName; var bOK: Boolean): string;
begin
  case RegTable.S[R1 + '.type'] of
    'int':
      begin
        bOK := True;
        if RegTable.S[R2 + '.type'] = 'int' then
        begin
          RegTimes := RegAlloc('int');
          RegTable.I[RegTimes + '.value'] := RegTable.I[R1 + '.value'] * RegTable.I[R2 + '.value'];
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegTimes := RegAlloc('real');
          RegTable.D[RegTimes + '.value'] := RegTable.I[R1 + '.value'] * RegTable.D[R2 + '.value'];
        end
        else begin
          bOK := False;
          yyerror('type error: real or integer type required');
        end;
      end;
    'real':
      begin
        bOK := True;
        if RegTable.S[R2 + '.type'] = 'int' then
        begin
          RegTimes := RegAlloc('real');
          RegTable.D[RegTimes + '.value'] := RegTable.D[R1 + '.value'] * RegTable.I[R2 + '.value'];
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegTimes := RegAlloc('real');
          RegTable.D[RegTimes + '.value'] := RegTable.D[R1 + '.value'] * RegTable.D[R2 + '.value'];
        end
        else begin
          bOK := False;
          yyerror('type error: real or integer type required');
        end;
      end;
  else
    bOK := False;
    yyerror('type for * error');
  end;
end;

function RegDivide(const R1, R2: TRegName; var bOK: Boolean): string;
begin
  try
  case RegTable.S[R1 + '.type'] of
    'int':
      begin
        bOK := True;
        if RegTable.S[R2 + '.type'] = 'int' then
        begin
          RegDivide := RegAlloc('real');
          RegTable.D[RegDivide + '.value'] := RegTable.I[R1 + '.value'] / RegTable.I[R2 + '.value'];
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegDivide := RegAlloc('real');
          RegTable.D[RegDivide + '.value'] := RegTable.I[R1 + '.value'] / RegTable.D[R2 + '.value'];
        end
        else begin
          bOK := False;
          yyerror('type error: real or integer type required');
        end;
      end;
    'real':
      begin
        bOK := True;
        if RegTable.S[R2 + '.type'] = 'int' then
        begin
          RegDivide := RegAlloc('real');
          RegTable.D[RegDivide + '.value'] := RegTable.D[R1 + '.value'] / RegTable.I[R2 + '.value'];
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegDivide := RegAlloc('real');
          RegTable.D[RegDivide + '.value'] := RegTable.D[R1 + '.value'] / RegTable.D[R2 + '.value'];
        end
        else begin
          bOK := False;
          yyerror('type error: real or integer type required');
        end;
      end;
  else
    bOK := False;
    yyerror('type for / error');
  end;
  except
    bOK := False;
    yyerror('runtime error');
  end;
end;

function RegPower(const R1, R2: TRegName; var bOK: Boolean): string;
begin
  case RegTable.S[R1 + '.type'] of
    'int':
      begin
        bOK := True;
        if RegTable.S[R2 + '.type'] = 'int' then
        begin
          RegPower := RegAlloc('int');
          RegTable.I[RegPower + '.value'] := Round(Power(RegTable.I[R1 + '.value'], RegTable.I[R2 + '.value']));
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegPower := RegAlloc('real');
          RegTable.D[RegPower + '.value'] := Power(RegTable.I[R1 + '.value'], RegTable.D[R2 + '.value']);
        end
        else begin
          bOK := False;
          yyerror('type error: real or integer type required');
        end;
      end;
    'real':
      begin
        bOK := True;
        if RegTable.S[R2 + '.type'] = 'int' then
        begin
          RegPower := RegAlloc('real');
          RegTable.D[RegPower + '.value'] := Power(RegTable.D[R1 + '.value'], RegTable.I[R2 + '.value']);
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegPower := RegAlloc('real');
          RegTable.D[RegPower + '.value'] := Power(RegTable.D[R1 + '.value'], RegTable.D[R2 + '.value']);
        end
        else begin
          bOK := False;
          yyerror('type error: real or integer type required');
        end;
      end;
  else
    bOK := False;
    yyerror('type for ** error');
  end;
end;

function RegMod(const R1, R2: TRegName; var bOK: Boolean): string;
begin
  bOK := (RegTable.S[R1 + '.type']  = 'int') and (RegTable.S[R2 + '.type'] = 'int');
  bOK := bOK and (RegTable.I[R2 + '.value'] <> 0);
  if bOK then
  begin
    RegMod := RegAlloc('int');
    RegTable.I[RegMod + '.value'] := RegTable.I[R1 + '.value'] mod RegTable.I[R2 + '.value'];
  end
  else
    yyerror('type for mod error');
end;

function RegDiv(const R1, R2: TRegName; out bOK: Boolean): string;
begin
  bOK := (RegTable.S[R1 + '.type']  = 'int') and (RegTable.S[R2 + '.type'] = 'int');
  bOK := bOK and (RegTable.I[R2 + '.value'] <> 0);
  if bOK then
  begin
    RegDiv := RegAlloc('int');
    RegTable.I[RegDiv + '.value'] := RegTable.I[R1 + '.value'] div RegTable.I[R2 + '.value'];
  end
  else
    yyerror('type for mod error');
end; 

function RegAnd(const R1, R2: TRegName; out bOK: Boolean): string;
begin
  bOK := True;
  if (RegTable.S[R1 + '.type']  = 'int') and (RegTable.S[R2 + '.type'] = 'int') then
  begin
    RegAnd := RegAlloc('int');
    RegTable.I[RegAnd + '.value'] := RegTable.I[R1 + '.value'] and RegTable.I[R2 + '.value'];
  end
  else if (RegTable.S[R1 + '.type']  = 'boolean') and (RegTable.S[R2 + '.type'] = 'boolean') then
  begin
    RegAnd := RegAlloc('boolean');
    RegTable.B[RegAnd + '.value'] := RegTable.B[R1 + '.value'] and RegTable.B[R2 + '.value'];
  end
  else begin
    bOK := False;
    yyerror('type for AND error');
  end;
end;

function RegOr(const R1, R2: TRegName; var bOK: Boolean): string;
begin
  bOK := True;
  if (RegTable.S[R1 + '.type']  = 'int') and (RegTable.S[R2 + '.type'] = 'int') then
  begin
    RegOr := RegAlloc('int');
    RegTable.I[RegOr + '.value'] := RegTable.I[R1 + '.value'] or RegTable.I[R2 + '.value'];
  end
  else if (RegTable.S[R1 + '.type']  = 'boolean') and (RegTable.S[R2 + '.type'] = 'boolean') then
  begin
    RegOr := RegAlloc('boolean');
    RegTable.B[RegOr + '.value'] := RegTable.B[R1 + '.value'] or RegTable.B[R2 + '.value'];
  end
  else begin
    bOK := False;
    yyerror('type for OR error');
  end;
end;

function RegXor(const R1, R2: TRegName; var bOK: Boolean): string;
begin
  bOK := True;
  if (RegTable.S[R1 + '.type']  = 'int') and (RegTable.S[R2 + '.type'] = 'int') then
  begin
    RegXor := RegAlloc('int');
    RegTable.I[RegXor + '.value'] := RegTable.I[R1 + '.value'] xor RegTable.I[R2 + '.value'];
  end
  else if (RegTable.S[R1 + '.type']  = 'boolean') and (RegTable.S[R2 + '.type'] = 'boolean') then
  begin
    RegXor := RegAlloc('boolean');
    RegTable.B[RegXor + '.value'] := RegTable.B[R1 + '.value'] xor RegTable.B[R2 + '.value'];
  end
  else begin
    bOK := False;
    yyerror('type for XOR error');
  end;
end;

function RegNot(const R1: TRegName; var bOK: Boolean): string;
begin
  bOK := True;
  if RegTable.S[R1 + '.type']  = 'int' then
  begin
    RegNot := RegAlloc('int');
    RegTable.I[RegNot + '.value'] := not RegTable.I[R1 + '.value'];
  end
  else if RegTable.S[R1 + '.type']  = 'boolean' then
  begin
    RegNot := RegAlloc('boolean');
    RegTable.B[RegNot + '.value'] := not RegTable.B[R1 + '.value'];
  end
  else begin
    bOK := False;
    yyerror('type for NOT error');
  end;
end;

function RegNeg(const R1: TRegName; var bOK: Boolean): string;
begin
  bOK := True;
  case RegTable.S[R1 + '.type'] of
    'int':
      begin
        RegNeg := RegAlloc('int');
        RegTable.I[RegNeg + '.value'] := -RegTable.I[R1 + '.value'];
      end;
    'real':
      begin
        RegNeg := RegAlloc('real');
        RegTable.D[RegNeg + '.value'] := -RegTable.D[R1 + '.value'];
      end
  else
    bOK := False;
    yyerror('type for - error');
  end;
end; 

function ReadValue(const S: string; var bOK: Boolean): string;
begin
  bOK := False;
  if Length(S) < 1 then Exit;
  bOK := True;
  if S[1] in ['0'..'9'] then
    ReadValue := S
  else if Assigned(SymTable.O[UpperCase(S) + '.reg']) then
    ReadValue := SymTable.S[UpperCase(S) + '.reg']
  else begin
    bOK := False;
    yyerror(Format('"%s" bad variable', [S]));
  end;
end;

function ReadInt(const S: string; var bOK: Boolean): Integer;
var
  T: string;
begin
  ReadInt := 0;
  T := ReadValue(S, bOK); writeln(RegTable.asjson(true));
  if bOK then 
  begin
      bOK := RegTable.S[T + '.type'] = 'int';
      if bOK then
        ReadInt := RegTable.I[T + '.value']
      else 
        yyerror('INTEGER required');
  end;
end;

function ReadReal(const S: string; var bOK: Boolean): Real;
var
  T: string;
begin
  ReadReal := 0;
  T := ReadValue(S, bOK);
  if bOK then 
  begin    
      if RegTable.S[T + '.type'] = 'int' then
        ReadReal := RegTable.I[T + '.value']
      else if RegTable.S[T + '.type'] = 'real' then
        ReadReal := RegTable.D[T + '.value']      
      else  begin
        bOK := False;
        yyerror('INTEGER required');
      end;
  end;
end;

procedure SendMsg(const S: string; const M: string);
var
  L: TStringList;
begin
  writeln('send ', S, ' ', M);

  if not Assigned(OnSendMessage) then Exit;
  
  L := TStringList.Create;
  L.Delimiter := ' ';
  L.StrictDelimiter := True;
  L.DelimitedText := M;
  OnSendMessage(S, StrToInt(L[0]), StrToInt(L[1]), StrToInt(L[2]));
  L.Free;
end;

function ConnectFeature(const Source, Target: string): Boolean;
begin
  writeln('ConnectFeature ', Source, ' ', Target);
  ConnectFeature := False;
  if Assigned(OnConnectFeature) then
    ConnectFeature := OnConnectFeature(Source, Target);
end;

function ConnectData(const Source, Target: string; const SourcePort, TargetPort: Integer): Boolean;
begin
  writeln(Format('ConnectData %s[%d] [%d]%s', [Source, SourcePort, TargetPort, Target]));
  ConnectData := False;
  if Assigned(OnConnectData) then
    ConnectData := OnConnectData(Source, Target, SourcePort, TargetPort);
end;

function ToStr(const S: string): string;
var
  L: TStringList;
  R: string;
begin
  ToStr := '';
  L := TStringList.Create;
  L.Delimiter := ' ';
  L.StrictDelimiter := True;
  L.DelimitedText := S;
  for R in L do
  begin
    case RegTable.S[R + '.type'] of
      'int':   ToStr := ToStr + IntToStr(RegTable.I[R + '.value']);
      'real':  ToStr := ToStr + FloatToStr(RegTable.D[R + '.value']);
      'string':ToStr := ToStr + RegTable.S[R + '.value'];
      'boolean':ToStr := ToStr + BoolToStr(RegTable.B[R + '.value'], True);
    end;
  end;
  L.Free;
end;

procedure CreateModules;
begin
  if Assigned(OnCreateModules) then 
    OnCreateModules(nil);
end;

const _AND = 257;
const _ARCCOS = 258;
const _ARCSIN = 259;
const ASSIGNMENT = 260;
const _BEGIN = 261;
const COLON = 262;
const COMMA = 263;
const CONNFEATURE = 264;
const CONNFEATUREDATA = 265;
const CONNDATA = 266;
const _CONST = 267;
const _COS = 268;
const _DIV = 269;
const DOT = 270;
const DOTDOT = 271;
const _ELSE = 272;
const _END = 273;
const EQUAL = 274;
const _EXP = 275;
const _FILE = 276;
const GE = 277;
const GT = 278;
const _ID = 279;
const _INTEGER = 280;
const _LAZRADIO = 281;
const LBRAC = 282;
const LBRACE = 283;
const LE = 284;
const _LOG = 285;
const LPAREN = 286;
const LT = 287;
const MINUS = 288;
const _MOD = 289;
const _NIL = 290;
const _NOT = 291;
const NOTEQUAL = 292;
const _ORD = 293;
const _OR = 294;
const PLUS = 295;
const _PRED = 296;
const _REAL = 297;
const _ROUND = 298;
const RBRAC = 299;
const RBRACE = 300;
const RPAREN = 301;
const SEND = 302;
const SEMICOLON = 303;
const _SIN = 304;
const SLASH = 305;
const STAR = 306;
const STARSTAR = 307;
const _SUCC = 308;
const _STR = 309;
const _STRING = 310;
const _TRUNC = 311;
const _VAL = 312;
const UPARROW = 313;
const _VAR = 314;
const _WRITELN = 315;
const _XOR = 316;
const ILLEGAL = 317;
const REALNUMBER = 318;
const DIGSEQ = 319;
const CHARACTER_STRING = 320;
const IDENTIFIER = 321;

type YYSType = record case Integer of
                 1 : ( yyString : String );
               end(*YYSType*);

var yylval : YYSType;

function yylex : Integer; forward;

function yyparse : Integer;

var yystate, yysp, yyn : Integer;
    yys : array [1..yymaxdepth] of Integer;
    yyv : array [1..yymaxdepth] of YYSType;
    yyval : YYSType;

procedure yyaction ( yyruleno : Integer );
  (* local definitions: *)
begin
  (* actions: *)
  case yyruleno of
   1 : begin
         writeln('program');
       end;
   2 : begin
         yyerror(':Text follows logical end of program.'); 
       end;
   3 : begin
         writeln('dot!');
       end;
   4 : begin
         yyval := yyv[yysp-1];
       end;
   5 : begin
         yyval.yyString := yyv[yysp-2].yyString + ' ' + yyv[yysp-0].yyString
       end;
   6 : begin
         yyval := yyv[yysp-0];
       end;
   7 : begin
         writeln('block!');
       end;
   8 : begin
         yyval := yyv[yysp-1];
       end;
   9 : begin
       end;
  10 : begin
         yyval := yyv[yysp-1];
       end;
  11 : begin
         yyval := yyv[yysp-0];
       end;
  12 : begin
         if DefVars(yyv[yysp-3].yyString, RegTable.S[yyv[yysp-1].yyString + '.type']) then
         SymTable.S['$1' + '.reg'] := yyv[yysp-1].yyString; 
       end;
  13 : begin
         yyval := yyv[yysp-0];
       end;
  14 : begin
         RegWrite(RegAlloc('boolean'), RegTable.O[yyv[yysp-2].yyString + '.value'].Compare(RegTable.O[yyv[yysp-0].yyString + '.value']) = cpEqu); 
       end;
  15 : begin
         RegWrite(RegAlloc('boolean'), RegTable.O[yyv[yysp-2].yyString + '.value'].Compare(RegTable.O[yyv[yysp-0].yyString + '.value']) <> cpEqu); 
       end;
  16 : begin
         RegWrite(RegAlloc('boolean'), RegTable.O[yyv[yysp-2].yyString + '.value'].Compare(RegTable.O[yyv[yysp-0].yyString + '.value']) = cpLess); 
       end;
  17 : begin
         RegWrite(RegAlloc('boolean'), RegTable.O[yyv[yysp-2].yyString + '.value'].Compare(RegTable.O[yyv[yysp-0].yyString + '.value']) = cpGreat); 
       end;
  18 : begin
         RegWrite(RegAlloc('boolean'), RegTable.O[yyv[yysp-2].yyString + '.value'].Compare(RegTable.O[yyv[yysp-0].yyString + '.value']) in [cpLess, cpEqu]); 
       end;
  19 : begin
         RegWrite(RegAlloc('boolean'), RegTable.O[yyv[yysp-2].yyString + '.value'].Compare(RegTable.O[yyv[yysp-0].yyString + '.value']) in [cpGreat, cpEqu]); 
       end;
  20 : begin
         yyval := yyv[yysp-0];
       end;
  21 : begin
         yyval.yyString := RegPlus(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
  22 : begin
         yyval.yyString := RegMinus(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
  23 : begin
         yyval.yyString := RegOr(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
  24 : begin
         yyval := yyv[yysp-0];
       end;
  25 : begin
         yyval.yyString := RegTimes(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
  26 : begin
         yyval.yyString := RegDivide(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
  27 : begin
         yyval.yyString := RegDiv(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
  28 : begin
         yyval.yyString := RegMod(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
  29 : begin
         yyval.yyString := RegAnd(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
  30 : begin
         yyval.yyString := yyv[yysp-0].yyString; 
       end;
  31 : begin
         yyval.yyString := RegNeg(yyv[yysp-0].yyString, RtOK); 
       end;
  32 : begin
         yyval := yyv[yysp-0];
       end;
  33 : begin
         yyval := yyv[yysp-0];
       end;
  34 : begin
         yyval.yyString := RegPower(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
  35 : begin
         yyval.yyString := ReadValue(yyv[yysp-0].yyString, RtOK); 
       end;
  36 : begin
         yyval.yyString := yyv[yysp-1].yyString; 
       end;
  37 : begin
         yyval := yyv[yysp-0];
       end;
  38 : begin
         yyval.yyString := RegNot(yyv[yysp-0].yyString, RtOK); 
       end;
  39 : begin
         yyval := yyv[yysp-0];
       end;
  40 : begin
         yyval.yyString := yyv[yysp-0].yyString; 
       end;
  41 : begin
         yyval.yyString := RegNeg(yyv[yysp-0].yyString, RtOK); 
       end;
  42 : begin
         yyval := yyv[yysp-0];
       end;
  43 : begin
         yyval := yyv[yysp-0];
       end;
  44 : begin
         yyval.yyString := ReadValue(yyv[yysp-0].yyString, RtOK); 
       end;
  45 : begin
         yyval := yyv[yysp-0];
       end;
  46 : begin
         CreateModules; 
       end;
  47 : begin
       end;
  48 : begin
         yyval := yyv[yysp-2];
       end;
  49 : begin
         yyval := yyv[yysp-0];
       end;
  50 : begin
         DefVars(yyv[yysp-2].yyString, 'int'); 
       end;
  51 : begin
         DefVars(yyv[yysp-2].yyString, 'real'); 
       end;
  52 : begin
         DefVars(yyv[yysp-2].yyString, 'string'); 
       end;
  53 : begin
         DefObjs(yyv[yysp-2].yyString, yyv[yysp-0].yyString); 
       end;
  54 : begin
         yyval := yyv[yysp-0];
       end;
  55 : begin
         yyval := yyv[yysp-2];
       end;
  56 : begin
         yyval := yyv[yysp-2];
       end;
  57 : begin
         yyval := yyv[yysp-0];
       end;
  58 : begin
         yyval := yyv[yysp-0];
       end;
  59 : begin
         yyval := yyv[yysp-0];
       end;
  60 : begin
         yyval := yyv[yysp-0];
       end;
  61 : begin
         yyval := yyv[yysp-0];
       end;
  62 : begin
         yyval := yyv[yysp-0];
       end;
  63 : begin
         yyval := yyv[yysp-0];
       end;
  64 : begin
         yyval := yyv[yysp-0];
       end;
  65 : begin
         yyval := yyv[yysp-0];
       end;
  66 : begin
       end;
  67 : begin
         yyval := yyv[yysp-0];
       end;
  68 : begin
         yyval := yyv[yysp-0];
       end;
  69 : begin
         yyval := yyv[yysp-0];
       end;
  70 : begin
         ConnectFeature(yyv[yysp-2].yyString, yyv[yysp-0].yyString); yyval.yyString := yyv[yysp-0].yyString; 
       end;
  71 : begin
         ConnectFeature(yyv[yysp-2].yyString, yyv[yysp-0].yyString); yyval.yyString := yyv[yysp-0].yyString; 
       end;
  72 : begin
         ConnectFeature(yyv[yysp-2].yyString, yyv[yysp-0].yyString); ConnectData(yyv[yysp-2].yyString, yyv[yysp-0].yyString, 0, 0); yyval.yyString := yyv[yysp-0].yyString; 
       end;
  73 : begin
         ConnectFeature(yyv[yysp-2].yyString, yyv[yysp-0].yyString); ConnectData(yyv[yysp-2].yyString, yyv[yysp-0].yyString, 0, 0); yyval.yyString := yyv[yysp-0].yyString; 
       end;
  74 : begin
         yyval := yyv[yysp-0];
       end;
  75 : begin
         ConnectData(yyv[yysp-8].yyString, yyv[yysp-0].yyString, ReadInt(yyv[yysp-6].yyString, RtOK), ReadInt(yyv[yysp-2].yyString, RtOK)); 
       end;
  76 : begin
         ConnectData(yyv[yysp-5].yyString, yyv[yysp-0].yyString, ReadInt(yyv[yysp-3].yyString, RtOK), 0); 
       end;
  77 : begin
         ConnectData(yyv[yysp-5].yyString, yyv[yysp-0].yyString, 0, ReadInt(yyv[yysp-2].yyString, RtOK)); 
       end;
  78 : begin
         ConnectData(yyv[yysp-2].yyString, yyv[yysp-0].yyString, 0, 0); yyval.yyString := yyv[yysp-0].yyString; 
       end;
  79 : begin
         ConnectData(yyv[yysp-2].yyString, yyv[yysp-0].yyString, 0, 0); yyval.yyString := yyv[yysp-0].yyString; 
       end;
  80 : begin
         SendMsg(yyv[yysp-2].yyString, yyv[yysp-0].yyString); 
       end;
  81 : begin
         SendMsg(yyv[yysp-2].yyString, yyv[yysp-0].yyString);
       end;
  82 : begin
         yyval.yyString := Format('%d %d %d', [ReadInt(yyv[yysp-5].yyString, RtOK), 
         ReadInt(yyv[yysp-3].yyString, RtOK), ReadInt(yyv[yysp-1].yyString, RtOK)]); 
       end;
  83 : begin
         yyval.yyString := Format('%d %d 0', [ReadInt(yyv[yysp-3].yyString, RtOK), ReadInt(yyv[yysp-1].yyString, RtOK)]); 
       end;
  84 : begin
         writeln(yyv[yysp-1].yyString); yyval.yyString := Format('%d 0 0', [ReadInt(yyv[yysp-1].yyString, RtOK)]); 
       end;
  85 : begin
         writeln(UpperCase(yyv[yysp-2].yyString) + '.reg'); writeln(SymTable.asjson(true)); if Assigned(SymTable.O[UpperCase(yyv[yysp-2].yyString) + '.reg']) then
         SymTable.S[UpperCase(yyv[yysp-2].yyString) + '.reg'] := yyv[yysp-0].yyString
         else
         yyerror(Format('"%s" bad variable', [yyv[yysp-2].yyString])); 
       end;
  86 : begin
         yyval := yyv[yysp-0];
       end;
  87 : begin
         yyval := yyv[yysp-0];
       end;
  88 : begin
         yyval := yyv[yysp-0];
       end;
  89 : begin
         RegWrite(RegAlloc('boolean'), RegTable.O[yyv[yysp-2].yyString + '.value'].Compare(RegTable.O[yyv[yysp-0].yyString + '.value']) = cpEqu); 
       end;
  90 : begin
         RegWrite(RegAlloc('boolean'), RegTable.O[yyv[yysp-2].yyString + '.value'].Compare(RegTable.O[yyv[yysp-0].yyString + '.value']) <> cpEqu); 
       end;
  91 : begin
         RegWrite(RegAlloc('boolean'), RegTable.O[yyv[yysp-2].yyString + '.value'].Compare(RegTable.O[yyv[yysp-0].yyString + '.value']) = cpLess); 
       end;
  92 : begin
         RegWrite(RegAlloc('boolean'), RegTable.O[yyv[yysp-2].yyString + '.value'].Compare(RegTable.O[yyv[yysp-0].yyString + '.value']) = cpGreat); 
       end;
  93 : begin
         RegWrite(RegAlloc('boolean'), RegTable.O[yyv[yysp-2].yyString + '.value'].Compare(RegTable.O[yyv[yysp-0].yyString + '.value']) in [cpLess, cpEqu]); 
       end;
  94 : begin
         RegWrite(RegAlloc('boolean'), RegTable.O[yyv[yysp-2].yyString + '.value'].Compare(RegTable.O[yyv[yysp-0].yyString + '.value']) in [cpGreat, cpEqu]); 
       end;
  95 : begin
         yyval := yyv[yysp-0];
       end;
  96 : begin
         yyval := yyv[yysp-0];
       end;
  97 : begin
         yyval.yyString := RegPlus(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
  98 : begin
         yyval.yyString := RegMinus(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
  99 : begin
         yyval.yyString := RegOr(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
 100 : begin
         yyval := yyv[yysp-0];
       end;
 101 : begin
         yyval.yyString := RegTimes(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
 102 : begin
         yyval.yyString := RegDivide(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
 103 : begin
         yyval.yyString := RegDiv(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
 104 : begin
         yyval.yyString := RegMod(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
 105 : begin
         yyval.yyString := RegAnd(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
 106 : begin
         yyval.yyString := yyv[yysp-0].yyString; 
       end;
 107 : begin
         yyval.yyString := RegNeg(yyv[yysp-0].yyString, RtOK); 
       end;
 108 : begin
         yyval := yyv[yysp-0];
       end;
 109 : begin
         yyval := yyv[yysp-0];
       end;
 110 : begin
         yyval.yyString := RegPower(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
 111 : begin
         yyval.yyString := ReadValue(yyv[yysp-0].yyString, RtOK); 
       end;
 112 : begin
         yyval := yyv[yysp-0];
       end;
 113 : begin
         yyval := yyv[yysp-0];
       end;
 114 : begin
         yyval.yyString := yyv[yysp-1].yyString; 
       end;
 115 : begin
         yyval.yyString := RegNot(yyv[yysp-0].yyString, RtOK); 
       end;
 116 : begin
         yyval.yyString := RegAlloc('real'); RegWrite(yyval.yyString, arccos(ReadReal(yyv[yysp-0].yyString, RtOK))); 
       end;
 117 : begin
         yyval.yyString := RegAlloc('real'); RegWrite(yyval.yyString, arcsin(ReadReal(yyv[yysp-0].yyString, RtOK))); 
       end;
 118 : begin
         yyval.yyString := RegAlloc('real'); RegWrite(yyval.yyString, cos(ReadReal(yyv[yysp-0].yyString, RtOK))); 
       end;
 119 : begin
         yyval.yyString := RegAlloc('real'); RegWrite(yyval.yyString, exp(ReadReal(yyv[yysp-0].yyString, RtOK))); 
       end;
 120 : begin
         yyval.yyString := RegAlloc('real'); RegWrite(yyval.yyString, log10(ReadReal(yyv[yysp-0].yyString, RtOK))); 
       end;
 121 : begin
         yyval.yyString := RegAlloc('int'); RegWrite(yyval.yyString, Pred(ReadInt(yyv[yysp-0].yyString, RtOK))); 
       end;
 122 : begin
         yyval.yyString := RegAlloc('int'); RegWrite(yyval.yyString, Round(ReadReal(yyv[yysp-0].yyString, RtOK))); 
       end;
 123 : begin
         yyval.yyString := RegAlloc('int'); RegWrite(yyval.yyString, sin(ReadReal(yyv[yysp-0].yyString, RtOK))); 
       end;
 124 : begin
         yyval.yyString := RegAlloc('int'); RegWrite(yyval.yyString, Succ(ReadInt(yyv[yysp-0].yyString, RtOK))); 
       end;
 125 : begin
         yyval.yyString := RegAlloc('string'); RegWrite(yyval.yyString, ToStr(yyv[yysp-0].yyString)); 
       end;
 126 : begin
         yyval.yyString := RegAlloc('int'); RegWrite(yyval.yyString, Trunc(ReadReal(yyv[yysp-0].yyString, RtOK))); 
       end;
 127 : begin
         yyval.yyString := RegAlloc('real'); // TODO: 
       end;
 128 : begin
         yyval.yyString := RegAlloc('real'); TempS := ToStr(yyv[yysp-0].yyString); RegWrite(yyval.yyString, TempS); writeln(TempS); 
       end;
 129 : begin
         yyval.yyString := yyv[yysp-1].yyString; 
       end;
 130 : begin
         yyval.yyString := yyv[yysp-2].yyString + ' ' + yyv[yysp-0].yyString; 
       end;
 131 : begin
         yyval := yyv[yysp-0];
       end;
 132 : begin
         yyval := yyv[yysp-0];
       end;
 133 : begin
         yyval := yyv[yysp-0];
       end;
 134 : begin
         yyval := yyv[yysp-0];
       end;
 135 : begin
         yyval := yyv[yysp-0];
       end;
 136 : begin
         yyval := yyv[yysp-0];
       end;
 137 : begin
         yyval := yyv[yysp-0];
       end;
 138 : begin
         yyval := yyv[yysp-0];
       end;
 139 : begin
         yyval := yyv[yysp-0];
       end;
 140 : begin
         yyval := yyv[yysp-0];
       end;
 141 : begin
         yyval := yyv[yysp-0];
       end;
  end;
end(*yyaction*);

(* parse table: *)

type YYARec = record
                sym, act : Integer;
              end;
     YYRRec = record
                len, sym : Integer;
              end;

const

yynacts   = 1110;
yyngotos  = 590;
yynstates = 233;
yynrules  = 141;

yya : array [1..yynacts] of YYARec = (
{ 0: }
  ( sym: 281; act: 4 ),
{ 1: }
  ( sym: 303; act: 6 ),
{ 2: }
  ( sym: 256; act: 7 ),
  ( sym: 0; act: -1 ),
{ 3: }
  ( sym: 0; act: 0 ),
{ 4: }
  ( sym: 321; act: 9 ),
{ 5: }
  ( sym: 267; act: 12 ),
  ( sym: 261; act: -9 ),
  ( sym: 314; act: -9 ),
{ 6: }
{ 7: }
{ 8: }
{ 9: }
{ 10: }
  ( sym: 314; act: 14 ),
  ( sym: 261; act: -47 ),
{ 11: }
  ( sym: 270; act: 15 ),
{ 12: }
  ( sym: 321; act: 9 ),
{ 13: }
  ( sym: 261; act: 21 ),
{ 14: }
  ( sym: 321; act: 9 ),
{ 15: }
{ 16: }
{ 17: }
  ( sym: 321; act: 9 ),
  ( sym: 261; act: -8 ),
  ( sym: 314; act: -8 ),
{ 18: }
  ( sym: 274; act: 27 ),
{ 19: }
{ 20: }
{ 21: }
  ( sym: 261; act: 21 ),
  ( sym: 321; act: 9 ),
  ( sym: 273; act: -66 ),
  ( sym: 303; act: -66 ),
{ 22: }
{ 23: }
  ( sym: 303; act: 6 ),
{ 24: }
  ( sym: 262; act: 45 ),
  ( sym: 263; act: 46 ),
{ 25: }
{ 26: }
{ 27: }
  ( sym: 286; act: 58 ),
  ( sym: 288; act: 59 ),
  ( sym: 291; act: 60 ),
  ( sym: 295; act: 61 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 28: }
{ 29: }
{ 30: }
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
{ 36: }
  ( sym: 273; act: 66 ),
  ( sym: 303; act: 6 ),
{ 37: }
{ 38: }
  ( sym: 266; act: 67 ),
  ( sym: 273; act: -74 ),
  ( sym: 303; act: -74 ),
{ 39: }
  ( sym: 265; act: 68 ),
  ( sym: 273; act: -68 ),
  ( sym: 303; act: -68 ),
{ 40: }
  ( sym: 264; act: 69 ),
  ( sym: 273; act: -67 ),
  ( sym: 303; act: -67 ),
{ 41: }
  ( sym: 302; act: 70 ),
  ( sym: 273; act: -64 ),
  ( sym: 303; act: -64 ),
{ 42: }
  ( sym: 260; act: 71 ),
  ( sym: 264; act: 72 ),
  ( sym: 265; act: 73 ),
  ( sym: 266; act: 74 ),
  ( sym: 282; act: 75 ),
  ( sym: 302; act: 76 ),
{ 43: }
  ( sym: 321; act: 9 ),
  ( sym: 261; act: -46 ),
{ 44: }
  ( sym: 321; act: 9 ),
{ 45: }
  ( sym: 280; act: 81 ),
  ( sym: 297; act: 82 ),
  ( sym: 310; act: 83 ),
  ( sym: 321; act: 9 ),
{ 46: }
{ 47: }
{ 48: }
{ 49: }
{ 50: }
{ 51: }
{ 52: }
  ( sym: 307; act: 84 ),
  ( sym: 257; act: -33 ),
  ( sym: 269; act: -33 ),
  ( sym: 274; act: -33 ),
  ( sym: 277; act: -33 ),
  ( sym: 278; act: -33 ),
  ( sym: 284; act: -33 ),
  ( sym: 287; act: -33 ),
  ( sym: 288; act: -33 ),
  ( sym: 289; act: -33 ),
  ( sym: 292; act: -33 ),
  ( sym: 294; act: -33 ),
  ( sym: 295; act: -33 ),
  ( sym: 301; act: -33 ),
  ( sym: 303; act: -33 ),
  ( sym: 305; act: -33 ),
  ( sym: 306; act: -33 ),
{ 53: }
{ 54: }
{ 55: }
  ( sym: 257; act: 85 ),
  ( sym: 269; act: 86 ),
  ( sym: 289; act: 87 ),
  ( sym: 305; act: 88 ),
  ( sym: 306; act: 89 ),
  ( sym: 274; act: -20 ),
  ( sym: 277; act: -20 ),
  ( sym: 278; act: -20 ),
  ( sym: 284; act: -20 ),
  ( sym: 287; act: -20 ),
  ( sym: 288; act: -20 ),
  ( sym: 292; act: -20 ),
  ( sym: 294; act: -20 ),
  ( sym: 295; act: -20 ),
  ( sym: 301; act: -20 ),
  ( sym: 303; act: -20 ),
{ 56: }
  ( sym: 274; act: 90 ),
  ( sym: 277; act: 91 ),
  ( sym: 278; act: 92 ),
  ( sym: 284; act: 93 ),
  ( sym: 287; act: 94 ),
  ( sym: 288; act: 95 ),
  ( sym: 292; act: 96 ),
  ( sym: 294; act: 97 ),
  ( sym: 295; act: 98 ),
  ( sym: 301; act: -13 ),
  ( sym: 303; act: -13 ),
{ 57: }
  ( sym: 303; act: 6 ),
{ 58: }
  ( sym: 286; act: 58 ),
  ( sym: 288; act: 59 ),
  ( sym: 291; act: 60 ),
  ( sym: 295; act: 61 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 59: }
  ( sym: 286; act: 58 ),
  ( sym: 288; act: 59 ),
  ( sym: 291; act: 60 ),
  ( sym: 295; act: 61 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 60: }
  ( sym: 286; act: 58 ),
  ( sym: 291; act: 60 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 61: }
  ( sym: 286; act: 58 ),
  ( sym: 288; act: 59 ),
  ( sym: 291; act: 60 ),
  ( sym: 295; act: 61 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 62: }
{ 63: }
{ 64: }
{ 65: }
  ( sym: 261; act: 21 ),
  ( sym: 321; act: 9 ),
  ( sym: 273; act: -66 ),
  ( sym: 303; act: -66 ),
{ 66: }
{ 67: }
  ( sym: 321; act: 9 ),
{ 68: }
  ( sym: 321; act: 9 ),
{ 69: }
  ( sym: 321; act: 9 ),
{ 70: }
  ( sym: 283; act: 109 ),
{ 71: }
  ( sym: 256; act: 120 ),
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 72: }
  ( sym: 321; act: 9 ),
{ 73: }
  ( sym: 321; act: 9 ),
{ 74: }
  ( sym: 282; act: 140 ),
  ( sym: 321; act: 9 ),
{ 75: }
  ( sym: 256; act: 120 ),
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 76: }
  ( sym: 283; act: 109 ),
{ 77: }
{ 78: }
{ 79: }
{ 80: }
{ 81: }
{ 82: }
{ 83: }
{ 84: }
  ( sym: 286; act: 58 ),
  ( sym: 291; act: 60 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 85: }
  ( sym: 286; act: 58 ),
  ( sym: 288; act: 59 ),
  ( sym: 291; act: 60 ),
  ( sym: 295; act: 61 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 86: }
  ( sym: 286; act: 58 ),
  ( sym: 288; act: 59 ),
  ( sym: 291; act: 60 ),
  ( sym: 295; act: 61 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 87: }
  ( sym: 286; act: 58 ),
  ( sym: 288; act: 59 ),
  ( sym: 291; act: 60 ),
  ( sym: 295; act: 61 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 88: }
  ( sym: 286; act: 58 ),
  ( sym: 288; act: 59 ),
  ( sym: 291; act: 60 ),
  ( sym: 295; act: 61 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 89: }
  ( sym: 286; act: 58 ),
  ( sym: 288; act: 59 ),
  ( sym: 291; act: 60 ),
  ( sym: 295; act: 61 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 90: }
  ( sym: 286; act: 58 ),
  ( sym: 288; act: 59 ),
  ( sym: 291; act: 60 ),
  ( sym: 295; act: 61 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 91: }
  ( sym: 286; act: 58 ),
  ( sym: 288; act: 59 ),
  ( sym: 291; act: 60 ),
  ( sym: 295; act: 61 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 92: }
  ( sym: 286; act: 58 ),
  ( sym: 288; act: 59 ),
  ( sym: 291; act: 60 ),
  ( sym: 295; act: 61 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 93: }
  ( sym: 286; act: 58 ),
  ( sym: 288; act: 59 ),
  ( sym: 291; act: 60 ),
  ( sym: 295; act: 61 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 94: }
  ( sym: 286; act: 58 ),
  ( sym: 288; act: 59 ),
  ( sym: 291; act: 60 ),
  ( sym: 295; act: 61 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 95: }
  ( sym: 286; act: 58 ),
  ( sym: 288; act: 59 ),
  ( sym: 291; act: 60 ),
  ( sym: 295; act: 61 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 96: }
  ( sym: 286; act: 58 ),
  ( sym: 288; act: 59 ),
  ( sym: 291; act: 60 ),
  ( sym: 295; act: 61 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 97: }
  ( sym: 286; act: 58 ),
  ( sym: 288; act: 59 ),
  ( sym: 291; act: 60 ),
  ( sym: 295; act: 61 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 98: }
  ( sym: 286; act: 58 ),
  ( sym: 288; act: 59 ),
  ( sym: 291; act: 60 ),
  ( sym: 295; act: 61 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 99: }
{ 100: }
  ( sym: 301; act: 159 ),
{ 101: }
{ 102: }
{ 103: }
{ 104: }
{ 105: }
{ 106: }
{ 107: }
{ 108: }
{ 109: }
  ( sym: 256; act: 120 ),
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 110: }
  ( sym: 286; act: 162 ),
{ 111: }
{ 112: }
{ 113: }
  ( sym: 307; act: 163 ),
  ( sym: 257; act: -109 ),
  ( sym: 263; act: -109 ),
  ( sym: 269; act: -109 ),
  ( sym: 273; act: -109 ),
  ( sym: 274; act: -109 ),
  ( sym: 277; act: -109 ),
  ( sym: 278; act: -109 ),
  ( sym: 284; act: -109 ),
  ( sym: 287; act: -109 ),
  ( sym: 288; act: -109 ),
  ( sym: 289; act: -109 ),
  ( sym: 292; act: -109 ),
  ( sym: 294; act: -109 ),
  ( sym: 295; act: -109 ),
  ( sym: 299; act: -109 ),
  ( sym: 300; act: -109 ),
  ( sym: 301; act: -109 ),
  ( sym: 303; act: -109 ),
  ( sym: 305; act: -109 ),
  ( sym: 306; act: -109 ),
{ 114: }
{ 115: }
{ 116: }
  ( sym: 257; act: 164 ),
  ( sym: 269; act: 165 ),
  ( sym: 289; act: 166 ),
  ( sym: 305; act: 167 ),
  ( sym: 306; act: 168 ),
  ( sym: 263; act: -96 ),
  ( sym: 273; act: -96 ),
  ( sym: 274; act: -96 ),
  ( sym: 277; act: -96 ),
  ( sym: 278; act: -96 ),
  ( sym: 284; act: -96 ),
  ( sym: 287; act: -96 ),
  ( sym: 288; act: -96 ),
  ( sym: 292; act: -96 ),
  ( sym: 294; act: -96 ),
  ( sym: 295; act: -96 ),
  ( sym: 299; act: -96 ),
  ( sym: 300; act: -96 ),
  ( sym: 301; act: -96 ),
  ( sym: 303; act: -96 ),
{ 117: }
  ( sym: 274; act: 169 ),
  ( sym: 277; act: 170 ),
  ( sym: 278; act: 171 ),
  ( sym: 284; act: 172 ),
  ( sym: 287; act: 173 ),
  ( sym: 288; act: 174 ),
  ( sym: 292; act: 175 ),
  ( sym: 294; act: 176 ),
  ( sym: 295; act: 177 ),
  ( sym: 263; act: -88 ),
  ( sym: 273; act: -88 ),
  ( sym: 299; act: -88 ),
  ( sym: 300; act: -88 ),
  ( sym: 301; act: -88 ),
  ( sym: 303; act: -88 ),
{ 118: }
{ 119: }
{ 120: }
{ 121: }
  ( sym: 286; act: 162 ),
{ 122: }
  ( sym: 286; act: 162 ),
{ 123: }
  ( sym: 286; act: 162 ),
{ 124: }
  ( sym: 286; act: 162 ),
{ 125: }
  ( sym: 286; act: 162 ),
{ 126: }
  ( sym: 256; act: 120 ),
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 127: }
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 128: }
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 291; act: 128 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 129: }
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 130: }
  ( sym: 286; act: 162 ),
{ 131: }
  ( sym: 286; act: 162 ),
{ 132: }
  ( sym: 286; act: 162 ),
{ 133: }
  ( sym: 286; act: 162 ),
{ 134: }
  ( sym: 286; act: 162 ),
{ 135: }
  ( sym: 286; act: 162 ),
{ 136: }
  ( sym: 286; act: 162 ),
{ 137: }
{ 138: }
{ 139: }
{ 140: }
  ( sym: 256; act: 120 ),
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 141: }
{ 142: }
  ( sym: 299; act: 195 ),
{ 143: }
{ 144: }
{ 145: }
{ 146: }
{ 147: }
{ 148: }
{ 149: }
{ 150: }
  ( sym: 288; act: 95 ),
  ( sym: 294; act: 97 ),
  ( sym: 295; act: 98 ),
  ( sym: 301; act: -14 ),
  ( sym: 303; act: -14 ),
{ 151: }
  ( sym: 288; act: 95 ),
  ( sym: 294; act: 97 ),
  ( sym: 295; act: 98 ),
  ( sym: 301; act: -19 ),
  ( sym: 303; act: -19 ),
{ 152: }
  ( sym: 288; act: 95 ),
  ( sym: 294; act: 97 ),
  ( sym: 295; act: 98 ),
  ( sym: 301; act: -17 ),
  ( sym: 303; act: -17 ),
{ 153: }
  ( sym: 288; act: 95 ),
  ( sym: 294; act: 97 ),
  ( sym: 295; act: 98 ),
  ( sym: 301; act: -18 ),
  ( sym: 303; act: -18 ),
{ 154: }
  ( sym: 288; act: 95 ),
  ( sym: 294; act: 97 ),
  ( sym: 295; act: 98 ),
  ( sym: 301; act: -16 ),
  ( sym: 303; act: -16 ),
{ 155: }
  ( sym: 257; act: 85 ),
  ( sym: 269; act: 86 ),
  ( sym: 289; act: 87 ),
  ( sym: 305; act: 88 ),
  ( sym: 306; act: 89 ),
  ( sym: 274; act: -22 ),
  ( sym: 277; act: -22 ),
  ( sym: 278; act: -22 ),
  ( sym: 284; act: -22 ),
  ( sym: 287; act: -22 ),
  ( sym: 288; act: -22 ),
  ( sym: 292; act: -22 ),
  ( sym: 294; act: -22 ),
  ( sym: 295; act: -22 ),
  ( sym: 301; act: -22 ),
  ( sym: 303; act: -22 ),
{ 156: }
  ( sym: 288; act: 95 ),
  ( sym: 294; act: 97 ),
  ( sym: 295; act: 98 ),
  ( sym: 301; act: -15 ),
  ( sym: 303; act: -15 ),
{ 157: }
  ( sym: 257; act: 85 ),
  ( sym: 269; act: 86 ),
  ( sym: 289; act: 87 ),
  ( sym: 305; act: 88 ),
  ( sym: 306; act: 89 ),
  ( sym: 274; act: -23 ),
  ( sym: 277; act: -23 ),
  ( sym: 278; act: -23 ),
  ( sym: 284; act: -23 ),
  ( sym: 287; act: -23 ),
  ( sym: 288; act: -23 ),
  ( sym: 292; act: -23 ),
  ( sym: 294; act: -23 ),
  ( sym: 295; act: -23 ),
  ( sym: 301; act: -23 ),
  ( sym: 303; act: -23 ),
{ 158: }
  ( sym: 257; act: 85 ),
  ( sym: 269; act: 86 ),
  ( sym: 289; act: 87 ),
  ( sym: 305; act: 88 ),
  ( sym: 306; act: 89 ),
  ( sym: 274; act: -21 ),
  ( sym: 277; act: -21 ),
  ( sym: 278; act: -21 ),
  ( sym: 284; act: -21 ),
  ( sym: 287; act: -21 ),
  ( sym: 288; act: -21 ),
  ( sym: 292; act: -21 ),
  ( sym: 294; act: -21 ),
  ( sym: 295; act: -21 ),
  ( sym: 301; act: -21 ),
  ( sym: 303; act: -21 ),
{ 159: }
{ 160: }
  ( sym: 263; act: 196 ),
  ( sym: 300; act: 197 ),
{ 161: }
{ 162: }
  ( sym: 256; act: 120 ),
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 163: }
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 291; act: 128 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 164: }
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 165: }
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 166: }
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 167: }
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 168: }
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 169: }
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 170: }
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 171: }
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 172: }
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 173: }
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 174: }
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 175: }
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 176: }
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 177: }
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 178: }
{ 179: }
{ 180: }
{ 181: }
{ 182: }
{ 183: }
  ( sym: 301; act: 216 ),
{ 184: }
{ 185: }
{ 186: }
{ 187: }
{ 188: }
{ 189: }
{ 190: }
{ 191: }
{ 192: }
{ 193: }
{ 194: }
  ( sym: 299; act: 217 ),
{ 195: }
  ( sym: 266; act: 218 ),
{ 196: }
  ( sym: 256; act: 120 ),
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 197: }
{ 198: }
{ 199: }
  ( sym: 263; act: 46 ),
  ( sym: 301; act: 221 ),
{ 200: }
{ 201: }
{ 202: }
{ 203: }
{ 204: }
{ 205: }
{ 206: }
{ 207: }
  ( sym: 288; act: 174 ),
  ( sym: 294; act: 176 ),
  ( sym: 295; act: 177 ),
  ( sym: 263; act: -89 ),
  ( sym: 273; act: -89 ),
  ( sym: 299; act: -89 ),
  ( sym: 300; act: -89 ),
  ( sym: 301; act: -89 ),
  ( sym: 303; act: -89 ),
{ 208: }
  ( sym: 288; act: 174 ),
  ( sym: 294; act: 176 ),
  ( sym: 295; act: 177 ),
  ( sym: 263; act: -94 ),
  ( sym: 273; act: -94 ),
  ( sym: 299; act: -94 ),
  ( sym: 300; act: -94 ),
  ( sym: 301; act: -94 ),
  ( sym: 303; act: -94 ),
{ 209: }
  ( sym: 288; act: 174 ),
  ( sym: 294; act: 176 ),
  ( sym: 295; act: 177 ),
  ( sym: 263; act: -92 ),
  ( sym: 273; act: -92 ),
  ( sym: 299; act: -92 ),
  ( sym: 300; act: -92 ),
  ( sym: 301; act: -92 ),
  ( sym: 303; act: -92 ),
{ 210: }
  ( sym: 288; act: 174 ),
  ( sym: 294; act: 176 ),
  ( sym: 295; act: 177 ),
  ( sym: 263; act: -93 ),
  ( sym: 273; act: -93 ),
  ( sym: 299; act: -93 ),
  ( sym: 300; act: -93 ),
  ( sym: 301; act: -93 ),
  ( sym: 303; act: -93 ),
{ 211: }
  ( sym: 288; act: 174 ),
  ( sym: 294; act: 176 ),
  ( sym: 295; act: 177 ),
  ( sym: 263; act: -91 ),
  ( sym: 273; act: -91 ),
  ( sym: 299; act: -91 ),
  ( sym: 300; act: -91 ),
  ( sym: 301; act: -91 ),
  ( sym: 303; act: -91 ),
{ 212: }
  ( sym: 257; act: 164 ),
  ( sym: 269; act: 165 ),
  ( sym: 289; act: 166 ),
  ( sym: 305; act: 167 ),
  ( sym: 306; act: 168 ),
  ( sym: 263; act: -98 ),
  ( sym: 273; act: -98 ),
  ( sym: 274; act: -98 ),
  ( sym: 277; act: -98 ),
  ( sym: 278; act: -98 ),
  ( sym: 284; act: -98 ),
  ( sym: 287; act: -98 ),
  ( sym: 288; act: -98 ),
  ( sym: 292; act: -98 ),
  ( sym: 294; act: -98 ),
  ( sym: 295; act: -98 ),
  ( sym: 299; act: -98 ),
  ( sym: 300; act: -98 ),
  ( sym: 301; act: -98 ),
  ( sym: 303; act: -98 ),
{ 213: }
  ( sym: 288; act: 174 ),
  ( sym: 294; act: 176 ),
  ( sym: 295; act: 177 ),
  ( sym: 263; act: -90 ),
  ( sym: 273; act: -90 ),
  ( sym: 299; act: -90 ),
  ( sym: 300; act: -90 ),
  ( sym: 301; act: -90 ),
  ( sym: 303; act: -90 ),
{ 214: }
  ( sym: 257; act: 164 ),
  ( sym: 269; act: 165 ),
  ( sym: 289; act: 166 ),
  ( sym: 305; act: 167 ),
  ( sym: 306; act: 168 ),
  ( sym: 263; act: -99 ),
  ( sym: 273; act: -99 ),
  ( sym: 274; act: -99 ),
  ( sym: 277; act: -99 ),
  ( sym: 278; act: -99 ),
  ( sym: 284; act: -99 ),
  ( sym: 287; act: -99 ),
  ( sym: 288; act: -99 ),
  ( sym: 292; act: -99 ),
  ( sym: 294; act: -99 ),
  ( sym: 295; act: -99 ),
  ( sym: 299; act: -99 ),
  ( sym: 300; act: -99 ),
  ( sym: 301; act: -99 ),
  ( sym: 303; act: -99 ),
{ 215: }
  ( sym: 257; act: 164 ),
  ( sym: 269; act: 165 ),
  ( sym: 289; act: 166 ),
  ( sym: 305; act: 167 ),
  ( sym: 306; act: 168 ),
  ( sym: 263; act: -97 ),
  ( sym: 273; act: -97 ),
  ( sym: 274; act: -97 ),
  ( sym: 277; act: -97 ),
  ( sym: 278; act: -97 ),
  ( sym: 284; act: -97 ),
  ( sym: 287; act: -97 ),
  ( sym: 288; act: -97 ),
  ( sym: 292; act: -97 ),
  ( sym: 294; act: -97 ),
  ( sym: 295; act: -97 ),
  ( sym: 299; act: -97 ),
  ( sym: 300; act: -97 ),
  ( sym: 301; act: -97 ),
  ( sym: 303; act: -97 ),
{ 216: }
{ 217: }
  ( sym: 321; act: 9 ),
{ 218: }
  ( sym: 282; act: 224 ),
  ( sym: 321; act: 9 ),
{ 219: }
  ( sym: 263; act: 226 ),
{ 220: }
  ( sym: 256; act: 120 ),
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 221: }
{ 222: }
{ 223: }
{ 224: }
  ( sym: 256; act: 120 ),
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 225: }
{ 226: }
  ( sym: 256; act: 120 ),
  ( sym: 258; act: 121 ),
  ( sym: 259; act: 122 ),
  ( sym: 268; act: 123 ),
  ( sym: 275; act: 124 ),
  ( sym: 285; act: 125 ),
  ( sym: 286; act: 126 ),
  ( sym: 288; act: 127 ),
  ( sym: 291; act: 128 ),
  ( sym: 295; act: 129 ),
  ( sym: 296; act: 130 ),
  ( sym: 298; act: 131 ),
  ( sym: 304; act: 132 ),
  ( sym: 308; act: 133 ),
  ( sym: 309; act: 134 ),
  ( sym: 311; act: 135 ),
  ( sym: 312; act: 136 ),
  ( sym: 318; act: 62 ),
  ( sym: 319; act: 63 ),
  ( sym: 320; act: 64 ),
  ( sym: 321; act: 9 ),
{ 227: }
{ 228: }
  ( sym: 299; act: 230 ),
{ 229: }
  ( sym: 300; act: 231 ),
{ 230: }
  ( sym: 321; act: 9 )
{ 231: }
{ 232: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -35; act: 1 ),
  ( sym: -34; act: 2 ),
  ( sym: -33; act: 3 ),
{ 1: }
  ( sym: -36; act: 5 ),
{ 2: }
{ 3: }
{ 4: }
  ( sym: -10; act: 8 ),
{ 5: }
  ( sym: -39; act: 10 ),
  ( sym: -37; act: 11 ),
{ 6: }
{ 7: }
{ 8: }
{ 9: }
{ 10: }
  ( sym: -40; act: 13 ),
{ 11: }
{ 12: }
  ( sym: -43; act: 16 ),
  ( sym: -42; act: 17 ),
  ( sym: -10; act: 18 ),
{ 13: }
  ( sym: -46; act: 19 ),
  ( sym: -41; act: 20 ),
{ 14: }
  ( sym: -45; act: 22 ),
  ( sym: -44; act: 23 ),
  ( sym: -11; act: 24 ),
  ( sym: -10; act: 25 ),
{ 15: }
{ 16: }
{ 17: }
  ( sym: -43; act: 26 ),
  ( sym: -10; act: 18 ),
{ 18: }
{ 19: }
{ 20: }
{ 21: }
  ( sym: -55; act: 28 ),
  ( sym: -54; act: 29 ),
  ( sym: -53; act: 30 ),
  ( sym: -52; act: 31 ),
  ( sym: -51; act: 32 ),
  ( sym: -50; act: 33 ),
  ( sym: -49; act: 34 ),
  ( sym: -48; act: 35 ),
  ( sym: -47; act: 36 ),
  ( sym: -46; act: 37 ),
  ( sym: -31; act: 38 ),
  ( sym: -30; act: 39 ),
  ( sym: -29; act: 40 ),
  ( sym: -27; act: 41 ),
  ( sym: -10; act: 42 ),
{ 22: }
{ 23: }
  ( sym: -36; act: 43 ),
{ 24: }
  ( sym: -38; act: 44 ),
{ 25: }
{ 26: }
{ 27: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 53 ),
  ( sym: -5; act: 54 ),
  ( sym: -4; act: 55 ),
  ( sym: -3; act: 56 ),
  ( sym: -2; act: 57 ),
{ 28: }
{ 29: }
{ 30: }
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
{ 36: }
  ( sym: -36; act: 65 ),
{ 37: }
{ 38: }
{ 39: }
{ 40: }
{ 41: }
{ 42: }
{ 43: }
  ( sym: -45; act: 77 ),
  ( sym: -11; act: 24 ),
  ( sym: -10; act: 25 ),
{ 44: }
  ( sym: -10; act: 78 ),
{ 45: }
  ( sym: -32; act: 79 ),
  ( sym: -10; act: 80 ),
{ 46: }
{ 47: }
{ 48: }
{ 49: }
{ 50: }
{ 51: }
{ 52: }
{ 53: }
{ 54: }
{ 55: }
{ 56: }
{ 57: }
  ( sym: -36; act: 99 ),
{ 58: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 53 ),
  ( sym: -5; act: 54 ),
  ( sym: -4; act: 55 ),
  ( sym: -3; act: 56 ),
  ( sym: -2; act: 100 ),
{ 59: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 53 ),
  ( sym: -5; act: 101 ),
{ 60: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 102 ),
{ 61: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 53 ),
  ( sym: -5; act: 103 ),
{ 62: }
{ 63: }
{ 64: }
{ 65: }
  ( sym: -55; act: 28 ),
  ( sym: -54; act: 29 ),
  ( sym: -53; act: 30 ),
  ( sym: -52; act: 31 ),
  ( sym: -51; act: 32 ),
  ( sym: -50; act: 33 ),
  ( sym: -49; act: 34 ),
  ( sym: -48; act: 104 ),
  ( sym: -46; act: 37 ),
  ( sym: -31; act: 38 ),
  ( sym: -30; act: 39 ),
  ( sym: -29; act: 40 ),
  ( sym: -27; act: 41 ),
  ( sym: -10; act: 42 ),
{ 66: }
{ 67: }
  ( sym: -10; act: 105 ),
{ 68: }
  ( sym: -10; act: 106 ),
{ 69: }
  ( sym: -10; act: 107 ),
{ 70: }
  ( sym: -28; act: 108 ),
{ 71: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 116 ),
  ( sym: -14; act: 117 ),
  ( sym: -13; act: 118 ),
  ( sym: -10; act: 119 ),
{ 72: }
  ( sym: -10; act: 137 ),
{ 73: }
  ( sym: -10; act: 138 ),
{ 74: }
  ( sym: -10; act: 139 ),
{ 75: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 116 ),
  ( sym: -14; act: 117 ),
  ( sym: -13; act: 141 ),
  ( sym: -12; act: 142 ),
  ( sym: -10; act: 119 ),
{ 76: }
  ( sym: -28; act: 143 ),
{ 77: }
{ 78: }
{ 79: }
{ 80: }
{ 81: }
{ 82: }
{ 83: }
{ 84: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 144 ),
{ 85: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 53 ),
  ( sym: -5; act: 145 ),
{ 86: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 53 ),
  ( sym: -5; act: 146 ),
{ 87: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 53 ),
  ( sym: -5; act: 147 ),
{ 88: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 53 ),
  ( sym: -5; act: 148 ),
{ 89: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 53 ),
  ( sym: -5; act: 149 ),
{ 90: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 53 ),
  ( sym: -5; act: 54 ),
  ( sym: -4; act: 55 ),
  ( sym: -3; act: 150 ),
{ 91: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 53 ),
  ( sym: -5; act: 54 ),
  ( sym: -4; act: 55 ),
  ( sym: -3; act: 151 ),
{ 92: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 53 ),
  ( sym: -5; act: 54 ),
  ( sym: -4; act: 55 ),
  ( sym: -3; act: 152 ),
{ 93: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 53 ),
  ( sym: -5; act: 54 ),
  ( sym: -4; act: 55 ),
  ( sym: -3; act: 153 ),
{ 94: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 53 ),
  ( sym: -5; act: 54 ),
  ( sym: -4; act: 55 ),
  ( sym: -3; act: 154 ),
{ 95: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 53 ),
  ( sym: -5; act: 54 ),
  ( sym: -4; act: 155 ),
{ 96: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 53 ),
  ( sym: -5; act: 54 ),
  ( sym: -4; act: 55 ),
  ( sym: -3; act: 156 ),
{ 97: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 53 ),
  ( sym: -5; act: 54 ),
  ( sym: -4; act: 157 ),
{ 98: }
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 50 ),
  ( sym: -10; act: 51 ),
  ( sym: -7; act: 52 ),
  ( sym: -6; act: 53 ),
  ( sym: -5; act: 54 ),
  ( sym: -4; act: 158 ),
{ 99: }
{ 100: }
{ 101: }
{ 102: }
{ 103: }
{ 104: }
{ 105: }
{ 106: }
{ 107: }
{ 108: }
{ 109: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 116 ),
  ( sym: -14; act: 117 ),
  ( sym: -13; act: 160 ),
  ( sym: -10; act: 119 ),
{ 110: }
  ( sym: -24; act: 161 ),
{ 111: }
{ 112: }
{ 113: }
{ 114: }
{ 115: }
{ 116: }
{ 117: }
{ 118: }
{ 119: }
{ 120: }
{ 121: }
  ( sym: -24; act: 178 ),
{ 122: }
  ( sym: -24; act: 179 ),
{ 123: }
  ( sym: -24; act: 180 ),
{ 124: }
  ( sym: -24; act: 181 ),
{ 125: }
  ( sym: -24; act: 182 ),
{ 126: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 116 ),
  ( sym: -14; act: 117 ),
  ( sym: -13; act: 183 ),
  ( sym: -10; act: 119 ),
{ 127: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 184 ),
  ( sym: -10; act: 119 ),
{ 128: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 185 ),
  ( sym: -10; act: 119 ),
{ 129: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 186 ),
  ( sym: -10; act: 119 ),
{ 130: }
  ( sym: -24; act: 187 ),
{ 131: }
  ( sym: -24; act: 188 ),
{ 132: }
  ( sym: -24; act: 189 ),
{ 133: }
  ( sym: -24; act: 190 ),
{ 134: }
  ( sym: -24; act: 191 ),
{ 135: }
  ( sym: -24; act: 192 ),
{ 136: }
  ( sym: -24; act: 193 ),
{ 137: }
{ 138: }
{ 139: }
{ 140: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 116 ),
  ( sym: -14; act: 117 ),
  ( sym: -13; act: 141 ),
  ( sym: -12; act: 194 ),
  ( sym: -10; act: 119 ),
{ 141: }
{ 142: }
{ 143: }
{ 144: }
{ 145: }
{ 146: }
{ 147: }
{ 148: }
{ 149: }
{ 150: }
{ 151: }
{ 152: }
{ 153: }
{ 154: }
{ 155: }
{ 156: }
{ 157: }
{ 158: }
{ 159: }
{ 160: }
{ 161: }
{ 162: }
  ( sym: -57; act: 110 ),
  ( sym: -26; act: 198 ),
  ( sym: -25; act: 199 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 116 ),
  ( sym: -14; act: 117 ),
  ( sym: -13; act: 200 ),
  ( sym: -10; act: 119 ),
{ 163: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 201 ),
  ( sym: -10; act: 119 ),
{ 164: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 202 ),
  ( sym: -10; act: 119 ),
{ 165: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 203 ),
  ( sym: -10; act: 119 ),
{ 166: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 204 ),
  ( sym: -10; act: 119 ),
{ 167: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 205 ),
  ( sym: -10; act: 119 ),
{ 168: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 206 ),
  ( sym: -10; act: 119 ),
{ 169: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 116 ),
  ( sym: -14; act: 207 ),
  ( sym: -10; act: 119 ),
{ 170: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 116 ),
  ( sym: -14; act: 208 ),
  ( sym: -10; act: 119 ),
{ 171: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 116 ),
  ( sym: -14; act: 209 ),
  ( sym: -10; act: 119 ),
{ 172: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 116 ),
  ( sym: -14; act: 210 ),
  ( sym: -10; act: 119 ),
{ 173: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 116 ),
  ( sym: -14; act: 211 ),
  ( sym: -10; act: 119 ),
{ 174: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 212 ),
  ( sym: -10; act: 119 ),
{ 175: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 116 ),
  ( sym: -14; act: 213 ),
  ( sym: -10; act: 119 ),
{ 176: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 214 ),
  ( sym: -10; act: 119 ),
{ 177: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 215 ),
  ( sym: -10; act: 119 ),
{ 178: }
{ 179: }
{ 180: }
{ 181: }
{ 182: }
{ 183: }
{ 184: }
{ 185: }
{ 186: }
{ 187: }
{ 188: }
{ 189: }
{ 190: }
{ 191: }
{ 192: }
{ 193: }
{ 194: }
{ 195: }
{ 196: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 116 ),
  ( sym: -14; act: 117 ),
  ( sym: -13; act: 219 ),
  ( sym: -10; act: 119 ),
{ 197: }
{ 198: }
{ 199: }
  ( sym: -38; act: 220 ),
{ 200: }
{ 201: }
{ 202: }
{ 203: }
{ 204: }
{ 205: }
{ 206: }
{ 207: }
{ 208: }
{ 209: }
{ 210: }
{ 211: }
{ 212: }
{ 213: }
{ 214: }
{ 215: }
{ 216: }
{ 217: }
  ( sym: -10; act: 222 ),
{ 218: }
  ( sym: -10; act: 223 ),
{ 219: }
  ( sym: -56; act: 225 ),
{ 220: }
  ( sym: -57; act: 110 ),
  ( sym: -26; act: 227 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 116 ),
  ( sym: -14; act: 117 ),
  ( sym: -13; act: 200 ),
  ( sym: -10; act: 119 ),
{ 221: }
{ 222: }
{ 223: }
{ 224: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 116 ),
  ( sym: -14; act: 117 ),
  ( sym: -13; act: 141 ),
  ( sym: -12; act: 228 ),
  ( sym: -10; act: 119 ),
{ 225: }
{ 226: }
  ( sym: -57; act: 110 ),
  ( sym: -23; act: 111 ),
  ( sym: -22; act: 47 ),
  ( sym: -21; act: 48 ),
  ( sym: -20; act: 49 ),
  ( sym: -19; act: 112 ),
  ( sym: -18; act: 113 ),
  ( sym: -17; act: 114 ),
  ( sym: -16; act: 115 ),
  ( sym: -15; act: 116 ),
  ( sym: -14; act: 117 ),
  ( sym: -13; act: 229 ),
  ( sym: -10; act: 119 ),
{ 227: }
{ 228: }
{ 229: }
{ 230: }
  ( sym: -10; act: 232 )
{ 231: }
{ 232: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 0,
{ 3: } 0,
{ 4: } 0,
{ 5: } 0,
{ 6: } -140,
{ 7: } -2,
{ 8: } -4,
{ 9: } -139,
{ 10: } 0,
{ 11: } 0,
{ 12: } 0,
{ 13: } 0,
{ 14: } 0,
{ 15: } -3,
{ 16: } -11,
{ 17: } 0,
{ 18: } 0,
{ 19: } -54,
{ 20: } -7,
{ 21: } 0,
{ 22: } -49,
{ 23: } 0,
{ 24: } 0,
{ 25: } -6,
{ 26: } -10,
{ 27: } 0,
{ 28: } -69,
{ 29: } -65,
{ 30: } -62,
{ 31: } -61,
{ 32: } -60,
{ 33: } -59,
{ 34: } -58,
{ 35: } -57,
{ 36: } 0,
{ 37: } -63,
{ 38: } 0,
{ 39: } 0,
{ 40: } 0,
{ 41: } 0,
{ 42: } 0,
{ 43: } 0,
{ 44: } 0,
{ 45: } 0,
{ 46: } -141,
{ 47: } -136,
{ 48: } -135,
{ 49: } -133,
{ 50: } -37,
{ 51: } -35,
{ 52: } 0,
{ 53: } -32,
{ 54: } -24,
{ 55: } 0,
{ 56: } 0,
{ 57: } 0,
{ 58: } 0,
{ 59: } 0,
{ 60: } 0,
{ 61: } 0,
{ 62: } -138,
{ 63: } -137,
{ 64: } -134,
{ 65: } 0,
{ 66: } -55,
{ 67: } 0,
{ 68: } 0,
{ 69: } 0,
{ 70: } 0,
{ 71: } 0,
{ 72: } 0,
{ 73: } 0,
{ 74: } 0,
{ 75: } 0,
{ 76: } 0,
{ 77: } -48,
{ 78: } -5,
{ 79: } -53,
{ 80: } -87,
{ 81: } -50,
{ 82: } -51,
{ 83: } -52,
{ 84: } 0,
{ 85: } 0,
{ 86: } 0,
{ 87: } 0,
{ 88: } 0,
{ 89: } 0,
{ 90: } 0,
{ 91: } 0,
{ 92: } 0,
{ 93: } 0,
{ 94: } 0,
{ 95: } 0,
{ 96: } 0,
{ 97: } 0,
{ 98: } 0,
{ 99: } -12,
{ 100: } 0,
{ 101: } -31,
{ 102: } -38,
{ 103: } -30,
{ 104: } -56,
{ 105: } -79,
{ 106: } -73,
{ 107: } -71,
{ 108: } -81,
{ 109: } 0,
{ 110: } 0,
{ 111: } -113,
{ 112: } -112,
{ 113: } 0,
{ 114: } -108,
{ 115: } -100,
{ 116: } 0,
{ 117: } 0,
{ 118: } -85,
{ 119: } -111,
{ 120: } -95,
{ 121: } 0,
{ 122: } 0,
{ 123: } 0,
{ 124: } 0,
{ 125: } 0,
{ 126: } 0,
{ 127: } 0,
{ 128: } 0,
{ 129: } 0,
{ 130: } 0,
{ 131: } 0,
{ 132: } 0,
{ 133: } 0,
{ 134: } 0,
{ 135: } 0,
{ 136: } 0,
{ 137: } -70,
{ 138: } -72,
{ 139: } -78,
{ 140: } 0,
{ 141: } -86,
{ 142: } 0,
{ 143: } -80,
{ 144: } -34,
{ 145: } -29,
{ 146: } -27,
{ 147: } -28,
{ 148: } -26,
{ 149: } -25,
{ 150: } 0,
{ 151: } 0,
{ 152: } 0,
{ 153: } 0,
{ 154: } 0,
{ 155: } 0,
{ 156: } 0,
{ 157: } 0,
{ 158: } 0,
{ 159: } -36,
{ 160: } 0,
{ 161: } -128,
{ 162: } 0,
{ 163: } 0,
{ 164: } 0,
{ 165: } 0,
{ 166: } 0,
{ 167: } 0,
{ 168: } 0,
{ 169: } 0,
{ 170: } 0,
{ 171: } 0,
{ 172: } 0,
{ 173: } 0,
{ 174: } 0,
{ 175: } 0,
{ 176: } 0,
{ 177: } 0,
{ 178: } -116,
{ 179: } -117,
{ 180: } -118,
{ 181: } -119,
{ 182: } -120,
{ 183: } 0,
{ 184: } -107,
{ 185: } -115,
{ 186: } -106,
{ 187: } -121,
{ 188: } -122,
{ 189: } -123,
{ 190: } -124,
{ 191: } -125,
{ 192: } -126,
{ 193: } -127,
{ 194: } 0,
{ 195: } 0,
{ 196: } 0,
{ 197: } -84,
{ 198: } -131,
{ 199: } 0,
{ 200: } -132,
{ 201: } -110,
{ 202: } -105,
{ 203: } -103,
{ 204: } -104,
{ 205: } -102,
{ 206: } -101,
{ 207: } 0,
{ 208: } 0,
{ 209: } 0,
{ 210: } 0,
{ 211: } 0,
{ 212: } 0,
{ 213: } 0,
{ 214: } 0,
{ 215: } 0,
{ 216: } -114,
{ 217: } 0,
{ 218: } 0,
{ 219: } 0,
{ 220: } 0,
{ 221: } -129,
{ 222: } -77,
{ 223: } -76,
{ 224: } 0,
{ 225: } -83,
{ 226: } 0,
{ 227: } -130,
{ 228: } 0,
{ 229: } 0,
{ 230: } 0,
{ 231: } -82,
{ 232: } -75
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 3,
{ 3: } 5,
{ 4: } 6,
{ 5: } 7,
{ 6: } 10,
{ 7: } 10,
{ 8: } 10,
{ 9: } 10,
{ 10: } 10,
{ 11: } 12,
{ 12: } 13,
{ 13: } 14,
{ 14: } 15,
{ 15: } 16,
{ 16: } 16,
{ 17: } 16,
{ 18: } 19,
{ 19: } 20,
{ 20: } 20,
{ 21: } 20,
{ 22: } 24,
{ 23: } 24,
{ 24: } 25,
{ 25: } 27,
{ 26: } 27,
{ 27: } 27,
{ 28: } 35,
{ 29: } 35,
{ 30: } 35,
{ 31: } 35,
{ 32: } 35,
{ 33: } 35,
{ 34: } 35,
{ 35: } 35,
{ 36: } 35,
{ 37: } 37,
{ 38: } 37,
{ 39: } 40,
{ 40: } 43,
{ 41: } 46,
{ 42: } 49,
{ 43: } 55,
{ 44: } 57,
{ 45: } 58,
{ 46: } 62,
{ 47: } 62,
{ 48: } 62,
{ 49: } 62,
{ 50: } 62,
{ 51: } 62,
{ 52: } 62,
{ 53: } 79,
{ 54: } 79,
{ 55: } 79,
{ 56: } 95,
{ 57: } 106,
{ 58: } 107,
{ 59: } 115,
{ 60: } 123,
{ 61: } 129,
{ 62: } 137,
{ 63: } 137,
{ 64: } 137,
{ 65: } 137,
{ 66: } 141,
{ 67: } 141,
{ 68: } 142,
{ 69: } 143,
{ 70: } 144,
{ 71: } 145,
{ 72: } 166,
{ 73: } 167,
{ 74: } 168,
{ 75: } 170,
{ 76: } 191,
{ 77: } 192,
{ 78: } 192,
{ 79: } 192,
{ 80: } 192,
{ 81: } 192,
{ 82: } 192,
{ 83: } 192,
{ 84: } 192,
{ 85: } 198,
{ 86: } 206,
{ 87: } 214,
{ 88: } 222,
{ 89: } 230,
{ 90: } 238,
{ 91: } 246,
{ 92: } 254,
{ 93: } 262,
{ 94: } 270,
{ 95: } 278,
{ 96: } 286,
{ 97: } 294,
{ 98: } 302,
{ 99: } 310,
{ 100: } 310,
{ 101: } 311,
{ 102: } 311,
{ 103: } 311,
{ 104: } 311,
{ 105: } 311,
{ 106: } 311,
{ 107: } 311,
{ 108: } 311,
{ 109: } 311,
{ 110: } 332,
{ 111: } 333,
{ 112: } 333,
{ 113: } 333,
{ 114: } 354,
{ 115: } 354,
{ 116: } 354,
{ 117: } 374,
{ 118: } 389,
{ 119: } 389,
{ 120: } 389,
{ 121: } 389,
{ 122: } 390,
{ 123: } 391,
{ 124: } 392,
{ 125: } 393,
{ 126: } 394,
{ 127: } 415,
{ 128: } 435,
{ 129: } 453,
{ 130: } 473,
{ 131: } 474,
{ 132: } 475,
{ 133: } 476,
{ 134: } 477,
{ 135: } 478,
{ 136: } 479,
{ 137: } 480,
{ 138: } 480,
{ 139: } 480,
{ 140: } 480,
{ 141: } 501,
{ 142: } 501,
{ 143: } 502,
{ 144: } 502,
{ 145: } 502,
{ 146: } 502,
{ 147: } 502,
{ 148: } 502,
{ 149: } 502,
{ 150: } 502,
{ 151: } 507,
{ 152: } 512,
{ 153: } 517,
{ 154: } 522,
{ 155: } 527,
{ 156: } 543,
{ 157: } 548,
{ 158: } 564,
{ 159: } 580,
{ 160: } 580,
{ 161: } 582,
{ 162: } 582,
{ 163: } 603,
{ 164: } 621,
{ 165: } 641,
{ 166: } 661,
{ 167: } 681,
{ 168: } 701,
{ 169: } 721,
{ 170: } 741,
{ 171: } 761,
{ 172: } 781,
{ 173: } 801,
{ 174: } 821,
{ 175: } 841,
{ 176: } 861,
{ 177: } 881,
{ 178: } 901,
{ 179: } 901,
{ 180: } 901,
{ 181: } 901,
{ 182: } 901,
{ 183: } 901,
{ 184: } 902,
{ 185: } 902,
{ 186: } 902,
{ 187: } 902,
{ 188: } 902,
{ 189: } 902,
{ 190: } 902,
{ 191: } 902,
{ 192: } 902,
{ 193: } 902,
{ 194: } 902,
{ 195: } 903,
{ 196: } 904,
{ 197: } 925,
{ 198: } 925,
{ 199: } 925,
{ 200: } 927,
{ 201: } 927,
{ 202: } 927,
{ 203: } 927,
{ 204: } 927,
{ 205: } 927,
{ 206: } 927,
{ 207: } 927,
{ 208: } 936,
{ 209: } 945,
{ 210: } 954,
{ 211: } 963,
{ 212: } 972,
{ 213: } 992,
{ 214: } 1001,
{ 215: } 1021,
{ 216: } 1041,
{ 217: } 1041,
{ 218: } 1042,
{ 219: } 1044,
{ 220: } 1045,
{ 221: } 1066,
{ 222: } 1066,
{ 223: } 1066,
{ 224: } 1066,
{ 225: } 1087,
{ 226: } 1087,
{ 227: } 1108,
{ 228: } 1108,
{ 229: } 1109,
{ 230: } 1110,
{ 231: } 1111,
{ 232: } 1111
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 4,
{ 3: } 5,
{ 4: } 6,
{ 5: } 9,
{ 6: } 9,
{ 7: } 9,
{ 8: } 9,
{ 9: } 9,
{ 10: } 11,
{ 11: } 12,
{ 12: } 13,
{ 13: } 14,
{ 14: } 15,
{ 15: } 15,
{ 16: } 15,
{ 17: } 18,
{ 18: } 19,
{ 19: } 19,
{ 20: } 19,
{ 21: } 23,
{ 22: } 23,
{ 23: } 24,
{ 24: } 26,
{ 25: } 26,
{ 26: } 26,
{ 27: } 34,
{ 28: } 34,
{ 29: } 34,
{ 30: } 34,
{ 31: } 34,
{ 32: } 34,
{ 33: } 34,
{ 34: } 34,
{ 35: } 34,
{ 36: } 36,
{ 37: } 36,
{ 38: } 39,
{ 39: } 42,
{ 40: } 45,
{ 41: } 48,
{ 42: } 54,
{ 43: } 56,
{ 44: } 57,
{ 45: } 61,
{ 46: } 61,
{ 47: } 61,
{ 48: } 61,
{ 49: } 61,
{ 50: } 61,
{ 51: } 61,
{ 52: } 78,
{ 53: } 78,
{ 54: } 78,
{ 55: } 94,
{ 56: } 105,
{ 57: } 106,
{ 58: } 114,
{ 59: } 122,
{ 60: } 128,
{ 61: } 136,
{ 62: } 136,
{ 63: } 136,
{ 64: } 136,
{ 65: } 140,
{ 66: } 140,
{ 67: } 141,
{ 68: } 142,
{ 69: } 143,
{ 70: } 144,
{ 71: } 165,
{ 72: } 166,
{ 73: } 167,
{ 74: } 169,
{ 75: } 190,
{ 76: } 191,
{ 77: } 191,
{ 78: } 191,
{ 79: } 191,
{ 80: } 191,
{ 81: } 191,
{ 82: } 191,
{ 83: } 191,
{ 84: } 197,
{ 85: } 205,
{ 86: } 213,
{ 87: } 221,
{ 88: } 229,
{ 89: } 237,
{ 90: } 245,
{ 91: } 253,
{ 92: } 261,
{ 93: } 269,
{ 94: } 277,
{ 95: } 285,
{ 96: } 293,
{ 97: } 301,
{ 98: } 309,
{ 99: } 309,
{ 100: } 310,
{ 101: } 310,
{ 102: } 310,
{ 103: } 310,
{ 104: } 310,
{ 105: } 310,
{ 106: } 310,
{ 107: } 310,
{ 108: } 310,
{ 109: } 331,
{ 110: } 332,
{ 111: } 332,
{ 112: } 332,
{ 113: } 353,
{ 114: } 353,
{ 115: } 353,
{ 116: } 373,
{ 117: } 388,
{ 118: } 388,
{ 119: } 388,
{ 120: } 388,
{ 121: } 389,
{ 122: } 390,
{ 123: } 391,
{ 124: } 392,
{ 125: } 393,
{ 126: } 414,
{ 127: } 434,
{ 128: } 452,
{ 129: } 472,
{ 130: } 473,
{ 131: } 474,
{ 132: } 475,
{ 133: } 476,
{ 134: } 477,
{ 135: } 478,
{ 136: } 479,
{ 137: } 479,
{ 138: } 479,
{ 139: } 479,
{ 140: } 500,
{ 141: } 500,
{ 142: } 501,
{ 143: } 501,
{ 144: } 501,
{ 145: } 501,
{ 146: } 501,
{ 147: } 501,
{ 148: } 501,
{ 149: } 501,
{ 150: } 506,
{ 151: } 511,
{ 152: } 516,
{ 153: } 521,
{ 154: } 526,
{ 155: } 542,
{ 156: } 547,
{ 157: } 563,
{ 158: } 579,
{ 159: } 579,
{ 160: } 581,
{ 161: } 581,
{ 162: } 602,
{ 163: } 620,
{ 164: } 640,
{ 165: } 660,
{ 166: } 680,
{ 167: } 700,
{ 168: } 720,
{ 169: } 740,
{ 170: } 760,
{ 171: } 780,
{ 172: } 800,
{ 173: } 820,
{ 174: } 840,
{ 175: } 860,
{ 176: } 880,
{ 177: } 900,
{ 178: } 900,
{ 179: } 900,
{ 180: } 900,
{ 181: } 900,
{ 182: } 900,
{ 183: } 901,
{ 184: } 901,
{ 185: } 901,
{ 186: } 901,
{ 187: } 901,
{ 188: } 901,
{ 189: } 901,
{ 190: } 901,
{ 191: } 901,
{ 192: } 901,
{ 193: } 901,
{ 194: } 902,
{ 195: } 903,
{ 196: } 924,
{ 197: } 924,
{ 198: } 924,
{ 199: } 926,
{ 200: } 926,
{ 201: } 926,
{ 202: } 926,
{ 203: } 926,
{ 204: } 926,
{ 205: } 926,
{ 206: } 926,
{ 207: } 935,
{ 208: } 944,
{ 209: } 953,
{ 210: } 962,
{ 211: } 971,
{ 212: } 991,
{ 213: } 1000,
{ 214: } 1020,
{ 215: } 1040,
{ 216: } 1040,
{ 217: } 1041,
{ 218: } 1043,
{ 219: } 1044,
{ 220: } 1065,
{ 221: } 1065,
{ 222: } 1065,
{ 223: } 1065,
{ 224: } 1086,
{ 225: } 1086,
{ 226: } 1107,
{ 227: } 1107,
{ 228: } 1108,
{ 229: } 1109,
{ 230: } 1110,
{ 231: } 1110,
{ 232: } 1110
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 4,
{ 2: } 5,
{ 3: } 5,
{ 4: } 5,
{ 5: } 6,
{ 6: } 8,
{ 7: } 8,
{ 8: } 8,
{ 9: } 8,
{ 10: } 8,
{ 11: } 9,
{ 12: } 9,
{ 13: } 12,
{ 14: } 14,
{ 15: } 18,
{ 16: } 18,
{ 17: } 18,
{ 18: } 20,
{ 19: } 20,
{ 20: } 20,
{ 21: } 20,
{ 22: } 35,
{ 23: } 35,
{ 24: } 36,
{ 25: } 37,
{ 26: } 37,
{ 27: } 37,
{ 28: } 48,
{ 29: } 48,
{ 30: } 48,
{ 31: } 48,
{ 32: } 48,
{ 33: } 48,
{ 34: } 48,
{ 35: } 48,
{ 36: } 48,
{ 37: } 49,
{ 38: } 49,
{ 39: } 49,
{ 40: } 49,
{ 41: } 49,
{ 42: } 49,
{ 43: } 49,
{ 44: } 52,
{ 45: } 53,
{ 46: } 55,
{ 47: } 55,
{ 48: } 55,
{ 49: } 55,
{ 50: } 55,
{ 51: } 55,
{ 52: } 55,
{ 53: } 55,
{ 54: } 55,
{ 55: } 55,
{ 56: } 55,
{ 57: } 55,
{ 58: } 56,
{ 59: } 67,
{ 60: } 75,
{ 61: } 81,
{ 62: } 89,
{ 63: } 89,
{ 64: } 89,
{ 65: } 89,
{ 66: } 103,
{ 67: } 103,
{ 68: } 104,
{ 69: } 105,
{ 70: } 106,
{ 71: } 107,
{ 72: } 120,
{ 73: } 121,
{ 74: } 122,
{ 75: } 123,
{ 76: } 137,
{ 77: } 138,
{ 78: } 138,
{ 79: } 138,
{ 80: } 138,
{ 81: } 138,
{ 82: } 138,
{ 83: } 138,
{ 84: } 138,
{ 85: } 145,
{ 86: } 153,
{ 87: } 161,
{ 88: } 169,
{ 89: } 177,
{ 90: } 185,
{ 91: } 195,
{ 92: } 205,
{ 93: } 215,
{ 94: } 225,
{ 95: } 235,
{ 96: } 244,
{ 97: } 254,
{ 98: } 263,
{ 99: } 272,
{ 100: } 272,
{ 101: } 272,
{ 102: } 272,
{ 103: } 272,
{ 104: } 272,
{ 105: } 272,
{ 106: } 272,
{ 107: } 272,
{ 108: } 272,
{ 109: } 272,
{ 110: } 285,
{ 111: } 286,
{ 112: } 286,
{ 113: } 286,
{ 114: } 286,
{ 115: } 286,
{ 116: } 286,
{ 117: } 286,
{ 118: } 286,
{ 119: } 286,
{ 120: } 286,
{ 121: } 286,
{ 122: } 287,
{ 123: } 288,
{ 124: } 289,
{ 125: } 290,
{ 126: } 291,
{ 127: } 304,
{ 128: } 314,
{ 129: } 322,
{ 130: } 332,
{ 131: } 333,
{ 132: } 334,
{ 133: } 335,
{ 134: } 336,
{ 135: } 337,
{ 136: } 338,
{ 137: } 339,
{ 138: } 339,
{ 139: } 339,
{ 140: } 339,
{ 141: } 353,
{ 142: } 353,
{ 143: } 353,
{ 144: } 353,
{ 145: } 353,
{ 146: } 353,
{ 147: } 353,
{ 148: } 353,
{ 149: } 353,
{ 150: } 353,
{ 151: } 353,
{ 152: } 353,
{ 153: } 353,
{ 154: } 353,
{ 155: } 353,
{ 156: } 353,
{ 157: } 353,
{ 158: } 353,
{ 159: } 353,
{ 160: } 353,
{ 161: } 353,
{ 162: } 353,
{ 163: } 368,
{ 164: } 377,
{ 165: } 387,
{ 166: } 397,
{ 167: } 407,
{ 168: } 417,
{ 169: } 427,
{ 170: } 439,
{ 171: } 451,
{ 172: } 463,
{ 173: } 475,
{ 174: } 487,
{ 175: } 498,
{ 176: } 510,
{ 177: } 521,
{ 178: } 532,
{ 179: } 532,
{ 180: } 532,
{ 181: } 532,
{ 182: } 532,
{ 183: } 532,
{ 184: } 532,
{ 185: } 532,
{ 186: } 532,
{ 187: } 532,
{ 188: } 532,
{ 189: } 532,
{ 190: } 532,
{ 191: } 532,
{ 192: } 532,
{ 193: } 532,
{ 194: } 532,
{ 195: } 532,
{ 196: } 532,
{ 197: } 545,
{ 198: } 545,
{ 199: } 545,
{ 200: } 546,
{ 201: } 546,
{ 202: } 546,
{ 203: } 546,
{ 204: } 546,
{ 205: } 546,
{ 206: } 546,
{ 207: } 546,
{ 208: } 546,
{ 209: } 546,
{ 210: } 546,
{ 211: } 546,
{ 212: } 546,
{ 213: } 546,
{ 214: } 546,
{ 215: } 546,
{ 216: } 546,
{ 217: } 546,
{ 218: } 547,
{ 219: } 548,
{ 220: } 549,
{ 221: } 563,
{ 222: } 563,
{ 223: } 563,
{ 224: } 563,
{ 225: } 577,
{ 226: } 577,
{ 227: } 590,
{ 228: } 590,
{ 229: } 590,
{ 230: } 590,
{ 231: } 591,
{ 232: } 591
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 3,
{ 1: } 4,
{ 2: } 4,
{ 3: } 4,
{ 4: } 5,
{ 5: } 7,
{ 6: } 7,
{ 7: } 7,
{ 8: } 7,
{ 9: } 7,
{ 10: } 8,
{ 11: } 8,
{ 12: } 11,
{ 13: } 13,
{ 14: } 17,
{ 15: } 17,
{ 16: } 17,
{ 17: } 19,
{ 18: } 19,
{ 19: } 19,
{ 20: } 19,
{ 21: } 34,
{ 22: } 34,
{ 23: } 35,
{ 24: } 36,
{ 25: } 36,
{ 26: } 36,
{ 27: } 47,
{ 28: } 47,
{ 29: } 47,
{ 30: } 47,
{ 31: } 47,
{ 32: } 47,
{ 33: } 47,
{ 34: } 47,
{ 35: } 47,
{ 36: } 48,
{ 37: } 48,
{ 38: } 48,
{ 39: } 48,
{ 40: } 48,
{ 41: } 48,
{ 42: } 48,
{ 43: } 51,
{ 44: } 52,
{ 45: } 54,
{ 46: } 54,
{ 47: } 54,
{ 48: } 54,
{ 49: } 54,
{ 50: } 54,
{ 51: } 54,
{ 52: } 54,
{ 53: } 54,
{ 54: } 54,
{ 55: } 54,
{ 56: } 54,
{ 57: } 55,
{ 58: } 66,
{ 59: } 74,
{ 60: } 80,
{ 61: } 88,
{ 62: } 88,
{ 63: } 88,
{ 64: } 88,
{ 65: } 102,
{ 66: } 102,
{ 67: } 103,
{ 68: } 104,
{ 69: } 105,
{ 70: } 106,
{ 71: } 119,
{ 72: } 120,
{ 73: } 121,
{ 74: } 122,
{ 75: } 136,
{ 76: } 137,
{ 77: } 137,
{ 78: } 137,
{ 79: } 137,
{ 80: } 137,
{ 81: } 137,
{ 82: } 137,
{ 83: } 137,
{ 84: } 144,
{ 85: } 152,
{ 86: } 160,
{ 87: } 168,
{ 88: } 176,
{ 89: } 184,
{ 90: } 194,
{ 91: } 204,
{ 92: } 214,
{ 93: } 224,
{ 94: } 234,
{ 95: } 243,
{ 96: } 253,
{ 97: } 262,
{ 98: } 271,
{ 99: } 271,
{ 100: } 271,
{ 101: } 271,
{ 102: } 271,
{ 103: } 271,
{ 104: } 271,
{ 105: } 271,
{ 106: } 271,
{ 107: } 271,
{ 108: } 271,
{ 109: } 284,
{ 110: } 285,
{ 111: } 285,
{ 112: } 285,
{ 113: } 285,
{ 114: } 285,
{ 115: } 285,
{ 116: } 285,
{ 117: } 285,
{ 118: } 285,
{ 119: } 285,
{ 120: } 285,
{ 121: } 286,
{ 122: } 287,
{ 123: } 288,
{ 124: } 289,
{ 125: } 290,
{ 126: } 303,
{ 127: } 313,
{ 128: } 321,
{ 129: } 331,
{ 130: } 332,
{ 131: } 333,
{ 132: } 334,
{ 133: } 335,
{ 134: } 336,
{ 135: } 337,
{ 136: } 338,
{ 137: } 338,
{ 138: } 338,
{ 139: } 338,
{ 140: } 352,
{ 141: } 352,
{ 142: } 352,
{ 143: } 352,
{ 144: } 352,
{ 145: } 352,
{ 146: } 352,
{ 147: } 352,
{ 148: } 352,
{ 149: } 352,
{ 150: } 352,
{ 151: } 352,
{ 152: } 352,
{ 153: } 352,
{ 154: } 352,
{ 155: } 352,
{ 156: } 352,
{ 157: } 352,
{ 158: } 352,
{ 159: } 352,
{ 160: } 352,
{ 161: } 352,
{ 162: } 367,
{ 163: } 376,
{ 164: } 386,
{ 165: } 396,
{ 166: } 406,
{ 167: } 416,
{ 168: } 426,
{ 169: } 438,
{ 170: } 450,
{ 171: } 462,
{ 172: } 474,
{ 173: } 486,
{ 174: } 497,
{ 175: } 509,
{ 176: } 520,
{ 177: } 531,
{ 178: } 531,
{ 179: } 531,
{ 180: } 531,
{ 181: } 531,
{ 182: } 531,
{ 183: } 531,
{ 184: } 531,
{ 185: } 531,
{ 186: } 531,
{ 187: } 531,
{ 188: } 531,
{ 189: } 531,
{ 190: } 531,
{ 191: } 531,
{ 192: } 531,
{ 193: } 531,
{ 194: } 531,
{ 195: } 531,
{ 196: } 544,
{ 197: } 544,
{ 198: } 544,
{ 199: } 545,
{ 200: } 545,
{ 201: } 545,
{ 202: } 545,
{ 203: } 545,
{ 204: } 545,
{ 205: } 545,
{ 206: } 545,
{ 207: } 545,
{ 208: } 545,
{ 209: } 545,
{ 210: } 545,
{ 211: } 545,
{ 212: } 545,
{ 213: } 545,
{ 214: } 545,
{ 215: } 545,
{ 216: } 545,
{ 217: } 546,
{ 218: } 547,
{ 219: } 548,
{ 220: } 562,
{ 221: } 562,
{ 222: } 562,
{ 223: } 562,
{ 224: } 576,
{ 225: } 576,
{ 226: } 589,
{ 227: } 589,
{ 228: } 589,
{ 229: } 589,
{ 230: } 590,
{ 231: } 590,
{ 232: } 590
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 1; sym: -33 ),
{ 2: } ( len: 2; sym: -33 ),
{ 3: } ( len: 4; sym: -34 ),
{ 4: } ( len: 2; sym: -35 ),
{ 5: } ( len: 3; sym: -11 ),
{ 6: } ( len: 1; sym: -11 ),
{ 7: } ( len: 3; sym: -37 ),
{ 8: } ( len: 2; sym: -39 ),
{ 9: } ( len: 0; sym: -39 ),
{ 10: } ( len: 2; sym: -42 ),
{ 11: } ( len: 1; sym: -42 ),
{ 12: } ( len: 4; sym: -43 ),
{ 13: } ( len: 1; sym: -2 ),
{ 14: } ( len: 3; sym: -2 ),
{ 15: } ( len: 3; sym: -2 ),
{ 16: } ( len: 3; sym: -2 ),
{ 17: } ( len: 3; sym: -2 ),
{ 18: } ( len: 3; sym: -2 ),
{ 19: } ( len: 3; sym: -2 ),
{ 20: } ( len: 1; sym: -3 ),
{ 21: } ( len: 3; sym: -3 ),
{ 22: } ( len: 3; sym: -3 ),
{ 23: } ( len: 3; sym: -3 ),
{ 24: } ( len: 1; sym: -4 ),
{ 25: } ( len: 3; sym: -4 ),
{ 26: } ( len: 3; sym: -4 ),
{ 27: } ( len: 3; sym: -4 ),
{ 28: } ( len: 3; sym: -4 ),
{ 29: } ( len: 3; sym: -4 ),
{ 30: } ( len: 2; sym: -5 ),
{ 31: } ( len: 2; sym: -5 ),
{ 32: } ( len: 1; sym: -5 ),
{ 33: } ( len: 1; sym: -6 ),
{ 34: } ( len: 3; sym: -6 ),
{ 35: } ( len: 1; sym: -7 ),
{ 36: } ( len: 3; sym: -7 ),
{ 37: } ( len: 1; sym: -7 ),
{ 38: } ( len: 2; sym: -7 ),
{ 39: } ( len: 1; sym: -8 ),
{ 40: } ( len: 2; sym: -8 ),
{ 41: } ( len: 2; sym: -8 ),
{ 42: } ( len: 1; sym: -8 ),
{ 43: } ( len: 1; sym: -9 ),
{ 44: } ( len: 1; sym: -9 ),
{ 45: } ( len: 1; sym: -9 ),
{ 46: } ( len: 3; sym: -40 ),
{ 47: } ( len: 0; sym: -40 ),
{ 48: } ( len: 3; sym: -44 ),
{ 49: } ( len: 1; sym: -44 ),
{ 50: } ( len: 3; sym: -45 ),
{ 51: } ( len: 3; sym: -45 ),
{ 52: } ( len: 3; sym: -45 ),
{ 53: } ( len: 3; sym: -45 ),
{ 54: } ( len: 1; sym: -41 ),
{ 55: } ( len: 3; sym: -46 ),
{ 56: } ( len: 3; sym: -47 ),
{ 57: } ( len: 1; sym: -47 ),
{ 58: } ( len: 1; sym: -48 ),
{ 59: } ( len: 1; sym: -48 ),
{ 60: } ( len: 1; sym: -49 ),
{ 61: } ( len: 1; sym: -50 ),
{ 62: } ( len: 1; sym: -52 ),
{ 63: } ( len: 1; sym: -52 ),
{ 64: } ( len: 1; sym: -52 ),
{ 65: } ( len: 1; sym: -52 ),
{ 66: } ( len: 0; sym: -51 ),
{ 67: } ( len: 1; sym: -54 ),
{ 68: } ( len: 1; sym: -54 ),
{ 69: } ( len: 1; sym: -54 ),
{ 70: } ( len: 3; sym: -29 ),
{ 71: } ( len: 3; sym: -29 ),
{ 72: } ( len: 3; sym: -30 ),
{ 73: } ( len: 3; sym: -30 ),
{ 74: } ( len: 1; sym: -55 ),
{ 75: } ( len: 9; sym: -55 ),
{ 76: } ( len: 6; sym: -55 ),
{ 77: } ( len: 6; sym: -55 ),
{ 78: } ( len: 3; sym: -31 ),
{ 79: } ( len: 3; sym: -31 ),
{ 80: } ( len: 3; sym: -27 ),
{ 81: } ( len: 3; sym: -27 ),
{ 82: } ( len: 7; sym: -28 ),
{ 83: } ( len: 5; sym: -28 ),
{ 84: } ( len: 3; sym: -28 ),
{ 85: } ( len: 3; sym: -53 ),
{ 86: } ( len: 1; sym: -12 ),
{ 87: } ( len: 1; sym: -32 ),
{ 88: } ( len: 1; sym: -13 ),
{ 89: } ( len: 3; sym: -13 ),
{ 90: } ( len: 3; sym: -13 ),
{ 91: } ( len: 3; sym: -13 ),
{ 92: } ( len: 3; sym: -13 ),
{ 93: } ( len: 3; sym: -13 ),
{ 94: } ( len: 3; sym: -13 ),
{ 95: } ( len: 1; sym: -13 ),
{ 96: } ( len: 1; sym: -14 ),
{ 97: } ( len: 3; sym: -14 ),
{ 98: } ( len: 3; sym: -14 ),
{ 99: } ( len: 3; sym: -14 ),
{ 100: } ( len: 1; sym: -15 ),
{ 101: } ( len: 3; sym: -15 ),
{ 102: } ( len: 3; sym: -15 ),
{ 103: } ( len: 3; sym: -15 ),
{ 104: } ( len: 3; sym: -15 ),
{ 105: } ( len: 3; sym: -15 ),
{ 106: } ( len: 2; sym: -16 ),
{ 107: } ( len: 2; sym: -16 ),
{ 108: } ( len: 1; sym: -16 ),
{ 109: } ( len: 1; sym: -17 ),
{ 110: } ( len: 3; sym: -17 ),
{ 111: } ( len: 1; sym: -18 ),
{ 112: } ( len: 1; sym: -18 ),
{ 113: } ( len: 1; sym: -18 ),
{ 114: } ( len: 3; sym: -18 ),
{ 115: } ( len: 2; sym: -18 ),
{ 116: } ( len: 2; sym: -23 ),
{ 117: } ( len: 2; sym: -23 ),
{ 118: } ( len: 2; sym: -23 ),
{ 119: } ( len: 2; sym: -23 ),
{ 120: } ( len: 2; sym: -23 ),
{ 121: } ( len: 2; sym: -23 ),
{ 122: } ( len: 2; sym: -23 ),
{ 123: } ( len: 2; sym: -23 ),
{ 124: } ( len: 2; sym: -23 ),
{ 125: } ( len: 2; sym: -23 ),
{ 126: } ( len: 2; sym: -23 ),
{ 127: } ( len: 2; sym: -23 ),
{ 128: } ( len: 2; sym: -23 ),
{ 129: } ( len: 3; sym: -24 ),
{ 130: } ( len: 3; sym: -25 ),
{ 131: } ( len: 1; sym: -25 ),
{ 132: } ( len: 1; sym: -26 ),
{ 133: } ( len: 1; sym: -19 ),
{ 134: } ( len: 1; sym: -19 ),
{ 135: } ( len: 1; sym: -20 ),
{ 136: } ( len: 1; sym: -20 ),
{ 137: } ( len: 1; sym: -21 ),
{ 138: } ( len: 1; sym: -22 ),
{ 139: } ( len: 1; sym: -10 ),
{ 140: } ( len: 1; sym: -36 ),
{ 141: } ( len: 1; sym: -38 )
);


const _error = 256; (* error token *)

function yyact(state, sym : Integer; var act : Integer) : Boolean;
  (* search action table *)
  var k : Integer;
  begin
    k := yyal[state];
    while (k<=yyah[state]) and (yya[k].sym<>sym) do inc(k);
    if k>yyah[state] then
      yyact := false
    else
      begin
        act := yya[k].act;
        yyact := true;
      end;
  end(*yyact*);

function yygoto(state, sym : Integer; var nstate : Integer) : Boolean;
  (* search goto table *)
  var k : Integer;
  begin
    k := yygl[state];
    while (k<=yygh[state]) and (yyg[k].sym<>sym) do inc(k);
    if k>yygh[state] then
      yygoto := false
    else
      begin
        nstate := yyg[k].act;
        yygoto := true;
      end;
  end(*yygoto*);

label parse, next, error, errlab, shift, reduce, accept, abort;

begin(*yyparse*)

  (* initialize: *)

  yystate := 0; yychar := -1; yynerrs := 0; yyerrflag := 0; yysp := 0;

{$ifdef yydebug}
  yydebug := true;
{$else}
  yydebug := false;
{$endif}

parse:

  (* push state and value: *)

  inc(yysp);
  if yysp>yymaxdepth then
    begin
      yyerror('yyparse stack overflow');
      goto abort;
    end;
  yys[yysp] := yystate; yyv[yysp] := yyval;

next:

  if (yyd[yystate]=0) and (yychar=-1) then
    (* get next symbol *)
    begin
      yychar := yylex; if yychar<0 then yychar := 0;
    end;

  if yydebug then writeln('state ', yystate, ', char ', yychar);

  (* determine parse action: *)

  yyn := yyd[yystate];
  if yyn<>0 then goto reduce; (* simple state *)

  (* no default action; search parse table *)

  if not yyact(yystate, yychar, yyn) then goto error
  else if yyn>0 then                      goto shift
  else if yyn<0 then                      goto reduce
  else                                    goto accept;

error:

  (* error; start error recovery: *)

  if yyerrflag=0 then yyerror('syntax error');

errlab:

  if yyerrflag=0 then inc(yynerrs);     (* new error *)

  if yyerrflag<=2 then                  (* incomplete recovery; try again *)
    begin
      yyerrflag := 3;
      (* uncover a state with shift action on error token *)
      while (yysp>0) and not ( yyact(yys[yysp], _error, yyn) and
                               (yyn>0) ) do
        begin
          if yydebug then
            if yysp>1 then
              writeln('error recovery pops state ', yys[yysp], ', uncovers ',
                      yys[yysp-1])
            else
              writeln('error recovery fails ... abort');
          dec(yysp);
        end;
      if yysp=0 then goto abort; (* parser has fallen from stack; abort *)
      yystate := yyn;            (* simulate shift on error *)
      goto parse;
    end
  else                                  (* no shift yet; discard symbol *)
    begin
      if yydebug then writeln('error recovery discards char ', yychar);
      if yychar=0 then goto abort; (* end of input; abort *)
      yychar := -1; goto next;     (* clear lookahead char and try again *)
    end;

shift:

  (* go to new state, clear lookahead character: *)

  yystate := yyn; yychar := -1; yyval := yylval;
  if yyerrflag>0 then dec(yyerrflag);

  goto parse;

reduce:

  (* execute action, pop rule from stack, and go to next state: *)

  if yydebug then writeln('reduce ', -yyn);

  yyflag := yyfnone; yyaction(-yyn);
  dec(yysp, yyr[-yyn].len);
  if yygoto(yys[yysp], yyr[-yyn].sym, yyn) then yystate := yyn;

  (* handle action calls to yyaccept, yyabort and yyerror: *)

  case yyflag of
    yyfaccept : goto accept;
    yyfabort  : goto abort;
    yyferror  : goto errlab;
  end;

  goto parse;

accept:

  yyparse := 0; exit;

abort:

  yyparse := 1; exit;

end(*yyparse*);


{$I lazradiolex.pas}

begin
  RegTable := SO('{}');

  if not Assigned(SymTable) then 
  begin
    SymTable := SO('{}');
    PredefineInt('RM_CONFIG', 200);
  end;

  if not Assigned(ObjTypes) then
    ObjTypes := SO('{"RTL": {}, "SPECTRUM": {}, "FREQDISCRIMINATOR": {}, "FMRECEIVER": {}, "AUDIOOUT": {}}');
  

  filename := paramStr(1);
  if filename='' then
    begin
      write('input file: ');
      readln(filename);
    end;
  assign(yyinput, filename);
  reset(yyinput);
  if yyparse=0 then writeln('successful parse!');
end.