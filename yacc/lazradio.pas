
(* Yacc parser template (TP Yacc V3.0), V1.2 6-17-91 AG *)

(* global definitions: *)


procedure ReportProjName(const Name: string);
begin
  if Assigned(OnProjName) then
    OnProjName(Name);
end;

procedure EmitMsg(const Line: string);
begin
  if not Assigned(OnWriteLn) then
    System.WriteLn(Line)
  else
    OnWriteLn(Line);
end;

function yywrap(): Boolean;
begin
  yywrap := True;
end;

procedure yyerror(msg : string);
begin
  EmitMsg(Format('%s: %d: %s at or before `%s`.', [filename, yylineno, msg, yytext]));
  yychar := 0; // abort 
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
  // writeln(SymTable.asjson(true));
end;

// for built-in types
function DefVars(const S: string; const T: string): Boolean;
var
  L: TStringList;
  V: string;
begin
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

function GetSymDisp(const N: string): string;
begin
  GetSymDisp := SymTable.S[UpperCase(N) + '.disp'];
  if GetSymDisp = '' then GetSymDisp := N;
end;

function DefObjs(const S: string; const T: string): Boolean;
var
  L: TStringList;
  V: string;
  R: string;
begin
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
    SymTable.O[UpperCase(V)] := SO(Format('{type: "%s", disp: "%s", obj: true}', [R, V]));
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
  T := ReadValue(S, bOK); 
  if bOK then 
  begin
      bOK := RegTable.S[T + '.type'] = 'int';
      if bOK then
        ReadInt := RegTable.I[T + '.value']
      else 
        yyerror('INTEGER required');
  end;
end;

function ReadMsgParam(const S: string; var bOK: Boolean): PtrUInt;
var
  T: string;
begin
  ReadMsgParam := 0;
  T := ReadValue(S, bOK); 
  if bOK then 
  begin
      bOK := RegTable.S[T + '.type'] = 'int';
      if bOK then
      begin
        ReadMsgParam := RegTable.I[T + '.value'];
        Exit;
      end;

      bOK := RegTable.S[T + '.type'] = 'string';
      if bOK then
      begin
        ReadMsgParam := OnMakeStrParam(RegTable.S[T + '.value']);
        Exit;
      end;

      yyerror('INTEGER/STRING required');
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

function StrVal(const S: string; var bOK: Boolean): string;
var
  R: Real;
  I: Integer;
  C: Integer;
begin
  val(S, R, C); bOK := C = 0;
  if bOK then
  begin
    StrVal := RegAlloc('real');
    RegWrite(StrVal, R);
    Exit;
  end;
  val(S, I, C); bOK := C = 0;
  if bOK then
  begin
    StrVal := RegAlloc('int');
    RegWrite(StrVal, I);
    Exit;
  end;
  StrVal := RegAlloc('int');  // default: 0
end;

function SendMsg(const S: string; const M: string): Boolean;
var
  L: TStringList;
begin
  // writeln('send ', S, ' ', M);
  SendMsg := False;
  if not Assigned(OnSendMessage) then Exit;
  
  L := TStringList.Create;
  L.Delimiter := ' ';
  L.StrictDelimiter := True;
  L.DelimitedText := M;
  SendMsg := OnSendMessage(GetSymDisp(S), StrToInt64(L[0]), StrToInt64(L[1]), StrToInt64(L[2]));
  L.Free;
end;

function ConnectFeature(const Source, Target: string): Boolean;
begin
  ConnectFeature := False;
  if Assigned(OnConnectFeature) then
    ConnectFeature := OnConnectFeature(GetSymDisp(Source), GetSymDisp(Target));
end;

function ConnectData(const Source, Target: string; const SourcePort, TargetPort: Integer): Boolean;
begin
  // writeln(Format('ConnectData %s[%d] [%d]%s', [Source, SourcePort, TargetPort, Target]));
  ConnectData := False;
  if Assigned(OnConnectData) then
    ConnectData := OnConnectData(GetSymDisp(Source), GetSymDisp(Target), SourcePort, TargetPort);
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
         
       end;
   2 : begin
         yyerror(':Text follows logical end of program.'); 
       end;
   3 : begin
         yyval := yyv[yysp-3];
       end;
   4 : begin
         ReportProjName(yyv[yysp-0].yyString); 
       end;
   5 : begin
         yyval.yyString := yyv[yysp-2].yyString + ' ' + yyv[yysp-0].yyString
       end;
   6 : begin
         yyval := yyv[yysp-0];
       end;
   7 : begin
         
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
         yyval := yyv[yysp-0];
       end;
  67 : begin
       end;
  68 : begin
         yyval := yyv[yysp-0];
       end;
  69 : begin
         yyval := yyv[yysp-0];
       end;
  70 : begin
         yyval := yyv[yysp-0];
       end;
  71 : begin
         ConnectFeature(yyv[yysp-2].yyString, yyv[yysp-0].yyString); yyval.yyString := yyv[yysp-0].yyString; 
       end;
  72 : begin
         ConnectFeature(yyv[yysp-2].yyString, yyv[yysp-0].yyString); yyval.yyString := yyv[yysp-0].yyString; 
       end;
  73 : begin
         ConnectFeature(yyv[yysp-2].yyString, yyv[yysp-0].yyString); ConnectData(yyv[yysp-2].yyString, yyv[yysp-0].yyString, 0, 0); yyval.yyString := yyv[yysp-0].yyString; 
       end;
  74 : begin
         ConnectFeature(yyv[yysp-2].yyString, yyv[yysp-0].yyString); ConnectData(yyv[yysp-2].yyString, yyv[yysp-0].yyString, 0, 0); yyval.yyString := yyv[yysp-0].yyString; 
       end;
  75 : begin
         yyval := yyv[yysp-0];
       end;
  76 : begin
         ConnectData(yyv[yysp-8].yyString, yyv[yysp-0].yyString, ReadInt(yyv[yysp-6].yyString, RtOK), ReadInt(yyv[yysp-2].yyString, RtOK)); 
       end;
  77 : begin
         ConnectData(yyv[yysp-5].yyString, yyv[yysp-0].yyString, ReadInt(yyv[yysp-3].yyString, RtOK), 0); 
       end;
  78 : begin
         ConnectData(yyv[yysp-5].yyString, yyv[yysp-0].yyString, 0, ReadInt(yyv[yysp-2].yyString, RtOK)); 
       end;
  79 : begin
         ConnectData(yyv[yysp-2].yyString, yyv[yysp-0].yyString, 0, 0); yyval.yyString := yyv[yysp-0].yyString; 
       end;
  80 : begin
         ConnectData(yyv[yysp-2].yyString, yyv[yysp-0].yyString, 0, 0); yyval.yyString := yyv[yysp-0].yyString; 
       end;
  81 : begin
         SendMsg(yyv[yysp-2].yyString, yyv[yysp-0].yyString); yyval.yyString := yyv[yysp-2].yyString;
       end;
  82 : begin
         SendMsg(yyv[yysp-2].yyString, yyv[yysp-0].yyString); yyval.yyString := yyv[yysp-2].yyString;
       end;
  83 : begin
         yyval.yyString := Format('%d %d %d', [ReadInt(yyv[yysp-5].yyString, RtOK), 
         ReadMsgParam(yyv[yysp-3].yyString, RtOK), ReadMsgParam(yyv[yysp-1].yyString, RtOK)]); 
       end;
  84 : begin
         yyval.yyString := Format('%d %d 0', [ReadInt(yyv[yysp-3].yyString, RtOK), ReadMsgParam(yyv[yysp-1].yyString, RtOK)]); 
       end;
  85 : begin
         yyval.yyString := Format('%d 0 0', [ReadInt(yyv[yysp-1].yyString, RtOK)]); 
       end;
  86 : begin
         if Assigned(SymTable.O[UpperCase(yyv[yysp-2].yyString) + '.reg']) then
         SymTable.S[UpperCase(yyv[yysp-2].yyString) + '.reg'] := yyv[yysp-0].yyString
         else
         yyerror(Format('"%s" bad variable', [yyv[yysp-2].yyString])); 
       end;
  87 : begin
         EmitMsg(ToStr(yyv[yysp-0].yyString)); 
       end;
  88 : begin
         yyval := yyv[yysp-0];
       end;
  89 : begin
         yyval := yyv[yysp-0];
       end;
  90 : begin
         yyval := yyv[yysp-0];
       end;
  91 : begin
         RegWrite(RegAlloc('boolean'), RegTable.O[yyv[yysp-2].yyString + '.value'].Compare(RegTable.O[yyv[yysp-0].yyString + '.value']) = cpEqu); 
       end;
  92 : begin
         RegWrite(RegAlloc('boolean'), RegTable.O[yyv[yysp-2].yyString + '.value'].Compare(RegTable.O[yyv[yysp-0].yyString + '.value']) <> cpEqu); 
       end;
  93 : begin
         RegWrite(RegAlloc('boolean'), RegTable.O[yyv[yysp-2].yyString + '.value'].Compare(RegTable.O[yyv[yysp-0].yyString + '.value']) = cpLess); 
       end;
  94 : begin
         RegWrite(RegAlloc('boolean'), RegTable.O[yyv[yysp-2].yyString + '.value'].Compare(RegTable.O[yyv[yysp-0].yyString + '.value']) = cpGreat); 
       end;
  95 : begin
         RegWrite(RegAlloc('boolean'), RegTable.O[yyv[yysp-2].yyString + '.value'].Compare(RegTable.O[yyv[yysp-0].yyString + '.value']) in [cpLess, cpEqu]); 
       end;
  96 : begin
         RegWrite(RegAlloc('boolean'), RegTable.O[yyv[yysp-2].yyString + '.value'].Compare(RegTable.O[yyv[yysp-0].yyString + '.value']) in [cpGreat, cpEqu]); 
       end;
  97 : begin
         yyval := yyv[yysp-0];
       end;
  98 : begin
         yyval := yyv[yysp-0];
       end;
  99 : begin
         yyval.yyString := RegPlus(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
 100 : begin
         yyval.yyString := RegMinus(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
 101 : begin
         yyval.yyString := RegOr(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
 102 : begin
         yyval := yyv[yysp-0];
       end;
 103 : begin
         yyval.yyString := RegTimes(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
 104 : begin
         yyval.yyString := RegDivide(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
 105 : begin
         yyval.yyString := RegDiv(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
 106 : begin
         yyval.yyString := RegMod(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
 107 : begin
         yyval.yyString := RegAnd(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
 108 : begin
         yyval.yyString := yyv[yysp-0].yyString; 
       end;
 109 : begin
         yyval.yyString := RegNeg(yyv[yysp-0].yyString, RtOK); 
       end;
 110 : begin
         yyval := yyv[yysp-0];
       end;
 111 : begin
         yyval := yyv[yysp-0];
       end;
 112 : begin
         yyval.yyString := RegPower(yyv[yysp-2].yyString, yyv[yysp-0].yyString, RtOK); 
       end;
 113 : begin
         yyval.yyString := ReadValue(yyv[yysp-0].yyString, RtOK); 
       end;
 114 : begin
         yyval := yyv[yysp-0];
       end;
 115 : begin
         yyval := yyv[yysp-0];
       end;
 116 : begin
         yyval.yyString := yyv[yysp-1].yyString; 
       end;
 117 : begin
         yyval.yyString := RegNot(yyv[yysp-0].yyString, RtOK); 
       end;
 118 : begin
         yyval.yyString := RegAlloc('real'); RegWrite(yyval.yyString, arccos(ReadReal(yyv[yysp-0].yyString, RtOK))); 
       end;
 119 : begin
         yyval.yyString := RegAlloc('real'); RegWrite(yyval.yyString, arcsin(ReadReal(yyv[yysp-0].yyString, RtOK))); 
       end;
 120 : begin
         yyval.yyString := RegAlloc('real'); RegWrite(yyval.yyString, cos(ReadReal(yyv[yysp-0].yyString, RtOK))); 
       end;
 121 : begin
         yyval.yyString := RegAlloc('real'); RegWrite(yyval.yyString, exp(ReadReal(yyv[yysp-0].yyString, RtOK))); 
       end;
 122 : begin
         yyval.yyString := RegAlloc('real'); RegWrite(yyval.yyString, log10(ReadReal(yyv[yysp-0].yyString, RtOK))); 
       end;
 123 : begin
         yyval.yyString := RegAlloc('int'); RegWrite(yyval.yyString, Pred(ReadInt(yyv[yysp-0].yyString, RtOK))); 
       end;
 124 : begin
         yyval.yyString := RegAlloc('int'); RegWrite(yyval.yyString, Round(ReadReal(yyv[yysp-0].yyString, RtOK))); 
       end;
 125 : begin
         yyval.yyString := RegAlloc('real'); RegWrite(yyval.yyString, sin(ReadReal(yyv[yysp-0].yyString, RtOK))); 
       end;
 126 : begin
         yyval.yyString := RegAlloc('int'); RegWrite(yyval.yyString, Succ(ReadInt(yyv[yysp-0].yyString, RtOK))); 
       end;
 127 : begin
         yyval.yyString := RegAlloc('string'); RegWrite(yyval.yyString, ToStr(yyv[yysp-0].yyString)); 
       end;
 128 : begin
         yyval.yyString := RegAlloc('int'); RegWrite(yyval.yyString, Trunc(ReadReal(yyv[yysp-0].yyString, RtOK))); 
       end;
 129 : begin
         yyval.yyString := StrVal(yyv[yysp-0].yyString, RtOK); 
       end;
 130 : begin
         yyval.yyString := yyv[yysp-1].yyString; 
       end;
 131 : begin
         yyval.yyString := yyv[yysp-2].yyString + ' ' + yyv[yysp-0].yyString; 
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
 142 : begin
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

yynacts   = 1113;
yyngotos  = 563;
yynstates = 234;
yynrules  = 142;

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
  ( sym: 315; act: 44 ),
  ( sym: 321; act: 9 ),
  ( sym: 273; act: -67 ),
  ( sym: 303; act: -67 ),
{ 22: }
{ 23: }
  ( sym: 303; act: 6 ),
{ 24: }
  ( sym: 262; act: 47 ),
  ( sym: 263; act: 48 ),
{ 25: }
{ 26: }
{ 27: }
  ( sym: 286; act: 60 ),
  ( sym: 288; act: 61 ),
  ( sym: 291; act: 62 ),
  ( sym: 295; act: 63 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
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
{ 37: }
  ( sym: 273; act: 68 ),
  ( sym: 303; act: 6 ),
{ 38: }
{ 39: }
  ( sym: 266; act: 69 ),
  ( sym: 273; act: -75 ),
  ( sym: 303; act: -75 ),
{ 40: }
  ( sym: 265; act: 70 ),
  ( sym: 273; act: -69 ),
  ( sym: 303; act: -69 ),
{ 41: }
  ( sym: 264; act: 71 ),
  ( sym: 273; act: -68 ),
  ( sym: 303; act: -68 ),
{ 42: }
  ( sym: 302; act: 72 ),
  ( sym: 273; act: -64 ),
  ( sym: 303; act: -64 ),
{ 43: }
  ( sym: 260; act: 73 ),
  ( sym: 264; act: 74 ),
  ( sym: 265; act: 75 ),
  ( sym: 266; act: 76 ),
  ( sym: 282; act: 77 ),
  ( sym: 302; act: 78 ),
{ 44: }
  ( sym: 286; act: 80 ),
{ 45: }
  ( sym: 321; act: 9 ),
  ( sym: 261; act: -46 ),
{ 46: }
  ( sym: 321; act: 9 ),
{ 47: }
  ( sym: 280; act: 85 ),
  ( sym: 297; act: 86 ),
  ( sym: 310; act: 87 ),
  ( sym: 321; act: 9 ),
{ 48: }
{ 49: }
{ 50: }
{ 51: }
{ 52: }
{ 53: }
{ 54: }
  ( sym: 307; act: 88 ),
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
{ 55: }
{ 56: }
{ 57: }
  ( sym: 257; act: 89 ),
  ( sym: 269; act: 90 ),
  ( sym: 289; act: 91 ),
  ( sym: 305; act: 92 ),
  ( sym: 306; act: 93 ),
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
{ 58: }
  ( sym: 274; act: 94 ),
  ( sym: 277; act: 95 ),
  ( sym: 278; act: 96 ),
  ( sym: 284; act: 97 ),
  ( sym: 287; act: 98 ),
  ( sym: 288; act: 99 ),
  ( sym: 292; act: 100 ),
  ( sym: 294; act: 101 ),
  ( sym: 295; act: 102 ),
  ( sym: 301; act: -13 ),
  ( sym: 303; act: -13 ),
{ 59: }
  ( sym: 303; act: 6 ),
{ 60: }
  ( sym: 286; act: 60 ),
  ( sym: 288; act: 61 ),
  ( sym: 291; act: 62 ),
  ( sym: 295; act: 63 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 61: }
  ( sym: 286; act: 60 ),
  ( sym: 288; act: 61 ),
  ( sym: 291; act: 62 ),
  ( sym: 295; act: 63 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 62: }
  ( sym: 286; act: 60 ),
  ( sym: 291; act: 62 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 63: }
  ( sym: 286; act: 60 ),
  ( sym: 288; act: 61 ),
  ( sym: 291; act: 62 ),
  ( sym: 295; act: 63 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 64: }
{ 65: }
{ 66: }
{ 67: }
  ( sym: 261; act: 21 ),
  ( sym: 315; act: 44 ),
  ( sym: 321; act: 9 ),
  ( sym: 273; act: -67 ),
  ( sym: 303; act: -67 ),
{ 68: }
{ 69: }
  ( sym: 321; act: 9 ),
{ 70: }
  ( sym: 321; act: 9 ),
{ 71: }
  ( sym: 321; act: 9 ),
{ 72: }
  ( sym: 283; act: 113 ),
{ 73: }
  ( sym: 256; act: 123 ),
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 74: }
  ( sym: 321; act: 9 ),
{ 75: }
  ( sym: 321; act: 9 ),
{ 76: }
  ( sym: 282; act: 143 ),
  ( sym: 321; act: 9 ),
{ 77: }
  ( sym: 256; act: 123 ),
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 78: }
  ( sym: 283; act: 113 ),
{ 79: }
{ 80: }
  ( sym: 256; act: 123 ),
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 81: }
{ 82: }
{ 83: }
{ 84: }
{ 85: }
{ 86: }
{ 87: }
{ 88: }
  ( sym: 286; act: 60 ),
  ( sym: 291; act: 62 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 89: }
  ( sym: 286; act: 60 ),
  ( sym: 288; act: 61 ),
  ( sym: 291; act: 62 ),
  ( sym: 295; act: 63 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 90: }
  ( sym: 286; act: 60 ),
  ( sym: 288; act: 61 ),
  ( sym: 291; act: 62 ),
  ( sym: 295; act: 63 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 91: }
  ( sym: 286; act: 60 ),
  ( sym: 288; act: 61 ),
  ( sym: 291; act: 62 ),
  ( sym: 295; act: 63 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 92: }
  ( sym: 286; act: 60 ),
  ( sym: 288; act: 61 ),
  ( sym: 291; act: 62 ),
  ( sym: 295; act: 63 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 93: }
  ( sym: 286; act: 60 ),
  ( sym: 288; act: 61 ),
  ( sym: 291; act: 62 ),
  ( sym: 295; act: 63 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 94: }
  ( sym: 286; act: 60 ),
  ( sym: 288; act: 61 ),
  ( sym: 291; act: 62 ),
  ( sym: 295; act: 63 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 95: }
  ( sym: 286; act: 60 ),
  ( sym: 288; act: 61 ),
  ( sym: 291; act: 62 ),
  ( sym: 295; act: 63 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 96: }
  ( sym: 286; act: 60 ),
  ( sym: 288; act: 61 ),
  ( sym: 291; act: 62 ),
  ( sym: 295; act: 63 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 97: }
  ( sym: 286; act: 60 ),
  ( sym: 288; act: 61 ),
  ( sym: 291; act: 62 ),
  ( sym: 295; act: 63 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 98: }
  ( sym: 286; act: 60 ),
  ( sym: 288; act: 61 ),
  ( sym: 291; act: 62 ),
  ( sym: 295; act: 63 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 99: }
  ( sym: 286; act: 60 ),
  ( sym: 288; act: 61 ),
  ( sym: 291; act: 62 ),
  ( sym: 295; act: 63 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 100: }
  ( sym: 286; act: 60 ),
  ( sym: 288; act: 61 ),
  ( sym: 291; act: 62 ),
  ( sym: 295; act: 63 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 101: }
  ( sym: 286; act: 60 ),
  ( sym: 288; act: 61 ),
  ( sym: 291; act: 62 ),
  ( sym: 295; act: 63 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 102: }
  ( sym: 286; act: 60 ),
  ( sym: 288; act: 61 ),
  ( sym: 291; act: 62 ),
  ( sym: 295; act: 63 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 103: }
{ 104: }
  ( sym: 301; act: 165 ),
{ 105: }
{ 106: }
{ 107: }
{ 108: }
{ 109: }
{ 110: }
{ 111: }
{ 112: }
{ 113: }
  ( sym: 256; act: 123 ),
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 114: }
{ 115: }
{ 116: }
  ( sym: 307; act: 167 ),
  ( sym: 257; act: -111 ),
  ( sym: 263; act: -111 ),
  ( sym: 269; act: -111 ),
  ( sym: 273; act: -111 ),
  ( sym: 274; act: -111 ),
  ( sym: 277; act: -111 ),
  ( sym: 278; act: -111 ),
  ( sym: 284; act: -111 ),
  ( sym: 287; act: -111 ),
  ( sym: 288; act: -111 ),
  ( sym: 289; act: -111 ),
  ( sym: 292; act: -111 ),
  ( sym: 294; act: -111 ),
  ( sym: 295; act: -111 ),
  ( sym: 299; act: -111 ),
  ( sym: 300; act: -111 ),
  ( sym: 301; act: -111 ),
  ( sym: 303; act: -111 ),
  ( sym: 305; act: -111 ),
  ( sym: 306; act: -111 ),
{ 117: }
{ 118: }
{ 119: }
  ( sym: 257; act: 168 ),
  ( sym: 269; act: 169 ),
  ( sym: 289; act: 170 ),
  ( sym: 305; act: 171 ),
  ( sym: 306; act: 172 ),
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
{ 120: }
  ( sym: 274; act: 173 ),
  ( sym: 277; act: 174 ),
  ( sym: 278; act: 175 ),
  ( sym: 284; act: 176 ),
  ( sym: 287; act: 177 ),
  ( sym: 288; act: 178 ),
  ( sym: 292; act: 179 ),
  ( sym: 294; act: 180 ),
  ( sym: 295; act: 181 ),
  ( sym: 263; act: -90 ),
  ( sym: 273; act: -90 ),
  ( sym: 299; act: -90 ),
  ( sym: 300; act: -90 ),
  ( sym: 301; act: -90 ),
  ( sym: 303; act: -90 ),
{ 121: }
{ 122: }
{ 123: }
{ 124: }
  ( sym: 286; act: 80 ),
{ 125: }
  ( sym: 286; act: 80 ),
{ 126: }
  ( sym: 286; act: 80 ),
{ 127: }
  ( sym: 286; act: 80 ),
{ 128: }
  ( sym: 286; act: 80 ),
{ 129: }
  ( sym: 256; act: 123 ),
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 130: }
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 131: }
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 291; act: 131 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 132: }
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 133: }
  ( sym: 286; act: 80 ),
{ 134: }
  ( sym: 286; act: 80 ),
{ 135: }
  ( sym: 286; act: 80 ),
{ 136: }
  ( sym: 286; act: 80 ),
{ 137: }
  ( sym: 286; act: 80 ),
{ 138: }
  ( sym: 286; act: 80 ),
{ 139: }
  ( sym: 286; act: 80 ),
{ 140: }
{ 141: }
{ 142: }
{ 143: }
  ( sym: 256; act: 123 ),
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 144: }
{ 145: }
  ( sym: 299; act: 199 ),
{ 146: }
{ 147: }
{ 148: }
  ( sym: 263; act: 48 ),
  ( sym: 301; act: 201 ),
{ 149: }
{ 150: }
{ 151: }
{ 152: }
{ 153: }
{ 154: }
{ 155: }
{ 156: }
  ( sym: 288; act: 99 ),
  ( sym: 294; act: 101 ),
  ( sym: 295; act: 102 ),
  ( sym: 301; act: -14 ),
  ( sym: 303; act: -14 ),
{ 157: }
  ( sym: 288; act: 99 ),
  ( sym: 294; act: 101 ),
  ( sym: 295; act: 102 ),
  ( sym: 301; act: -19 ),
  ( sym: 303; act: -19 ),
{ 158: }
  ( sym: 288; act: 99 ),
  ( sym: 294; act: 101 ),
  ( sym: 295; act: 102 ),
  ( sym: 301; act: -17 ),
  ( sym: 303; act: -17 ),
{ 159: }
  ( sym: 288; act: 99 ),
  ( sym: 294; act: 101 ),
  ( sym: 295; act: 102 ),
  ( sym: 301; act: -18 ),
  ( sym: 303; act: -18 ),
{ 160: }
  ( sym: 288; act: 99 ),
  ( sym: 294; act: 101 ),
  ( sym: 295; act: 102 ),
  ( sym: 301; act: -16 ),
  ( sym: 303; act: -16 ),
{ 161: }
  ( sym: 257; act: 89 ),
  ( sym: 269; act: 90 ),
  ( sym: 289; act: 91 ),
  ( sym: 305; act: 92 ),
  ( sym: 306; act: 93 ),
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
{ 162: }
  ( sym: 288; act: 99 ),
  ( sym: 294; act: 101 ),
  ( sym: 295; act: 102 ),
  ( sym: 301; act: -15 ),
  ( sym: 303; act: -15 ),
{ 163: }
  ( sym: 257; act: 89 ),
  ( sym: 269; act: 90 ),
  ( sym: 289; act: 91 ),
  ( sym: 305; act: 92 ),
  ( sym: 306; act: 93 ),
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
{ 164: }
  ( sym: 257; act: 89 ),
  ( sym: 269; act: 90 ),
  ( sym: 289; act: 91 ),
  ( sym: 305; act: 92 ),
  ( sym: 306; act: 93 ),
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
{ 165: }
{ 166: }
  ( sym: 263; act: 202 ),
  ( sym: 300; act: 203 ),
{ 167: }
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 291; act: 131 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 168: }
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 169: }
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 170: }
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 171: }
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 172: }
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 173: }
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 174: }
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 175: }
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 176: }
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 177: }
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 178: }
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 179: }
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 180: }
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 181: }
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 182: }
{ 183: }
{ 184: }
{ 185: }
{ 186: }
{ 187: }
  ( sym: 301; act: 219 ),
{ 188: }
{ 189: }
{ 190: }
{ 191: }
{ 192: }
{ 193: }
{ 194: }
{ 195: }
{ 196: }
{ 197: }
{ 198: }
  ( sym: 299; act: 220 ),
{ 199: }
  ( sym: 266; act: 221 ),
{ 200: }
  ( sym: 256; act: 123 ),
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 201: }
{ 202: }
  ( sym: 256; act: 123 ),
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 203: }
{ 204: }
{ 205: }
{ 206: }
{ 207: }
{ 208: }
{ 209: }
{ 210: }
  ( sym: 288; act: 178 ),
  ( sym: 294; act: 180 ),
  ( sym: 295; act: 181 ),
  ( sym: 263; act: -91 ),
  ( sym: 273; act: -91 ),
  ( sym: 299; act: -91 ),
  ( sym: 300; act: -91 ),
  ( sym: 301; act: -91 ),
  ( sym: 303; act: -91 ),
{ 211: }
  ( sym: 288; act: 178 ),
  ( sym: 294; act: 180 ),
  ( sym: 295; act: 181 ),
  ( sym: 263; act: -96 ),
  ( sym: 273; act: -96 ),
  ( sym: 299; act: -96 ),
  ( sym: 300; act: -96 ),
  ( sym: 301; act: -96 ),
  ( sym: 303; act: -96 ),
{ 212: }
  ( sym: 288; act: 178 ),
  ( sym: 294; act: 180 ),
  ( sym: 295; act: 181 ),
  ( sym: 263; act: -94 ),
  ( sym: 273; act: -94 ),
  ( sym: 299; act: -94 ),
  ( sym: 300; act: -94 ),
  ( sym: 301; act: -94 ),
  ( sym: 303; act: -94 ),
{ 213: }
  ( sym: 288; act: 178 ),
  ( sym: 294; act: 180 ),
  ( sym: 295; act: 181 ),
  ( sym: 263; act: -95 ),
  ( sym: 273; act: -95 ),
  ( sym: 299; act: -95 ),
  ( sym: 300; act: -95 ),
  ( sym: 301; act: -95 ),
  ( sym: 303; act: -95 ),
{ 214: }
  ( sym: 288; act: 178 ),
  ( sym: 294; act: 180 ),
  ( sym: 295; act: 181 ),
  ( sym: 263; act: -93 ),
  ( sym: 273; act: -93 ),
  ( sym: 299; act: -93 ),
  ( sym: 300; act: -93 ),
  ( sym: 301; act: -93 ),
  ( sym: 303; act: -93 ),
{ 215: }
  ( sym: 257; act: 168 ),
  ( sym: 269; act: 169 ),
  ( sym: 289; act: 170 ),
  ( sym: 305; act: 171 ),
  ( sym: 306; act: 172 ),
  ( sym: 263; act: -100 ),
  ( sym: 273; act: -100 ),
  ( sym: 274; act: -100 ),
  ( sym: 277; act: -100 ),
  ( sym: 278; act: -100 ),
  ( sym: 284; act: -100 ),
  ( sym: 287; act: -100 ),
  ( sym: 288; act: -100 ),
  ( sym: 292; act: -100 ),
  ( sym: 294; act: -100 ),
  ( sym: 295; act: -100 ),
  ( sym: 299; act: -100 ),
  ( sym: 300; act: -100 ),
  ( sym: 301; act: -100 ),
  ( sym: 303; act: -100 ),
{ 216: }
  ( sym: 288; act: 178 ),
  ( sym: 294; act: 180 ),
  ( sym: 295; act: 181 ),
  ( sym: 263; act: -92 ),
  ( sym: 273; act: -92 ),
  ( sym: 299; act: -92 ),
  ( sym: 300; act: -92 ),
  ( sym: 301; act: -92 ),
  ( sym: 303; act: -92 ),
{ 217: }
  ( sym: 257; act: 168 ),
  ( sym: 269; act: 169 ),
  ( sym: 289; act: 170 ),
  ( sym: 305; act: 171 ),
  ( sym: 306; act: 172 ),
  ( sym: 263; act: -101 ),
  ( sym: 273; act: -101 ),
  ( sym: 274; act: -101 ),
  ( sym: 277; act: -101 ),
  ( sym: 278; act: -101 ),
  ( sym: 284; act: -101 ),
  ( sym: 287; act: -101 ),
  ( sym: 288; act: -101 ),
  ( sym: 292; act: -101 ),
  ( sym: 294; act: -101 ),
  ( sym: 295; act: -101 ),
  ( sym: 299; act: -101 ),
  ( sym: 300; act: -101 ),
  ( sym: 301; act: -101 ),
  ( sym: 303; act: -101 ),
{ 218: }
  ( sym: 257; act: 168 ),
  ( sym: 269; act: 169 ),
  ( sym: 289; act: 170 ),
  ( sym: 305; act: 171 ),
  ( sym: 306; act: 172 ),
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
{ 219: }
{ 220: }
  ( sym: 321; act: 9 ),
{ 221: }
  ( sym: 282; act: 226 ),
  ( sym: 321; act: 9 ),
{ 222: }
{ 223: }
  ( sym: 263; act: 227 ),
  ( sym: 300; act: 228 ),
{ 224: }
{ 225: }
{ 226: }
  ( sym: 256; act: 123 ),
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 227: }
  ( sym: 256; act: 123 ),
  ( sym: 258; act: 124 ),
  ( sym: 259; act: 125 ),
  ( sym: 268; act: 126 ),
  ( sym: 275; act: 127 ),
  ( sym: 285; act: 128 ),
  ( sym: 286; act: 129 ),
  ( sym: 288; act: 130 ),
  ( sym: 291; act: 131 ),
  ( sym: 295; act: 132 ),
  ( sym: 296; act: 133 ),
  ( sym: 298; act: 134 ),
  ( sym: 304; act: 135 ),
  ( sym: 308; act: 136 ),
  ( sym: 309; act: 137 ),
  ( sym: 311; act: 138 ),
  ( sym: 312; act: 139 ),
  ( sym: 318; act: 64 ),
  ( sym: 319; act: 65 ),
  ( sym: 320; act: 66 ),
  ( sym: 321; act: 9 ),
{ 228: }
{ 229: }
  ( sym: 299; act: 231 ),
{ 230: }
  ( sym: 300; act: 232 ),
{ 231: }
  ( sym: 321; act: 9 )
{ 232: }
{ 233: }
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
  ( sym: -56; act: 28 ),
  ( sym: -55; act: 29 ),
  ( sym: -54; act: 30 ),
  ( sym: -53; act: 31 ),
  ( sym: -52; act: 32 ),
  ( sym: -51; act: 33 ),
  ( sym: -50; act: 34 ),
  ( sym: -49; act: 35 ),
  ( sym: -48; act: 36 ),
  ( sym: -47; act: 37 ),
  ( sym: -46; act: 38 ),
  ( sym: -31; act: 39 ),
  ( sym: -30; act: 40 ),
  ( sym: -29; act: 41 ),
  ( sym: -27; act: 42 ),
  ( sym: -10; act: 43 ),
{ 22: }
{ 23: }
  ( sym: -36; act: 45 ),
{ 24: }
  ( sym: -38; act: 46 ),
{ 25: }
{ 26: }
{ 27: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 55 ),
  ( sym: -5; act: 56 ),
  ( sym: -4; act: 57 ),
  ( sym: -3; act: 58 ),
  ( sym: -2; act: 59 ),
{ 28: }
{ 29: }
{ 30: }
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
{ 36: }
{ 37: }
  ( sym: -36; act: 67 ),
{ 38: }
{ 39: }
{ 40: }
{ 41: }
{ 42: }
{ 43: }
{ 44: }
  ( sym: -24; act: 79 ),
{ 45: }
  ( sym: -45; act: 81 ),
  ( sym: -11; act: 24 ),
  ( sym: -10; act: 25 ),
{ 46: }
  ( sym: -10; act: 82 ),
{ 47: }
  ( sym: -32; act: 83 ),
  ( sym: -10; act: 84 ),
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
{ 58: }
{ 59: }
  ( sym: -36; act: 103 ),
{ 60: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 55 ),
  ( sym: -5; act: 56 ),
  ( sym: -4; act: 57 ),
  ( sym: -3; act: 58 ),
  ( sym: -2; act: 104 ),
{ 61: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 55 ),
  ( sym: -5; act: 105 ),
{ 62: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 106 ),
{ 63: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 55 ),
  ( sym: -5; act: 107 ),
{ 64: }
{ 65: }
{ 66: }
{ 67: }
  ( sym: -56; act: 28 ),
  ( sym: -55; act: 29 ),
  ( sym: -54; act: 30 ),
  ( sym: -53; act: 31 ),
  ( sym: -52; act: 32 ),
  ( sym: -51; act: 33 ),
  ( sym: -50; act: 34 ),
  ( sym: -49; act: 35 ),
  ( sym: -48; act: 108 ),
  ( sym: -46; act: 38 ),
  ( sym: -31; act: 39 ),
  ( sym: -30; act: 40 ),
  ( sym: -29; act: 41 ),
  ( sym: -27; act: 42 ),
  ( sym: -10; act: 43 ),
{ 68: }
{ 69: }
  ( sym: -10; act: 109 ),
{ 70: }
  ( sym: -10; act: 110 ),
{ 71: }
  ( sym: -10; act: 111 ),
{ 72: }
  ( sym: -28; act: 112 ),
{ 73: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 119 ),
  ( sym: -14; act: 120 ),
  ( sym: -13; act: 121 ),
  ( sym: -10; act: 122 ),
{ 74: }
  ( sym: -10; act: 140 ),
{ 75: }
  ( sym: -10; act: 141 ),
{ 76: }
  ( sym: -10; act: 142 ),
{ 77: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 119 ),
  ( sym: -14; act: 120 ),
  ( sym: -13; act: 144 ),
  ( sym: -12; act: 145 ),
  ( sym: -10; act: 122 ),
{ 78: }
  ( sym: -28; act: 146 ),
{ 79: }
{ 80: }
  ( sym: -26; act: 147 ),
  ( sym: -25; act: 148 ),
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 119 ),
  ( sym: -14; act: 120 ),
  ( sym: -13; act: 149 ),
  ( sym: -10; act: 122 ),
{ 81: }
{ 82: }
{ 83: }
{ 84: }
{ 85: }
{ 86: }
{ 87: }
{ 88: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 150 ),
{ 89: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 55 ),
  ( sym: -5; act: 151 ),
{ 90: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 55 ),
  ( sym: -5; act: 152 ),
{ 91: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 55 ),
  ( sym: -5; act: 153 ),
{ 92: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 55 ),
  ( sym: -5; act: 154 ),
{ 93: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 55 ),
  ( sym: -5; act: 155 ),
{ 94: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 55 ),
  ( sym: -5; act: 56 ),
  ( sym: -4; act: 57 ),
  ( sym: -3; act: 156 ),
{ 95: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 55 ),
  ( sym: -5; act: 56 ),
  ( sym: -4; act: 57 ),
  ( sym: -3; act: 157 ),
{ 96: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 55 ),
  ( sym: -5; act: 56 ),
  ( sym: -4; act: 57 ),
  ( sym: -3; act: 158 ),
{ 97: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 55 ),
  ( sym: -5; act: 56 ),
  ( sym: -4; act: 57 ),
  ( sym: -3; act: 159 ),
{ 98: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 55 ),
  ( sym: -5; act: 56 ),
  ( sym: -4; act: 57 ),
  ( sym: -3; act: 160 ),
{ 99: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 55 ),
  ( sym: -5; act: 56 ),
  ( sym: -4; act: 161 ),
{ 100: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 55 ),
  ( sym: -5; act: 56 ),
  ( sym: -4; act: 57 ),
  ( sym: -3; act: 162 ),
{ 101: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 55 ),
  ( sym: -5; act: 56 ),
  ( sym: -4; act: 163 ),
{ 102: }
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 52 ),
  ( sym: -10; act: 53 ),
  ( sym: -7; act: 54 ),
  ( sym: -6; act: 55 ),
  ( sym: -5; act: 56 ),
  ( sym: -4; act: 164 ),
{ 103: }
{ 104: }
{ 105: }
{ 106: }
{ 107: }
{ 108: }
{ 109: }
{ 110: }
{ 111: }
{ 112: }
{ 113: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 119 ),
  ( sym: -14; act: 120 ),
  ( sym: -13; act: 166 ),
  ( sym: -10; act: 122 ),
{ 114: }
{ 115: }
{ 116: }
{ 117: }
{ 118: }
{ 119: }
{ 120: }
{ 121: }
{ 122: }
{ 123: }
{ 124: }
  ( sym: -24; act: 182 ),
{ 125: }
  ( sym: -24; act: 183 ),
{ 126: }
  ( sym: -24; act: 184 ),
{ 127: }
  ( sym: -24; act: 185 ),
{ 128: }
  ( sym: -24; act: 186 ),
{ 129: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 119 ),
  ( sym: -14; act: 120 ),
  ( sym: -13; act: 187 ),
  ( sym: -10; act: 122 ),
{ 130: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 188 ),
  ( sym: -10; act: 122 ),
{ 131: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 189 ),
  ( sym: -10; act: 122 ),
{ 132: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 190 ),
  ( sym: -10; act: 122 ),
{ 133: }
  ( sym: -24; act: 191 ),
{ 134: }
  ( sym: -24; act: 192 ),
{ 135: }
  ( sym: -24; act: 193 ),
{ 136: }
  ( sym: -24; act: 194 ),
{ 137: }
  ( sym: -24; act: 195 ),
{ 138: }
  ( sym: -24; act: 196 ),
{ 139: }
  ( sym: -24; act: 197 ),
{ 140: }
{ 141: }
{ 142: }
{ 143: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 119 ),
  ( sym: -14; act: 120 ),
  ( sym: -13; act: 144 ),
  ( sym: -12; act: 198 ),
  ( sym: -10; act: 122 ),
{ 144: }
{ 145: }
{ 146: }
{ 147: }
{ 148: }
  ( sym: -38; act: 200 ),
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
{ 163: }
{ 164: }
{ 165: }
{ 166: }
{ 167: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 204 ),
  ( sym: -10; act: 122 ),
{ 168: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 205 ),
  ( sym: -10; act: 122 ),
{ 169: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 206 ),
  ( sym: -10; act: 122 ),
{ 170: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 207 ),
  ( sym: -10; act: 122 ),
{ 171: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 208 ),
  ( sym: -10; act: 122 ),
{ 172: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 209 ),
  ( sym: -10; act: 122 ),
{ 173: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 119 ),
  ( sym: -14; act: 210 ),
  ( sym: -10; act: 122 ),
{ 174: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 119 ),
  ( sym: -14; act: 211 ),
  ( sym: -10; act: 122 ),
{ 175: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 119 ),
  ( sym: -14; act: 212 ),
  ( sym: -10; act: 122 ),
{ 176: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 119 ),
  ( sym: -14; act: 213 ),
  ( sym: -10; act: 122 ),
{ 177: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 119 ),
  ( sym: -14; act: 214 ),
  ( sym: -10; act: 122 ),
{ 178: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 215 ),
  ( sym: -10; act: 122 ),
{ 179: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 119 ),
  ( sym: -14; act: 216 ),
  ( sym: -10; act: 122 ),
{ 180: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 217 ),
  ( sym: -10; act: 122 ),
{ 181: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 218 ),
  ( sym: -10; act: 122 ),
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
{ 197: }
{ 198: }
{ 199: }
{ 200: }
  ( sym: -26; act: 222 ),
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 119 ),
  ( sym: -14; act: 120 ),
  ( sym: -13; act: 149 ),
  ( sym: -10; act: 122 ),
{ 201: }
{ 202: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 119 ),
  ( sym: -14; act: 120 ),
  ( sym: -13; act: 223 ),
  ( sym: -10; act: 122 ),
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
{ 218: }
{ 219: }
{ 220: }
  ( sym: -10; act: 224 ),
{ 221: }
  ( sym: -10; act: 225 ),
{ 222: }
{ 223: }
{ 224: }
{ 225: }
{ 226: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 119 ),
  ( sym: -14; act: 120 ),
  ( sym: -13; act: 144 ),
  ( sym: -12; act: 229 ),
  ( sym: -10; act: 122 ),
{ 227: }
  ( sym: -23; act: 114 ),
  ( sym: -22; act: 49 ),
  ( sym: -21; act: 50 ),
  ( sym: -20; act: 51 ),
  ( sym: -19; act: 115 ),
  ( sym: -18; act: 116 ),
  ( sym: -17; act: 117 ),
  ( sym: -16; act: 118 ),
  ( sym: -15; act: 119 ),
  ( sym: -14; act: 120 ),
  ( sym: -13; act: 230 ),
  ( sym: -10; act: 122 ),
{ 228: }
{ 229: }
{ 230: }
{ 231: }
  ( sym: -10; act: 233 )
{ 232: }
{ 233: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 0,
{ 3: } 0,
{ 4: } 0,
{ 5: } 0,
{ 6: } -141,
{ 7: } -2,
{ 8: } -4,
{ 9: } -140,
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
{ 28: } -70,
{ 29: } -66,
{ 30: } -65,
{ 31: } -62,
{ 32: } -61,
{ 33: } -60,
{ 34: } -59,
{ 35: } -58,
{ 36: } -57,
{ 37: } 0,
{ 38: } -63,
{ 39: } 0,
{ 40: } 0,
{ 41: } 0,
{ 42: } 0,
{ 43: } 0,
{ 44: } 0,
{ 45: } 0,
{ 46: } 0,
{ 47: } 0,
{ 48: } -142,
{ 49: } -137,
{ 50: } -136,
{ 51: } -134,
{ 52: } -37,
{ 53: } -35,
{ 54: } 0,
{ 55: } -32,
{ 56: } -24,
{ 57: } 0,
{ 58: } 0,
{ 59: } 0,
{ 60: } 0,
{ 61: } 0,
{ 62: } 0,
{ 63: } 0,
{ 64: } -139,
{ 65: } -138,
{ 66: } -135,
{ 67: } 0,
{ 68: } -55,
{ 69: } 0,
{ 70: } 0,
{ 71: } 0,
{ 72: } 0,
{ 73: } 0,
{ 74: } 0,
{ 75: } 0,
{ 76: } 0,
{ 77: } 0,
{ 78: } 0,
{ 79: } -87,
{ 80: } 0,
{ 81: } -48,
{ 82: } -5,
{ 83: } -53,
{ 84: } -89,
{ 85: } -50,
{ 86: } -51,
{ 87: } -52,
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
{ 99: } 0,
{ 100: } 0,
{ 101: } 0,
{ 102: } 0,
{ 103: } -12,
{ 104: } 0,
{ 105: } -31,
{ 106: } -38,
{ 107: } -30,
{ 108: } -56,
{ 109: } -80,
{ 110: } -74,
{ 111: } -72,
{ 112: } -82,
{ 113: } 0,
{ 114: } -115,
{ 115: } -114,
{ 116: } 0,
{ 117: } -110,
{ 118: } -102,
{ 119: } 0,
{ 120: } 0,
{ 121: } -86,
{ 122: } -113,
{ 123: } -97,
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
{ 137: } 0,
{ 138: } 0,
{ 139: } 0,
{ 140: } -71,
{ 141: } -73,
{ 142: } -79,
{ 143: } 0,
{ 144: } -88,
{ 145: } 0,
{ 146: } -81,
{ 147: } -132,
{ 148: } 0,
{ 149: } -133,
{ 150: } -34,
{ 151: } -29,
{ 152: } -27,
{ 153: } -28,
{ 154: } -26,
{ 155: } -25,
{ 156: } 0,
{ 157: } 0,
{ 158: } 0,
{ 159: } 0,
{ 160: } 0,
{ 161: } 0,
{ 162: } 0,
{ 163: } 0,
{ 164: } 0,
{ 165: } -36,
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
{ 178: } 0,
{ 179: } 0,
{ 180: } 0,
{ 181: } 0,
{ 182: } -118,
{ 183: } -119,
{ 184: } -120,
{ 185: } -121,
{ 186: } -122,
{ 187: } 0,
{ 188: } -109,
{ 189: } -117,
{ 190: } -108,
{ 191: } -123,
{ 192: } -124,
{ 193: } -125,
{ 194: } -126,
{ 195: } -127,
{ 196: } -128,
{ 197: } -129,
{ 198: } 0,
{ 199: } 0,
{ 200: } 0,
{ 201: } -130,
{ 202: } 0,
{ 203: } -85,
{ 204: } -112,
{ 205: } -107,
{ 206: } -105,
{ 207: } -106,
{ 208: } -104,
{ 209: } -103,
{ 210: } 0,
{ 211: } 0,
{ 212: } 0,
{ 213: } 0,
{ 214: } 0,
{ 215: } 0,
{ 216: } 0,
{ 217: } 0,
{ 218: } 0,
{ 219: } -116,
{ 220: } 0,
{ 221: } 0,
{ 222: } -131,
{ 223: } 0,
{ 224: } -78,
{ 225: } -77,
{ 226: } 0,
{ 227: } 0,
{ 228: } -84,
{ 229: } 0,
{ 230: } 0,
{ 231: } 0,
{ 232: } -83,
{ 233: } -76
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
{ 22: } 25,
{ 23: } 25,
{ 24: } 26,
{ 25: } 28,
{ 26: } 28,
{ 27: } 28,
{ 28: } 36,
{ 29: } 36,
{ 30: } 36,
{ 31: } 36,
{ 32: } 36,
{ 33: } 36,
{ 34: } 36,
{ 35: } 36,
{ 36: } 36,
{ 37: } 36,
{ 38: } 38,
{ 39: } 38,
{ 40: } 41,
{ 41: } 44,
{ 42: } 47,
{ 43: } 50,
{ 44: } 56,
{ 45: } 57,
{ 46: } 59,
{ 47: } 60,
{ 48: } 64,
{ 49: } 64,
{ 50: } 64,
{ 51: } 64,
{ 52: } 64,
{ 53: } 64,
{ 54: } 64,
{ 55: } 81,
{ 56: } 81,
{ 57: } 81,
{ 58: } 97,
{ 59: } 108,
{ 60: } 109,
{ 61: } 117,
{ 62: } 125,
{ 63: } 131,
{ 64: } 139,
{ 65: } 139,
{ 66: } 139,
{ 67: } 139,
{ 68: } 144,
{ 69: } 144,
{ 70: } 145,
{ 71: } 146,
{ 72: } 147,
{ 73: } 148,
{ 74: } 169,
{ 75: } 170,
{ 76: } 171,
{ 77: } 173,
{ 78: } 194,
{ 79: } 195,
{ 80: } 195,
{ 81: } 216,
{ 82: } 216,
{ 83: } 216,
{ 84: } 216,
{ 85: } 216,
{ 86: } 216,
{ 87: } 216,
{ 88: } 216,
{ 89: } 222,
{ 90: } 230,
{ 91: } 238,
{ 92: } 246,
{ 93: } 254,
{ 94: } 262,
{ 95: } 270,
{ 96: } 278,
{ 97: } 286,
{ 98: } 294,
{ 99: } 302,
{ 100: } 310,
{ 101: } 318,
{ 102: } 326,
{ 103: } 334,
{ 104: } 334,
{ 105: } 335,
{ 106: } 335,
{ 107: } 335,
{ 108: } 335,
{ 109: } 335,
{ 110: } 335,
{ 111: } 335,
{ 112: } 335,
{ 113: } 335,
{ 114: } 356,
{ 115: } 356,
{ 116: } 356,
{ 117: } 377,
{ 118: } 377,
{ 119: } 377,
{ 120: } 397,
{ 121: } 412,
{ 122: } 412,
{ 123: } 412,
{ 124: } 412,
{ 125: } 413,
{ 126: } 414,
{ 127: } 415,
{ 128: } 416,
{ 129: } 417,
{ 130: } 438,
{ 131: } 458,
{ 132: } 476,
{ 133: } 496,
{ 134: } 497,
{ 135: } 498,
{ 136: } 499,
{ 137: } 500,
{ 138: } 501,
{ 139: } 502,
{ 140: } 503,
{ 141: } 503,
{ 142: } 503,
{ 143: } 503,
{ 144: } 524,
{ 145: } 524,
{ 146: } 525,
{ 147: } 525,
{ 148: } 525,
{ 149: } 527,
{ 150: } 527,
{ 151: } 527,
{ 152: } 527,
{ 153: } 527,
{ 154: } 527,
{ 155: } 527,
{ 156: } 527,
{ 157: } 532,
{ 158: } 537,
{ 159: } 542,
{ 160: } 547,
{ 161: } 552,
{ 162: } 568,
{ 163: } 573,
{ 164: } 589,
{ 165: } 605,
{ 166: } 605,
{ 167: } 607,
{ 168: } 625,
{ 169: } 645,
{ 170: } 665,
{ 171: } 685,
{ 172: } 705,
{ 173: } 725,
{ 174: } 745,
{ 175: } 765,
{ 176: } 785,
{ 177: } 805,
{ 178: } 825,
{ 179: } 845,
{ 180: } 865,
{ 181: } 885,
{ 182: } 905,
{ 183: } 905,
{ 184: } 905,
{ 185: } 905,
{ 186: } 905,
{ 187: } 905,
{ 188: } 906,
{ 189: } 906,
{ 190: } 906,
{ 191: } 906,
{ 192: } 906,
{ 193: } 906,
{ 194: } 906,
{ 195: } 906,
{ 196: } 906,
{ 197: } 906,
{ 198: } 906,
{ 199: } 907,
{ 200: } 908,
{ 201: } 929,
{ 202: } 929,
{ 203: } 950,
{ 204: } 950,
{ 205: } 950,
{ 206: } 950,
{ 207: } 950,
{ 208: } 950,
{ 209: } 950,
{ 210: } 950,
{ 211: } 959,
{ 212: } 968,
{ 213: } 977,
{ 214: } 986,
{ 215: } 995,
{ 216: } 1015,
{ 217: } 1024,
{ 218: } 1044,
{ 219: } 1064,
{ 220: } 1064,
{ 221: } 1065,
{ 222: } 1067,
{ 223: } 1067,
{ 224: } 1069,
{ 225: } 1069,
{ 226: } 1069,
{ 227: } 1090,
{ 228: } 1111,
{ 229: } 1111,
{ 230: } 1112,
{ 231: } 1113,
{ 232: } 1114,
{ 233: } 1114
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
{ 21: } 24,
{ 22: } 24,
{ 23: } 25,
{ 24: } 27,
{ 25: } 27,
{ 26: } 27,
{ 27: } 35,
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
{ 44: } 56,
{ 45: } 58,
{ 46: } 59,
{ 47: } 63,
{ 48: } 63,
{ 49: } 63,
{ 50: } 63,
{ 51: } 63,
{ 52: } 63,
{ 53: } 63,
{ 54: } 80,
{ 55: } 80,
{ 56: } 80,
{ 57: } 96,
{ 58: } 107,
{ 59: } 108,
{ 60: } 116,
{ 61: } 124,
{ 62: } 130,
{ 63: } 138,
{ 64: } 138,
{ 65: } 138,
{ 66: } 138,
{ 67: } 143,
{ 68: } 143,
{ 69: } 144,
{ 70: } 145,
{ 71: } 146,
{ 72: } 147,
{ 73: } 168,
{ 74: } 169,
{ 75: } 170,
{ 76: } 172,
{ 77: } 193,
{ 78: } 194,
{ 79: } 194,
{ 80: } 215,
{ 81: } 215,
{ 82: } 215,
{ 83: } 215,
{ 84: } 215,
{ 85: } 215,
{ 86: } 215,
{ 87: } 215,
{ 88: } 221,
{ 89: } 229,
{ 90: } 237,
{ 91: } 245,
{ 92: } 253,
{ 93: } 261,
{ 94: } 269,
{ 95: } 277,
{ 96: } 285,
{ 97: } 293,
{ 98: } 301,
{ 99: } 309,
{ 100: } 317,
{ 101: } 325,
{ 102: } 333,
{ 103: } 333,
{ 104: } 334,
{ 105: } 334,
{ 106: } 334,
{ 107: } 334,
{ 108: } 334,
{ 109: } 334,
{ 110: } 334,
{ 111: } 334,
{ 112: } 334,
{ 113: } 355,
{ 114: } 355,
{ 115: } 355,
{ 116: } 376,
{ 117: } 376,
{ 118: } 376,
{ 119: } 396,
{ 120: } 411,
{ 121: } 411,
{ 122: } 411,
{ 123: } 411,
{ 124: } 412,
{ 125: } 413,
{ 126: } 414,
{ 127: } 415,
{ 128: } 416,
{ 129: } 437,
{ 130: } 457,
{ 131: } 475,
{ 132: } 495,
{ 133: } 496,
{ 134: } 497,
{ 135: } 498,
{ 136: } 499,
{ 137: } 500,
{ 138: } 501,
{ 139: } 502,
{ 140: } 502,
{ 141: } 502,
{ 142: } 502,
{ 143: } 523,
{ 144: } 523,
{ 145: } 524,
{ 146: } 524,
{ 147: } 524,
{ 148: } 526,
{ 149: } 526,
{ 150: } 526,
{ 151: } 526,
{ 152: } 526,
{ 153: } 526,
{ 154: } 526,
{ 155: } 526,
{ 156: } 531,
{ 157: } 536,
{ 158: } 541,
{ 159: } 546,
{ 160: } 551,
{ 161: } 567,
{ 162: } 572,
{ 163: } 588,
{ 164: } 604,
{ 165: } 604,
{ 166: } 606,
{ 167: } 624,
{ 168: } 644,
{ 169: } 664,
{ 170: } 684,
{ 171: } 704,
{ 172: } 724,
{ 173: } 744,
{ 174: } 764,
{ 175: } 784,
{ 176: } 804,
{ 177: } 824,
{ 178: } 844,
{ 179: } 864,
{ 180: } 884,
{ 181: } 904,
{ 182: } 904,
{ 183: } 904,
{ 184: } 904,
{ 185: } 904,
{ 186: } 904,
{ 187: } 905,
{ 188: } 905,
{ 189: } 905,
{ 190: } 905,
{ 191: } 905,
{ 192: } 905,
{ 193: } 905,
{ 194: } 905,
{ 195: } 905,
{ 196: } 905,
{ 197: } 905,
{ 198: } 906,
{ 199: } 907,
{ 200: } 928,
{ 201: } 928,
{ 202: } 949,
{ 203: } 949,
{ 204: } 949,
{ 205: } 949,
{ 206: } 949,
{ 207: } 949,
{ 208: } 949,
{ 209: } 949,
{ 210: } 958,
{ 211: } 967,
{ 212: } 976,
{ 213: } 985,
{ 214: } 994,
{ 215: } 1014,
{ 216: } 1023,
{ 217: } 1043,
{ 218: } 1063,
{ 219: } 1063,
{ 220: } 1064,
{ 221: } 1066,
{ 222: } 1066,
{ 223: } 1068,
{ 224: } 1068,
{ 225: } 1068,
{ 226: } 1089,
{ 227: } 1110,
{ 228: } 1110,
{ 229: } 1111,
{ 230: } 1112,
{ 231: } 1113,
{ 232: } 1113,
{ 233: } 1113
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
{ 22: } 36,
{ 23: } 36,
{ 24: } 37,
{ 25: } 38,
{ 26: } 38,
{ 27: } 38,
{ 28: } 49,
{ 29: } 49,
{ 30: } 49,
{ 31: } 49,
{ 32: } 49,
{ 33: } 49,
{ 34: } 49,
{ 35: } 49,
{ 36: } 49,
{ 37: } 49,
{ 38: } 50,
{ 39: } 50,
{ 40: } 50,
{ 41: } 50,
{ 42: } 50,
{ 43: } 50,
{ 44: } 50,
{ 45: } 51,
{ 46: } 54,
{ 47: } 55,
{ 48: } 57,
{ 49: } 57,
{ 50: } 57,
{ 51: } 57,
{ 52: } 57,
{ 53: } 57,
{ 54: } 57,
{ 55: } 57,
{ 56: } 57,
{ 57: } 57,
{ 58: } 57,
{ 59: } 57,
{ 60: } 58,
{ 61: } 69,
{ 62: } 77,
{ 63: } 83,
{ 64: } 91,
{ 65: } 91,
{ 66: } 91,
{ 67: } 91,
{ 68: } 106,
{ 69: } 106,
{ 70: } 107,
{ 71: } 108,
{ 72: } 109,
{ 73: } 110,
{ 74: } 122,
{ 75: } 123,
{ 76: } 124,
{ 77: } 125,
{ 78: } 138,
{ 79: } 139,
{ 80: } 139,
{ 81: } 153,
{ 82: } 153,
{ 83: } 153,
{ 84: } 153,
{ 85: } 153,
{ 86: } 153,
{ 87: } 153,
{ 88: } 153,
{ 89: } 160,
{ 90: } 168,
{ 91: } 176,
{ 92: } 184,
{ 93: } 192,
{ 94: } 200,
{ 95: } 210,
{ 96: } 220,
{ 97: } 230,
{ 98: } 240,
{ 99: } 250,
{ 100: } 259,
{ 101: } 269,
{ 102: } 278,
{ 103: } 287,
{ 104: } 287,
{ 105: } 287,
{ 106: } 287,
{ 107: } 287,
{ 108: } 287,
{ 109: } 287,
{ 110: } 287,
{ 111: } 287,
{ 112: } 287,
{ 113: } 287,
{ 114: } 299,
{ 115: } 299,
{ 116: } 299,
{ 117: } 299,
{ 118: } 299,
{ 119: } 299,
{ 120: } 299,
{ 121: } 299,
{ 122: } 299,
{ 123: } 299,
{ 124: } 299,
{ 125: } 300,
{ 126: } 301,
{ 127: } 302,
{ 128: } 303,
{ 129: } 304,
{ 130: } 316,
{ 131: } 325,
{ 132: } 332,
{ 133: } 341,
{ 134: } 342,
{ 135: } 343,
{ 136: } 344,
{ 137: } 345,
{ 138: } 346,
{ 139: } 347,
{ 140: } 348,
{ 141: } 348,
{ 142: } 348,
{ 143: } 348,
{ 144: } 361,
{ 145: } 361,
{ 146: } 361,
{ 147: } 361,
{ 148: } 361,
{ 149: } 362,
{ 150: } 362,
{ 151: } 362,
{ 152: } 362,
{ 153: } 362,
{ 154: } 362,
{ 155: } 362,
{ 156: } 362,
{ 157: } 362,
{ 158: } 362,
{ 159: } 362,
{ 160: } 362,
{ 161: } 362,
{ 162: } 362,
{ 163: } 362,
{ 164: } 362,
{ 165: } 362,
{ 166: } 362,
{ 167: } 362,
{ 168: } 370,
{ 169: } 379,
{ 170: } 388,
{ 171: } 397,
{ 172: } 406,
{ 173: } 415,
{ 174: } 426,
{ 175: } 437,
{ 176: } 448,
{ 177: } 459,
{ 178: } 470,
{ 179: } 480,
{ 180: } 491,
{ 181: } 501,
{ 182: } 511,
{ 183: } 511,
{ 184: } 511,
{ 185: } 511,
{ 186: } 511,
{ 187: } 511,
{ 188: } 511,
{ 189: } 511,
{ 190: } 511,
{ 191: } 511,
{ 192: } 511,
{ 193: } 511,
{ 194: } 511,
{ 195: } 511,
{ 196: } 511,
{ 197: } 511,
{ 198: } 511,
{ 199: } 511,
{ 200: } 511,
{ 201: } 524,
{ 202: } 524,
{ 203: } 536,
{ 204: } 536,
{ 205: } 536,
{ 206: } 536,
{ 207: } 536,
{ 208: } 536,
{ 209: } 536,
{ 210: } 536,
{ 211: } 536,
{ 212: } 536,
{ 213: } 536,
{ 214: } 536,
{ 215: } 536,
{ 216: } 536,
{ 217: } 536,
{ 218: } 536,
{ 219: } 536,
{ 220: } 536,
{ 221: } 537,
{ 222: } 538,
{ 223: } 538,
{ 224: } 538,
{ 225: } 538,
{ 226: } 538,
{ 227: } 551,
{ 228: } 563,
{ 229: } 563,
{ 230: } 563,
{ 231: } 563,
{ 232: } 564,
{ 233: } 564
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
{ 21: } 35,
{ 22: } 35,
{ 23: } 36,
{ 24: } 37,
{ 25: } 37,
{ 26: } 37,
{ 27: } 48,
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
{ 44: } 50,
{ 45: } 53,
{ 46: } 54,
{ 47: } 56,
{ 48: } 56,
{ 49: } 56,
{ 50: } 56,
{ 51: } 56,
{ 52: } 56,
{ 53: } 56,
{ 54: } 56,
{ 55: } 56,
{ 56: } 56,
{ 57: } 56,
{ 58: } 56,
{ 59: } 57,
{ 60: } 68,
{ 61: } 76,
{ 62: } 82,
{ 63: } 90,
{ 64: } 90,
{ 65: } 90,
{ 66: } 90,
{ 67: } 105,
{ 68: } 105,
{ 69: } 106,
{ 70: } 107,
{ 71: } 108,
{ 72: } 109,
{ 73: } 121,
{ 74: } 122,
{ 75: } 123,
{ 76: } 124,
{ 77: } 137,
{ 78: } 138,
{ 79: } 138,
{ 80: } 152,
{ 81: } 152,
{ 82: } 152,
{ 83: } 152,
{ 84: } 152,
{ 85: } 152,
{ 86: } 152,
{ 87: } 152,
{ 88: } 159,
{ 89: } 167,
{ 90: } 175,
{ 91: } 183,
{ 92: } 191,
{ 93: } 199,
{ 94: } 209,
{ 95: } 219,
{ 96: } 229,
{ 97: } 239,
{ 98: } 249,
{ 99: } 258,
{ 100: } 268,
{ 101: } 277,
{ 102: } 286,
{ 103: } 286,
{ 104: } 286,
{ 105: } 286,
{ 106: } 286,
{ 107: } 286,
{ 108: } 286,
{ 109: } 286,
{ 110: } 286,
{ 111: } 286,
{ 112: } 286,
{ 113: } 298,
{ 114: } 298,
{ 115: } 298,
{ 116: } 298,
{ 117: } 298,
{ 118: } 298,
{ 119: } 298,
{ 120: } 298,
{ 121: } 298,
{ 122: } 298,
{ 123: } 298,
{ 124: } 299,
{ 125: } 300,
{ 126: } 301,
{ 127: } 302,
{ 128: } 303,
{ 129: } 315,
{ 130: } 324,
{ 131: } 331,
{ 132: } 340,
{ 133: } 341,
{ 134: } 342,
{ 135: } 343,
{ 136: } 344,
{ 137: } 345,
{ 138: } 346,
{ 139: } 347,
{ 140: } 347,
{ 141: } 347,
{ 142: } 347,
{ 143: } 360,
{ 144: } 360,
{ 145: } 360,
{ 146: } 360,
{ 147: } 360,
{ 148: } 361,
{ 149: } 361,
{ 150: } 361,
{ 151: } 361,
{ 152: } 361,
{ 153: } 361,
{ 154: } 361,
{ 155: } 361,
{ 156: } 361,
{ 157: } 361,
{ 158: } 361,
{ 159: } 361,
{ 160: } 361,
{ 161: } 361,
{ 162: } 361,
{ 163: } 361,
{ 164: } 361,
{ 165: } 361,
{ 166: } 361,
{ 167: } 369,
{ 168: } 378,
{ 169: } 387,
{ 170: } 396,
{ 171: } 405,
{ 172: } 414,
{ 173: } 425,
{ 174: } 436,
{ 175: } 447,
{ 176: } 458,
{ 177: } 469,
{ 178: } 479,
{ 179: } 490,
{ 180: } 500,
{ 181: } 510,
{ 182: } 510,
{ 183: } 510,
{ 184: } 510,
{ 185: } 510,
{ 186: } 510,
{ 187: } 510,
{ 188: } 510,
{ 189: } 510,
{ 190: } 510,
{ 191: } 510,
{ 192: } 510,
{ 193: } 510,
{ 194: } 510,
{ 195: } 510,
{ 196: } 510,
{ 197: } 510,
{ 198: } 510,
{ 199: } 510,
{ 200: } 523,
{ 201: } 523,
{ 202: } 535,
{ 203: } 535,
{ 204: } 535,
{ 205: } 535,
{ 206: } 535,
{ 207: } 535,
{ 208: } 535,
{ 209: } 535,
{ 210: } 535,
{ 211: } 535,
{ 212: } 535,
{ 213: } 535,
{ 214: } 535,
{ 215: } 535,
{ 216: } 535,
{ 217: } 535,
{ 218: } 535,
{ 219: } 535,
{ 220: } 536,
{ 221: } 537,
{ 222: } 537,
{ 223: } 537,
{ 224: } 537,
{ 225: } 537,
{ 226: } 550,
{ 227: } 562,
{ 228: } 562,
{ 229: } 562,
{ 230: } 562,
{ 231: } 563,
{ 232: } 563,
{ 233: } 563
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
{ 66: } ( len: 1; sym: -52 ),
{ 67: } ( len: 0; sym: -51 ),
{ 68: } ( len: 1; sym: -54 ),
{ 69: } ( len: 1; sym: -54 ),
{ 70: } ( len: 1; sym: -54 ),
{ 71: } ( len: 3; sym: -29 ),
{ 72: } ( len: 3; sym: -29 ),
{ 73: } ( len: 3; sym: -30 ),
{ 74: } ( len: 3; sym: -30 ),
{ 75: } ( len: 1; sym: -56 ),
{ 76: } ( len: 9; sym: -56 ),
{ 77: } ( len: 6; sym: -56 ),
{ 78: } ( len: 6; sym: -56 ),
{ 79: } ( len: 3; sym: -31 ),
{ 80: } ( len: 3; sym: -31 ),
{ 81: } ( len: 3; sym: -27 ),
{ 82: } ( len: 3; sym: -27 ),
{ 83: } ( len: 7; sym: -28 ),
{ 84: } ( len: 5; sym: -28 ),
{ 85: } ( len: 3; sym: -28 ),
{ 86: } ( len: 3; sym: -53 ),
{ 87: } ( len: 2; sym: -55 ),
{ 88: } ( len: 1; sym: -12 ),
{ 89: } ( len: 1; sym: -32 ),
{ 90: } ( len: 1; sym: -13 ),
{ 91: } ( len: 3; sym: -13 ),
{ 92: } ( len: 3; sym: -13 ),
{ 93: } ( len: 3; sym: -13 ),
{ 94: } ( len: 3; sym: -13 ),
{ 95: } ( len: 3; sym: -13 ),
{ 96: } ( len: 3; sym: -13 ),
{ 97: } ( len: 1; sym: -13 ),
{ 98: } ( len: 1; sym: -14 ),
{ 99: } ( len: 3; sym: -14 ),
{ 100: } ( len: 3; sym: -14 ),
{ 101: } ( len: 3; sym: -14 ),
{ 102: } ( len: 1; sym: -15 ),
{ 103: } ( len: 3; sym: -15 ),
{ 104: } ( len: 3; sym: -15 ),
{ 105: } ( len: 3; sym: -15 ),
{ 106: } ( len: 3; sym: -15 ),
{ 107: } ( len: 3; sym: -15 ),
{ 108: } ( len: 2; sym: -16 ),
{ 109: } ( len: 2; sym: -16 ),
{ 110: } ( len: 1; sym: -16 ),
{ 111: } ( len: 1; sym: -17 ),
{ 112: } ( len: 3; sym: -17 ),
{ 113: } ( len: 1; sym: -18 ),
{ 114: } ( len: 1; sym: -18 ),
{ 115: } ( len: 1; sym: -18 ),
{ 116: } ( len: 3; sym: -18 ),
{ 117: } ( len: 2; sym: -18 ),
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
{ 129: } ( len: 2; sym: -23 ),
{ 130: } ( len: 3; sym: -24 ),
{ 131: } ( len: 3; sym: -25 ),
{ 132: } ( len: 1; sym: -25 ),
{ 133: } ( len: 1; sym: -26 ),
{ 134: } ( len: 1; sym: -19 ),
{ 135: } ( len: 1; sym: -19 ),
{ 136: } ( len: 1; sym: -20 ),
{ 137: } ( len: 1; sym: -20 ),
{ 138: } ( len: 1; sym: -21 ),
{ 139: } ( len: 1; sym: -22 ),
{ 140: } ( len: 1; sym: -10 ),
{ 141: } ( len: 1; sym: -36 ),
{ 142: } ( len: 1; sym: -38 )
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

function Interpret(const Fn: string): Boolean;
begin
  if not Assigned(RegTable) then
    RegTable := SO('{}');

  if not Assigned(SymTable) then 
  begin
    SymTable := SO('{}');
  end;

  if not Assigned(ObjTypes) then
    ObjTypes := SO('{"RTL": {}, "SPECTRUM": {}, "FREQDISCRIMINATOR": {}, "FMRECEIVER": {}, "AUDIOOUT": {}}');
  
  filename := Fn;
  yylineno := 0;
  RtOK := True;
  assign(yyinput, Fn);
  reset(yyinput);
  Interpret := yyparse=0;
  CloseFile(yyinput);
end;