
/* PAS.Y: ISO Level 0 Pascal grammar, adapted to TP Yacc 2-28-89 AG
   To compile: yacc lazradio.y
               lex lazradiolex.l
*/

%{

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
  // writeln('send ', S, ' ', M);
  if not Assigned(OnSendMessage) then Exit;
  
  L := TStringList.Create;
  L.Delimiter := ' ';
  L.StrictDelimiter := True;
  L.DelimitedText := M;
  OnSendMessage(GetSymDisp(S), StrToInt(L[0]), StrToInt(L[1]), StrToInt(L[2]));
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

%}

/* Note the Pascal keyword tokens are stropped with leading underscore
   (e.g. _AND) in the Turbo Pascal Yacc version, because these identifiers
   will be declared as token numbers in the output file generated by Yacc,
   and hence must not collide with Turbo Pascal keywords. */

%token _AND _ARCCOS _ARCSIN ASSIGNMENT _BEGIN
%token COLON COMMA CONNFEATURE CONNFEATUREDATA CONNDATA _CONST _COS
%token _DIV DOT DOTDOT _ELSE _END EQUAL _EXP _FILE
%token GE GT _ID _INTEGER  _LAZRADIO LBRAC LBRACE LE _LOG LPAREN LT MINUS _MOD _NIL _NOT
%token NOTEQUAL _ORD _OR PLUS _PRED _REAL _ROUND RBRAC RBRACE
%token RPAREN SEND SEMICOLON _SIN SLASH STAR STARSTAR _SUCC _STR _STRING _TRUNC _VAL
%token UPARROW _VAR _WRITELN _XOR

%token ILLEGAL

%token <String>     REALNUMBER
%token <String>     DIGSEQ
%token <String>     CHARACTER_STRING
%token <String>     IDENTIFIER

%type <String>     cexpression
%type <String>     csimple_expression
%type <String>     cterm
%type <String>     cfactor
%type <String>     cexponentiation
%type <String>     cprimary
%type <String>     constant
%type <String>     non_string

%type <String>     identifier
%type <String>     identifier_list

%type <String>     index_expression
%type <String>     expression
%type <String>     simple_expression
%type <String>     term
%type <String>     factor
%type <String>     exponentiation
%type <String>     primary
%type <String>     unsigned_constant
%type <String>     unsigned_number
%type <String>     unsigned_integer
%type <String>     unsigned_real
%type <String>     function_designator
%type <String>     params
%type <String>     actual_parameter_list
%type <String>     actual_parameter

%type <String>     send_statement
%type <String>     radio_message
%type <String>     connection_feature_statement
%type <String>     connection_feature_data_statement
%type <String>     connection_data_statement_0

%type <String>     type_denoter

%%

file : program {}
    | program error
	{ yyerror(':Text follows logical end of program.'); }
    ;

program : program_heading semicolon block DOT
    ;

program_heading : _LAZRADIO identifier { ReportProjName($2); }
    ;

identifier_list : identifier_list comma identifier { $$ := $1 + ' ' + $3}
    | identifier 
    ;

block : constant_definition_part
    variable_declaration_part
    statement_part {}
    ;

constant_definition_part : _CONST constant_list
    |
    ;

constant_list : constant_list constant_definition
    | constant_definition
    ;

constant_definition : identifier EQUAL cexpression semicolon { if DefVars($1, RegTable.S[$3 + '.type']) then
                                                                 SymTable.S['$1' + '.reg'] := $3; }
    ;

/*constant : cexpression ;        /* good stuff! */

cexpression : csimple_expression
    | csimple_expression EQUAL csimple_expression { RegWrite(RegAlloc('boolean'), RegTable.O[$1 + '.value'].Compare(RegTable.O[$3 + '.value']) = cpEqu); }
    | csimple_expression NOTEQUAL csimple_expression { RegWrite(RegAlloc('boolean'), RegTable.O[$1 + '.value'].Compare(RegTable.O[$3 + '.value']) <> cpEqu); }
    | csimple_expression LT csimple_expression { RegWrite(RegAlloc('boolean'), RegTable.O[$1 + '.value'].Compare(RegTable.O[$3 + '.value']) = cpLess); }
    | csimple_expression GT csimple_expression { RegWrite(RegAlloc('boolean'), RegTable.O[$1 + '.value'].Compare(RegTable.O[$3 + '.value']) = cpGreat); }
    | csimple_expression LE csimple_expression { RegWrite(RegAlloc('boolean'), RegTable.O[$1 + '.value'].Compare(RegTable.O[$3 + '.value']) in [cpLess, cpEqu]); }
    | csimple_expression GE csimple_expression { RegWrite(RegAlloc('boolean'), RegTable.O[$1 + '.value'].Compare(RegTable.O[$3 + '.value']) in [cpGreat, cpEqu]); }
    ;

csimple_expression : cterm
    | csimple_expression PLUS cterm { $$ := RegPlus($1, $3, RtOK); }
    | csimple_expression MINUS cterm { $$ := RegMinus($1, $3, RtOK); }
    | csimple_expression _OR cterm { $$ := RegOr($1, $3, RtOK); }
    ;

cterm : cfactor
    | cterm STAR cfactor { $$ := RegTimes($1, $3, RtOK); }
    | cterm SLASH cfactor { $$ := RegDivide($1, $3, RtOK); }
    | cterm _DIV cfactor { $$ := RegDiv($1, $3, RtOK); }
    | cterm _MOD cfactor { $$ := RegMod($1, $3, RtOK); }
    | cterm _AND cfactor { $$ := RegAnd($1, $3, RtOK); }
    ;

cfactor : PLUS cfactor { $$ := $2; }
    | MINUS cfactor { $$ := RegNeg($2, RtOK); }
    | cexponentiation
    ;

cexponentiation : cprimary
    | cprimary STARSTAR cexponentiation { $$ := RegPower($1, $3, RtOK); }
    ;

cprimary : identifier { $$ := ReadValue($1, RtOK); }
    | LPAREN cexpression RPAREN { $$ := $2; }
    | unsigned_constant
    | _NOT cprimary { $$ := RegNot($2, RtOK); }
    ;

constant : non_string
    | PLUS non_string   { $$ := $2; }
    | MINUS non_string  { $$ := RegNeg($2, RtOK); }
    | CHARACTER_STRING
    ;

non_string : DIGSEQ
    | identifier  { $$ := ReadValue($1, RtOK); }
    | REALNUMBER
    ;

variable_declaration_part : _VAR variable_declaration_list semicolon { CreateModules; }
    |
    ;

variable_declaration_list :
      variable_declaration_list semicolon variable_declaration
    | variable_declaration
    ;

variable_declaration : identifier_list COLON _INTEGER { DefVars($1, 'int'); }
    | identifier_list COLON _REAL { DefVars($1, 'real'); }
    | identifier_list COLON _STRING { DefVars($1, 'string'); }
    | identifier_list COLON type_denoter { DefObjs($1, $3); }
    ;

statement_part : compound_statement ;

compound_statement : _BEGIN statement_sequence _END;

statement_sequence : statement_sequence semicolon statement
    | statement
    ;

statement : open_statement
    | closed_statement
    ;

open_statement : non_labeled_open_statement
    ;

closed_statement : non_labeled_closed_statement
    ;

non_labeled_closed_statement : assignment_statement
    | compound_statement
    | send_statement
    | connection_statement 
    | write_statement
    ;

non_labeled_open_statement :;

connection_statement: connection_feature_statement
    | connection_feature_data_statement
    | connection_data_statement
    ;

connection_feature_statement: identifier CONNFEATURE identifier { ConnectFeature($1, $3); $$ := $3; }
    | connection_feature_statement CONNFEATURE identifier { ConnectFeature($1, $3); $$ := $3; }
    ;

connection_feature_data_statement: identifier CONNFEATUREDATA identifier { ConnectFeature($1, $3); ConnectData($1, $3, 0, 0); $$ := $3; }
    | connection_feature_data_statement CONNFEATUREDATA identifier { ConnectFeature($1, $3); ConnectData($1, $3, 0, 0); $$ := $3; }
    ;

connection_data_statement: connection_data_statement_0
    | identifier LBRAC index_expression RBRAC CONNDATA LBRAC index_expression RBRAC identifier  { ConnectData($1, $9, ReadInt($3, RtOK), ReadInt($7, RtOK)); }
    | identifier LBRAC index_expression RBRAC CONNDATA identifier   { ConnectData($1, $6, ReadInt($3, RtOK), 0); }
    | identifier CONNDATA  LBRAC index_expression RBRAC identifier   { ConnectData($1, $6, 0, ReadInt($4, RtOK)); }
    ;

connection_data_statement_0: identifier CONNDATA identifier  { ConnectData($1, $3, 0, 0); $$ := $3; }
    | connection_data_statement_0 CONNDATA identifier  { ConnectData($1, $3, 0, 0); $$ := $3; }

send_statement : identifier SEND radio_message { SendMsg($1, $3); $$ := $1;}
    | send_statement SEND radio_message  {SendMsg($1, $3); $$ := $1;}
    ;

radio_message : LBRACE expression COMMA expression COMMA expression RBRACE  { $$ := Format('%d %d %d', [ReadInt($2, RtOK), 
                                                                                       ReadInt($4, RtOK), ReadInt($6, RtOK)]); }
    | LBRACE expression COMMA expression RBRACE  { $$ := Format('%d %d 0', [ReadInt($2, RtOK), ReadInt($4, RtOK)]); }
    | LBRACE expression RBRACE  { $$ := Format('%d 0 0', [ReadInt($2, RtOK)]); }
    ;

assignment_statement : identifier ASSIGNMENT expression {if Assigned(SymTable.O[UpperCase($1) + '.reg']) then
                                                            SymTable.S[UpperCase($1) + '.reg'] := $3
                                                          else
                                                            yyerror(Format('"%s" bad variable', [$1])); }
    ;

write_statement : _WRITELN params { EmitMsg(ToStr($2)); } 
index_expression : expression ;

type_denoter: identifier /* module types */
    ;

expression : simple_expression
    | simple_expression EQUAL simple_expression { RegWrite(RegAlloc('boolean'), RegTable.O[$1 + '.value'].Compare(RegTable.O[$3 + '.value']) = cpEqu); }
    | simple_expression NOTEQUAL simple_expression { RegWrite(RegAlloc('boolean'), RegTable.O[$1 + '.value'].Compare(RegTable.O[$3 + '.value']) <> cpEqu); }
    | simple_expression LT simple_expression { RegWrite(RegAlloc('boolean'), RegTable.O[$1 + '.value'].Compare(RegTable.O[$3 + '.value']) = cpLess); }
    | simple_expression GT simple_expression { RegWrite(RegAlloc('boolean'), RegTable.O[$1 + '.value'].Compare(RegTable.O[$3 + '.value']) = cpGreat); }
    | simple_expression LE simple_expression { RegWrite(RegAlloc('boolean'), RegTable.O[$1 + '.value'].Compare(RegTable.O[$3 + '.value']) in [cpLess, cpEqu]); }
    | simple_expression GE simple_expression { RegWrite(RegAlloc('boolean'), RegTable.O[$1 + '.value'].Compare(RegTable.O[$3 + '.value']) in [cpGreat, cpEqu]); }
    | error
    ;

simple_expression : term
    | simple_expression PLUS term { $$ := RegPlus($1, $3, RtOK); }
    | simple_expression MINUS term { $$ := RegMinus($1, $3, RtOK); }
    | simple_expression _OR term { $$ := RegOr($1, $3, RtOK); }
    ;

term : factor
    | term STAR factor { $$ := RegTimes($1, $3, RtOK); }
    | term SLASH factor { $$ := RegDivide($1, $3, RtOK); }
    | term _DIV factor { $$ := RegDiv($1, $3, RtOK); }
    | term _MOD factor { $$ := RegMod($1, $3, RtOK); }
    | term _AND factor { $$ := RegAnd($1, $3, RtOK); }
    ;

factor : PLUS factor { $$ := $2; }
    | MINUS factor { $$ := RegNeg($2, RtOK); }
    | exponentiation
    ;

exponentiation : primary
    | primary STARSTAR exponentiation { $$ := RegPower($1, $3, RtOK); }
    ;

primary : identifier           {  $$ := ReadValue($1, RtOK); }
    | unsigned_constant
    | function_designator
    | LPAREN expression RPAREN { $$ := $2; }
    | _NOT primary        { $$ := RegNot($2, RtOK); }      
    ;

function_designator : _ARCCOS params { $$ := RegAlloc('real'); RegWrite($$, arccos(ReadReal($2, RtOK))); }
    | _ARCSIN params { $$ := RegAlloc('real'); RegWrite($$, arcsin(ReadReal($2, RtOK))); }
    | _COS params { $$ := RegAlloc('real'); RegWrite($$, cos(ReadReal($2, RtOK))); }
    | _EXP params { $$ := RegAlloc('real'); RegWrite($$, exp(ReadReal($2, RtOK))); }
    | _LOG params { $$ := RegAlloc('real'); RegWrite($$, log10(ReadReal($2, RtOK))); }
    | _PRED params { $$ := RegAlloc('int'); RegWrite($$, Pred(ReadInt($2, RtOK))); } 
    | _ROUND params { $$ := RegAlloc('int'); RegWrite($$, Round(ReadReal($2, RtOK))); } 
    | _SIN params { $$ := RegAlloc('real'); RegWrite($$, sin(ReadReal($2, RtOK))); }
    | _SUCC params { $$ := RegAlloc('int'); RegWrite($$, Succ(ReadInt($2, RtOK))); } 
    | _STR params { $$ := RegAlloc('string'); RegWrite($$, ToStr($2)); } 
    | _TRUNC params { $$ := RegAlloc('int'); RegWrite($$, Trunc(ReadReal($2, RtOK))); } 
    | _VAL params { $$ := RegAlloc('real'); // TODO: } 
    ;

params : LPAREN actual_parameter_list RPAREN   { $$ := $2; }
    ;

actual_parameter_list : actual_parameter_list comma actual_parameter  { $$ := $1 + ' ' + $3; }
    | actual_parameter
    ;

actual_parameter : expression
    ;

unsigned_constant : unsigned_number
    | CHARACTER_STRING
    ;

unsigned_number : unsigned_integer | unsigned_real ;

unsigned_integer : DIGSEQ
    ;

unsigned_real : REALNUMBER
    ;

identifier : IDENTIFIER 
    ;

semicolon : SEMICOLON
    ;

comma : COMMA
    ;

%%

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
  assign(yyinput, Fn);
  reset(yyinput);
  Interpret := yyparse=0;
  CloseFile(yyinput);
end;
