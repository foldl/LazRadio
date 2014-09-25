
/* PAS.Y: ISO Level 0 Pascal grammar, adapted to TP Yacc 2-28-89 AG
   To compile: yacc lazradio.y
               lex lazradiolex.l
*/

%{

uses SysUtils, Classes, LexLib, YaccLib, superobject;

type
  TCreateModule = function (const Name: string; const T: string): Boolean of object;
  TSendMessage = function (const Name: string; const V1, V2, V3: PtrUInt): Boolean of object;
  
  TRegName = string;

var filename : String;
    ObjTypes: ISuperObject = nil;
    yywrap: Boolean = True;
    RtOK: Boolean;
    SymTable: ISuperObject = nil;
    RegTable: ISuperObject = nil;
    RegIndex: Integer = 0;
    OnCreateModule: TCreateModule = nil;
    OnSendMessage: TSendMessage = nil;

procedure yyerror(msg : string);
  begin
    writeln(filename, ': ', yylineno, ': ',
            msg, ' at or before `', yytext, '''.')
  end(*yyerror*);

function IsDefined(const S: string): Boolean;
begin
  IsDefined := Assigned(SymTable.O[S]);
end;

function AllocReg(const T: string): TRegName;
begin
  AllocReg := IntToStr(RegIndex);
  RegTable.O[AllocReg] := SO(Format('type: "%s"', [T]));
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
  RegWrite := RegTable.S[R + '.type'] = 'symbol';
  if RegWrite then 
    RegTable.S[R + '.name'] := V;
end;

// for built-in types
function DefVars(const S: string; const T: string): Boolean;
var
  L: TStringList;
  V: string;
  R: string;
begin
  L := TStringList.Create;
  L.Delimiter := ' ';
  L.StrictDelimiter := True;
  Result := False;
  for V in L do
  begin
    if IsDefined(V) then 
    begin
      yyerror(Format('"%s" is already defined.', [V]));
      Exit;
    end;
    SymTable.O[UpperCase(S)] := SO(Format('type: "%s"; disp: "%s"; reg: "%s"', [T, S, AllocReg(T)]));
  end;
  Result := True;
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
  Result := False;
  for V in L do
  begin
    if IsDefined(V) then 
    begin
      yyerror(Format('"%s" is already defined.', [V]));
      Exit;
    end;
    R := UpperCase(T);
    if not Assigned(ObjTypes.O[R]) then 
    begin
      yyerror(Format('type "%s" is unkown.', [T]));
      Exit;
    end;
    SymTable.O[UpperCase(S)] := SO(Format('type: "%s"; disp: "%s"', [R, S]));
  end;
  Result := True;
end;

function RegPlus(const R1, R2: TRegName; out bOK: Boolean): string;
begin
  case RegTable.S[R1 + '.type'] of
    'string':
      begin
        bOK := RegTable.S[R2 + '.type'] = 'string';
        if bOK then
        begin
          RegPlus := AllocReg('string');
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
          RegPlus := AllocReg('int');
          RegTable.I[RegPlus + '.value'] := RegTable.I[R1 + '.value'] + RegTable.I[R2 + '.value'];
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegPlus := AllocReg('real');
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
          RegPlus := AllocReg('real');
          RegTable.D[RegPlus + '.value'] := RegTable.D[R1 + '.value'] + RegTable.I[R2 + '.value'];
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegPlus := AllocReg('real');
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

function RegMinus(const R1, R2: TRegName; out bOK: Boolean): string;
begin
  case RegTable.S[R1 + '.type'] of
    'int':
      begin
        bOK := True;
        if RegTable.S[R2 + '.type'] = 'int' then
        begin
          RegMinus := AllocReg('int');
          RegTable.I[RegPlus + '.value'] := RegTable.I[R1 + '.value'] - RegTable.I[R2 + '.value'];
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegMinus := AllocReg('real');
          RegTable.D[RegPlus + '.value'] := RegTable.I[R1 + '.value'] - RegTable.D[R2 + '.value'];
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
          RegMinus := AllocReg('real');
          RegTable.D[RegPlus + '.value'] := RegTable.D[R1 + '.value'] - RegTable.I[R2 + '.value'];
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegMinus := AllocReg('real');
          RegTable.D[RegPlus + '.value'] := RegTable.D[R1 + '.value'] - RegTable.D[R2 + '.value'];
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

function RegTimes(const R1, R2: TRegName; out bOK: Boolean): string;
begin
  case RegTable.S[R1 + '.type'] of
    'int':
      begin
        bOK := True;
        if RegTable.S[R2 + '.type'] = 'int' then
        begin
          RegTimes := AllocReg('int');
          RegTable.I[RegPlus + '.value'] := RegTable.I[R1 + '.value'] * RegTable.I[R2 + '.value'];
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegTimes := AllocReg('real');
          RegTable.D[RegPlus + '.value'] := RegTable.I[R1 + '.value'] * RegTable.D[R2 + '.value'];
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
          RegTimes := AllocReg('real');
          RegTable.D[RegPlus + '.value'] := RegTable.D[R1 + '.value'] * RegTable.I[R2 + '.value'];
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegTimes := AllocReg('real');
          RegTable.D[RegPlus + '.value'] := RegTable.D[R1 + '.value'] * RegTable.D[R2 + '.value'];
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

function RegDivide(const R1, R2: TRegName; out bOK: Boolean): string;
begin
  try
  case RegTable.S[R1 + '.type'] of
    'int':
      begin
        bOK := True;
        if RegTable.S[R2 + '.type'] = 'int' then
        begin
          RegDivide := AllocReg('real');
          RegTable.D[RegPlus + '.value'] := RegTable.I[R1 + '.value'] / RegTable.I[R2 + '.value'];
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegDivide := AllocReg('real');
          RegTable.D[RegPlus + '.value'] := RegTable.I[R1 + '.value'] / RegTable.D[R2 + '.value'];
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
          RegDivide := AllocReg('real');
          RegTable.D[RegPlus + '.value'] := RegTable.D[R1 + '.value'] / RegTable.I[R2 + '.value'];
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegDivide := AllocReg('real');
          RegTable.D[RegPlus + '.value'] := RegTable.D[R1 + '.value'] / RegTable.D[R2 + '.value'];
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

function RegPower(const R1, R2: TRegName; out bOK: Boolean): string;
begin
  case RegTable.S[R1 + '.type'] of
    'int':
      begin
        bOK := True;
        if RegTable.S[R2 + '.type'] = 'int' then
        begin
          RegPower := AllocReg('int');
          RegTable.I[RegPlus + '.value'] := Round(Pow(RegTable.I[R1 + '.value'], RegTable.I[R2 + '.value']));
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegPower := AllocReg('real');
          RegTable.D[RegPlus + '.value'] := Pow(RegTable.I[R1 + '.value'], RegTable.D[R2 + '.value']);
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
          RegPower := AllocReg('real');
          RegTable.D[RegPlus + '.value'] := Pow(RegTable.D[R1 + '.value'], RegTable.I[R2 + '.value']);
        end
        else if RegTable.S[R2 + '.type'] = 'real' then
        begin
          RegPower := AllocReg('real');
          RegTable.D[RegPlus + '.value'] := Pow(RegTable.D[R1 + '.value'], RegTable.D[R2 + '.value']);
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

function RegMod(const R1, R2: TRegName; out bOK: Boolean): string;
begin
  bOK := (RegTable.S[R1 + '.type']  = 'int') and (RegTable.S[R2 + '.type'] = 'int');
  bOK := bOK and (RegTable.I[R2 + '.value'] <> 0);
  if bOK then
  begin
    RegMod := AllocReg('int');
    RegTable.I[RegPlus + '.value'] := RegTable.I[R1 + '.value'] mod RegTable.I[R2 + '.value'];
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
    RegDiv := AllocReg('int');
    RegTable.I[RegPlus + '.value'] := RegTable.I[R1 + '.value'] div RegTable.I[R2 + '.value'];
  end
  else
    yyerror('type for mod error');
end; 

function RegAnd(const R1, R2: TRegName; out bOK: Boolean): string;
begin
  bOK := True;
  if (RegTable.S[R1 + '.type']  = 'int') and (RegTable.S[R2 + '.type'] = 'int') then
  begin
    RegAnd := AllocReg('int');
    RegTable.I[RegPlus + '.value'] := RegTable.I[R1 + '.value'] and RegTable.I[R2 + '.value'];
  end
  else if (RegTable.S[R1 + '.type']  = 'boolean') and (RegTable.S[R2 + '.type'] = 'boolean') then
  begin
    RegAnd := AllocReg('boolean');
    RegTable.B[RegPlus + '.value'] := RegTable.B[R1 + '.value'] and RegTable.B[R2 + '.value'];
  end
  else begin
    bOK := False;
    yyerror('type for AND error');
  end;
end;

function RegOr(const R1, R2: TRegName; out bOK: Boolean): string;
begin
  bOK := True;
  if (RegTable.S[R1 + '.type']  = 'int') and (RegTable.S[R2 + '.type'] = 'int') then
  begin
    RegOr := AllocReg('int');
    RegTable.I[RegPlus + '.value'] := RegTable.I[R1 + '.value'] or RegTable.I[R2 + '.value'];
  end
  else if (RegTable.S[R1 + '.type']  = 'boolean') and (RegTable.S[R2 + '.type'] = 'boolean') then
  begin
    RegOr := AllocReg('boolean');
    RegTable.B[RegPlus + '.value'] := RegTable.B[R1 + '.value'] or RegTable.B[R2 + '.value'];
  end
  else begin
    bOK := False;
    yyerror('type for OR error');
  end;
end;

function RegXor(const R1, R2: TRegName; out bOK: Boolean): string;
begin
  bOK := True;
  if (RegTable.S[R1 + '.type']  = 'int') and (RegTable.S[R2 + '.type'] = 'int') then
  begin
    RegXor := AllocReg('int');
    RegTable.I[RegPlus + '.value'] := RegTable.I[R1 + '.value'] xor RegTable.I[R2 + '.value'];
  end
  else if (RegTable.S[R1 + '.type']  = 'boolean') and (RegTable.S[R2 + '.type'] = 'boolean') then
  begin
    RegXor := AllocReg('boolean');
    RegTable.B[RegPlus + '.value'] := RegTable.B[R1 + '.value'] xor RegTable.B[R2 + '.value'];
  end
  else begin
    bOK := False;
    yyerror('type for XOR error');
  end;
end;

function RegNot(const R1; out bOK: Boolean): string;
begin
  bOK := True;
  if RegTable.S[R1 + '.type']  = 'int' then
  begin
    RegNot := AllocReg('int');
    RegTable.I[RegPlus + '.value'] := not RegTable.I[R1 + '.value'];
  end
  else if RegTable.S[R1 + '.type']  = 'boolean' then
  begin
    RegNot := AllocReg('boolean');
    RegTable.B[RegPlus + '.value'] := not RegTable.B[R1 + '.value'];
  end
  else begin
    bOK := False;
    yyerror('type for NOT error');
  end;
end;

function RegNeg(const R1; out bOK: Boolean): string;
begin
  bOK := True;
  case RegTable.S[R1 + '.type'] of
    'int':
      begin
        RegNeg := AllocReg('int');
        RegTable.I[RegNeg + '.value'] := -RegTable.I[R1 + '.value'];
      end;
    'real':
      begin
        RegNeg := AllocReg('real');
        RegTable.D[RegNeg + '.value'] := -RegTable.D[R1 + '.value'];
      end
  else
    bOK := False;
    yyerror('type for - error');
  end;
end; 

function ReadValue(const S: string; out bOK: Boolean): string;
begin
  bOK := False;
  if Length(S) < 1 then Exit;
  bOK := True;
  if S[1] in ['0'-'9'] then
    RegValue := S
  else if Assigned(SymTable.O[S + '.reg']) then
    RegValue := SymTable.S[S + '.reg']
  else begin
    bOK := False;
    yyerror(Format('"%s" bad variable', [S]));
  end;
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
%token NOTEQUAL _ORD _OR PLUS _PRED _REAL RBRAC RBRACE
%token RPAREN SEND SEMICOLON _SIN SLASH STAR STARSTAR _SUCC _STR _STRING _VAL
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

%type <String>     type_denoter

%%

file : program {writeln('program');}
    | program error
	{ yyerror(':Text follows logical end of program.'); }
    ;

program : program_heading semicolon block DOT {writeln('dot!');}
    ;

program_heading : _LAZRADIO identifier
    ;

identifier_list : identifier_list comma identifier { $$ := $1 + ' ' + $3}
    | identifier 
    ;

block : constant_definition_part
    variable_declaration_part
    statement_part {writeln('block!');}
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
    | csimple_expression EQUAL csimple_expression { RegWrite(AllocReg('boolean'), RegTable.O[$1 + '.value'].Compare(RegTable.O[$3 + '.value']) = cpEqu); }
    | csimple_expression NOTEQUAL csimple_expression { RegWrite(AllocReg('boolean'), RegTable.O[$1 + '.value'].Compare(RegTable.O[$3 + '.value']) <> cpEqu); }
    | csimple_expression LT csimple_expression { RegWrite(AllocReg('boolean'), RegTable.O[$1 + '.value'].Compare(RegTable.O[$3 + '.value']) = cpLess); }
    | csimple_expression GT csimple_expression { RegWrite(AllocReg('boolean'), RegTable.O[$1 + '.value'].Compare(RegTable.O[$3 + '.value']) = cpGreat); }
    | csimple_expression LE csimple_expression { RegWrite(AllocReg('boolean'), RegTable.O[$1 + '.value'].Compare(RegTable.O[$3 + '.value']) in [cpLess, cpEqu]); }
    | csimple_expression GE csimple_expression { RegWrite(AllocReg('boolean'), RegTable.O[$1 + '.value'].Compare(RegTable.O[$3 + '.value']) in [cpGreat, cpEqu]); }
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
    | cprimary STARSTAR cexponentiation { $$ := RegPow($1, $3, RtOK); }
    ;

cprimary : identifier { $$ := ReadValue($1); }
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
    | identifier  { $$ := ReadValue($1); }
    | REALNUMBER
    ;

variable_declaration_part : _VAR variable_declaration_list semicolon
    |
    ;

variable_declaration_list :
      variable_declaration_list semicolon variable_declaration
    | variable_declaration
    ;

variable_declaration : identifier_list COLON _INTEGER { DefVars($1, 'int'); }
    | identifier_list COLON _REAL { DefVars($1, 'real'); }
    | identifier_list COLON _STRING { DefVars($1, 'string'); }
    | identifier_list COLON type_denoter { DefVars($1, $3); }
    ;

formal_parameter_list : LPAREN formal_parameter_section_list RPAREN ;

formal_parameter_section_list : formal_parameter_section_list semicolon
 formal_parameter_section
    | formal_parameter_section
    ;

formal_parameter_section : value_parameter_specification
    | variable_parameter_specification
    | procedural_parameter_specification
    | functional_parameter_specification
    ;

value_parameter_specification : identifier_list COLON identifier
    ;

variable_parameter_specification : _VAR identifier_list COLON identifier
    ;

result_type : identifier ;

function_identification : _FUNCTION identifier ;

function_block : block ;

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
    ;

non_labeled_open_statement :;

connection_statement: connection_feature_statement
    | connection_feature_data_statement
    | connection_data_statement
    ;

connection_feature_statement: identifier CONNFEATURE identifier
    | identifier CONNFEATURE connection_feature_statement
    ;

connection_feature_data_statement: identifier CONNFEATUREDATA identifier
    | identifier CONNFEATUREDATA connection_feature_data_statement
    ;

connection_data_statement: identifier CONNDATA identifier
    | identifier CONNDATA connection_data_statement
    | indexed_variable CONNDATA indexed_variable
    ;

send_statement : identifier SEND LBRACE expression COMMA expression COMMA expression RBRACE 
    | identifier SEND LBRACE expression COMMA expression _RBRACE        
    | identifier SEND LBRACE expression RBRACE     {writeln('send!!!!');}  

assignment_statement : variable_access ASSIGNMENT expression
    ;

variable_access : identifier
    ;

indexed_variable : variable_access LBRAC index_expression_list RBRAC
    ;

index_expression_list : index_expression_list comma index_expression
    | index_expression
    ;

index_expression : expression ;

procedure_statement : identifier params
    | identifier
    ;

params : LPAREN actual_parameter_list RPAREN ;

actual_parameter_list : actual_parameter_list comma actual_parameter
    | actual_parameter
    ;

type_denoter: identifier /* module types */
    ;

/*
 * this forces you to check all this to be sure that only write and
 * writeln use the 2nd and 3rd forms, you really can't do it easily in
 * the grammar, especially since write and writeln aren't reserved
 */
actual_parameter : expression
    | expression COLON expression
    | expression COLON expression COLON expression
    ;

control_variable : identifier ;

boolean_expression : expression ;

expression : simple_expression
    | simple_expression relop simple_expression
    | error
    ;

simple_expression : term
    | simple_expression addop term
    ;

term : factor
    | term mulop factor
    ;

factor : sign factor
    | exponentiation
    ;

exponentiation : primary
    | primary STARSTAR exponentiation
    ;

primary : variable_access
    | unsigned_constant
    | function_designator
    | set_constructor
    | LPAREN expression RPAREN
    | _NOT primary
    ;

unsigned_constant : unsigned_number
    | CHARACTER_STRING
    | _NIL
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

begin
  if not Assigned(SymTable) then SymTable := SO('{}');
  if not Assigned(ObjTypes) then ObjTypes := SO('{}');
  RegTable := SO('{}');

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
