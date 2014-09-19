
(* Yacc parser template (TP Yacc V3.0), V1.2 6-17-91 AG *)

(* global definitions: *)


uses SysUtils, Classes, LexLib, YaccLib, superobject;

type
  TCreateModule = function (const Name: string; const T: string): Boolean of object;
  TSendMessage = function (const Name: string; const V1, V2, V3: PtrUInt): Boolean of object;

var filename : String;
    yywrap: Boolean = True;
    SymTable: ISuperObject = nil;
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

function DefVars(const S: string; const T: string): Boolean;
var
  L: TStringList;
  V: string;
begin
  L := TStringList.Create;
  L.Delimiter := ' ';
  L.StrictDelimiter := True; 
  DefVars := False;
  for V in L do
  begin
    if IsDefined(V) then Exit;
    SymTable.O[S] := SO(Format('type: %s', [T]));
  end;
  DefVars := True;
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
const RBRAC = 298;
const RBRACE = 299;
const RPAREN = 300;
const SEND = 301;
const SEMICOLON = 302;
const _SIN = 303;
const SLASH = 304;
const STAR = 305;
const STARSTAR = 306;
const _SUCC = 307;
const _STR = 308;
const _STRING = 309;
const _VAL = 310;
const UPARROW = 311;
const _VAR = 312;
const _WRITELN = 313;
const _XOR = 314;
const ILLEGAL = 315;
const REALNUMBER = 316;
const DIGSEQ = 317;
const CHARACTER_STRING = 318;
const IDENTIFIER = 319;

type YYSType = record case Integer of
                 1 : ( yyInteger : Integer );
                 2 : ( yyReal : Real );
                 3 : ( yyString : String );
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
         yyval := yyv[yysp-3];
       end;
  13 : begin
         yyval := yyv[yysp-0];
       end;
  14 : begin
         yyval := yyv[yysp-2];
       end;
  15 : begin
         yyval := yyv[yysp-0];
       end;
  16 : begin
         yyval := yyv[yysp-2];
       end;
  17 : begin
         yyval := yyv[yysp-0];
       end;
  18 : begin
         yyval := yyv[yysp-2];
       end;
  19 : begin
         yyval := yyv[yysp-1];
       end;
  20 : begin
         yyval := yyv[yysp-0];
       end;
  21 : begin
         yyval := yyv[yysp-0];
       end;
  22 : begin
         yyval := yyv[yysp-2];
       end;
  23 : begin
         yyval := yyv[yysp-0];
       end;
  24 : begin
         yyval := yyv[yysp-2];
       end;
  25 : begin
         yyval := yyv[yysp-0];
       end;
  26 : begin
         yyval := yyv[yysp-1];
       end;
  27 : begin
         yyval := yyv[yysp-0];
       end;
  28 : begin
         yyval := yyv[yysp-1];
       end;
  29 : begin
         yyval := yyv[yysp-0];
       end;
  30 : begin
         yyval := yyv[yysp-0];
       end;
  31 : begin
         yyval := yyv[yysp-0];
       end;
  32 : begin
         yyval := yyv[yysp-0];
       end;
  33 : begin
         yyval := yyv[yysp-0];
       end;
  34 : begin
         yyval := yyv[yysp-0];
       end;
  35 : begin
         yyval := yyv[yysp-0];
       end;
  36 : begin
         yyval := yyv[yysp-0];
       end;
  37 : begin
         yyval := yyv[yysp-2];
       end;
  38 : begin
       end;
  39 : begin
         yyval := yyv[yysp-2];
       end;
  40 : begin
         yyval := yyv[yysp-0];
       end;
  41 : begin
         DefVars(yyv[yysp-2].yyString, 'int'); 
       end;
  42 : begin
         DefVars(yyv[yysp-2].yyString, 'real'); 
       end;
  43 : begin
         DefVars(yyv[yysp-2].yyString, 'string'); 
       end;
  44 : begin
         DefVars(yyv[yysp-2].yyString, yyv[yysp-0].yyString); 
       end;
  45 : begin
         yyval := yyv[yysp-2];
       end;
  46 : begin
         yyval := yyv[yysp-2];
       end;
  47 : begin
         yyval := yyv[yysp-0];
       end;
  48 : begin
         yyval := yyv[yysp-0];
       end;
  49 : begin
         yyval := yyv[yysp-0];
       end;
  50 : begin
         yyval := yyv[yysp-0];
       end;
  51 : begin
         yyval := yyv[yysp-0];
       end;
  52 : begin
         yyval := yyv[yysp-2];
       end;
  53 : begin
         yyval := yyv[yysp-3];
       end;
  54 : begin
         yyval := yyv[yysp-0];
       end;
  55 : begin
         yyval := yyv[yysp-1];
       end;
  56 : begin
         yyval := yyv[yysp-0];
       end;
  57 : begin
         yyval := yyv[yysp-0];
       end;
  58 : begin
         writeln('compound_statement');
       end;
  59 : begin
         yyval := yyv[yysp-2];
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
         yyval := yyv[yysp-0];
       end;
  68 : begin
         yyval := yyv[yysp-0];
       end;
  69 : begin
       end;
  70 : begin
         yyval := yyv[yysp-0];
       end;
  71 : begin
         yyval := yyv[yysp-0];
       end;
  72 : begin
         yyval := yyv[yysp-0];
       end;
  73 : begin
         yyval := yyv[yysp-2];
       end;
  74 : begin
         yyval := yyv[yysp-2];
       end;
  75 : begin
         yyval := yyv[yysp-2];
       end;
  76 : begin
         yyval := yyv[yysp-2];
       end;
  77 : begin
         yyval := yyv[yysp-2];
       end;
  78 : begin
         yyval := yyv[yysp-2];
       end;
  79 : begin
         yyval := yyv[yysp-2];
       end;
  80 : begin
         yyval := yyv[yysp-8];
       end;
  81 : begin
         yyval := yyv[yysp-6];
       end;
  82 : begin
         writeln('send!!!!');
       end;
  83 : begin
         yyval := yyv[yysp-2];
       end;
  84 : begin
         yyval := yyv[yysp-0];
       end;
  85 : begin
         yyval := yyv[yysp-0];
       end;
  86 : begin
         yyval := yyv[yysp-0];
       end;
  87 : begin
         yyval := yyv[yysp-3];
       end;
  88 : begin
         yyval := yyv[yysp-2];
       end;
  89 : begin
         yyval := yyv[yysp-0];
       end;
  90 : begin
         yyval := yyv[yysp-0];
       end;
  91 : begin
         yyval := yyv[yysp-1];
       end;
  92 : begin
         yyval := yyv[yysp-0];
       end;
  93 : begin
         yyval := yyv[yysp-2];
       end;
  94 : begin
         yyval := yyv[yysp-2];
       end;
  95 : begin
         yyval := yyv[yysp-0];
       end;
  96 : begin
         yyval := yyv[yysp-0];
       end;
  97 : begin
         yyval := yyv[yysp-0];
       end;
  98 : begin
         yyval := yyv[yysp-2];
       end;
  99 : begin
         yyval := yyv[yysp-4];
       end;
 100 : begin
         yyval := yyv[yysp-0];
       end;
 101 : begin
         yyval := yyv[yysp-0];
       end;
 102 : begin
         yyval := yyv[yysp-0];
       end;
 103 : begin
         yyval := yyv[yysp-2];
       end;
 104 : begin
         yyval := yyv[yysp-0];
       end;
 105 : begin
         yyval := yyv[yysp-0];
       end;
 106 : begin
         yyval := yyv[yysp-2];
       end;
 107 : begin
         yyval := yyv[yysp-0];
       end;
 108 : begin
         yyval := yyv[yysp-2];
       end;
 109 : begin
         yyval := yyv[yysp-1];
       end;
 110 : begin
         yyval := yyv[yysp-0];
       end;
 111 : begin
         yyval := yyv[yysp-0];
       end;
 112 : begin
         yyval := yyv[yysp-2];
       end;
 113 : begin
         yyval := yyv[yysp-0];
       end;
 114 : begin
         yyval := yyv[yysp-0];
       end;
 115 : begin
         yyval := yyv[yysp-0];
       end;
 116 : begin
         yyval := yyv[yysp-0];
       end;
 117 : begin
         yyval := yyv[yysp-2];
       end;
 118 : begin
         yyval := yyv[yysp-1];
       end;
 119 : begin
         yyval := yyv[yysp-0];
       end;
 120 : begin
         yyval := yyv[yysp-0];
       end;
 121 : begin
         yyval := yyv[yysp-0];
       end;
 122 : begin
         yyval := yyv[yysp-0];
       end;
 123 : begin
         yyval := yyv[yysp-0];
       end;
 124 : begin
         yyval := yyv[yysp-0];
       end;
 125 : begin
         yyval := yyv[yysp-0];
       end;
 126 : begin
         yyval := yyv[yysp-0];
       end;
 127 : begin
         yyval := yyv[yysp-0];
       end;
 128 : begin
         yyval := yyv[yysp-0];
       end;
 129 : begin
         yyval := yyv[yysp-0];
       end;
 130 : begin
         yyval := yyv[yysp-0];
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

yynacts   = 438;
yyngotos  = 367;
yynstates = 164;
yynrules  = 142;

yya : array [1..yynacts] of YYARec = (
{ 0: }
  ( sym: 281; act: 4 ),
{ 1: }
  ( sym: 302; act: 6 ),
{ 2: }
  ( sym: 256; act: 7 ),
  ( sym: 0; act: -1 ),
{ 3: }
  ( sym: 0; act: 0 ),
{ 4: }
  ( sym: 319; act: 9 ),
{ 5: }
  ( sym: 267; act: 12 ),
  ( sym: 261; act: -9 ),
  ( sym: 312; act: -9 ),
{ 6: }
{ 7: }
{ 8: }
{ 9: }
{ 10: }
  ( sym: 312; act: 14 ),
  ( sym: 261; act: -38 ),
{ 11: }
  ( sym: 270; act: 15 ),
{ 12: }
  ( sym: 319; act: 9 ),
{ 13: }
  ( sym: 261; act: 21 ),
{ 14: }
  ( sym: 319; act: 9 ),
{ 15: }
{ 16: }
{ 17: }
  ( sym: 319; act: 9 ),
  ( sym: 261; act: -8 ),
  ( sym: 312; act: -8 ),
{ 18: }
  ( sym: 274; act: 27 ),
{ 19: }
{ 20: }
{ 21: }
  ( sym: 261; act: 21 ),
  ( sym: 319; act: 9 ),
  ( sym: 273; act: -69 ),
  ( sym: 302; act: -69 ),
{ 22: }
{ 23: }
  ( sym: 302; act: 6 ),
{ 24: }
  ( sym: 262; act: 47 ),
  ( sym: 263; act: 48 ),
{ 25: }
{ 26: }
{ 27: }
  ( sym: 286; act: 61 ),
  ( sym: 288; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 64 ),
  ( sym: 295; act: 65 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 28: }
{ 29: }
  ( sym: 260; act: 69 ),
  ( sym: 282; act: 70 ),
{ 30: }
  ( sym: 266; act: 71 ),
  ( sym: 260; act: -85 ),
  ( sym: 282; act: -85 ),
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
{ 36: }
{ 37: }
{ 38: }
{ 39: }
{ 40: }
{ 41: }
{ 42: }
  ( sym: 273; act: 73 ),
  ( sym: 302; act: 6 ),
{ 43: }
{ 44: }
  ( sym: 264; act: 74 ),
  ( sym: 265; act: 75 ),
  ( sym: 266; act: 76 ),
  ( sym: 301; act: 77 ),
  ( sym: 260; act: -84 ),
  ( sym: 282; act: -84 ),
{ 45: }
  ( sym: 319; act: 9 ),
  ( sym: 261; act: -37 ),
{ 46: }
  ( sym: 319; act: 9 ),
{ 47: }
  ( sym: 280; act: 82 ),
  ( sym: 297; act: 83 ),
  ( sym: 309; act: 84 ),
  ( sym: 319; act: 9 ),
{ 48: }
{ 49: }
{ 50: }
{ 51: }
{ 52: }
{ 53: }
  ( sym: 306; act: 85 ),
  ( sym: 257; act: -21 ),
  ( sym: 269; act: -21 ),
  ( sym: 274; act: -21 ),
  ( sym: 277; act: -21 ),
  ( sym: 278; act: -21 ),
  ( sym: 284; act: -21 ),
  ( sym: 287; act: -21 ),
  ( sym: 288; act: -21 ),
  ( sym: 289; act: -21 ),
  ( sym: 292; act: -21 ),
  ( sym: 294; act: -21 ),
  ( sym: 295; act: -21 ),
  ( sym: 300; act: -21 ),
  ( sym: 302; act: -21 ),
  ( sym: 304; act: -21 ),
  ( sym: 305; act: -21 ),
{ 54: }
{ 55: }
  ( sym: 286; act: 61 ),
  ( sym: 288; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 64 ),
  ( sym: 295; act: 65 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 56: }
{ 57: }
  ( sym: 257; act: 88 ),
  ( sym: 269; act: 89 ),
  ( sym: 289; act: 90 ),
  ( sym: 304; act: 91 ),
  ( sym: 305; act: 92 ),
  ( sym: 274; act: -15 ),
  ( sym: 277; act: -15 ),
  ( sym: 278; act: -15 ),
  ( sym: 284; act: -15 ),
  ( sym: 287; act: -15 ),
  ( sym: 288; act: -15 ),
  ( sym: 292; act: -15 ),
  ( sym: 294; act: -15 ),
  ( sym: 295; act: -15 ),
  ( sym: 300; act: -15 ),
  ( sym: 302; act: -15 ),
{ 58: }
  ( sym: 274; act: 95 ),
  ( sym: 277; act: 96 ),
  ( sym: 278; act: 97 ),
  ( sym: 284; act: 98 ),
  ( sym: 287; act: 99 ),
  ( sym: 288; act: 100 ),
  ( sym: 292; act: 101 ),
  ( sym: 294; act: 102 ),
  ( sym: 295; act: 103 ),
  ( sym: 300; act: -13 ),
  ( sym: 302; act: -13 ),
{ 59: }
  ( sym: 302; act: 6 ),
{ 60: }
{ 61: }
  ( sym: 286; act: 61 ),
  ( sym: 288; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 64 ),
  ( sym: 295; act: 65 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 62: }
{ 63: }
{ 64: }
  ( sym: 286; act: 61 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 64 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 65: }
{ 66: }
{ 67: }
{ 68: }
{ 69: }
  ( sym: 256; act: 120 ),
  ( sym: 286; act: 121 ),
  ( sym: 288; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 122 ),
  ( sym: 295; act: 65 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 70: }
  ( sym: 256; act: 120 ),
  ( sym: 286; act: 121 ),
  ( sym: 288; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 122 ),
  ( sym: 295; act: 65 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 71: }
  ( sym: 319; act: 9 ),
{ 72: }
  ( sym: 261; act: 21 ),
  ( sym: 319; act: 9 ),
  ( sym: 273; act: -69 ),
  ( sym: 302; act: -69 ),
{ 73: }
{ 74: }
  ( sym: 319; act: 9 ),
{ 75: }
  ( sym: 319; act: 9 ),
{ 76: }
  ( sym: 319; act: 9 ),
{ 77: }
  ( sym: 283; act: 135 ),
{ 78: }
{ 79: }
{ 80: }
{ 81: }
{ 82: }
{ 83: }
{ 84: }
{ 85: }
  ( sym: 286; act: 61 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 64 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 86: }
{ 87: }
  ( sym: 286; act: 61 ),
  ( sym: 288; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 64 ),
  ( sym: 295; act: 65 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 88: }
{ 89: }
{ 90: }
{ 91: }
{ 92: }
{ 93: }
  ( sym: 286; act: 61 ),
  ( sym: 288; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 64 ),
  ( sym: 295; act: 65 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 94: }
  ( sym: 286; act: 61 ),
  ( sym: 288; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 64 ),
  ( sym: 295; act: 65 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 95: }
{ 96: }
{ 97: }
{ 98: }
{ 99: }
{ 100: }
{ 101: }
{ 102: }
{ 103: }
{ 104: }
{ 105: }
  ( sym: 300; act: 140 ),
{ 106: }
{ 107: }
{ 108: }
{ 109: }
  ( sym: 306; act: 141 ),
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
  ( sym: 298; act: -111 ),
  ( sym: 299; act: -111 ),
  ( sym: 300; act: -111 ),
  ( sym: 302; act: -111 ),
  ( sym: 304; act: -111 ),
  ( sym: 305; act: -111 ),
{ 110: }
{ 111: }
{ 112: }
  ( sym: 257; act: 88 ),
  ( sym: 269; act: 89 ),
  ( sym: 289; act: 90 ),
  ( sym: 304; act: 91 ),
  ( sym: 305; act: 92 ),
  ( sym: 263; act: -105 ),
  ( sym: 273; act: -105 ),
  ( sym: 274; act: -105 ),
  ( sym: 277; act: -105 ),
  ( sym: 278; act: -105 ),
  ( sym: 284; act: -105 ),
  ( sym: 287; act: -105 ),
  ( sym: 288; act: -105 ),
  ( sym: 292; act: -105 ),
  ( sym: 294; act: -105 ),
  ( sym: 295; act: -105 ),
  ( sym: 298; act: -105 ),
  ( sym: 299; act: -105 ),
  ( sym: 300; act: -105 ),
  ( sym: 302; act: -105 ),
{ 113: }
  ( sym: 274; act: 95 ),
  ( sym: 277; act: 96 ),
  ( sym: 278; act: 97 ),
  ( sym: 284; act: 98 ),
  ( sym: 287; act: 99 ),
  ( sym: 288; act: 100 ),
  ( sym: 292; act: 101 ),
  ( sym: 294; act: 102 ),
  ( sym: 295; act: 103 ),
  ( sym: 263; act: -102 ),
  ( sym: 273; act: -102 ),
  ( sym: 298; act: -102 ),
  ( sym: 299; act: -102 ),
  ( sym: 300; act: -102 ),
  ( sym: 302; act: -102 ),
{ 114: }
  ( sym: 282; act: 70 ),
  ( sym: 257; act: -113 ),
  ( sym: 263; act: -113 ),
  ( sym: 269; act: -113 ),
  ( sym: 273; act: -113 ),
  ( sym: 274; act: -113 ),
  ( sym: 277; act: -113 ),
  ( sym: 278; act: -113 ),
  ( sym: 284; act: -113 ),
  ( sym: 287; act: -113 ),
  ( sym: 288; act: -113 ),
  ( sym: 289; act: -113 ),
  ( sym: 292; act: -113 ),
  ( sym: 294; act: -113 ),
  ( sym: 295; act: -113 ),
  ( sym: 298; act: -113 ),
  ( sym: 299; act: -113 ),
  ( sym: 300; act: -113 ),
  ( sym: 302; act: -113 ),
  ( sym: 304; act: -113 ),
  ( sym: 305; act: -113 ),
  ( sym: 306; act: -113 ),
{ 115: }
{ 116: }
{ 117: }
{ 118: }
  ( sym: 286; act: 121 ),
  ( sym: 288; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 122 ),
  ( sym: 295; act: 65 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 119: }
{ 120: }
{ 121: }
  ( sym: 256; act: 120 ),
  ( sym: 286; act: 121 ),
  ( sym: 288; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 122 ),
  ( sym: 295; act: 65 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 122: }
  ( sym: 286; act: 121 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 122 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 123: }
{ 124: }
  ( sym: 263; act: 48 ),
  ( sym: 298; act: 149 ),
{ 125: }
{ 126: }
  ( sym: 282; act: 70 ),
{ 127: }
  ( sym: 273; act: -79 ),
  ( sym: 302; act: -79 ),
  ( sym: 282; act: -85 ),
{ 128: }
{ 129: }
{ 130: }
  ( sym: 264; act: 74 ),
  ( sym: 273; act: -73 ),
  ( sym: 302; act: -73 ),
{ 131: }
{ 132: }
  ( sym: 265; act: 75 ),
  ( sym: 273; act: -75 ),
  ( sym: 302; act: -75 ),
{ 133: }
{ 134: }
  ( sym: 266; act: 76 ),
  ( sym: 273; act: -77 ),
  ( sym: 302; act: -77 ),
  ( sym: 282; act: -84 ),
{ 135: }
  ( sym: 256; act: 120 ),
  ( sym: 286; act: 121 ),
  ( sym: 288; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 122 ),
  ( sym: 295; act: 65 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 136: }
{ 137: }
{ 138: }
  ( sym: 257; act: 88 ),
  ( sym: 269; act: 89 ),
  ( sym: 289; act: 90 ),
  ( sym: 304; act: 91 ),
  ( sym: 305; act: 92 ),
  ( sym: 274; act: -16 ),
  ( sym: 277; act: -16 ),
  ( sym: 278; act: -16 ),
  ( sym: 284; act: -16 ),
  ( sym: 287; act: -16 ),
  ( sym: 288; act: -16 ),
  ( sym: 292; act: -16 ),
  ( sym: 294; act: -16 ),
  ( sym: 295; act: -16 ),
  ( sym: 300; act: -16 ),
  ( sym: 302; act: -16 ),
{ 139: }
  ( sym: 288; act: 100 ),
  ( sym: 294; act: 102 ),
  ( sym: 295; act: 103 ),
  ( sym: 300; act: -14 ),
  ( sym: 302; act: -14 ),
{ 140: }
{ 141: }
  ( sym: 286; act: 121 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 122 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 142: }
  ( sym: 286; act: 121 ),
  ( sym: 288; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 122 ),
  ( sym: 295; act: 65 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 143: }
  ( sym: 286; act: 121 ),
  ( sym: 288; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 122 ),
  ( sym: 295; act: 65 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 144: }
  ( sym: 286; act: 121 ),
  ( sym: 288; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 122 ),
  ( sym: 295; act: 65 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 145: }
{ 146: }
  ( sym: 300; act: 155 ),
{ 147: }
{ 148: }
  ( sym: 256; act: 120 ),
  ( sym: 286; act: 121 ),
  ( sym: 288; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 122 ),
  ( sym: 295; act: 65 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 149: }
{ 150: }
  ( sym: 263; act: 157 ),
  ( sym: 299; act: 158 ),
{ 151: }
{ 152: }
{ 153: }
  ( sym: 257; act: 88 ),
  ( sym: 269; act: 89 ),
  ( sym: 289; act: 90 ),
  ( sym: 304; act: 91 ),
  ( sym: 305; act: 92 ),
  ( sym: 263; act: -106 ),
  ( sym: 273; act: -106 ),
  ( sym: 274; act: -106 ),
  ( sym: 277; act: -106 ),
  ( sym: 278; act: -106 ),
  ( sym: 284; act: -106 ),
  ( sym: 287; act: -106 ),
  ( sym: 288; act: -106 ),
  ( sym: 292; act: -106 ),
  ( sym: 294; act: -106 ),
  ( sym: 295; act: -106 ),
  ( sym: 298; act: -106 ),
  ( sym: 299; act: -106 ),
  ( sym: 300; act: -106 ),
  ( sym: 302; act: -106 ),
{ 154: }
  ( sym: 288; act: 100 ),
  ( sym: 294; act: 102 ),
  ( sym: 295; act: 103 ),
  ( sym: 263; act: -103 ),
  ( sym: 273; act: -103 ),
  ( sym: 298; act: -103 ),
  ( sym: 299; act: -103 ),
  ( sym: 300; act: -103 ),
  ( sym: 302; act: -103 ),
{ 155: }
{ 156: }
{ 157: }
  ( sym: 256; act: 120 ),
  ( sym: 286; act: 121 ),
  ( sym: 288; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 122 ),
  ( sym: 295; act: 65 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 158: }
{ 159: }
  ( sym: 263; act: 161 ),
{ 160: }
{ 161: }
  ( sym: 256; act: 120 ),
  ( sym: 286; act: 121 ),
  ( sym: 288; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 291; act: 122 ),
  ( sym: 295; act: 65 ),
  ( sym: 316; act: 66 ),
  ( sym: 317; act: 67 ),
  ( sym: 318; act: 68 ),
  ( sym: 319; act: 9 ),
{ 162: }
  ( sym: 299; act: 163 )
{ 163: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -7; act: 1 ),
  ( sym: -6; act: 2 ),
  ( sym: -5; act: 3 ),
{ 1: }
  ( sym: -8; act: 5 ),
{ 2: }
{ 3: }
{ 4: }
  ( sym: -2; act: 8 ),
{ 5: }
  ( sym: -11; act: 10 ),
  ( sym: -9; act: 11 ),
{ 6: }
{ 7: }
{ 8: }
{ 9: }
{ 10: }
  ( sym: -12; act: 13 ),
{ 11: }
{ 12: }
  ( sym: -15; act: 16 ),
  ( sym: -14; act: 17 ),
  ( sym: -2; act: 18 ),
{ 13: }
  ( sym: -45; act: 19 ),
  ( sym: -13; act: 20 ),
{ 14: }
  ( sym: -33; act: 22 ),
  ( sym: -32; act: 23 ),
  ( sym: -3; act: 24 ),
  ( sym: -2; act: 25 ),
{ 15: }
{ 16: }
{ 17: }
  ( sym: -15; act: 26 ),
  ( sym: -2; act: 18 ),
{ 18: }
{ 19: }
{ 20: }
{ 21: }
  ( sym: -62; act: 28 ),
  ( sym: -61; act: 29 ),
  ( sym: -58; act: 30 ),
  ( sym: -57; act: 31 ),
  ( sym: -56; act: 32 ),
  ( sym: -55; act: 33 ),
  ( sym: -54; act: 34 ),
  ( sym: -53; act: 35 ),
  ( sym: -52; act: 36 ),
  ( sym: -51; act: 37 ),
  ( sym: -50; act: 38 ),
  ( sym: -49; act: 39 ),
  ( sym: -48; act: 40 ),
  ( sym: -47; act: 41 ),
  ( sym: -46; act: 42 ),
  ( sym: -45; act: 43 ),
  ( sym: -2; act: 44 ),
{ 22: }
{ 23: }
  ( sym: -8; act: 45 ),
{ 24: }
  ( sym: -10; act: 46 ),
{ 25: }
{ 26: }
{ 27: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -26; act: 52 ),
  ( sym: -25; act: 53 ),
  ( sym: -24; act: 54 ),
  ( sym: -23; act: 55 ),
  ( sym: -21; act: 56 ),
  ( sym: -19; act: 57 ),
  ( sym: -17; act: 58 ),
  ( sym: -16; act: 59 ),
  ( sym: -2; act: 60 ),
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
{ 38: }
{ 39: }
{ 40: }
{ 41: }
{ 42: }
  ( sym: -8; act: 72 ),
{ 43: }
{ 44: }
{ 45: }
  ( sym: -33; act: 78 ),
  ( sym: -3; act: 24 ),
  ( sym: -2; act: 25 ),
{ 46: }
  ( sym: -2; act: 79 ),
{ 47: }
  ( sym: -4; act: 80 ),
  ( sym: -2; act: 81 ),
{ 48: }
{ 49: }
{ 50: }
{ 51: }
{ 52: }
{ 53: }
{ 54: }
{ 55: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -26; act: 52 ),
  ( sym: -25; act: 53 ),
  ( sym: -24; act: 54 ),
  ( sym: -23; act: 55 ),
  ( sym: -21; act: 86 ),
  ( sym: -2; act: 60 ),
{ 56: }
{ 57: }
  ( sym: -22; act: 87 ),
{ 58: }
  ( sym: -20; act: 93 ),
  ( sym: -18; act: 94 ),
{ 59: }
  ( sym: -8; act: 104 ),
{ 60: }
{ 61: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -26; act: 52 ),
  ( sym: -25; act: 53 ),
  ( sym: -24; act: 54 ),
  ( sym: -23; act: 55 ),
  ( sym: -21; act: 56 ),
  ( sym: -19; act: 57 ),
  ( sym: -17; act: 58 ),
  ( sym: -16; act: 105 ),
  ( sym: -2; act: 60 ),
{ 62: }
{ 63: }
{ 64: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -26; act: 52 ),
  ( sym: -25; act: 106 ),
  ( sym: -2; act: 60 ),
{ 65: }
{ 66: }
{ 67: }
{ 68: }
{ 69: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -77; act: 107 ),
  ( sym: -76; act: 108 ),
  ( sym: -75; act: 109 ),
  ( sym: -74; act: 110 ),
  ( sym: -73; act: 111 ),
  ( sym: -72; act: 112 ),
  ( sym: -71; act: 113 ),
  ( sym: -62; act: 28 ),
  ( sym: -61; act: 114 ),
  ( sym: -59; act: 115 ),
  ( sym: -58; act: 116 ),
  ( sym: -26; act: 117 ),
  ( sym: -23; act: 118 ),
  ( sym: -2; act: 119 ),
{ 70: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -77; act: 107 ),
  ( sym: -76; act: 108 ),
  ( sym: -75; act: 109 ),
  ( sym: -74; act: 110 ),
  ( sym: -73; act: 111 ),
  ( sym: -72; act: 112 ),
  ( sym: -71; act: 113 ),
  ( sym: -64; act: 123 ),
  ( sym: -63; act: 124 ),
  ( sym: -62; act: 28 ),
  ( sym: -61; act: 114 ),
  ( sym: -59; act: 125 ),
  ( sym: -58; act: 116 ),
  ( sym: -26; act: 117 ),
  ( sym: -23; act: 118 ),
  ( sym: -2; act: 119 ),
{ 71: }
  ( sym: -62; act: 28 ),
  ( sym: -61; act: 126 ),
  ( sym: -58; act: 127 ),
  ( sym: -2; act: 119 ),
{ 72: }
  ( sym: -62; act: 28 ),
  ( sym: -61; act: 29 ),
  ( sym: -58; act: 30 ),
  ( sym: -57; act: 31 ),
  ( sym: -56; act: 32 ),
  ( sym: -55; act: 33 ),
  ( sym: -54; act: 34 ),
  ( sym: -53; act: 35 ),
  ( sym: -52; act: 36 ),
  ( sym: -51; act: 37 ),
  ( sym: -50; act: 38 ),
  ( sym: -49; act: 39 ),
  ( sym: -48; act: 40 ),
  ( sym: -47; act: 128 ),
  ( sym: -45; act: 43 ),
  ( sym: -2; act: 44 ),
{ 73: }
{ 74: }
  ( sym: -55; act: 129 ),
  ( sym: -2; act: 130 ),
{ 75: }
  ( sym: -56; act: 131 ),
  ( sym: -2; act: 132 ),
{ 76: }
  ( sym: -62; act: 28 ),
  ( sym: -61; act: 126 ),
  ( sym: -58; act: 30 ),
  ( sym: -57; act: 133 ),
  ( sym: -2; act: 134 ),
{ 77: }
{ 78: }
{ 79: }
{ 80: }
{ 81: }
{ 82: }
{ 83: }
{ 84: }
{ 85: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -26; act: 52 ),
  ( sym: -25; act: 53 ),
  ( sym: -24; act: 136 ),
  ( sym: -2; act: 60 ),
{ 86: }
{ 87: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -26; act: 52 ),
  ( sym: -25; act: 53 ),
  ( sym: -24; act: 54 ),
  ( sym: -23; act: 55 ),
  ( sym: -21; act: 137 ),
  ( sym: -2; act: 60 ),
{ 88: }
{ 89: }
{ 90: }
{ 91: }
{ 92: }
{ 93: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -26; act: 52 ),
  ( sym: -25; act: 53 ),
  ( sym: -24; act: 54 ),
  ( sym: -23; act: 55 ),
  ( sym: -21; act: 56 ),
  ( sym: -19; act: 138 ),
  ( sym: -2; act: 60 ),
{ 94: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -26; act: 52 ),
  ( sym: -25; act: 53 ),
  ( sym: -24; act: 54 ),
  ( sym: -23; act: 55 ),
  ( sym: -21; act: 56 ),
  ( sym: -19; act: 57 ),
  ( sym: -17; act: 139 ),
  ( sym: -2; act: 60 ),
{ 95: }
{ 96: }
{ 97: }
{ 98: }
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
{ 110: }
{ 111: }
{ 112: }
  ( sym: -22; act: 142 ),
{ 113: }
  ( sym: -20; act: 143 ),
  ( sym: -18; act: 144 ),
{ 114: }
{ 115: }
{ 116: }
{ 117: }
{ 118: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -77; act: 107 ),
  ( sym: -76; act: 108 ),
  ( sym: -75; act: 109 ),
  ( sym: -74; act: 110 ),
  ( sym: -73; act: 145 ),
  ( sym: -62; act: 28 ),
  ( sym: -61; act: 114 ),
  ( sym: -58; act: 116 ),
  ( sym: -26; act: 117 ),
  ( sym: -23; act: 118 ),
  ( sym: -2; act: 119 ),
{ 119: }
{ 120: }
{ 121: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -77; act: 107 ),
  ( sym: -76; act: 108 ),
  ( sym: -75; act: 109 ),
  ( sym: -74; act: 110 ),
  ( sym: -73; act: 111 ),
  ( sym: -72; act: 112 ),
  ( sym: -71; act: 113 ),
  ( sym: -62; act: 28 ),
  ( sym: -61; act: 114 ),
  ( sym: -59; act: 146 ),
  ( sym: -58; act: 116 ),
  ( sym: -26; act: 117 ),
  ( sym: -23; act: 118 ),
  ( sym: -2; act: 119 ),
{ 122: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -77; act: 107 ),
  ( sym: -76; act: 108 ),
  ( sym: -75; act: 147 ),
  ( sym: -62; act: 28 ),
  ( sym: -61; act: 114 ),
  ( sym: -58; act: 116 ),
  ( sym: -26; act: 117 ),
  ( sym: -2; act: 119 ),
{ 123: }
{ 124: }
  ( sym: -10; act: 148 ),
{ 125: }
{ 126: }
{ 127: }
{ 128: }
{ 129: }
{ 130: }
{ 131: }
{ 132: }
{ 133: }
{ 134: }
{ 135: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -77; act: 107 ),
  ( sym: -76; act: 108 ),
  ( sym: -75; act: 109 ),
  ( sym: -74; act: 110 ),
  ( sym: -73; act: 111 ),
  ( sym: -72; act: 112 ),
  ( sym: -71; act: 113 ),
  ( sym: -62; act: 28 ),
  ( sym: -61; act: 114 ),
  ( sym: -59; act: 150 ),
  ( sym: -58; act: 116 ),
  ( sym: -26; act: 117 ),
  ( sym: -23; act: 118 ),
  ( sym: -2; act: 119 ),
{ 136: }
{ 137: }
{ 138: }
  ( sym: -22; act: 87 ),
{ 139: }
  ( sym: -20; act: 93 ),
{ 140: }
{ 141: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -77; act: 107 ),
  ( sym: -76; act: 108 ),
  ( sym: -75; act: 109 ),
  ( sym: -74; act: 151 ),
  ( sym: -62; act: 28 ),
  ( sym: -61; act: 114 ),
  ( sym: -58; act: 116 ),
  ( sym: -26; act: 117 ),
  ( sym: -2; act: 119 ),
{ 142: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -77; act: 107 ),
  ( sym: -76; act: 108 ),
  ( sym: -75; act: 109 ),
  ( sym: -74; act: 110 ),
  ( sym: -73; act: 152 ),
  ( sym: -62; act: 28 ),
  ( sym: -61; act: 114 ),
  ( sym: -58; act: 116 ),
  ( sym: -26; act: 117 ),
  ( sym: -23; act: 118 ),
  ( sym: -2; act: 119 ),
{ 143: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -77; act: 107 ),
  ( sym: -76; act: 108 ),
  ( sym: -75; act: 109 ),
  ( sym: -74; act: 110 ),
  ( sym: -73; act: 111 ),
  ( sym: -72; act: 153 ),
  ( sym: -62; act: 28 ),
  ( sym: -61; act: 114 ),
  ( sym: -58; act: 116 ),
  ( sym: -26; act: 117 ),
  ( sym: -23; act: 118 ),
  ( sym: -2; act: 119 ),
{ 144: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -77; act: 107 ),
  ( sym: -76; act: 108 ),
  ( sym: -75; act: 109 ),
  ( sym: -74; act: 110 ),
  ( sym: -73; act: 111 ),
  ( sym: -72; act: 112 ),
  ( sym: -71; act: 154 ),
  ( sym: -62; act: 28 ),
  ( sym: -61; act: 114 ),
  ( sym: -58; act: 116 ),
  ( sym: -26; act: 117 ),
  ( sym: -23; act: 118 ),
  ( sym: -2; act: 119 ),
{ 145: }
{ 146: }
{ 147: }
{ 148: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -77; act: 107 ),
  ( sym: -76; act: 108 ),
  ( sym: -75; act: 109 ),
  ( sym: -74; act: 110 ),
  ( sym: -73; act: 111 ),
  ( sym: -72; act: 112 ),
  ( sym: -71; act: 113 ),
  ( sym: -64; act: 156 ),
  ( sym: -62; act: 28 ),
  ( sym: -61; act: 114 ),
  ( sym: -59; act: 125 ),
  ( sym: -58; act: 116 ),
  ( sym: -26; act: 117 ),
  ( sym: -23; act: 118 ),
  ( sym: -2; act: 119 ),
{ 149: }
{ 150: }
{ 151: }
{ 152: }
{ 153: }
  ( sym: -22; act: 142 ),
{ 154: }
  ( sym: -20; act: 143 ),
{ 155: }
{ 156: }
{ 157: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -77; act: 107 ),
  ( sym: -76; act: 108 ),
  ( sym: -75; act: 109 ),
  ( sym: -74; act: 110 ),
  ( sym: -73; act: 111 ),
  ( sym: -72; act: 112 ),
  ( sym: -71; act: 113 ),
  ( sym: -62; act: 28 ),
  ( sym: -61; act: 114 ),
  ( sym: -59; act: 159 ),
  ( sym: -58; act: 116 ),
  ( sym: -26; act: 117 ),
  ( sym: -23; act: 118 ),
  ( sym: -2; act: 119 ),
{ 158: }
{ 159: }
  ( sym: -60; act: 160 ),
{ 160: }
{ 161: }
  ( sym: -80; act: 49 ),
  ( sym: -79; act: 50 ),
  ( sym: -78; act: 51 ),
  ( sym: -77; act: 107 ),
  ( sym: -76; act: 108 ),
  ( sym: -75; act: 109 ),
  ( sym: -74; act: 110 ),
  ( sym: -73; act: 111 ),
  ( sym: -72; act: 112 ),
  ( sym: -71; act: 113 ),
  ( sym: -62; act: 28 ),
  ( sym: -61; act: 114 ),
  ( sym: -59; act: 162 ),
  ( sym: -58; act: 116 ),
  ( sym: -26; act: 117 ),
  ( sym: -23; act: 118 ),
  ( sym: -2; act: 119 )
{ 162: }
{ 163: }
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
{ 19: } -57,
{ 20: } -7,
{ 21: } 0,
{ 22: } -40,
{ 23: } 0,
{ 24: } 0,
{ 25: } -6,
{ 26: } -10,
{ 27: } 0,
{ 28: } -86,
{ 29: } 0,
{ 30: } 0,
{ 31: } -72,
{ 32: } -71,
{ 33: } -70,
{ 34: } -68,
{ 35: } -67,
{ 36: } -65,
{ 37: } -64,
{ 38: } -63,
{ 39: } -62,
{ 40: } -61,
{ 41: } -60,
{ 42: } 0,
{ 43: } -66,
{ 44: } 0,
{ 45: } 0,
{ 46: } 0,
{ 47: } 0,
{ 48: } -142,
{ 49: } -123,
{ 50: } -122,
{ 51: } -119,
{ 52: } -25,
{ 53: } 0,
{ 54: } -20,
{ 55: } 0,
{ 56: } -17,
{ 57: } 0,
{ 58: } 0,
{ 59: } 0,
{ 60: } -23,
{ 61: } 0,
{ 62: } -31,
{ 63: } -121,
{ 64: } 0,
{ 65: } -30,
{ 66: } -125,
{ 67: } -124,
{ 68: } -120,
{ 69: } 0,
{ 70: } 0,
{ 71: } 0,
{ 72: } 0,
{ 73: } -58,
{ 74: } 0,
{ 75: } 0,
{ 76: } 0,
{ 77: } 0,
{ 78: } -39,
{ 79: } -5,
{ 80: } -44,
{ 81: } -96,
{ 82: } -41,
{ 83: } -42,
{ 84: } -43,
{ 85: } 0,
{ 86: } -19,
{ 87: } 0,
{ 88: } -133,
{ 89: } -131,
{ 90: } -132,
{ 91: } -130,
{ 92: } -129,
{ 93: } 0,
{ 94: } 0,
{ 95: } -134,
{ 96: } -139,
{ 97: } -137,
{ 98: } -138,
{ 99: } -136,
{ 100: } -127,
{ 101: } -135,
{ 102: } -128,
{ 103: } -126,
{ 104: } -12,
{ 105: } 0,
{ 106: } -26,
{ 107: } -116,
{ 108: } -115,
{ 109: } 0,
{ 110: } -110,
{ 111: } -107,
{ 112: } 0,
{ 113: } 0,
{ 114: } 0,
{ 115: } -83,
{ 116: } -85,
{ 117: } -114,
{ 118: } 0,
{ 119: } -84,
{ 120: } -104,
{ 121: } 0,
{ 122: } 0,
{ 123: } -89,
{ 124: } 0,
{ 125: } -90,
{ 126: } 0,
{ 127: } 0,
{ 128: } -59,
{ 129: } -74,
{ 130: } 0,
{ 131: } -76,
{ 132: } 0,
{ 133: } -78,
{ 134: } 0,
{ 135: } 0,
{ 136: } -22,
{ 137: } -18,
{ 138: } 0,
{ 139: } 0,
{ 140: } -24,
{ 141: } 0,
{ 142: } 0,
{ 143: } 0,
{ 144: } 0,
{ 145: } -109,
{ 146: } 0,
{ 147: } -118,
{ 148: } 0,
{ 149: } -87,
{ 150: } 0,
{ 151: } -112,
{ 152: } -108,
{ 153: } 0,
{ 154: } 0,
{ 155: } -117,
{ 156: } -88,
{ 157: } 0,
{ 158: } -82,
{ 159: } 0,
{ 160: } -81,
{ 161: } 0,
{ 162: } 0,
{ 163: } -80
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
{ 28: } 36,
{ 29: } 36,
{ 30: } 38,
{ 31: } 41,
{ 32: } 41,
{ 33: } 41,
{ 34: } 41,
{ 35: } 41,
{ 36: } 41,
{ 37: } 41,
{ 38: } 41,
{ 39: } 41,
{ 40: } 41,
{ 41: } 41,
{ 42: } 41,
{ 43: } 43,
{ 44: } 43,
{ 45: } 49,
{ 46: } 51,
{ 47: } 52,
{ 48: } 56,
{ 49: } 56,
{ 50: } 56,
{ 51: } 56,
{ 52: } 56,
{ 53: } 56,
{ 54: } 73,
{ 55: } 73,
{ 56: } 82,
{ 57: } 82,
{ 58: } 98,
{ 59: } 109,
{ 60: } 110,
{ 61: } 110,
{ 62: } 119,
{ 63: } 119,
{ 64: } 119,
{ 65: } 126,
{ 66: } 126,
{ 67: } 126,
{ 68: } 126,
{ 69: } 126,
{ 70: } 136,
{ 71: } 146,
{ 72: } 147,
{ 73: } 151,
{ 74: } 151,
{ 75: } 152,
{ 76: } 153,
{ 77: } 154,
{ 78: } 155,
{ 79: } 155,
{ 80: } 155,
{ 81: } 155,
{ 82: } 155,
{ 83: } 155,
{ 84: } 155,
{ 85: } 155,
{ 86: } 162,
{ 87: } 162,
{ 88: } 171,
{ 89: } 171,
{ 90: } 171,
{ 91: } 171,
{ 92: } 171,
{ 93: } 171,
{ 94: } 180,
{ 95: } 189,
{ 96: } 189,
{ 97: } 189,
{ 98: } 189,
{ 99: } 189,
{ 100: } 189,
{ 101: } 189,
{ 102: } 189,
{ 103: } 189,
{ 104: } 189,
{ 105: } 189,
{ 106: } 190,
{ 107: } 190,
{ 108: } 190,
{ 109: } 190,
{ 110: } 211,
{ 111: } 211,
{ 112: } 211,
{ 113: } 231,
{ 114: } 246,
{ 115: } 268,
{ 116: } 268,
{ 117: } 268,
{ 118: } 268,
{ 119: } 277,
{ 120: } 277,
{ 121: } 277,
{ 122: } 287,
{ 123: } 294,
{ 124: } 294,
{ 125: } 296,
{ 126: } 296,
{ 127: } 297,
{ 128: } 300,
{ 129: } 300,
{ 130: } 300,
{ 131: } 303,
{ 132: } 303,
{ 133: } 306,
{ 134: } 306,
{ 135: } 310,
{ 136: } 320,
{ 137: } 320,
{ 138: } 320,
{ 139: } 336,
{ 140: } 341,
{ 141: } 341,
{ 142: } 348,
{ 143: } 357,
{ 144: } 366,
{ 145: } 375,
{ 146: } 375,
{ 147: } 376,
{ 148: } 376,
{ 149: } 386,
{ 150: } 386,
{ 151: } 388,
{ 152: } 388,
{ 153: } 388,
{ 154: } 408,
{ 155: } 417,
{ 156: } 417,
{ 157: } 417,
{ 158: } 427,
{ 159: } 427,
{ 160: } 428,
{ 161: } 428,
{ 162: } 438,
{ 163: } 439
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
{ 27: } 35,
{ 28: } 35,
{ 29: } 37,
{ 30: } 40,
{ 31: } 40,
{ 32: } 40,
{ 33: } 40,
{ 34: } 40,
{ 35: } 40,
{ 36: } 40,
{ 37: } 40,
{ 38: } 40,
{ 39: } 40,
{ 40: } 40,
{ 41: } 40,
{ 42: } 42,
{ 43: } 42,
{ 44: } 48,
{ 45: } 50,
{ 46: } 51,
{ 47: } 55,
{ 48: } 55,
{ 49: } 55,
{ 50: } 55,
{ 51: } 55,
{ 52: } 55,
{ 53: } 72,
{ 54: } 72,
{ 55: } 81,
{ 56: } 81,
{ 57: } 97,
{ 58: } 108,
{ 59: } 109,
{ 60: } 109,
{ 61: } 118,
{ 62: } 118,
{ 63: } 118,
{ 64: } 125,
{ 65: } 125,
{ 66: } 125,
{ 67: } 125,
{ 68: } 125,
{ 69: } 135,
{ 70: } 145,
{ 71: } 146,
{ 72: } 150,
{ 73: } 150,
{ 74: } 151,
{ 75: } 152,
{ 76: } 153,
{ 77: } 154,
{ 78: } 154,
{ 79: } 154,
{ 80: } 154,
{ 81: } 154,
{ 82: } 154,
{ 83: } 154,
{ 84: } 154,
{ 85: } 161,
{ 86: } 161,
{ 87: } 170,
{ 88: } 170,
{ 89: } 170,
{ 90: } 170,
{ 91: } 170,
{ 92: } 170,
{ 93: } 179,
{ 94: } 188,
{ 95: } 188,
{ 96: } 188,
{ 97: } 188,
{ 98: } 188,
{ 99: } 188,
{ 100: } 188,
{ 101: } 188,
{ 102: } 188,
{ 103: } 188,
{ 104: } 188,
{ 105: } 189,
{ 106: } 189,
{ 107: } 189,
{ 108: } 189,
{ 109: } 210,
{ 110: } 210,
{ 111: } 210,
{ 112: } 230,
{ 113: } 245,
{ 114: } 267,
{ 115: } 267,
{ 116: } 267,
{ 117: } 267,
{ 118: } 276,
{ 119: } 276,
{ 120: } 276,
{ 121: } 286,
{ 122: } 293,
{ 123: } 293,
{ 124: } 295,
{ 125: } 295,
{ 126: } 296,
{ 127: } 299,
{ 128: } 299,
{ 129: } 299,
{ 130: } 302,
{ 131: } 302,
{ 132: } 305,
{ 133: } 305,
{ 134: } 309,
{ 135: } 319,
{ 136: } 319,
{ 137: } 319,
{ 138: } 335,
{ 139: } 340,
{ 140: } 340,
{ 141: } 347,
{ 142: } 356,
{ 143: } 365,
{ 144: } 374,
{ 145: } 374,
{ 146: } 375,
{ 147: } 375,
{ 148: } 385,
{ 149: } 385,
{ 150: } 387,
{ 151: } 387,
{ 152: } 387,
{ 153: } 407,
{ 154: } 416,
{ 155: } 416,
{ 156: } 416,
{ 157: } 426,
{ 158: } 426,
{ 159: } 427,
{ 160: } 427,
{ 161: } 437,
{ 162: } 438,
{ 163: } 438
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
{ 22: } 37,
{ 23: } 37,
{ 24: } 38,
{ 25: } 39,
{ 26: } 39,
{ 27: } 39,
{ 28: } 51,
{ 29: } 51,
{ 30: } 51,
{ 31: } 51,
{ 32: } 51,
{ 33: } 51,
{ 34: } 51,
{ 35: } 51,
{ 36: } 51,
{ 37: } 51,
{ 38: } 51,
{ 39: } 51,
{ 40: } 51,
{ 41: } 51,
{ 42: } 51,
{ 43: } 52,
{ 44: } 52,
{ 45: } 52,
{ 46: } 55,
{ 47: } 56,
{ 48: } 58,
{ 49: } 58,
{ 50: } 58,
{ 51: } 58,
{ 52: } 58,
{ 53: } 58,
{ 54: } 58,
{ 55: } 58,
{ 56: } 67,
{ 57: } 67,
{ 58: } 68,
{ 59: } 70,
{ 60: } 71,
{ 61: } 71,
{ 62: } 83,
{ 63: } 83,
{ 64: } 83,
{ 65: } 89,
{ 66: } 89,
{ 67: } 89,
{ 68: } 89,
{ 69: } 89,
{ 70: } 106,
{ 71: } 125,
{ 72: } 129,
{ 73: } 145,
{ 74: } 145,
{ 75: } 147,
{ 76: } 149,
{ 77: } 154,
{ 78: } 154,
{ 79: } 154,
{ 80: } 154,
{ 81: } 154,
{ 82: } 154,
{ 83: } 154,
{ 84: } 154,
{ 85: } 154,
{ 86: } 161,
{ 87: } 161,
{ 88: } 170,
{ 89: } 170,
{ 90: } 170,
{ 91: } 170,
{ 92: } 170,
{ 93: } 170,
{ 94: } 180,
{ 95: } 191,
{ 96: } 191,
{ 97: } 191,
{ 98: } 191,
{ 99: } 191,
{ 100: } 191,
{ 101: } 191,
{ 102: } 191,
{ 103: } 191,
{ 104: } 191,
{ 105: } 191,
{ 106: } 191,
{ 107: } 191,
{ 108: } 191,
{ 109: } 191,
{ 110: } 191,
{ 111: } 191,
{ 112: } 191,
{ 113: } 192,
{ 114: } 194,
{ 115: } 194,
{ 116: } 194,
{ 117: } 194,
{ 118: } 194,
{ 119: } 208,
{ 120: } 208,
{ 121: } 208,
{ 122: } 225,
{ 123: } 236,
{ 124: } 236,
{ 125: } 237,
{ 126: } 237,
{ 127: } 237,
{ 128: } 237,
{ 129: } 237,
{ 130: } 237,
{ 131: } 237,
{ 132: } 237,
{ 133: } 237,
{ 134: } 237,
{ 135: } 237,
{ 136: } 254,
{ 137: } 254,
{ 138: } 254,
{ 139: } 255,
{ 140: } 256,
{ 141: } 256,
{ 142: } 268,
{ 143: } 282,
{ 144: } 297,
{ 145: } 313,
{ 146: } 313,
{ 147: } 313,
{ 148: } 313,
{ 149: } 331,
{ 150: } 331,
{ 151: } 331,
{ 152: } 331,
{ 153: } 331,
{ 154: } 332,
{ 155: } 333,
{ 156: } 333,
{ 157: } 333,
{ 158: } 350,
{ 159: } 350,
{ 160: } 351,
{ 161: } 351,
{ 162: } 368,
{ 163: } 368
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
{ 21: } 36,
{ 22: } 36,
{ 23: } 37,
{ 24: } 38,
{ 25: } 38,
{ 26: } 38,
{ 27: } 50,
{ 28: } 50,
{ 29: } 50,
{ 30: } 50,
{ 31: } 50,
{ 32: } 50,
{ 33: } 50,
{ 34: } 50,
{ 35: } 50,
{ 36: } 50,
{ 37: } 50,
{ 38: } 50,
{ 39: } 50,
{ 40: } 50,
{ 41: } 50,
{ 42: } 51,
{ 43: } 51,
{ 44: } 51,
{ 45: } 54,
{ 46: } 55,
{ 47: } 57,
{ 48: } 57,
{ 49: } 57,
{ 50: } 57,
{ 51: } 57,
{ 52: } 57,
{ 53: } 57,
{ 54: } 57,
{ 55: } 66,
{ 56: } 66,
{ 57: } 67,
{ 58: } 69,
{ 59: } 70,
{ 60: } 70,
{ 61: } 82,
{ 62: } 82,
{ 63: } 82,
{ 64: } 88,
{ 65: } 88,
{ 66: } 88,
{ 67: } 88,
{ 68: } 88,
{ 69: } 105,
{ 70: } 124,
{ 71: } 128,
{ 72: } 144,
{ 73: } 144,
{ 74: } 146,
{ 75: } 148,
{ 76: } 153,
{ 77: } 153,
{ 78: } 153,
{ 79: } 153,
{ 80: } 153,
{ 81: } 153,
{ 82: } 153,
{ 83: } 153,
{ 84: } 153,
{ 85: } 160,
{ 86: } 160,
{ 87: } 169,
{ 88: } 169,
{ 89: } 169,
{ 90: } 169,
{ 91: } 169,
{ 92: } 169,
{ 93: } 179,
{ 94: } 190,
{ 95: } 190,
{ 96: } 190,
{ 97: } 190,
{ 98: } 190,
{ 99: } 190,
{ 100: } 190,
{ 101: } 190,
{ 102: } 190,
{ 103: } 190,
{ 104: } 190,
{ 105: } 190,
{ 106: } 190,
{ 107: } 190,
{ 108: } 190,
{ 109: } 190,
{ 110: } 190,
{ 111: } 190,
{ 112: } 191,
{ 113: } 193,
{ 114: } 193,
{ 115: } 193,
{ 116: } 193,
{ 117: } 193,
{ 118: } 207,
{ 119: } 207,
{ 120: } 207,
{ 121: } 224,
{ 122: } 235,
{ 123: } 235,
{ 124: } 236,
{ 125: } 236,
{ 126: } 236,
{ 127: } 236,
{ 128: } 236,
{ 129: } 236,
{ 130: } 236,
{ 131: } 236,
{ 132: } 236,
{ 133: } 236,
{ 134: } 236,
{ 135: } 253,
{ 136: } 253,
{ 137: } 253,
{ 138: } 254,
{ 139: } 255,
{ 140: } 255,
{ 141: } 267,
{ 142: } 281,
{ 143: } 296,
{ 144: } 312,
{ 145: } 312,
{ 146: } 312,
{ 147: } 312,
{ 148: } 330,
{ 149: } 330,
{ 150: } 330,
{ 151: } 330,
{ 152: } 330,
{ 153: } 331,
{ 154: } 332,
{ 155: } 332,
{ 156: } 332,
{ 157: } 349,
{ 158: } 349,
{ 159: } 350,
{ 160: } 350,
{ 161: } 367,
{ 162: } 367,
{ 163: } 367
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 1; sym: -5 ),
{ 2: } ( len: 2; sym: -5 ),
{ 3: } ( len: 4; sym: -6 ),
{ 4: } ( len: 2; sym: -7 ),
{ 5: } ( len: 3; sym: -3 ),
{ 6: } ( len: 1; sym: -3 ),
{ 7: } ( len: 3; sym: -9 ),
{ 8: } ( len: 2; sym: -11 ),
{ 9: } ( len: 0; sym: -11 ),
{ 10: } ( len: 2; sym: -14 ),
{ 11: } ( len: 1; sym: -14 ),
{ 12: } ( len: 4; sym: -15 ),
{ 13: } ( len: 1; sym: -16 ),
{ 14: } ( len: 3; sym: -16 ),
{ 15: } ( len: 1; sym: -17 ),
{ 16: } ( len: 3; sym: -17 ),
{ 17: } ( len: 1; sym: -19 ),
{ 18: } ( len: 3; sym: -19 ),
{ 19: } ( len: 2; sym: -21 ),
{ 20: } ( len: 1; sym: -21 ),
{ 21: } ( len: 1; sym: -24 ),
{ 22: } ( len: 3; sym: -24 ),
{ 23: } ( len: 1; sym: -25 ),
{ 24: } ( len: 3; sym: -25 ),
{ 25: } ( len: 1; sym: -25 ),
{ 26: } ( len: 2; sym: -25 ),
{ 27: } ( len: 1; sym: -27 ),
{ 28: } ( len: 2; sym: -27 ),
{ 29: } ( len: 1; sym: -27 ),
{ 30: } ( len: 1; sym: -23 ),
{ 31: } ( len: 1; sym: -23 ),
{ 32: } ( len: 1; sym: -28 ),
{ 33: } ( len: 1; sym: -28 ),
{ 34: } ( len: 1; sym: -28 ),
{ 35: } ( len: 1; sym: -29 ),
{ 36: } ( len: 1; sym: -31 ),
{ 37: } ( len: 3; sym: -12 ),
{ 38: } ( len: 0; sym: -12 ),
{ 39: } ( len: 3; sym: -32 ),
{ 40: } ( len: 1; sym: -32 ),
{ 41: } ( len: 3; sym: -33 ),
{ 42: } ( len: 3; sym: -33 ),
{ 43: } ( len: 3; sym: -33 ),
{ 44: } ( len: 3; sym: -33 ),
{ 45: } ( len: 3; sym: -34 ),
{ 46: } ( len: 3; sym: -35 ),
{ 47: } ( len: 1; sym: -35 ),
{ 48: } ( len: 1; sym: -36 ),
{ 49: } ( len: 1; sym: -36 ),
{ 50: } ( len: 1; sym: -36 ),
{ 51: } ( len: 1; sym: -36 ),
{ 52: } ( len: 3; sym: -37 ),
{ 53: } ( len: 4; sym: -38 ),
{ 54: } ( len: 1; sym: -41 ),
{ 55: } ( len: 2; sym: -42 ),
{ 56: } ( len: 1; sym: -44 ),
{ 57: } ( len: 1; sym: -13 ),
{ 58: } ( len: 3; sym: -45 ),
{ 59: } ( len: 3; sym: -46 ),
{ 60: } ( len: 1; sym: -46 ),
{ 61: } ( len: 1; sym: -47 ),
{ 62: } ( len: 1; sym: -47 ),
{ 63: } ( len: 1; sym: -48 ),
{ 64: } ( len: 1; sym: -49 ),
{ 65: } ( len: 1; sym: -51 ),
{ 66: } ( len: 1; sym: -51 ),
{ 67: } ( len: 1; sym: -51 ),
{ 68: } ( len: 1; sym: -51 ),
{ 69: } ( len: 0; sym: -50 ),
{ 70: } ( len: 1; sym: -54 ),
{ 71: } ( len: 1; sym: -54 ),
{ 72: } ( len: 1; sym: -54 ),
{ 73: } ( len: 3; sym: -55 ),
{ 74: } ( len: 3; sym: -55 ),
{ 75: } ( len: 3; sym: -56 ),
{ 76: } ( len: 3; sym: -56 ),
{ 77: } ( len: 3; sym: -57 ),
{ 78: } ( len: 3; sym: -57 ),
{ 79: } ( len: 3; sym: -57 ),
{ 80: } ( len: 9; sym: -53 ),
{ 81: } ( len: 7; sym: -53 ),
{ 82: } ( len: 5; sym: -53 ),
{ 83: } ( len: 3; sym: -52 ),
{ 84: } ( len: 1; sym: -61 ),
{ 85: } ( len: 1; sym: -61 ),
{ 86: } ( len: 1; sym: -61 ),
{ 87: } ( len: 4; sym: -58 ),
{ 88: } ( len: 3; sym: -63 ),
{ 89: } ( len: 1; sym: -63 ),
{ 90: } ( len: 1; sym: -64 ),
{ 91: } ( len: 2; sym: -65 ),
{ 92: } ( len: 1; sym: -65 ),
{ 93: } ( len: 3; sym: -66 ),
{ 94: } ( len: 3; sym: -67 ),
{ 95: } ( len: 1; sym: -67 ),
{ 96: } ( len: 1; sym: -4 ),
{ 97: } ( len: 1; sym: -68 ),
{ 98: } ( len: 3; sym: -68 ),
{ 99: } ( len: 5; sym: -68 ),
{ 100: } ( len: 1; sym: -69 ),
{ 101: } ( len: 1; sym: -70 ),
{ 102: } ( len: 1; sym: -59 ),
{ 103: } ( len: 3; sym: -59 ),
{ 104: } ( len: 1; sym: -59 ),
{ 105: } ( len: 1; sym: -71 ),
{ 106: } ( len: 3; sym: -71 ),
{ 107: } ( len: 1; sym: -72 ),
{ 108: } ( len: 3; sym: -72 ),
{ 109: } ( len: 2; sym: -73 ),
{ 110: } ( len: 1; sym: -73 ),
{ 111: } ( len: 1; sym: -74 ),
{ 112: } ( len: 3; sym: -74 ),
{ 113: } ( len: 1; sym: -75 ),
{ 114: } ( len: 1; sym: -75 ),
{ 115: } ( len: 1; sym: -75 ),
{ 116: } ( len: 1; sym: -75 ),
{ 117: } ( len: 3; sym: -75 ),
{ 118: } ( len: 2; sym: -75 ),
{ 119: } ( len: 1; sym: -26 ),
{ 120: } ( len: 1; sym: -26 ),
{ 121: } ( len: 1; sym: -26 ),
{ 122: } ( len: 1; sym: -78 ),
{ 123: } ( len: 1; sym: -78 ),
{ 124: } ( len: 1; sym: -79 ),
{ 125: } ( len: 1; sym: -80 ),
{ 126: } ( len: 1; sym: -20 ),
{ 127: } ( len: 1; sym: -20 ),
{ 128: } ( len: 1; sym: -20 ),
{ 129: } ( len: 1; sym: -22 ),
{ 130: } ( len: 1; sym: -22 ),
{ 131: } ( len: 1; sym: -22 ),
{ 132: } ( len: 1; sym: -22 ),
{ 133: } ( len: 1; sym: -22 ),
{ 134: } ( len: 1; sym: -18 ),
{ 135: } ( len: 1; sym: -18 ),
{ 136: } ( len: 1; sym: -18 ),
{ 137: } ( len: 1; sym: -18 ),
{ 138: } ( len: 1; sym: -18 ),
{ 139: } ( len: 1; sym: -18 ),
{ 140: } ( len: 1; sym: -2 ),
{ 141: } ( len: 1; sym: -8 ),
{ 142: } ( len: 1; sym: -10 )
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
  if not Assigned(SymTable) then SymTable := SO('{}');

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