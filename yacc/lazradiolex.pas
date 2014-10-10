
(* lexical analyzer template (TP Lex V3.0), V1.0 3-2-91 AG *)

(* global definitions: *)


(* lexical analyzer for LazRadio .lzr file *)



function str_escape(s: string): string;
begin
  str_escape := StringReplace(s, '''''', '''', [rfReplaceAll]);
  str_escape := Copy(str_escape, 2, Length(str_escape) - 2);
end;

function is_keyword(id : string; var token : integer) : boolean;
  (* checks whether id is Pascal keyword; if so, returns corresponding
     token number in token *)
  (* built-in functions/procedures are treated as keywords too *)
  const
    id_len = 20;
  type
    Ident = string[id_len];
  const
    no_of_keywords = 30;
    keyword : array [1..no_of_keywords] of Ident = (
      'AND',       'ARCCOS',    'ARCSIN',   'BEGIN', 
      'CONST',     'COS',       'DIV',    
      'END',       'EXP', 
      'ID',         'INTEGER',
      'LAZRADIO',   'LOG',
      'MOD',        'NOT',   
      'OR',         'ORD',
      'PRED',
      'REAL',       'ROUND',
      'SIN',        'SUCC',    'STR',         'STRING',
      'TRUNC',
      'VAL',        'VAR',        'WRITE',    'WRITELN',
      'XOR');
    keyword_token : array [1..no_of_keywords] of integer = (
      _AND,        _ARCCOS,     _ARCSIN,    _BEGIN, 
      _CONST,      _COS,        _DIV,
      _END,        _EXP,
      _ID,         _INTEGER,
      _LAZRADIO,    _LOG,
      _MOD,         _NOT,
      _OR,          _ORD,
      _PRED,
      _REAL,        _ROUND,
      _SIN,         _SUCC,      _STR,    _STRING,
      _TRUNC,
      _VAL,         _VAR,         _WRITELN,   _WRITELN,
      _XOR);
  var m, n, k : integer;
  begin
    id := UpperCase(id);
    (* binary search: *)
    m := 1; n := no_of_keywords;
    while m<=n do
      begin
        k := m+(n-m) div 2;
        if id=keyword[k] then
          begin
            is_keyword := true;
            token := keyword_token[k];
            exit
          end
        else if id>keyword[k] then
          m := k+1
        else
          n := k-1
      end;
    is_keyword := false
  end(*is_keyword*);




function yylex : Integer;

procedure yyaction ( yyruleno : Integer );
  (* local definitions: *)


var c  : char;
    kw : integer;
    result : integer;
    tr: Real;

begin
  (* actions: *)
  case yyruleno of
  1:
                         	if is_keyword(yytext, kw) then
                          return(kw)
                        else begin
                          yylval.yyString := yytext;
                          return(IDENTIFIER);
                        end;

  2:
    			return(ASSIGNMENT);
  3:
                	begin
                      yylval.yyString := RegAlloc('string');
                      RegWrite(yylval.yyString, str_escape(yytext)); 
                      return(CHARACTER_STRING);
                    end;
  4:
   			return(COLON);
  5:
    			return(CONNFEATURE);
  6:
   			return(COMMA);
  7:
      			begin
                  yylval.yyString := RegAlloc('int');
                  RegWrite(yylval.yyString, StrToInt(yytext)); 
                  return(DIGSEQ);
                end;
  8:
   			return(DOT);
  9:
   			return(EQUAL);
  10:
    			return(CONNFEATUREDATA);
  11:
    			return(GE);
  12:
   			return(GT);
  13:
   			return(LBRAC);
  14:
    			return(LE);
  15:
   			return(LPAREN);
  16:
   			return(LT);
  17:
   			return(MINUS);
  18:
    			return(NOTEQUAL);
  19:
   			return(PLUS);
  20:
   			return(RBRAC);
  21:
               	 begin
                   val(yytext, tr, result);
				   if result=0 then
                   begin
                     yylval.yyString := RegAlloc('real');
                     RegWrite(yylval.yyString, tr);
				     return(REALNUMBER)
                   end
				   else
				     return(ILLEGAL)
                 end;
  22:
   			return(RPAREN);
  23:
   			return(SEMICOLON);
  24:
   			return(SLASH);
  25:
   			return(STAR);
  26:
    		return(STARSTAR);
  27:
            return(CONNDATA);
  28:
   			return(SEND);
  29:
            begin
                repeat
                  C := get_char;
                  case C of
                    #10, #13, #0: Break;
                  end; 
                until False;
            end;
  30:
   		    return(LBRACE);
  31:
   		    return(RBRACE);
  32:
         		;
  33:
 			begin return(ILLEGAL); end;
  end;
end(*yyaction*);

(* DFA table: *)

type YYTRec = record
                cc : set of Char;
                s  : Integer;
              end;

const

yynmarks   = 58;
yynmatches = 58;
yyntrans   = 71;
yynstates  = 42;

yyk : array [1..yynmarks] of Integer = (
  { 0: }
  { 1: }
  { 2: }
  1,
  33,
  { 3: }
  4,
  33,
  { 4: }
  33,
  { 5: }
  6,
  33,
  { 6: }
  7,
  33,
  { 7: }
  8,
  33,
  { 8: }
  9,
  33,
  { 9: }
  12,
  33,
  { 10: }
  13,
  33,
  { 11: }
  16,
  33,
  { 12: }
  15,
  33,
  { 13: }
  17,
  33,
  { 14: }
  19,
  33,
  { 15: }
  20,
  33,
  { 16: }
  22,
  33,
  { 17: }
  23,
  33,
  { 18: }
  24,
  33,
  { 19: }
  25,
  33,
  { 20: }
  28,
  33,
  { 21: }
  30,
  33,
  { 22: }
  31,
  33,
  { 23: }
  32,
  { 24: }
  32,
  33,
  { 25: }
  33,
  { 26: }
  1,
  { 27: }
  2,
  { 28: }
  5,
  { 29: }
  { 30: }
  { 31: }
  7,
  { 32: }
  { 33: }
  10,
  { 34: }
  11,
  { 35: }
  14,
  { 36: }
  18,
  { 37: }
  27,
  { 38: }
  29,
  { 39: }
  26,
  { 40: }
  3,
  { 41: }
  21
);

yym : array [1..yynmatches] of Integer = (
{ 0: }
{ 1: }
{ 2: }
  1,
  33,
{ 3: }
  4,
  33,
{ 4: }
  33,
{ 5: }
  6,
  33,
{ 6: }
  7,
  33,
{ 7: }
  8,
  33,
{ 8: }
  9,
  33,
{ 9: }
  12,
  33,
{ 10: }
  13,
  33,
{ 11: }
  16,
  33,
{ 12: }
  15,
  33,
{ 13: }
  17,
  33,
{ 14: }
  19,
  33,
{ 15: }
  20,
  33,
{ 16: }
  22,
  33,
{ 17: }
  23,
  33,
{ 18: }
  24,
  33,
{ 19: }
  25,
  33,
{ 20: }
  28,
  33,
{ 21: }
  30,
  33,
{ 22: }
  31,
  33,
{ 23: }
  32,
{ 24: }
  32,
  33,
{ 25: }
  33,
{ 26: }
  1,
{ 27: }
  2,
{ 28: }
  5,
{ 29: }
{ 30: }
{ 31: }
  7,
{ 32: }
{ 33: }
  10,
{ 34: }
  11,
{ 35: }
  14,
{ 36: }
  18,
{ 37: }
  27,
{ 38: }
  29,
{ 39: }
  26,
{ 40: }
  3,
{ 41: }
  21
);

yyt : array [1..yyntrans] of YYTrec = (
{ 0: }
  ( cc: [ #1..#8,#11,#13..#31,'"'..'&','?','@','\',
            '^'..'`','|','~'..#255 ]; s: 25),
  ( cc: [ #9,#12,' ' ]; s: 24),
  ( cc: [ #10 ]; s: 23),
  ( cc: [ '!' ]; s: 20),
  ( cc: [ '''' ]; s: 4),
  ( cc: [ '(' ]; s: 12),
  ( cc: [ ')' ]; s: 16),
  ( cc: [ '*' ]; s: 19),
  ( cc: [ '+' ]; s: 14),
  ( cc: [ ',' ]; s: 5),
  ( cc: [ '-' ]; s: 13),
  ( cc: [ '.' ]; s: 7),
  ( cc: [ '/' ]; s: 18),
  ( cc: [ '0'..'9' ]; s: 6),
  ( cc: [ ':' ]; s: 3),
  ( cc: [ ';' ]; s: 17),
  ( cc: [ '<' ]; s: 11),
  ( cc: [ '=' ]; s: 8),
  ( cc: [ '>' ]; s: 9),
  ( cc: [ 'A'..'Z','a'..'z' ]; s: 2),
  ( cc: [ '[' ]; s: 10),
  ( cc: [ ']' ]; s: 15),
  ( cc: [ '{' ]; s: 21),
  ( cc: [ '}' ]; s: 22),
{ 1: }
  ( cc: [ #1..#8,#11,#13..#31,'"'..'&','?','@','\',
            '^'..'`','|','~'..#255 ]; s: 25),
  ( cc: [ #9,#12,' ' ]; s: 24),
  ( cc: [ #10 ]; s: 23),
  ( cc: [ '!' ]; s: 20),
  ( cc: [ '''' ]; s: 4),
  ( cc: [ '(' ]; s: 12),
  ( cc: [ ')' ]; s: 16),
  ( cc: [ '*' ]; s: 19),
  ( cc: [ '+' ]; s: 14),
  ( cc: [ ',' ]; s: 5),
  ( cc: [ '-' ]; s: 13),
  ( cc: [ '.' ]; s: 7),
  ( cc: [ '/' ]; s: 18),
  ( cc: [ '0'..'9' ]; s: 6),
  ( cc: [ ':' ]; s: 3),
  ( cc: [ ';' ]; s: 17),
  ( cc: [ '<' ]; s: 11),
  ( cc: [ '=' ]; s: 8),
  ( cc: [ '>' ]; s: 9),
  ( cc: [ 'A'..'Z','a'..'z' ]; s: 2),
  ( cc: [ '[' ]; s: 10),
  ( cc: [ ']' ]; s: 15),
  ( cc: [ '{' ]; s: 21),
  ( cc: [ '}' ]; s: 22),
{ 2: }
  ( cc: [ '-','0'..'9','A'..'Z','_','a'..'z' ]; s: 26),
{ 3: }
  ( cc: [ '=' ]; s: 27),
  ( cc: [ '>' ]; s: 28),
{ 4: }
  ( cc: [ #1..'&','('..#255 ]; s: 29),
  ( cc: [ '''' ]; s: 30),
{ 5: }
{ 6: }
  ( cc: [ '.' ]; s: 32),
  ( cc: [ '0'..'9' ]; s: 31),
{ 7: }
{ 8: }
  ( cc: [ '>' ]; s: 33),
{ 9: }
  ( cc: [ '=' ]; s: 34),
{ 10: }
{ 11: }
  ( cc: [ '=' ]; s: 35),
  ( cc: [ '>' ]; s: 36),
{ 12: }
{ 13: }
  ( cc: [ '>' ]; s: 37),
{ 14: }
{ 15: }
{ 16: }
{ 17: }
{ 18: }
  ( cc: [ '/' ]; s: 38),
{ 19: }
  ( cc: [ '*' ]; s: 39),
{ 20: }
{ 21: }
{ 22: }
{ 23: }
{ 24: }
{ 25: }
{ 26: }
  ( cc: [ '-','0'..'9','A'..'Z','_','a'..'z' ]; s: 26),
{ 27: }
{ 28: }
{ 29: }
  ( cc: [ #1..'&','('..#255 ]; s: 29),
  ( cc: [ '''' ]; s: 40),
{ 30: }
  ( cc: [ '''' ]; s: 29),
{ 31: }
  ( cc: [ '.' ]; s: 32),
  ( cc: [ '0'..'9' ]; s: 31),
{ 32: }
  ( cc: [ '0'..'9' ]; s: 41),
{ 33: }
{ 34: }
{ 35: }
{ 36: }
{ 37: }
{ 38: }
{ 39: }
{ 40: }
  ( cc: [ '''' ]; s: 29),
{ 41: }
  ( cc: [ '0'..'9' ]; s: 41)
);

yykl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 3,
{ 4: } 5,
{ 5: } 6,
{ 6: } 8,
{ 7: } 10,
{ 8: } 12,
{ 9: } 14,
{ 10: } 16,
{ 11: } 18,
{ 12: } 20,
{ 13: } 22,
{ 14: } 24,
{ 15: } 26,
{ 16: } 28,
{ 17: } 30,
{ 18: } 32,
{ 19: } 34,
{ 20: } 36,
{ 21: } 38,
{ 22: } 40,
{ 23: } 42,
{ 24: } 43,
{ 25: } 45,
{ 26: } 46,
{ 27: } 47,
{ 28: } 48,
{ 29: } 49,
{ 30: } 49,
{ 31: } 49,
{ 32: } 50,
{ 33: } 50,
{ 34: } 51,
{ 35: } 52,
{ 36: } 53,
{ 37: } 54,
{ 38: } 55,
{ 39: } 56,
{ 40: } 57,
{ 41: } 58
);

yykh : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 2,
{ 3: } 4,
{ 4: } 5,
{ 5: } 7,
{ 6: } 9,
{ 7: } 11,
{ 8: } 13,
{ 9: } 15,
{ 10: } 17,
{ 11: } 19,
{ 12: } 21,
{ 13: } 23,
{ 14: } 25,
{ 15: } 27,
{ 16: } 29,
{ 17: } 31,
{ 18: } 33,
{ 19: } 35,
{ 20: } 37,
{ 21: } 39,
{ 22: } 41,
{ 23: } 42,
{ 24: } 44,
{ 25: } 45,
{ 26: } 46,
{ 27: } 47,
{ 28: } 48,
{ 29: } 48,
{ 30: } 48,
{ 31: } 49,
{ 32: } 49,
{ 33: } 50,
{ 34: } 51,
{ 35: } 52,
{ 36: } 53,
{ 37: } 54,
{ 38: } 55,
{ 39: } 56,
{ 40: } 57,
{ 41: } 58
);

yyml : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 3,
{ 4: } 5,
{ 5: } 6,
{ 6: } 8,
{ 7: } 10,
{ 8: } 12,
{ 9: } 14,
{ 10: } 16,
{ 11: } 18,
{ 12: } 20,
{ 13: } 22,
{ 14: } 24,
{ 15: } 26,
{ 16: } 28,
{ 17: } 30,
{ 18: } 32,
{ 19: } 34,
{ 20: } 36,
{ 21: } 38,
{ 22: } 40,
{ 23: } 42,
{ 24: } 43,
{ 25: } 45,
{ 26: } 46,
{ 27: } 47,
{ 28: } 48,
{ 29: } 49,
{ 30: } 49,
{ 31: } 49,
{ 32: } 50,
{ 33: } 50,
{ 34: } 51,
{ 35: } 52,
{ 36: } 53,
{ 37: } 54,
{ 38: } 55,
{ 39: } 56,
{ 40: } 57,
{ 41: } 58
);

yymh : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 2,
{ 3: } 4,
{ 4: } 5,
{ 5: } 7,
{ 6: } 9,
{ 7: } 11,
{ 8: } 13,
{ 9: } 15,
{ 10: } 17,
{ 11: } 19,
{ 12: } 21,
{ 13: } 23,
{ 14: } 25,
{ 15: } 27,
{ 16: } 29,
{ 17: } 31,
{ 18: } 33,
{ 19: } 35,
{ 20: } 37,
{ 21: } 39,
{ 22: } 41,
{ 23: } 42,
{ 24: } 44,
{ 25: } 45,
{ 26: } 46,
{ 27: } 47,
{ 28: } 48,
{ 29: } 48,
{ 30: } 48,
{ 31: } 49,
{ 32: } 49,
{ 33: } 50,
{ 34: } 51,
{ 35: } 52,
{ 36: } 53,
{ 37: } 54,
{ 38: } 55,
{ 39: } 56,
{ 40: } 57,
{ 41: } 58
);

yytl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 25,
{ 2: } 49,
{ 3: } 50,
{ 4: } 52,
{ 5: } 54,
{ 6: } 54,
{ 7: } 56,
{ 8: } 56,
{ 9: } 57,
{ 10: } 58,
{ 11: } 58,
{ 12: } 60,
{ 13: } 60,
{ 14: } 61,
{ 15: } 61,
{ 16: } 61,
{ 17: } 61,
{ 18: } 61,
{ 19: } 62,
{ 20: } 63,
{ 21: } 63,
{ 22: } 63,
{ 23: } 63,
{ 24: } 63,
{ 25: } 63,
{ 26: } 63,
{ 27: } 64,
{ 28: } 64,
{ 29: } 64,
{ 30: } 66,
{ 31: } 67,
{ 32: } 69,
{ 33: } 70,
{ 34: } 70,
{ 35: } 70,
{ 36: } 70,
{ 37: } 70,
{ 38: } 70,
{ 39: } 70,
{ 40: } 70,
{ 41: } 71
);

yyth : array [0..yynstates-1] of Integer = (
{ 0: } 24,
{ 1: } 48,
{ 2: } 49,
{ 3: } 51,
{ 4: } 53,
{ 5: } 53,
{ 6: } 55,
{ 7: } 55,
{ 8: } 56,
{ 9: } 57,
{ 10: } 57,
{ 11: } 59,
{ 12: } 59,
{ 13: } 60,
{ 14: } 60,
{ 15: } 60,
{ 16: } 60,
{ 17: } 60,
{ 18: } 61,
{ 19: } 62,
{ 20: } 62,
{ 21: } 62,
{ 22: } 62,
{ 23: } 62,
{ 24: } 62,
{ 25: } 62,
{ 26: } 63,
{ 27: } 63,
{ 28: } 63,
{ 29: } 65,
{ 30: } 66,
{ 31: } 68,
{ 32: } 69,
{ 33: } 69,
{ 34: } 69,
{ 35: } 69,
{ 36: } 69,
{ 37: } 69,
{ 38: } 69,
{ 39: } 69,
{ 40: } 70,
{ 41: } 71
);


var yyn : Integer;

label start, scan, action;

begin

start:

  (* initialize: *)

  yynew;

scan:

  (* mark positions and matches: *)

  for yyn := yykl[yystate] to     yykh[yystate] do yymark(yyk[yyn]);
  for yyn := yymh[yystate] downto yyml[yystate] do yymatch(yym[yyn]);

  if yytl[yystate]>yyth[yystate] then goto action; (* dead state *)

  (* get next character: *)

  yyscan;

  (* determine action: *)

  yyn := yytl[yystate];
  while (yyn<=yyth[yystate]) and not (yyactchar in yyt[yyn].cc) do inc(yyn);
  if yyn>yyth[yystate] then goto action;
    (* no transition on yyactchar in this state *)

  (* switch to new state: *)

  yystate := yyt[yyn].s;

  goto scan;

action:

  (* execute action: *)

  if yyfind(yyrule) then
    begin
      yyaction(yyrule);
      if yyreject then goto action;
    end
  else if not yydefault and yywrap() then
    begin
      yyclear;
      return(0);
    end;

  if not yydone then goto start;

  yylex := yyretval;

end(*yylex*);

