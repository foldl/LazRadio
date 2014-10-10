
%{

(* lexical analyzer for LazRadio .lzr file *)

%}

%{

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

%}

NQUOTE    [^']

%%

%{

var c  : char;
    kw : integer;
    result : integer;
    tr: Real;
%}

[a-zA-Z]([_\-a-zA-Z0-9])*	if is_keyword(yytext, kw) then
                          return(kw)
                        else begin
                          yylval.yyString := yytext;
                          return(IDENTIFIER);
                        end;

":="			return(ASSIGNMENT);
'({NQUOTE}|'')+'	begin
                      yylval.yyString := RegAlloc('string');
                      RegWrite(yylval.yyString, str_escape(yytext)); 
                      return(CHARACTER_STRING);
                    end;
":"			return(COLON);
":>"			return(CONNFEATURE);
","			return(COMMA);
[0-9]+			begin
                  yylval.yyString := RegAlloc('int');
                  RegWrite(yylval.yyString, StrToInt(yytext)); 
                  return(DIGSEQ);
                end;
"."			return(DOT);
"="			return(EQUAL);
"=>"			return(CONNFEATUREDATA);
">="			return(GE);
">"			return(GT);
"["			return(LBRAC);
"<="			return(LE);
"("			return(LPAREN);
"<"			return(LT);
"-"			return(MINUS);
"<>"			return(NOTEQUAL);
"+"			return(PLUS);
"]"			return(RBRAC);
[0-9]+"."[0-9]+	 begin
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
")"			return(RPAREN);
";"			return(SEMICOLON);
"/"			return(SLASH);
"*"			return(STAR);
"**"		return(STARSTAR);
"->"        return(CONNDATA);
"!"			return(SEND);
"//"        begin
                repeat
                  C := get_char;
                  case C of
                    #10, #13, #0: Break;
                  end; 
                until False;
            end;
"{"		    return(LBRACE);
"}"		    return(RBRACE);
[ \n\t\f]		;
.			begin return(ILLEGAL); end;
