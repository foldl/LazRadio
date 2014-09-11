
%{

(* lexical analyzer for LazRadio .lar file *)

%}

%{

procedure commenteof;
  begin
    writeln('unexpected EOF inside comment at line ', yylineno);
  end(*commenteof*);

function upper(str : String) : String;
  (* converts str to uppercase *)
  var i : integer;
  begin
    for i := 1 to length(str) do
      str[i] := upCase(str[i]);
    upper := str
  end(*upper*);

function is_keyword(id : string; var token : integer) : boolean;
  (* checks whether id is Pascal keyword; if so, returns corresponding
     token number in token *)
  const
    id_len = 20;
  type
    Ident = string[id_len];
  const
    no_of_keywords = 13;
    keyword : array [1..no_of_keywords] of Ident = (
      'AND',       'BEGIN', 
      'CONST',     'DIV',    
      'ELSE',      'END',        
      'FILE',   
      'LAZRADIO',
      'MOD',       'NOT',   
      'OR',   
      'VAR',
      'XOR');
    keyword_token : array [1..no_of_keywords] of integer = (
      _AND,        _BEGIN, 
      _CONST,      _DIV,
      _ELSE,       _END,   
      _FILE,  
      _LAZRADIO,
      _MOD,        _NOT,
      _OR, 
      _VAR,
      _XOR);
  var m, n, k : integer;
  begin
    id := upper(id);
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

%}

[a-zA-Z]([\-a-zA-Z0-9])*	if is_keyword(yytext, kw) then
                          return(kw)
                        else
                          return(IDENTIFIER);

":="			return(ASSIGNMENT);
'({NQUOTE}|'')+'	return(CHARACTER_STRING);
":"			return(COLON);
":>"			return(CONNFEATURE);
","			return(COMMA);
[0-9]+			return(DIGSEQ);
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
[0-9]+"."[0-9]+		return(REALNUMBER);
")"			return(RPAREN);
";"			return(SEMICOLON);
"/"			return(SLASH);
"*"			return(STAR);
"**"		return(STARSTAR);
"->"        return(CONNDATA);
"^"			return(UPARROW);
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

.			return(ILLEGAL);
