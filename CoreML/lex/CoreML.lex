type lexresult = Token.token
val eof = fn () => Token.EOF
fun atoi s = valOf (Int.fromString s)

%%

%structure CoreMLLex
alpha = [A-Za-z];
digit = [0-9];
id    = {alpha}({alpha}|{digit})*;
num   = {digit}+;
frac  = "."{num};
exp   = [eE](~?){num};
real  = (~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
integer = (~?){num};
symbol = "," | "." | "..." | ":" | "=>" | "[" | "]" | "|";
sym_eq = "=";
sym_semicolon = ";";
ws    = "\ " | "\t" | "\r\n" | "\n" | "\r";

%%

\"[^"]*\" => (Token.STRING
              (String.substring
                 (yytext,1,String.size yytext - 2)));
"_" => (Token.UNDERBAR);
"andalso" => (Token.KW_ANDALSO);
"and" => (Token.KW_AND);
"as" => (Token.KW_AS);
"case" => (Token.KW_CASE);
"do" => (Token.KW_DO);
"else" => (Token.KW_ELSE);
"end" => (Token.KW_END);
"exception" => (Token.KW_EXCEPTION);
"fn" => (Token.KW_FN);
"fun" => (Token.KW_FUN);
"handle" => (Token.KW_HANDLE);
"if" => (Token.KW_IF);
"in" => (Token.KW_IN);
"infix" => (Token.KW_INFIX);
"infixr" => (Token.KW_INFIXR);
"nonfix" => (Token.KW_NONFIX);
"let" => (Token.KW_LET);
"local" => (Token.KW_LOCAL);
"of" => (Token.KW_OF);
"op" => (Token.KW_OP);
"open" => (Token.KW_OPEN);
"orelse" => (Token.KW_ORELSE);
"raise" => (Token.KW_RAISE);
"rec" => (Token.KW_REC);
"then" => (Token.KW_THEN);
"use" => (Token.KW_USE);
"val" => (Token.KW_VAL);
"while" => (Token.KW_WHILE);
{id} => (Token.ID yytext);
{real} => (Token.REAL yytext);
{num} => (Token.CONSTINT yytext);
{integer} => (Token.INT yytext);
{symbol} => (Token.SYMBOL yytext);
{sym_eq} => (Token.SYM_EQ);
{sym_semicolon} => (Token.SYM_SEMICOLON);

{ws} => (lex());
. => (Token.SPECIAL yytext);
