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
keyword = "andalso" | "and" | "as" | "case" | "do" | "end" | "exception" | "fn"
  | "fun" | "handle" | "if" | "in" | "infix" | "infixr" | "nonfix" | "let"
  | "local" | "of" | "op" | "open" | "orelse" | "raise" | "rec" | "then" | "use"
  | "while";

kw_val = "val";

symbol = "," | "." | "..." | ":" | "=>" | "[" | "]" | "|";
sym_eq = "=";
sym_semicolon = ";";
ws    = "\ " | "\t" | "\r\n" | "\n" | "\r";

%%

\"[^"]*\" => (Token.STRING
              (String.substring
                 (yytext,1,String.size yytext - 2)));
"_" => (Token.UNDERBAR);
{keyword} => (Token.KEYWORD yytext);
{kw_val} => (Token.KW_VAL);
{id} => (Token.ID yytext);
{real} => (Token.REAL yytext);
{num} => (Token.CONSTINT yytext);
{integer} => (Token.INT yytext);
{symbol} => (Token.SYMBOL yytext);
{sym_eq} => (Token.SYM_EQ);
{sym_semicolon} => (Token.SYM_SEMICOLON);

{ws} => (lex());
. => (Token.SPECIAL yytext);
