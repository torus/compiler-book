%%

%structure CoreMLLex
alpha = [A-Za-z];
digit = [0-9];
id    = {alpha}({alpha}|{digit})*;
num   = {digit}+;
frac  = "."{num};
exp   = [eE](~?){num};
real  = (~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
ws    = "\ " | "\t" | "\r\n" | "\n" | "\r";

%%

