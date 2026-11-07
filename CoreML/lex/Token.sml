structure Token =
struct
  datatype token
    = EOF | UNDERBAR | ID of string | STRING of string
      | REAL of string | INT of string | SPECIAL of string
      | CONSTINT of string
      | KW_ANDALSO
      | KW_AND
      | KW_AS
      | KW_CASE
      | KW_DO
      | KW_ELSE
      | KW_END
      | KW_EXCEPTION
      | KW_FN
      | KW_FUN
      | KW_HANDLE
      | KW_IF
      | KW_IN
      | KW_INFIX
      | KW_INFIXR
      | KW_NONFIX
      | KW_LET
      | KW_LOCAL
      | KW_OF
      | KW_OP
      | KW_OPEN
      | KW_ORELSE
      | KW_RAISE
      | KW_REC
      | KW_THEN
      | KW_USE
      | KW_VAL
      | KW_WHILE
      | SYM_COMMA
      | SYM_DOTS
      | SYM_PERIOD
      | SYM_COLON
      | SYM_SEMICOLON
      | SYM_DARROW
      | SYM_EQ
      | SYM_LBRACKET
      | SYM_RBRACKET
      | SYM_BAR
  fun toString token =
      case token of
        EOF => "EOF"
      | UNDERBAR => "UNDERBAR"
      | ID s => "ID " ^ s
      | STRING s => "STRING \"" ^ s ^ "\""
      | REAL s => "REAL " ^ s
      | INT s => "INT " ^ s
      | SPECIAL s => "SPECIAL " ^ s
      | CONSTINT s => "CONSTINT " ^ s
      | KW_ANDALSO => "ANDALSO"
      | KW_AND => "AND"
      | KW_AS => "AS"
      | KW_CASE => "CASE"
      | KW_DO => "DO"
      | KW_ELSE => "ELSE"
      | KW_END => "END"
      | KW_EXCEPTION => "EXCEPTION"
      | KW_FN => "FN"
      | KW_FUN => "FUN"
      | KW_HANDLE => "HANDLE"
      | KW_IF => "IF"
      | KW_IN => "IN"
      | KW_INFIX => "INFIX"
      | KW_INFIXR => "INFIXR"
      | KW_NONFIX => "NONFIX"
      | KW_LET => "LET"
      | KW_LOCAL => "LOCAL"
      | KW_OF => "OF"
      | KW_OP => "OP"
      | KW_OPEN => "OPEN"
      | KW_ORELSE => "ORELSE"
      | KW_RAISE => "RAISE"
      | KW_REC => "REC"
      | KW_THEN => "THEN"
      | KW_USE => "USE"
      | KW_VAL => "VAL"
      | KW_WHILE => "WHILE"
      | SYM_COMMA => "COMMA"
      | SYM_DOTS => "DOTS"
      | SYM_PERIOD => "PERIOD"
      | SYM_COLON => "COLON"
      | SYM_SEMICOLON => "SEMICOLON"
      | SYM_DARROW => "DARROW"
      | SYM_EQ => "EQ"
      | SYM_LBRACKET => "LBRACKET"
      | SYM_RBRACKET => "RBRACKET"
      | SYM_BAR => "BAR"
end
