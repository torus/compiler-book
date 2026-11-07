structure Token =
struct
  datatype token
    = EOF | UNDERBAR | ID of string | STRING of string
      | REAL of string | INT of string | SPECIAL of string
      | CONSTINT of string | KEYWORD of string | SYMBOL of string
      | KW_VAL
      | SYM_EQ
      | SYM_SEMICOLON
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
      | KEYWORD s => "KEYWORD " ^ s
      | KW_VAL => "VAL"
      | SYMBOL s => "SYMBOL " ^ s
      | SYM_EQ => "EQ"
      | SYM_SEMICOLON => "SEMICOLON"
end
