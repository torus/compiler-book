structure Token =
struct
  datatype token
    = EOF | UNDERBAR | ID of string | STRING of string
      | REAL of string | SPECIAL of string
      | CONSTINT of string | KEYWORD of string | SYMBOL of string
  fun toString token =
      case token of
        EOF => "EOF"
      | UNDERBAR => "UNDERBAR"
      | ID s => "ID(" ^ s ^ ")"
      | STRING s => "STRING(\"" ^ s ^ "\")"
      | REAL s => "REAL(" ^ s ^ ")"
      | SPECIAL s => "SPECIAL(" ^ s ^ ")"
      | CONSTINT s => "CONSTINT(" ^ s ^ ")"
      | KEYWORD s => "KEYWORD(" ^ s ^ ")"
      | SYMBOL s => "SYMBOL(" ^ s ^ ")"
end
