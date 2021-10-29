{
  open Parser;;
}

rule token = parse
  | [' ' '\t' '\n']      { token lexbuf } (* saute les whitespace *)

  | eof                   { EOF }

  | '+'                   { PLUS }
  | '*'                   { TIMES }
  | '-'                   { MINUS }
  | '/'                   { DIVIDE }

  | '('                   { LPAREN }
  | ')'                   { RPAREN }

  | "begin"               { BEGIN }
  | "end"                 { END }

  | '['                   { LBRACKET }
  | ']'                   { RBRACKET }
  | "::"                  { COLCOL }

  | ","                   { COMMA }

  | '='                   { EQUAL }
  | '<'                   { LT }
  | '>'                   { GT }
  | "<="                  { LEQ }
  | ">="                  { GEQ }
  | "<>"                  { NEQ }

  | "&&"                  { LAND }
  | "||"                  { LOR }

  | "!"                   { BANG }
  | ":="                  { COLEQ }
  | ";"                   { SEMCOL }

  | "let"                 { LET }
  | "rec"                 { REC }

  | "in"                  { IN }
  | ";;"                  { DSEMCOL }

  | "fun"                 { FUN }
  | "match"               { MATCH }
  | "with"                { WITH }
  | "function"            { FUNCTION }
  | "|"                   { PIPE }
  | "->"                  { ARROW }

  | "if"                  { IF }
  | "then"                { THEN }
  | "else"                { ELSE }

  | "try"                 { TRY }
  | "with"                { WITH }
  | "E"                   { E }
  | "raise"               { RAISE }

  | ("true"|"false") as s { BOOL (bool_of_string s) }
  | ['0'-'9']+ as s       { INT (int_of_string s) }

  | '_'                   { UND }
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9''_']* as s  { ID s }

