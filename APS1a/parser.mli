type token =
  | CBOOL of (bool)
  | NUM of (int)
  | IDENT of (string)
  | OPRIM of (string)
  | LPAR
  | RPAR
  | LBRACKET
  | RBRACKET
  | SEMICOL
  | COLON
  | COMMA
  | TIMES
  | ARROW
  | CONST
  | FUN
  | REC
  | VAR
  | PROC
  | VARP
  | ADR
  | ECHO
  | BOOL
  | INT
  | VOID
  | IF
  | SET
  | WHILE
  | CALL

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.prog
