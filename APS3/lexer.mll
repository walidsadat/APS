(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == S-expressions Syntaxe ML                                             == *)
(* == Fichier: lexer.mll                                                   == *)
(* ==  Lexique                                                             == *)
(* ========================================================================== *)

{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof

}
rule token = parse
    [' ' '\t' '\n']       { token lexbuf }     (* skip blanks *)
  | '['              { LBRACKET }
  | ']'              { RBRACKET }
  | '('              { LPAR }
  | ')'              { RPAR }
  | ';'              { SEMICOL }
  | ':'              { COLON }
  | ','              { COMMA }
  | '*'              { TIMES }
  | "->"             { ARROW }
  | "CONST"          { CONST }
  | "FUN"            { FUN }
  | "REC"            { REC }
  | "ECHO"           { ECHO }
  | "VAR"            { VAR }
  | "PROC"           { PROC }
  | "bool"           { BOOL }
  | "int"            { INT }
  | "void"           { VOID }
  | "vec"            { VEC }
  | "true" as lxm    { CBOOL(bool_of_string lxm) }
  | "false" as lxm   { CBOOL(bool_of_string lxm) }
  | "not"            { OPRIM("not") }
  | "and"            { OPRIM("and") }
  | "or"             { OPRIM("or") }
  | "eq"             { OPRIM("eq") }
  | "lt"             { OPRIM("lt") }
  | "add"            { OPRIM("add") }
  | "sub"            { OPRIM("sub") }
  | "mul"            { OPRIM("mul") }
  | "div"            { OPRIM("div") }
  | "len"            { OPRIM("len") }
  | "nth"            { OPRIM("nth") }
  | "alloc"          { OPRIM("alloc") }
  | "if"             { IF }
  | "var"            { VARP }
  | "adr"            { ADR }
  | "IF"             { IF }
  | "SET"            { SET }
  | "WHILE"          { WHILE }
  | "CALL"           { CALL }
  | "RETURN"         { RETURN }
  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | eof              { raise Eof }