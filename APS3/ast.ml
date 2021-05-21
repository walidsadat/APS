(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == S-expression Syntaxe ML                                              == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)

type prog =
  bloc

and bloc =
  cmd list

and cmd =
    Dec of dec
  | Stat of stat
  | Return of sexpr

and dec =
    ConstDec of string * stype * sexpr
  | FunDec of string * stype * arg list * sexpr
  | FunRecDec of string * stype * arg list * sexpr
  | VarDec of string * stype
  | ProcDec of string * arg list * bloc
  | ProcRecDec of string * arg list * bloc
  | FunPDec of string * stype * arg list * bloc
  | FunRecPDec of string * stype * arg list * bloc

and stype =
    BoolType
  | IntType
  | VecType of stype
  | Types of stype list * stype

and arg = 
    Arg of string * stype
  | Argp of string * stype

and stat =
    Echo of sexpr
  | Set of lvalue * sexpr
  | IfStat of sexpr * bloc * bloc
  | While of sexpr * bloc
  | Call of string * sexprp list

and lvalue =
    Lvar of string
  | Lnth of lvalue * sexpr

and sexprp =
    ASTAdr of string
  | ASTExpr of sexpr

and sexpr =
    ASTBool of bool
  | ASTNum of int
  | ASTId of string
  | ASTIf of sexpr * sexpr * sexpr
  | ASTOp of string * sexpr list
  | ASTApp of sexpr * sexpr list
  | ASTFunAbs of arg list * sexpr