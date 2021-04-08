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
  cmd list

and block =
  cmd list

and cmd =
  Stat of stat
  | Def of def

and stype =
  BoolType
  | IntType
  | VoidType
  | VecType of stype
  | Types of stype list * stype

and arg = string * stype

and def =
  ConstDef of string * stype * sexpr
  | FunDef of string * stype * arg list * sexpr
  | RecFunDef of string * stype * arg list * sexpr
  | VarDef of string * stype
  | ProcDef of string * arg list * block
  | RecProcDef of string * arg list * block

and stat =
  Echo of sexpr
  |Set of string * sexpr
  |IfStat of sexpr * block * block
  |While of sexpr * block
  |Call of string * sexpr list
  |SetLvalue of lvalue * sexpr

and lvalue =
    LvalueId of string
    |Lvalue of lvalue * sexpr

and sexpr =
  ASTBool of bool
  | ASTNum of int
  | ASTId of string
  | ASTIf of sexpr * sexpr * sexpr
  | ASTOp of string * sexpr list
  | ASTApp of sexpr * sexpr list
  | ASTFunAbs of arg list * sexpr



