%{
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == S-expression Syntaxe ML                                              == *)
(* == Fichier: parser.mly                                                  == *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

%}
  
%token <bool> CBOOL
%token <int> NUM
%token <string> IDENT
%token <string> OPRIM
%token LPAR RPAR LBRACKET RBRACKET SEMICOL COLON COMMA TIMES ARROW
%token CONST FUN REC VAR PROC
%token ECHO
%token BOOL INT VOID VEC
%token IF
%token SET WHILE CALL

%type <Ast.prog> prog
%type <Ast.block> block
%type <Ast.cmd list> cmds
%type <Ast.stype> type
%type <Ast.stype list> types
%type <Ast.arg> arg
%type <Ast.arg list> args
%type <Ast.def> def
%type <Ast.stat> stat
%type <Ast.lvalue> lvalue
%type <Ast.sexpr> sexpr
%type <Ast.sexpr list> sexprs

%start prog             /* the entry point */

%%
  prog:
    LBRACKET cmds RBRACKET {$2}
  ;

  block:
    LBRACKET cmds RBRACKET {$2}

  cmds:
      stat                        { [Stat $1]}
    | def SEMICOL cmds          { (Def $1)::$3}
    | stat SEMICOL cmds         { (Stat $1)::$3}     
  ;

  type:
      BOOL                        { BoolType }
    | INT                         { IntType }
    | VOID                        { VoidType}
    | LPAR VEC type RPAR          { VecType($3)}
    | LPAR types ARROW type RPAR  { Types ($2, $4) }
  ;

  types:
      type                      { [$1] }
    | type TIMES types       { $1::$3 }
  ;

  arg:
    IDENT COLON type           { ($1,$3)}
  ;

  args:
      arg                      { [$1] }
    | arg COMMA args           { $1::$3 }
  ;

  def:
      CONST IDENT type sexpr      { ConstDef($2,$3,$4) }
    | FUN IDENT type LBRACKET args RBRACKET sexpr { FunDef ($2, $3, $5, $7)}
    | FUN REC IDENT type LBRACKET args RBRACKET sexpr { RecFunDef ($3, $4, $6, $8)}
    | VAR IDENT type      {VarDef($2, $3)}
    | PROC IDENT LBRACKET args RBRACKET block {ProcDef($2,$4,$6)}
    | PROC REC IDENT LBRACKET args RBRACKET block {RecProcDef($3,$5,$7)}
  ;

  stat:
    ECHO sexpr                   {Echo($2)}
    |SET IDENT sexpr             {Set($2,$3)}
    |IF sexpr block block        {IfStat($2,$3,$4)}
    |WHILE sexpr block           {While($2,$3)}
    |CALL IDENT sexprs           {Call($2,$3)}
    |SET lvalue sexpr              {SetLvalue($2,$3)}
  ;

lvalue:
  IDENT                         {LvalueId($1)}
  |LPAR OPRIM lvalue sexpr RPAR   {if $2 = "nth" then Lvalue($3,$4) else raise Parsing.Parse_error }

  sexpr:
      CBOOL                          { ASTBool($1) }
    | NUM                            { ASTNum($1) }
    | IDENT                          { ASTId($1) }
    | LPAR IF sexpr sexpr sexpr RPAR { ASTIf($3,$4,$5) }
    | LPAR OPRIM sexprs RPAR         { ASTOp($2,$3) }
    | LBRACKET args RBRACKET sexpr   { ASTFunAbs($2,$4) }
    | LPAR sexpr sexprs RPAR         { ASTApp($2, $3) }
  ;

  sexprs:
      sexpr       { [$1] }
    | sexpr sexprs { $1::$2 }
  ;