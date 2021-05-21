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
%token VARP ADR
%token ECHO RETURN
%token BOOL INT VEC
%token IF
%token SET WHILE CALL

%type <Ast.prog> prog
%type <Ast.bloc> bloc
%type <Ast.cmd list> cmds
%type <Ast.dec> dec
%type <Ast.stype> type
%type <Ast.stype list> types
%type <Ast.arg list> args
%type <Ast.arg> arg
%type <Ast.arg list> argsp
%type <Ast.arg> argp
%type <Ast.stat> stat
%type <Ast.lvalue> lvalue
%type <Ast.sexprp> sexprp
%type <Ast.sexprp list> sexprsp
%type <Ast.sexpr> sexpr
%type <Ast.sexpr list> sexprs

%start prog             /* the entry point */

%%
  prog:
    bloc {$1}
  ;

  bloc:
    LBRACKET cmds RBRACKET {$2}
  ;

  cmds:
      stat                                            {[Stat $1]}
    | dec SEMICOL cmds                                {(Dec $1)::$3}
    | stat SEMICOL cmds                               {(Stat $1)::$3}
    | RETURN sexpr                                    {[Return $2]}
  ;

  dec:
      CONST IDENT type sexpr                          {ConstDec ($2,$3,$4)}
    | FUN IDENT type LBRACKET args RBRACKET bloc      {FunPDec ($2,$3,$5,$7)}
    | FUN REC IDENT type LBRACKET args RBRACKET bloc  {FunRecPDec ($3,$4,$6,$8)}
    | FUN IDENT type LBRACKET args RBRACKET sexpr     {FunDec ($2,$3,$5,$7)}
    | FUN REC IDENT type LBRACKET args RBRACKET sexpr {FunRecDec ($3,$4,$6,$8)}
    | VAR IDENT stype                                 {VarDec($2,$3)}
    | PROC IDENT LBRACKET argsp RBRACKET bloc         {ProcDec($2,$4,$6)}
    | PROC REC IDENT LBRACKET argsp RBRACKET bloc     {ProcRecDec($3,$5,$7)}
  ;

  type:
      stype                                           {$1}
    | LPAR types ARROW type RPAR                      {Types ($2, $4)}
  ;

  types:
      type                                            {[$1]}
    | type TIMES types                                {$1::$3}
  ;

  stype:
      BOOL                                            {BoolType}
    | INT                                             {IntType}
    | LPAR VEC type RPAR                              {VecType $3}

  args:
      arg                                             {[$1]}
    | arg COMMA args                                  {$1::$3}
  ;

  arg:
      IDENT COLON type                                {Arg ($1,$3)}
  ;

  argsp:
      argp                                            {[$1]}
    | argp COMMA argsp                                {$1::$3}
  ;

  argp:
      IDENT COLON type                                 {Arg ($1,$3)}
    | VARP IDENT COLON type                            {Argp ($2,$4)}
  ;

  stat:
      ECHO sexpr                                        {Echo $2}
    | SET lvalue sexpr                                  {Set ($2,$3)}
    | IF sexpr bloc bloc                              {IfStat ($2,$3,$4)}
    | WHILE sexpr bloc                                 {While ($2,$3)}
    | CALL IDENT sexprsp                                {Call ($2,$3)}
  ;

  lvalue:
      IDENT                                             {Lvar $1}
    | LPAR OPRIM lvalue sexpr RPAR                      {if $2 = "nth" then Lnth ($3,$4) else raise Parsing.Parse_error}

  sexprsp:
      sexprp                                            {[$1]}
    | sexprp sexprsp                                    {$1::$2}
  ;

  sexprp:
      sexpr                                             {ASTExpr $1}
    | LPAR ADR IDENT RPAR                               {ASTAdr $3}
  ;

  sexpr:
      CBOOL                                             {ASTBool $1}
    | NUM                                               {ASTNum $1}
    | IDENT                                             {ASTId $1}
    | LPAR IF sexpr sexpr sexpr RPAR                    {ASTIf ($3,$4,$5)}
    | LPAR OPRIM sexprs RPAR                            {ASTOp ($2,$3)}
    | LPAR sexpr sexprs RPAR                            {ASTApp ($2,$3)}
    | LBRACKET args RBRACKET sexpr                      {ASTFunAbs($2,$4)}
  ;

  sexprs:
      sexpr                                             {[$1]}
    | sexpr sexprs                                      {$1::$2}
  ;