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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
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

# 47 "parser.ml"
let yytransl_const = [|
  261 (* LPAR *);
  262 (* RPAR *);
  263 (* LBRACKET *);
  264 (* RBRACKET *);
  265 (* SEMICOL *);
  266 (* COLON *);
  267 (* COMMA *);
  268 (* TIMES *);
  269 (* ARROW *);
  270 (* CONST *);
  271 (* FUN *);
  272 (* REC *);
  273 (* VAR *);
  274 (* PROC *);
  275 (* VARP *);
  276 (* ADR *);
  277 (* ECHO *);
  278 (* BOOL *);
  279 (* INT *);
  280 (* VOID *);
  281 (* IF *);
  282 (* SET *);
  283 (* WHILE *);
  284 (* CALL *);
    0|]

let yytransl_block = [|
  257 (* CBOOL *);
  258 (* NUM *);
  259 (* IDENT *);
  260 (* OPRIM *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\003\000\004\000\004\000\004\000\
\004\000\004\000\004\000\005\000\005\000\005\000\005\000\006\000\
\006\000\007\000\007\000\008\000\009\000\009\000\010\000\010\000\
\011\000\011\000\011\000\011\000\011\000\013\000\013\000\012\000\
\012\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\015\000\015\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\004\000\007\000\008\000\
\003\000\006\000\007\000\001\000\001\000\001\000\005\000\001\000\
\003\000\001\000\003\000\003\000\001\000\003\000\003\000\004\000\
\002\000\003\000\004\000\003\000\003\000\001\000\002\000\001\000\
\004\000\001\000\001\000\001\000\006\000\004\000\004\000\004\000\
\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\043\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\034\000\
\035\000\036\000\000\000\000\000\025\000\000\000\000\000\000\000\
\000\000\002\000\000\000\000\000\000\000\012\000\013\000\014\000\
\000\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\026\000\028\000\000\000\
\000\000\029\000\032\000\004\000\005\000\000\000\000\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\027\000\000\000\
\031\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\042\000\038\000\000\000\039\000\020\000\040\000\
\019\000\000\000\017\000\000\000\000\000\000\000\023\000\000\000\
\010\000\022\000\000\000\000\000\033\000\015\000\007\000\000\000\
\024\000\011\000\037\000\008\000"

let yydgoto = "\002\000\
\004\000\005\000\015\000\016\000\062\000\063\000\051\000\052\000\
\069\000\070\000\017\000\057\000\058\000\072\000\073\000"

let yysindex = "\007\000\
\004\255\000\000\067\255\000\000\000\000\017\255\003\255\036\255\
\038\255\056\255\056\255\039\255\056\255\057\255\058\255\061\255\
\062\255\013\255\013\255\074\255\013\255\072\255\083\255\000\000\
\000\000\000\000\048\255\086\255\000\000\004\255\056\255\004\255\
\073\255\000\000\067\255\067\255\013\255\000\000\000\000\000\000\
\056\255\089\255\013\255\000\000\037\255\090\255\056\255\056\255\
\056\255\055\255\091\255\087\255\004\255\000\000\000\000\023\255\
\073\255\000\000\000\000\000\000\000\000\088\255\092\255\000\000\
\086\255\094\255\093\255\101\255\098\255\096\255\037\255\056\255\
\102\255\056\255\103\255\013\255\056\255\086\255\000\000\107\255\
\000\000\013\255\013\255\104\255\086\255\013\255\105\255\004\255\
\037\255\106\255\000\000\000\000\056\255\000\000\000\000\000\000\
\000\000\110\255\000\000\111\255\056\255\112\255\000\000\013\255\
\000\000\000\000\004\255\113\255\000\000\000\000\000\000\056\255\
\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\114\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\115\255\000\000\000\000\000\000\000\000\
\005\255\000\000\000\000\000\000\000\000\100\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\116\255\000\000\119\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\236\255\009\000\000\000\242\255\029\000\193\255\000\000\
\201\255\000\000\000\000\000\000\061\000\246\255\216\255"

let yytablesize = 125
let yytable = "\029\000\
\030\000\084\000\032\000\041\000\042\000\019\000\044\000\001\000\
\075\000\053\000\003\000\055\000\030\000\030\000\097\000\090\000\
\049\000\037\000\020\000\018\000\054\000\102\000\059\000\024\000\
\025\000\026\000\047\000\027\000\066\000\028\000\064\000\091\000\
\079\000\106\000\038\000\039\000\040\000\074\000\021\000\067\000\
\022\000\031\000\080\000\060\000\061\000\049\000\059\000\048\000\
\024\000\025\000\026\000\047\000\027\000\023\000\028\000\068\000\
\024\000\025\000\026\000\033\000\027\000\095\000\028\000\093\000\
\076\000\034\000\096\000\105\000\100\000\035\000\036\000\103\000\
\048\000\024\000\025\000\026\000\043\000\056\000\045\000\028\000\
\006\000\007\000\108\000\008\000\009\000\046\000\114\000\010\000\
\050\000\113\000\111\000\011\000\012\000\013\000\014\000\065\000\
\071\000\078\000\077\000\082\000\085\000\116\000\086\000\087\000\
\083\000\088\000\089\000\092\000\094\000\098\000\099\000\101\000\
\016\000\107\000\104\000\109\000\110\000\081\000\115\000\112\000\
\000\000\003\000\018\000\021\000\041\000"

let yycheck = "\010\000\
\011\000\065\000\013\000\018\000\019\000\003\001\021\000\001\000\
\049\000\030\000\007\001\032\000\008\001\009\001\078\000\071\000\
\027\000\005\001\016\001\003\001\031\000\085\000\033\000\001\001\
\002\001\003\001\004\001\005\001\043\000\007\001\041\000\072\000\
\053\000\089\000\022\001\023\001\024\001\048\000\003\001\003\001\
\003\001\003\001\020\001\035\000\036\000\056\000\057\000\025\001\
\001\001\002\001\003\001\004\001\005\001\016\001\007\001\019\001\
\001\001\002\001\003\001\003\001\005\001\076\000\007\001\074\000\
\010\001\008\001\077\000\088\000\083\000\009\001\009\001\086\000\
\025\001\001\001\002\001\003\001\003\001\005\001\007\001\007\001\
\014\001\015\001\093\000\017\001\018\001\003\001\107\000\021\001\
\003\001\104\000\101\000\025\001\026\001\027\001\028\001\007\001\
\007\001\011\001\008\001\012\001\007\001\112\000\010\001\003\001\
\013\001\008\001\011\001\006\001\006\001\003\001\082\000\008\001\
\013\001\008\001\010\001\006\001\006\001\057\000\006\001\008\001\
\255\255\008\001\008\001\008\001\006\001"

let yynames_const = "\
  LPAR\000\
  RPAR\000\
  LBRACKET\000\
  RBRACKET\000\
  SEMICOL\000\
  COLON\000\
  COMMA\000\
  TIMES\000\
  ARROW\000\
  CONST\000\
  FUN\000\
  REC\000\
  VAR\000\
  PROC\000\
  VARP\000\
  ADR\000\
  ECHO\000\
  BOOL\000\
  INT\000\
  VOID\000\
  IF\000\
  SET\000\
  WHILE\000\
  CALL\000\
  "

let yynames_block = "\
  CBOOL\000\
  NUM\000\
  IDENT\000\
  OPRIM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 48 "parser.mly"
          (_1)
# 235 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 52 "parser.mly"
                           (_2)
# 242 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 55 "parser.mly"
                                  ( [Stat _1])
# 249 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 56 "parser.mly"
                                ( (Def _1)::_3)
# 257 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 57 "parser.mly"
                                ( (Stat _1)::_3)
# 265 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.stype) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.sexpr) in
    Obj.repr(
# 61 "parser.mly"
                                  ( ConstDef(_2,_3,_4) )
# 274 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.stype) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.sexpr) in
    Obj.repr(
# 62 "parser.mly"
                                                  ( FunDef (_2, _3, _5, _7))
# 284 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.stype) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.sexpr) in
    Obj.repr(
# 63 "parser.mly"
                                                      ( RecFunDef (_3, _4, _6, _8))
# 294 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.stype) in
    Obj.repr(
# 64 "parser.mly"
                          (VarDef(_2, _3))
# 302 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 65 "parser.mly"
                                               (ProcDef(_2,_4,_6))
# 311 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 66 "parser.mly"
                                                   (RecProcDef(_3,_5,_7))
# 320 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
                                  ( BoolType )
# 326 "parser.ml"
               : Ast.stype))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                                  ( IntType )
# 332 "parser.ml"
               : Ast.stype))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                                  ( VoidType)
# 338 "parser.ml"
               : Ast.stype))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.stype list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.stype) in
    Obj.repr(
# 73 "parser.mly"
                                  ( Types (_2, _4) )
# 346 "parser.ml"
               : Ast.stype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stype) in
    Obj.repr(
# 77 "parser.mly"
                                ( [_1] )
# 353 "parser.ml"
               : Ast.stype list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.stype list) in
    Obj.repr(
# 78 "parser.mly"
                             ( _1::_3 )
# 361 "parser.ml"
               : Ast.stype list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 82 "parser.mly"
                               ( [_1] )
# 368 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg list) in
    Obj.repr(
# 83 "parser.mly"
                               ( _1::_3 )
# 376 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.stype) in
    Obj.repr(
# 87 "parser.mly"
                               (Arg(_1,_3))
# 384 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 91 "parser.mly"
                            ([_1])
# 391 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg list) in
    Obj.repr(
# 92 "parser.mly"
                             ( _1::_3 )
# 399 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.stype) in
    Obj.repr(
# 96 "parser.mly"
                                      (Arg(_1,_3))
# 407 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.stype) in
    Obj.repr(
# 97 "parser.mly"
                                      (Argp(_2,_4))
# 415 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.sexpr) in
    Obj.repr(
# 101 "parser.mly"
                                 (Echo(_2))
# 422 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.sexpr) in
    Obj.repr(
# 102 "parser.mly"
                                 (Set(_2,_3))
# 430 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.sexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 103 "parser.mly"
                                 (IfStat(_2,_3,_4))
# 439 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.sexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 104 "parser.mly"
                                 (While(_2,_3))
# 447 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.sexprp list) in
    Obj.repr(
# 105 "parser.mly"
                                  (Call(_2,_3))
# 455 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.sexprp) in
    Obj.repr(
# 109 "parser.mly"
                                     ( [_1] )
# 462 "parser.ml"
               : Ast.sexprp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.sexprp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.sexprp list) in
    Obj.repr(
# 110 "parser.mly"
                                     ( _1::_2 )
# 470 "parser.ml"
               : Ast.sexprp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.sexpr) in
    Obj.repr(
# 114 "parser.mly"
                                     (ASTExpr(_1))
# 477 "parser.ml"
               : Ast.sexprp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 115 "parser.mly"
                                     (ASTAdr(_3))
# 484 "parser.ml"
               : Ast.sexprp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 119 "parser.mly"
                                     ( ASTBool(_1) )
# 491 "parser.ml"
               : Ast.sexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 120 "parser.mly"
                                     ( ASTNum(_1) )
# 498 "parser.ml"
               : Ast.sexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 121 "parser.mly"
                                     ( ASTId(_1) )
# 505 "parser.ml"
               : Ast.sexpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.sexpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.sexpr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.sexpr) in
    Obj.repr(
# 122 "parser.mly"
                                     ( ASTIf(_3,_4,_5) )
# 514 "parser.ml"
               : Ast.sexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.sexpr list) in
    Obj.repr(
# 123 "parser.mly"
                                     ( ASTOp(_2,_3) )
# 522 "parser.ml"
               : Ast.sexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.sexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.sexpr list) in
    Obj.repr(
# 124 "parser.mly"
                                     ( ASTApp(_2,_3) )
# 530 "parser.ml"
               : Ast.sexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.sexpr) in
    Obj.repr(
# 125 "parser.mly"
                                     ( ASTFunAbs(_2,_4) )
# 538 "parser.ml"
               : Ast.sexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.sexpr) in
    Obj.repr(
# 129 "parser.mly"
                  ( [_1] )
# 545 "parser.ml"
               : Ast.sexpr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.sexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.sexpr list) in
    Obj.repr(
# 130 "parser.mly"
                   ( _1::_2 )
# 553 "parser.ml"
               : Ast.sexpr list))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
