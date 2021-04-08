(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == S-expressions Syntaxe ML                                             == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast

let rec prolog_of_prog cmds =
  "prog(["^prolog_of_cmds cmds^"])"

and prolog_of_cmds cmds = 
  match cmds with
  [] -> ""
  |[h] -> prolog_of_cmd h
  |h::t -> prolog_of_cmd h ^","^prolog_of_cmds t

and prolog_of_cmd cmd =
  match cmd with
    Stat stat -> prolog_of_stat stat
    | Def def -> prolog_of_def def

and prolog_of_type t =
  match t with
    BoolType -> "bool"
    | IntType -> "int"
    | VoidType -> "void"
    | Types (ts,t) -> "types(["^(String.concat "," (List.map prolog_of_type ts))^"],"^(prolog_of_type t)^")"

and prolog_of_arg arg =
  match arg with
    (id,t) -> "(\"" ^ id ^ "\"," ^ prolog_of_type t^")"

and prolog_of_args args =
  match args with
    [] -> ""
    |[a] -> prolog_of_arg a
    |a::t -> prolog_of_arg a ^ "," ^ prolog_of_args t
  
and prolog_of_def def =
  match def with
    ConstDef (id, t, e) -> "const(\""^id^"\","^(prolog_of_type t)^","^(prolog_of_sexpr e)^")"                       
    | FunDef (id, t, args, e) ->"fun(\""^id^"\","^(prolog_of_type t)^",["^(prolog_of_args args)^"],"^(prolog_of_sexpr e)^")"
    | RecFunDef (id, t, args, e) -> "funrec(\""^id^"\","^(prolog_of_type t)^",["^(prolog_of_args args)^"],"^(prolog_of_sexpr e)^")"

and prolog_of_stat stat =
  match stat with
    Echo e -> "echo("^prolog_of_sexpr e^")" 

and prolog_of_sexpr e =
  match e with
      ASTBool true -> "true"
    | ASTBool false -> "false"
    | ASTNum n -> string_of_int n
    | ASTId x -> "ident(\""^x^"\")"
    | ASTIf (c, i, e) -> "if("^(prolog_of_sexpr c)^","^(prolog_of_sexpr i)^","^(prolog_of_sexpr e)^")"
    | ASTOp (op, es) -> op^"("^(prolog_of_sexprs es)^")"
    | ASTApp(e, es) -> "app("^(prolog_of_sexpr e)^",["^(prolog_of_sexprs es)^"])"
    | ASTFunAbs(args, e) -> "funabs(["^(prolog_of_args args)^"],"^(prolog_of_sexpr e)^")"

    and prolog_of_sexprs es =
    match es with
      [] -> ""
      |[e] -> prolog_of_sexpr e
      |e::t -> prolog_of_sexpr e ^ "," ^ prolog_of_sexprs t
;;

let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let e = Parser.prog Lexer.token lexbuf in
    print_string(prolog_of_prog e);
    print_string ".";
  with Lexer.Eof ->
    exit 0