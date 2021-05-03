open Ast

type value = Value of int | Closure of closure | RecClosure of recClosure
and closure = sexpr * (value list -> env)
and recClosure = (value -> closure)
and flux = int list
and env = (string * value) list

let rec int_of_value v =
  match v with
    Value i -> i
    | _ -> failwith "Error not a Value"

and getInEnv env e =
  match env with
    (s,v)::t when e = s -> v
    |_::t -> getInEnv t e
    |[] -> failwith "Not in env"

and eval_prog p =
  let (_, flux) = List.fold_left eval_cmd ([], []) p in
    List.rev flux

and eval_cmd (env,flux) cmd =
  match cmd with
    Stat stat -> (env, eval_stat env flux stat)
    | Def def -> (eval_def env def, flux)
  
and eval_args env args =
  fun a -> (List.combine (fst (List.split args)) a)@env

and eval_def env def =
  match def with
    ConstDef (id, _, e) -> (id, eval_sexpr env e)::env
    | FunDef (id, _, args, e) -> (id,Closure(e,eval_args env args))::env
    | RecFunDef (id, _, args, e) -> (id, RecClosure(fun f -> (e, fun a -> (id,f)::(List.combine (fst (List.split args)) a)@env)))::env

and eval_stat env flux stat =
  match stat with
  | Echo e -> ( match (eval_sexpr env e) with
                    Value i -> (i::flux)
                    | _ -> failwith "Error ECHO" )

and eval_sexpr env value =
  match value with
    ASTBool true -> Value 1
    | ASTBool false -> Value 0
    | ASTNum n -> Value n
    | ASTId x -> getInEnv env x
    | ASTIf (c, i, e) -> (match (eval_sexpr env c) with
                          Value 1 -> eval_sexpr env i
                          |Value 0 -> eval_sexpr env e
                          |_ -> failwith "Error in if condition")
    | ASTOp (op,vals) -> eval_op env (op,vals)
    | ASTApp(e, es) -> let c = eval_sexpr env e and args = (List.map (eval_sexpr env) es) in
                            (match c with
                            Closure (e', r) -> eval_sexpr (r args) e'
                            | RecClosure rf -> let (e', r) = (rf c) in eval_sexpr (r args) e'
                            | _ -> failwith "Error in App")
    | ASTFunAbs(args, e) -> Closure (e, eval_args env args)

and eval_op env (op,vals) = 
  match (op,vals) with
    ("not",[val1]) -> (match (eval_sexpr env val1) with
                    Value 1 -> Value 0
                    |Value 0 -> Value 1
                    |_ -> failwith "Error in NOT argument")
    | ("and",[val1;val2]) -> if (int_of_value (eval_sexpr env val1)) == 0 then Value 0 else if (int_of_value (eval_sexpr env val2)) == 0 then Value 0 else Value 1
    | ("or",[val1;val2]) -> if (int_of_value (eval_sexpr env val1)) == 1 then Value 1 else if (int_of_value (eval_sexpr env val2)) == 1 then Value 1 else Value 0
    | ("eq",[val1;val2]) -> if (int_of_value (eval_sexpr env val1)) == (int_of_value (eval_sexpr env val2)) then Value 1 else Value 0
    | ("lt",[val1;val2])  -> if (int_of_value (eval_sexpr env val1)) < (int_of_value (eval_sexpr env val2)) then Value 1 else Value 0
    | ("add",[val1;val2])  -> Value ((int_of_value (eval_sexpr env val1)) + (int_of_value (eval_sexpr env val2)))
    | ("sub",[val1;val2])  -> Value ((int_of_value (eval_sexpr env val1)) - (int_of_value (eval_sexpr env val2)))
    | ("mul",[val1;val2])  -> Value ((int_of_value (eval_sexpr env val1)) * (int_of_value (eval_sexpr env val2)))
    | ("div",[val1;val2])  -> Value ((int_of_value (eval_sexpr env val1)) / (int_of_value (eval_sexpr env val2)))
    |_ -> failwith "Error in OP arguments"
;;

let rec print_list l =
  match l with
  [] -> ()
  |h::t -> print_int h; print_string "\n";print_list t

;;

let fname = Sys.argv.(1) in
let ic = open_in fname in
try
  let lexbuf = Lexing.from_channel ic in
  let e = Parser.prog Lexer.token lexbuf in
  print_list(eval_prog e);
with Lexer.Eof ->
  exit 0