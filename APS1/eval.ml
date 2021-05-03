open Ast

type value = Value of int | Closure of closure | RecClosure of recClosure
            | ProcClosure of procClosure | RecProcClosure of recProcClosure
            | Address of int
and closure = sexpr * (value list -> env)
and recClosure = (value -> closure)
and procClosure = (cmd list * (value list -> env))
and recProcClosure = value -> procClosure
and flux = int list
and env = (string * value) list

and espace = None | Some of value | Any
and mem = (int * espace) list

let rec int_of_value v =
  match v with
    Value i -> i
    | _ -> failwith "Error not a Value"

and getInEnv env e =
  match env with
    [] -> failwith "Not in env"
    |(s,v)::t when e = s -> v
    |_::t -> getInEnv t e

and getInMem mem adr =
  match mem with
      [] -> failwith "Not in mem"
      |(k,v)::t when k = adr -> v
      |_::t -> getInMem t adr

and newAddress mem =(
  let rec aux m a =
    match m with
      [] -> Address (a+1)
      |(k,_)::t when k > a -> aux t k
      |(k,_)::t  -> aux t a
  in aux mem 0)

and alloc_memory mem =
  let adr = newAddress mem in
      match adr with
        Address a -> (adr, mem@[(a,Any)])
        |_ -> failwith "Error in allocation"

and replaceElem l x e =(
    let rec aux l x e lr = 
      match l with
        [] -> lr
        |h::t when h = x -> aux t x e lr@[e]
        |h::t ->aux t x e lr@[h]
    in aux l x e [])

and editInMem mem adr v =
  let rec aux m adr v =
    match m with
      [] -> failwith "Not in mem"
      |(k,e)::t when k = adr -> replaceElem mem (k,e) (k,v)
      |_::t -> aux t adr v
    in aux mem adr v

and eval_prog p =
  let (_, flux) = eval_block ([], [], []) p in
    List.rev flux

and eval_block etatInit b =
  let (_,mem,flux) = List.fold_left eval_cmd etatInit b in
  (mem, flux)

and eval_cmd (env,mem,flux) cmd =
  match cmd with
    Stat stat -> let (mem, flux) = eval_stat (env,mem,flux) stat in (env,mem,flux)
    | Def def -> let (env, mem) = eval_def (env, mem) def in  (env,mem,flux)
  
and eval_args env args =
  fun a -> (List.combine (fst (List.split args)) a)@env

and eval_def (env, mem) def =
  match def with
    ConstDef (id, _, e) -> ((id, eval_sexpr (env, mem) e)::env,mem)
    | FunDef (id, _, args, e) -> ((id,Closure(e,eval_args env args))::env,mem)
    | RecFunDef (id, _, args, e) -> ((id, RecClosure(fun f -> (e, fun a -> (id,f)::(List.combine (fst (List.split args)) a)@env)))::env,mem)
    | VarDef (id,_) -> let (a, memRes) = alloc_memory mem in ((id, a)::env, memRes)
    | ProcDef (id, args, b) -> ((id, ProcClosure(b, eval_args env args))::env, mem)
    | RecProcDef (id, args, b) -> ((id, RecProcClosure(fun p -> (b, fun a -> (id,p)::(List.combine (fst (List.split args)) a)@env)))::env,mem)

and eval_stat (env,mem,flux) stat =
  match stat with
  |Echo e -> ( match (eval_sexpr (env, mem) e) with
                    Value i -> (mem,(i::flux))
                    | _ -> failwith "Error ECHO" )
  |Set (id, e) -> ( match (getInEnv env id) with
                    Address a -> let mem' = editInMem mem a (Some (eval_sexpr (env, mem) e)) in (mem',flux)
                    |_ -> failwith (id^" not a Var"))
  |IfStat (c, i, e) -> (match (eval_sexpr (env, mem) c) with
                        Value 1 -> let (mem,flux) = eval_block (env, mem, flux) i in
                                    ((mem, flux))
                        |Value 0 -> let (mem,flux) = eval_block (env, mem, flux) e in
                        ((mem, flux))
                        |_ -> failwith "Error in if condition")
  |While (c,b) -> (match (eval_sexpr (env, mem) c) with
                    Value 0 -> (mem,flux)
                    |Value 1 -> let (m,f) = eval_block (env, mem,flux) b in
                                eval_stat (env, m,f) (While (c,b))
                    |_ -> failwith "Error in while condition")
  |Call (id,e) -> let p = getInEnv env id and args = (List.map (eval_sexpr (env,mem)) e) in
                (match p with
                ProcClosure (b, r) -> let (m,f) = eval_block((r args), mem, flux) b in
                (m,f)
                |RecProcClosure rp -> let (b, r) = rp p in
                let (m,f) = eval_block((r args), mem, flux) b in
                (m,f)
                |_ -> failwith "Procedure error")

and eval_sexpr (env, mem) value =
  match value with
    ASTBool true -> Value 1
    | ASTBool false -> Value 0
    | ASTNum n -> Value n
    | ASTId x -> (match getInEnv env x with
                  Address a -> (match getInMem mem a with
                                Some v -> v
                                |Any -> Value 0
                                |_ -> failwith (x^" not in memory"))
                  | v -> v)
    | ASTIf (c, i, e) -> (match (eval_sexpr (env, mem) c) with
                          Value 1 -> eval_sexpr (env, mem) i
                          |Value 0 -> eval_sexpr (env, mem) e
                          |_ -> failwith "Error in if condition")
    | ASTOp (op,vals) -> eval_op (env,mem) (op,vals)
    | ASTApp(e, es) -> let c = eval_sexpr (env, mem) e and args = (List.map (eval_sexpr (env, mem)) es) in
                            (match c with
                            Closure (e', r) -> eval_sexpr ((r args),mem) e'
                            | RecClosure rf -> let (e', r) = (rf c) in eval_sexpr ((r args),mem) e'
                            | _ -> failwith "Error in App")
    | ASTFunAbs(args, e) -> Closure (e, eval_args env args)

    and eval_op (env,mem) (op,vals) = 
    match (op,vals) with
      ("not",[val1]) -> (match (eval_sexpr (env,mem) val1) with
                      Value 1 -> Value 0
                      |Value 0 -> Value 1
                      |_ -> failwith "Error in NOT argument")
      | ("and",[val1;val2]) -> if (int_of_value (eval_sexpr (env,mem) val1)) == 0 then Value 0 else if (int_of_value (eval_sexpr (env,mem) val2)) == 0 then Value 0 else Value 1
      | ("or",[val1;val2]) -> if (int_of_value (eval_sexpr (env,mem) val1)) == 1 then Value 1 else if (int_of_value (eval_sexpr (env,mem) val2)) == 1 then Value 1 else Value 0
      | ("eq",[val1;val2]) -> if (int_of_value (eval_sexpr (env,mem) val1)) == (int_of_value (eval_sexpr (env,mem) val2)) then Value 1 else Value 0
      | ("lt",[val1;val2])  -> if (int_of_value (eval_sexpr (env,mem) val1)) < (int_of_value (eval_sexpr (env,mem) val2)) then Value 1 else Value 0
      | ("add",[val1;val2])  -> Value ((int_of_value (eval_sexpr (env,mem) val1)) + (int_of_value (eval_sexpr (env,mem) val2)))
      | ("sub",[val1;val2])  -> Value ((int_of_value (eval_sexpr (env,mem) val1)) - (int_of_value (eval_sexpr (env,mem) val2)))
      | ("mul",[val1;val2])  -> Value ((int_of_value (eval_sexpr (env,mem) val1)) * (int_of_value (eval_sexpr (env,mem) val2)))
      | ("div",[val1;val2])  -> Value ((int_of_value (eval_sexpr (env,mem) val1)) / (int_of_value (eval_sexpr (env,mem) val2)))
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