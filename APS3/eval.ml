open Ast

type value = Value of int | Closure of sexpr * (value list -> env) | RecClosure of (value -> (sexpr * (value list -> env)))
            | Address of int | ProcClosure of (cmd list * (value list -> env)) | RecProcClosure of (value -> (cmd list * (value list -> env)))
            | Block of int * int
and flux = int list
and env = (string * value) list
and address = None | Some of value | Any 
and memory = address list

exception ArrayOutOfBoundsExceptionAPS

let rec int_of_value v =
  match v with
    Value i -> i
    | _ -> failwith "Error not a Value"

and getInEnv env e =
  match env with
    (s,v)::t when e = s -> v
    |_::t -> getInEnv t e
    |[] -> failwith "Not in env"

and alloc_memory m n =
  let cree = ref (None:int option) in
  for i = 0 to (Array.length m) - n do
    if !cree = None then (
      let vide = ref true in
      for j = i to i+n-1 do
        if m.(j) <> None then vide := false;
      done;
      if !vide then cree := Some i;
    )
  done;
  match !cree with
  | Some k -> (
      for i = k to k+n-1 do
        m.(i) <- Any
      done;
      (k, m)
    )
  | None -> (
      let l = Array.length m in
      let m' = Array.make (l*2) None in
      Array.blit m 0 m' 0 l;
      alloc_memory m' n
    )

and eval_prog p =
  let (_, flux) = eval_block ([], [|None|], []) p in
    List.rev flux

and eval_block etatInit b =
  let (_,mem,flux) = List.fold_left eval_cmd etatInit b in
  (mem, flux)

and eval_cmd (env,mem,flux) cmd =
  match cmd with
    Stat stat -> let (mem, flux) = eval_stat (env,mem,flux) stat in (env,mem,flux)
    | Def def -> let (env, mem) = eval_def (env, mem) def in  (env,mem,flux)
  
and eval_args env args =
  fun a -> (List.combine (List.map get_id_arg args) a)@env

and get_id_arg arg =
      match arg with
        Arg (id,v) -> id
        |Argp (id,v) -> id

and from_arg arg =
      match arg with
        Arg (id,v) -> (id,v)
        |Argp (id,v) -> (id,v)

and eval_def (env, mem) def =
  match def with
    ConstDef (id, _, e) -> let (v, mem') = eval_sexpr (env, mem) e in ((id, v)::env,mem')
    | FunDef (id, _, args, e) -> ((id,Closure(e,eval_args env args))::env,mem)
    | RecFunDef (id, _, args, e) -> ((id, RecClosure(fun f -> (e, fun a -> (id,f)::(List.combine (List.map get_id_arg args) a)@env)))::env,mem)
    | VarDef (id,_) -> let (a, memRes) = alloc_memory mem 1 in ((id, Address a)::env, memRes)
    | ProcDef (id, args, b) -> ((id, ProcClosure(b, eval_args env args))::env, mem)
    | RecProcDef (id, args, b) -> ((id, RecProcClosure(fun p -> (b, fun a -> (id,p)::(List.combine (List.map get_id_arg args) a)@env)))::env,mem)

and eval_stat (env,mem,flux) stat =
  match stat with
  |Echo e -> ( match (eval_sexpr (env, mem) e) with
                    (Value i,mem') -> (mem',(i::flux))
                    | _ -> failwith "Error ECHO" )
  |Set (id, e) -> let (lval, mem') = eval_sexpr(env, mem) e in
                    (match eval_lvalue (env, mem') id with
                        (Address a, mem'') -> mem''.(a) <- Some lval; (mem'', flux)
                        |_ -> failwith "Error in Set, not a var")
  |IfStat (c, i, e) -> (match (eval_sexpr (env, mem) c) with
                        (Value 1,mem') -> let (mem'',flux) = eval_block (env, mem', flux) i in
                                    ((mem'', flux))
                        |(Value 0,mem') -> let (mem'',flux) = eval_block (env, mem', flux) e in
                        ((mem'', flux))
                        |_ -> failwith "Error in if condition")
  |While (c,b) -> (match (eval_sexpr (env, mem) c) with
                    (Value 0,mem') -> (mem',flux)
                    |(Value 1,mem') -> let (mem'',f) = eval_block (env, mem',flux) b in
                                eval_stat (env, mem'',f) (While (c,b))
                    |_ -> failwith "Error in while condition")
  |Call (id,e) -> let p = getInEnv env id in
                  let (args, mem') = (List.fold_left(fun (a, m) e -> let (v, m') = eval_sexprp (env, m) e in v::a, m') ([], mem) e) in
                  (match p with
                    ProcClosure (b, r) -> let (mem'', flux') = eval_block ((r (List.rev args)), mem', flux) b in
                      (mem'', flux')
                    | RecProcClosure rp -> let (b, r) = rp p in
                      let (mem'', flux') = eval_block ((r (List.rev args)), mem', flux) b in
                      (mem'', flux')
                    | _ -> failwith "Error in Call")

and eval_lvalue (env, mem) lval =
    match lval with
    LvalueId id -> (match getInEnv env id with
                      Address a -> (Address a, mem)
                      |Block (b,n) -> (Block(b,n), mem)
                      |_ -> failwith "Not an Lvalue"
                      )
    |Lvalue (lval, e) -> let (b, mem') = eval_lvalue (env,mem) lval in
                          let (e', mem'') = eval_sexpr (env, mem') e in 
                          (match (b, e') with
                            (Block (b, n), Value i) -> if i < 0 || i > n then raise ArrayOutOfBoundsExceptionAPS;
                                  (Address (b+i), mem'')
                            |(Address a, Value i) -> (match mem''.(a) with
                                        Some (Block (b,n)) -> if i < 0 || i > n then raise ArrayOutOfBoundsExceptionAPS;
                                        (Address (b+i), mem'')
                                        |_ -> failwith "Error eval Lvalue")
                            |_ -> failwith "Error Lvalue")

and eval_sexprp (env, mem) value =
  match value with
    ASTAdr x -> (match getInEnv env x with
                  Address a -> (match mem.(a) with
                                Some v -> (v,mem)
                                |Any -> (Value 0,mem)
                                |_ -> failwith (x^" not in memory"))
                  | v -> (v,mem))
  | ASTExpr e -> eval_sexpr (env, mem) e

and eval_sexpr (env, mem) value =
  match value with
    ASTBool true -> (Value 1,mem)
    | ASTBool false -> (Value 0,mem)
    | ASTNum n -> (Value n,mem)
    | ASTId x -> (match getInEnv env x with
                  Address a -> (match mem.(a) with
                                Some v -> (v,mem)
                                |Any -> (Value 0,mem)
                                |_ -> failwith (x^" not in memory"))
                  | v -> (v,mem))
    | ASTIf (c, i, e) -> (match (eval_sexpr (env, mem) c) with
                          (Value 1,mem') -> eval_sexpr (env, mem') i
                          |(Value 0,mem') -> eval_sexpr (env, mem) e
                          |_ -> failwith "Error in if condition")
    | ASTOp (op,vals) -> let (vals', mem') = (List.fold_left (fun (a, m) e -> let (v, m') = eval_sexpr (env, m) e in v::a, m') ([], mem) vals) in
                      eval_op (env,mem') (op,vals')
    | ASTApp(f, es) -> let (c,mem') = eval_sexpr (env, mem) f in
                      let (args, mem'') = (List.fold_left (fun (a, m) e -> let (v, m') = eval_sexpr (env, m) e in v::a, m') ([], mem') es) in
                            (match c with
                              Closure (e', r) -> eval_sexpr ((r (List.rev args)),mem'') e'
                            | RecClosure rf -> let (e', r) = (rf c) in eval_sexpr ((r (List.rev args)),mem'') e'
                            | _ -> failwith "Error in App")
    | ASTFunAbs(args, e) -> (Closure (e, eval_args env args),mem)

    and eval_op (env,mem) (op,vals) = 
    match (op,vals) with
      ("not",[val1]) -> (match val1 with
                      Value 1 -> (Value 0,mem)
                      |Value 0 -> (Value 1,mem)
                      |_ -> failwith "Error in NOT argument")
      | ("and",[val1;val2]) -> if (int_of_value val1) == 0 then (Value 0,mem) else if (int_of_value val2) == 0 then (Value 0,mem) else (Value 1,mem)
      | ("or",[val1;val2]) -> if (int_of_value val1) == 1 then (Value 1,mem) else if (int_of_value val2) == 1 then (Value 1,mem) else (Value 0,mem)
      | ("eq",[val1;val2]) -> if (int_of_value val1) == (int_of_value val2) then (Value 1,mem) else (Value 0,mem)
      | ("lt",[val1;val2])  -> if (int_of_value val1) < (int_of_value val2) then (Value 1,mem) else (Value 0,mem)
      | ("add",[val1;val2])  -> (Value ((int_of_value val1) + (int_of_value val2)),mem)
      | ("sub",[val1;val2])  -> (Value ((int_of_value val1) - (int_of_value val2)),mem)
      | ("mul",[val1;val2])  -> (Value ((int_of_value val1) * (int_of_value val2)),mem)
      | ("div",[val1;val2])  -> (Value ((int_of_value val1) / (int_of_value val2)),mem)
      | ("len",[val1]) -> (match val1 with
                            Block (_,n) -> (Value n,mem)
                            |_ -> failwith "Error in LEN")
      | ("nth", [val1;val2]) -> (match (val1,val2) with
                                (Block (a,n), Value i) -> if i >= n || i < 0 then raise ArrayOutOfBoundsExceptionAPS;
                                      (match mem.(a+i) with
                                      Some v -> (v,mem)
                                      |Any -> (Value 0, mem)
                                      |None -> failwith "Error in NTH, not in memory")
                                |_ -> failwith "Error in NTH"
      )

      |("alloc", [val1]) -> (match val1 with
                                Value n -> let (a,mem') = alloc_memory mem n in
                                            (Block (a,n), mem')
                                |_ -> failwith "Error in ALLOC")
      |_ -> failwith "Error in OP"
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