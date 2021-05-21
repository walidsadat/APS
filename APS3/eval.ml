open Ast

type valeur = Valeur of int | Closure of closure | RecClosure of recClosure
            | ProcClosure of procClosure | RecProcClosure of recProcClosure
            | Adresse of int | Block of int
and closure = sexpr * (valeur list -> env)
and recClosure = (valeur -> closure)
and procClosure = (cmd list * (valeur list -> env))
and recProcClosure = valeur -> procClosure
and flux = int list
and env = (string * valeur) list

and espace = None | Some of valeur | Any
and mem = (int * espace) list

and valueB = Empty | ValeurB of valeur

exception IndexOutOfBoundsException

let rec int_of_valeur v =
  match v with
    Valeur i -> i
    | _ -> 0

let rec int_of_adresse adr =
  match adr with
    Adresse a -> a
    | _ -> failwith "Not an Address"

and getFromEnv env e =
  match env with
      [] -> failwith "Not in env"
    | (s,v)::t when e = s -> v
    | _::t -> getFromEnv t e

and getFromMem mem adr =
  match mem with
      [] -> failwith ((string_of_int adr) ^ " Not in mem")
    | (k,v)::t when k = adr -> ( match v with
                                      Some valeur -> valeur
                                    | Any -> Valeur 0
                                    | _ -> failwith "Not in mem"
                              )
    | _::t -> getFromMem t adr

and newAdresse mem =(
  let rec aux m a =
    match m with
        [] -> Adresse (a+1)
      | (k,_)::t when k > a -> aux t k
      | (k,_)::t  -> aux t a
  in aux mem 0)

and alloc_memory mem =
  let adr = newAdresse mem in
      match adr with
          Adresse a -> (adr, mem@[(a,Any)])
        | _ -> failwith "Error in memory allocation"

and alloc_block mem n =
  let adr = newAdresse mem in
    let mem' = mem@[(int_of_adresse adr, Some (Valeur n))] in
    match adr with
        Adresse adri -> (let rec aux mem n i adri =
                        match i with
                            i when i == n ->(adri, mem@[(adri+i,Any)])
                          | i -> aux (mem@[(adri+i,Any)]) n (i+1) adri
                      in aux mem' n 1 adri
                    )
      | _ -> failwith "Error in block allocation"

and remplaceElem l x e =
  let rec aux l x e lr = 
      match l with
          [] -> lr
        | h::t when h = x -> aux t x e lr@[e]
        | h::t ->aux t x e lr@[h]
  in aux l x e []

and editInMem mem adr v =
  let rec aux m adr v =
    match m with
        [] -> failwith (string_of_int adr^"Not in memory")
      | (k,e)::t when k = adr -> remplaceElem mem (k,e) (k,v)
      | _::t -> aux t adr v
  in aux mem adr v

and eval_prog bk =
  let (_,_, flux) = eval_block ([], [], []) bk in
    List.rev flux

and eval_block (env,mem,flux) cs = eval_cmds (env,mem,flux) cs

and eval_cmds (env,mem,flux) cmds =
  match cmds with
      (Dec d)::cs -> eval_cmds (eval_dec (env, mem,flux) d) cs
    | (Stat s)::cs -> (match eval_stat (env,mem,flux) s with
                          (Empty,mem',flux') -> eval_cmds (env,mem',flux') cs
                        | (ValeurB v,mem',flux') -> (ValeurB v, mem',flux')
                      )
    | [Return e] -> let (v ,mem', flux') = eval_sexpr (env,mem,flux) e in (ValeurB v, mem', flux')
    | [] -> (Empty, mem, flux)
    | _ -> failwith "Error in cmds"

and eval_args env args =
  fun a -> (List.combine (List.map get_id_arg args) a)@env

and get_id_arg arg =
  match arg with
      Arg  (x,_) -> x
    | Argp (x,_) -> x

and eval_dec (env, mem,flux) dec =
  match dec with
      ConstDec (x, _, e) -> let (v,mem',flux') = eval_sexpr (env, mem,flux) e in ((x, v)::env,mem',flux')
    | FunDec (x, _, args, e) -> ((x,Closure(e,eval_args env args))::env,mem,flux)
    | FunRecDec (x, _, args, e) -> ((x, RecClosure(fun f -> (e, fun a -> (x,f)::(List.combine (List.map get_id_arg args) a)@env)))::env,mem,flux)
    | VarDec (x,_) -> let (a, memRes) = alloc_memory mem in ((x, a)::env, memRes,flux)
    | ProcDec (x, args, bk) -> ((x, ProcClosure(bk, eval_args env args))::env, mem,flux)
    | ProcRecDec (x, args, bk) -> ((x, RecProcClosure(fun p -> (bk, fun a -> (x,p)::(List.combine (List.map get_id_arg args) a)@env)))::env,mem,flux)
    | FunPDec (x, _,args, bk) -> ((x, ProcClosure(bk, eval_args env args))::env, mem,flux)
    | FunRecPDec (x,_,args, bk) -> ((x, RecProcClosure(fun p -> (bk, fun a -> (x,p)::(List.combine (List.map get_id_arg args) a)@env)))::env,mem,flux)

and eval_stat (env,mem,flux) stat =
  match stat with
      Echo e -> ( match (eval_sexpr (env,mem,flux) e) with
                      (Valeur n,mem',flux') -> (Empty,mem',(n::flux'))
                    | _ -> failwith "Error ECHO"
              )
    | Set (e1, e2) -> let (a,mem',flux') = eval_lvalue (env,mem,flux) e1 in
                      let (v,mem',flux') = eval_sexpr (env,mem',flux') e2 in
                          let mem' = editInMem mem' a (Some v) in (Empty, mem',flux')
    | IfStat (e, bk1, bk2) -> ( match (eval_sexpr (env, mem,flux) e) with
                                    (Valeur 1,mem',flux') -> let (v,mem',flux') = eval_block (env, mem', flux') bk1 in
                                                              (v,mem',flux')
                                  | (Valeur 0,mem',flux') -> let (v,mem',flux') = eval_block (env, mem', flux') bk2 in
                                                              (v,mem',flux')
                                  | _ -> failwith "Error in if condition"
                            )
    | While (e,bk) -> ( match (eval_sexpr (env,mem,flux) e) with
                            (Valeur 0,mem',flux') -> (Empty,mem',flux')
                          | (Valeur 1,mem',flux') -> let (v,mem',f) = eval_block (env, mem',flux') bk in
                                  (
                                    match v with 
                                        Empty -> eval_stat (env, mem', flux') (While (e,bk))
                                      | ValeurB _ -> (v, mem', flux')
                                  )
                          | _ -> failwith "Error in while condition")
    | Call (x,es) -> let p = getFromEnv env x in
                      let (args, mem',flux') = (List.fold_left(fun (a, m,f ) es -> let (v, m',f') = eval_sexprp (env, m,flux) es in
                        v::a, m',f') ([], mem,flux) es) in
                          ( match p with
                                ProcClosure (bk, r) -> let (v,mem'', flux') = eval_block ((r (List.rev args)), mem', flux') bk in
                                                        (v,mem'', flux')
                              | RecProcClosure rp -> let (bk, r) = rp p in
                                                      let (v,mem'', flux') = eval_block ((r (List.rev args)), mem', flux') bk in
                                                        (v,mem'', flux')
                              | _ -> failwith "Error in Call"
                          )

and eval_lvalue (env,mem,flux) lval =
  match lval with
      Lvar x -> ( match getFromEnv env x with
                      Adresse a -> (a,mem,flux)
                    | Block (a) -> ((a+1),mem,flux)
                    | _ -> failwith "Not an Lvalue"
                  )
    | Lnth (e1, e2) -> ( match eval_lvalue (env,mem,flux) e1 with
                            (a,mem',flux') -> ( match eval_sexpr (env,mem',flux') e2 with
                                                    (Valeur i,mem'',flux'') -> 
                                                                                ( match getFromMem mem'' (a+i) with
                                                                                    Valeur n -> ((a+i),mem'',flux'')
                                                                                  | Block(a') -> if (i >= (int_of_valeur (getFromMem mem' a')) || i < 0) then raise IndexOutOfBoundsException;
                                                                                                    ((a'+1),mem'',flux'')
                                                                                  |_ ->failwith "Error in lnth"
                                                                              )
                                                  | _ -> failwith "Error in lnth"
                                              )
                        )

and eval_sexprp (env, mem,flux) valeur =
  match valeur with
      ASTAdr x -> (match getFromEnv env x with
                      Adresse a -> (Adresse a,mem,flux)
                    | v -> (v,mem,flux)
                  )
    | ASTExpr e -> eval_sexpr (env, mem,flux) e

and eval_sexpr (env, mem, flux) valeur =
  match valeur with
      ASTBool true -> (Valeur 1,mem, flux)
    | ASTBool false -> (Valeur 0,mem, flux)
    | ASTNum n -> (Valeur n,mem, flux)
    | ASTId x -> (match getFromEnv env x with
                      Adresse a -> (getFromMem mem a,mem, flux)
                    | v -> (v,mem,flux)
                  )
    | ASTIf (e1, e2, e3) -> ( match (eval_sexpr (env, mem,flux) e1) with
                                  (Valeur 1,mem',flux') -> eval_sexpr (env, mem',flux') e2
                                | (Valeur 0,mem',flux') -> eval_sexpr (env, mem',flux') e3
                                | _ -> failwith "Error in if condition"
                            )
    | ASTOp (op,vals) -> eval_op (env,mem,flux) (op,vals)
    | ASTFunAbs (args, e) -> (Closure (e, eval_args env args),mem,flux)
    | ASTApp (f, es) -> let (e,mem',flux') = eval_sexpr (env, mem,flux) f in
                          let (args, mem'',flux'') = (List.fold_left (fun (a, m,f) e -> let (v, m',f') = eval_sexpr (env, m,f) e in v::a, m',f') ([], mem',flux') es) in
                            ( match e with
                                  Closure (e', r) -> eval_sexpr ((r (List.rev args)),mem'',flux'') e'
                                | RecClosure rf -> let (e', r) = (rf e) in eval_sexpr ((r (List.rev args)),mem'',flux'') e'
                                | ProcClosure (b, r) -> let (v, mem''', flux''') = eval_block((r (List.rev args)),mem'',flux'') b in
                                  ( match v with
                                        ValeurB v -> (v, mem''', flux''')
                                      | Empty -> failwith "App FunRecProc : Empty valeur"
                                  )
                                | RecProcClosure rp -> let (b, r) = (rp e) in
                                  let (v, mem''', flux''') = eval_block ((r (List.rev args)), mem'', flux'') b in
                                    ( match v with
                                          ValeurB v -> (v, mem''', flux''')
                                        | Empty -> failwith "App FunRecProc : Empty valeur"
                                    )
                                | _ -> failwith "Error in App"
                            )

and eval_op (env,mem,flux) (op,vals) = 
  match (op,vals) with
      ("not",[val1]) -> ( match (eval_sexpr (env,mem,flux) val1) with
                              (Valeur 1,mem',flux') -> (Valeur 0,mem',flux')
                            | (Valeur 0,mem',flux') -> (Valeur 1,mem',flux')
                            | _ -> failwith "Error in NOT argument"
                        )
    | ("and",[val1;val2]) -> let (val1',mem',flux') = eval_sexpr (env,mem,flux) val1 in
                              let (val2',mem',flux') = eval_sexpr (env,mem',flux') val2 in
                              ( match ((int_of_valeur val1'),(int_of_valeur val2')) with
                                    (1,1) -> (Valeur 1,mem',flux')
                                  | (_,_) -> (Valeur 0,mem',flux')
                              )
    | ("or",[val1 ;val2]) -> let (val1',mem',flux') = eval_sexpr (env,mem,flux) val1 in
                              let (val2',mem',flux') = eval_sexpr (env,mem',flux') val2 in
                              ( match ((int_of_valeur val1'),(int_of_valeur val2')) with
                                    (0,0) -> (Valeur 0,mem',flux')
                                  | (_,_) -> (Valeur 1,mem',flux')
                              )
    | ("eq",[val1 ;val2]) -> let (val1',mem',flux') = eval_sexpr (env,mem,flux) val1 in
                              let (val2',mem',flux') = eval_sexpr (env,mem',flux') val2 in
                              ( match (int_of_valeur val1') == (int_of_valeur val2') with
                                    true -> (Valeur 1,mem',flux')
                                  | false -> (Valeur 0,mem',flux')
                              )
    | ("lt",[val1 ;val2]) -> let (val1',mem',flux') = eval_sexpr (env,mem,flux) val1 in
                              let (val2',mem',flux') = eval_sexpr (env,mem',flux') val2 in
                              ( match (int_of_valeur val1') < (int_of_valeur val2') with
                                    true -> (Valeur 1,mem',flux')
                                  | false -> (Valeur 0,mem',flux')
                              )
    | ("add",[val1;val2]) -> let (val1',mem',flux') = eval_sexpr (env,mem,flux) val1 in
                              let (val2',mem',flux') = eval_sexpr (env,mem',flux') val2
                                in (Valeur ((int_of_valeur val1') + (int_of_valeur val2')),mem',flux')
    | ("sub",[val1;val2]) -> let (val1',mem',flux') = eval_sexpr (env,mem,flux) val1 in
                              let (val2',mem',flux') = eval_sexpr (env,mem',flux') val2
                                in (Valeur ((int_of_valeur val1') - (int_of_valeur val2')),mem',flux')
    | ("mul",[val1;val2]) -> let (val1',mem',flux') = eval_sexpr (env,mem,flux) val1 in
                              let (val2',mem',flux') = eval_sexpr (env,mem',flux') val2
                                in (Valeur ((int_of_valeur val1') * (int_of_valeur val2')),mem',flux')
    | ("div",[val1;val2]) -> let (val1',mem',flux') = eval_sexpr (env,mem,flux) val1 in
                              let (val2',mem',flux') = eval_sexpr (env,mem',flux') val2
                                in (Valeur ((int_of_valeur val1') / (int_of_valeur val2')),mem',flux')
    | ("alloc",[val1]) -> ( match eval_sexpr (env, mem,flux) val1 with
                                (Valeur n,mem',flux') when n > 0 -> let (a,mem') = alloc_block mem n in
                                                                      (Block (a), mem',flux')
                              | _ -> failwith "Error in ALLOC"
                          )
    | ("len",[val1]) -> ( match eval_sexpr (env, mem,flux) val1 with
                                (Block a,mem',flux') -> (getFromMem mem' a, mem',flux')
                              | _ -> failwith "Error in LEN"
                          )
    | ("nth",[val1;val2]) -> let(val1', mem',flux') = eval_sexpr (env, mem,flux) val1 in
                              let (val2', mem',flux') = eval_sexpr (env, mem',flux') val2 in
                              (match val1', val2' with
                                  (Block adr, Valeur i) -> if (i >= (int_of_valeur (getFromMem mem' adr)) || i < 0) then raise IndexOutOfBoundsException;
                                                                (getFromMem mem' (adr+i+1),mem',flux')
                                | _ -> failwith "Error in NTH, not in memory"
                              )
    | _ -> failwith "Error in OP arguments"


and print_flux l =
        match l with
        [] -> ()
        |h::t -> print_int h; print_string "\n"; print_flux t

and print_mem mem =
    match mem with
      [] -> ()
      |(k,Some (Valeur v))::t -> print_int k; print_string " "; print_int(v); print_string "\n";print_mem t
      |(k,Some (Adresse v))::t -> print_int k; print_string " "; print_int(v); print_string "\n";print_mem t
      |(k,Any)::t -> print_int k; print_string " Any\n";print_mem t
      |(_,_)::t -> print_mem t

;;

let fname = Sys.argv.(1) in
let ic = open_in fname in
      try
        let lexbuf = Lexing.from_channel ic in
        let e = Parser.prog Lexer.token lexbuf in
        print_flux(eval_prog e);
      with Lexer.Eof ->
        exit 0