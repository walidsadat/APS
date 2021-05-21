%% Check if a var is in the context
inCtx([(X,T)|_],X,T).
inCtx([(_,_)|C],X,T) :- inCtx(C,X,T).

%% Check types of a list of arguments
checkArgsType([],[]).
checkArgsType([(_,T)|ArgsList],[T|TypesList]) :- checkArgsType(ArgsList,TypesList).

%% Check types of a list of expressions (exprs)
checkExprsListType(_,[],[]).
checkExprsListType(Ctx, [Expr|ExprsList], [T|TypesList]) :- exprType(Ctx, Expr, T), checkExprsListType(Ctx, ExprsList, TypesList).

%% Check types of a list of expressions (expars)
checkExprsListTypeP(_,[],[]).
checkExprsListTypeP(Ctx, [Expr|ExprsList], [T|TypesList]) :- exparType(Ctx, Expr, T), checkExprsListTypeP(Ctx, ExprsList, TypesList).

%% Check a program
%% progType(prog, void)
progType(bloc(Bk),void) :- cmdsType([], Bk, void).

%% Check a bloc
%% blocType(ctx,bloc, void)
blocType(ctx,bloc(Cs),T) :- cmdsType(ctx, Cs, T).

%% Check a list of commands
%% cmdsType(Ctx, cmds, type)
cmdsType(Ctx, [Dec|Cs], T) :- defType(Ctx, Dec, CtxRes), cmdsType(CtxRes, Cs, T).
cmdsType(Ctx, [Stat|Cs], T) :- statType(Ctx, Stat, void), cmdsType(Ctx, Cs, T).
cmdsType(Ctx, [Stat|Cs], T) :- statType(Ctx, Stat, union(T,void)), cmdsType(Ctx, Cs, T).
cmdsType(Ctx, [Stat], T) :- T\=void, statType(Ctx, Stat, T).
cmdsType(Ctx, [ret(E)], T) :- exprType(Ctx, E, T).
cmdsType(_,[],void).

%% Check a definition
%% defType(Ctx, def, CtxRes)
defType(Ctx,const(X,T,E), [(X,T)|Ctx]) :- exprType(Ctx,E,T).
defType(Ctx,fun(X,T,Args,E), [(X, types(TypeArgs,T))|Ctx]) :- checkArgsType(Args, TypeArgs), append(Args, Ctx, CtxRes), exprType(CtxRes, E, T).
defType(Ctx,funrec(X,T,Args,E), [(X, types(TypeArgs,T))|Ctx]) :- checkArgsType(Args, TypeArgs), append(Args, Ctx, CtxRes), exprType([(X,types(TypeArgs,T))|CtxRes],E,T).
defType(Ctx,var(X,ref(T)),[(X,ref(T))|Ctx]).
defType(Ctx,proc(X,Argsp,bloc(B)), [(X, types(TypeArgsp,void))|Ctx]) :- checkArgsType(Argsp, TypeArgsp), append(Argsp, Ctx, CtxRes), cmdsType(CtxRes, B, void).
defType(Ctx,procrec(X,Argsp,bloc(B)), [(X, types(TypeArgsp,void))|Ctx]) :- checkArgsType(Argsp, TypeArgsp), append(Argsp, Ctx, CtxRes), cmdsType([(X,types(TypeArgsp,void))|CtxRes], B, void).
defType(Ctx,funp(X,T,Argsp,bloc(B)), [(X, types(TypeArgsp,T))|Ctx]) :- checkArgsType(Argsp, TypeArgsp), append(Argsp, Ctx, CtxRes), cmdsType(CtxRes, B, T).
defType(Ctx,funrecp(X,T,Argsp,bloc(B)), [(X, types(TypeArgsp,T))|Ctx]) :- checkArgsType(Argsp, TypeArgsp), append(Argsp, Ctx, CtxRes), cmdsType([(X,types(TypeArgsp,T))|CtxRes], B, T).

%% Check a statement
%% statType(ctx, stat, type)
statType(Ctx,echo(E),void) :- exprType(Ctx,E,int).
statType(Ctx,set(X,E),void) :- exprType(Ctx,X,T), exprType(Ctx,E,T).
statType(Ctx,ifstat(E,bloc(Bk1),bloc(Bk2)),T) :- exprType(Ctx,E,bool), cmdsType(Ctx, Bk1, T), cmdsType(Ctx, Bk2, T).
statType(Ctx,ifstat(E,bloc(Bk1),bloc(Bk2)),union(T,void)) :- T\=void, exprType(Ctx,E,bool), cmdsType(Ctx, Bk1, void), cmdsType(Ctx, Bk2, T).
statType(Ctx,ifstat(E,bloc(Bk1),bloc(Bk2)),union(T,void)) :- T\=void, exprType(Ctx,E,bool), cmdsType(Ctx, Bk1, T), cmdsType(Ctx, Bk2, void).
statType(Ctx,while(E, bloc(Bk)),union(T,void)) :- exprType(Ctx,E,bool), cmdsType(Ctx, Bk, T).
statType(Ctx,call(X,Es),void) :- checkExprsListTypeP(Ctx, Es, T), inCtx(Ctx, X, types(T,void)).

%% Check an expar
%% exparType(ctx, expr, type)
exparType(Ctx,adr(ident(X)),ref(T)) :- inCtx(Ctx,X,ref(T)).
exparType(Ctx,expr(X),T) :- exprType(Ctx,X,T), \+(T=ref(_)).

%% Check an expression
%% exprType(ctx, expr, type)
exprType(_,true,bool).
exprType(_,false,bool).
exprType(_,N,int) :- integer(N).
exprType(Ctx,ident(X),T) :- inCtx(Ctx,X,T).
exprType(Ctx,ident(X),T) :- inCtx(Ctx,X,ref(T)).
exprType(Ctx,if(E1,E2,E3),T) :- exprType(Ctx,E1,bool), exprType(Ctx,E2,T), exprType(Ctx,E3,T).
exprType(Ctx,not(E),bool) :- exprType(Ctx,E,bool).
exprType(Ctx,and(E1,E2), bool) :- exprType(Ctx,E1,bool), exprType(Ctx,E2,bool).
exprType(Ctx,or(E1,E2), bool) :- exprType(Ctx,E1,bool), exprType(Ctx,E2,bool).
exprType(Ctx,eq(E1,E2), bool) :- exprType(Ctx,E1,int), exprType(Ctx,E2,int).
exprType(Ctx,lt(E1,E2), bool) :- exprType(Ctx,E1,int), exprType(Ctx,E2,int).
exprType(Ctx,add(E1,E2), int) :- exprType(Ctx,E1,int), exprType(Ctx,E2,int).
exprType(Ctx,sub(E1,E2), int) :- exprType(Ctx,E1,int), exprType(Ctx,E2,int).
exprType(Ctx,mul(E1,E2), int) :- exprType(Ctx,E1,int), exprType(Ctx,E2,int).
exprType(Ctx,div(E1,E2), int) :- exprType(Ctx,E1,int), exprType(Ctx,E2,int).
exprType(Ctx,app(E,Es),Tr) :- checkExprsListType(Ctx, Es, Ts), exprType(Ctx,E, types(Ts,Tr)).
exprType(Ctx,funabs(Xs,E),types(T,Tr)) :- checkArgsType(Xs, T), append(Xs,Ctx,Ctx2), exprType(Ctx2, E, Tr).
exprType(Ctx,alloc(E), vec(_)) :- exprType(Ctx,E,int).
exprType(Ctx,len(E), int) :- exprType(Ctx,E,vec(_)).
exprType(Ctx,nth(E1,E2),T) :- exprType(Ctx,E1,vec(T)), exprType(Ctx,E2,int).

typeCheck(P,ok) :- progType(P,void).
typeCheck(_,ko).

exitCode(ok) :- halt(0).
exitCode(_) :- halt(1).

main_stdin :- 
    read(user_input,T),
    typeCheck(T,R),
    print(R),
    nl,
    exitCode(R).