%% inCtx(ctx, var, type)

inCtx([(V,T)|_],V,T).
inCtx([(_,_)|C],V,T) :- inCtx(C,V,T).

checkArgsType([],[]).
checkArgsType([(_,T)|ArgsList],[T|TypeList]) :- checkArgsType(ArgsList,TypeList).

checkListType(_,[],[]).
checkListType(Ctx, [E|ExprList], [T|TypeList]) :- exprType(Ctx, E, T), checkListType(Ctx, ExprList, TypeList).

%% Check a program
%% progType(prog, void)

progType(prog(Cs),void) :- cmdsType([], Cs, void).

%% Check commands
%% cmdsType(Ctx, cmds, type)

cmdsType(Ctx, [Stat|Cs], void) :- statType(Ctx, Stat, void), cmdsType(Ctx, Cs, void).
cmdsType(Ctx, [Def|Cs], void) :- defType(Ctx, Def, CtxRes), cmdsType(CtxRes, Cs, void).
cmdsType(_,[],void).

%% Check a definition
%% defType(Ctx, def, CtxRes)

defType(Ctx, const(X,T,E), [(X,T)|Ctx]) :- exprType(Ctx,E,T).
defType(Ctx,fun(X,Tr,Xts,E), [(X, types(T,Tr))|Ctx]) :- checkArgsType(Xts, T), append(Xts, Ctx, CtxRes), exprType(CtxRes, E, Tr).
defType(Ctx,funrec(X,Tr,Xts,E), [(X, types(T,Tr))|Ctx]) :- checkArgsType(Xts, T), append(Xts, Ctx, CtxRes), exprType([(X,types(T,Tr))|CtxRes],E,Tr).

%% Check a statement
%% statType(ctx, stat, type)

statType(Ctx,echo(E),void) :- exprType(Ctx,E,int).

%% Check an expression
%% exprType(ctx, expr, type)

exprType(_,true,bool).
exprType(_,false,bool).
exprType(_,N,int) :- integer(N).
exprType(Ctx,ident(X),T) :- inCtx(Ctx,X,T).
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
exprType(Ctx,app(E,Es),Tr) :- checkListType(Ctx, Es, Ts), exprType(Ctx,E, types(Ts,Tr)).
exprType(Ctx,funabs(Xs,E),types(T,Tr)) :- checkArgsType(Xs, T), append(Xs,Ctx,Ctx2), exprType(Ctx2, E, Tr).

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