% orVoid(type)
simplifier(orVoid(orVoid(T)), orVoid(T)).
simplifier(orVoid(void), void).
simplifier(T, T) :- not(T = orVoid(orVoid(_))), not(T = orVoid(void)).


%% inCtx(ctx, var, type)

inCtx([(V,T)|_],V,T).
inCtx([(_,_)|C],V,T) :- inCtx(C,V,T).

checkArgsType([],[]).
checkArgsType([(_,T)|ArgsList],[T|TypeList]) :- checkArgsType(ArgsList,TypeList).

checkListType(_,[],[]).
checkListType(Ctx, [E|ExprList], [T|TypeList]) :- exprType(Ctx, E, T), checkListType(Ctx, ExprList, TypeList).

checkListTypeP(_,[],[]).
checkListTypeP(Ctx, [E|ExprList], [T|TypeList]) :- exparType(Ctx, E, T), checkListTypeP(Ctx, ExprList, TypeList).

%% Check a program
%% progType(prog, void)

progType(prog(Cs),void) :- cmdsType([], Cs, void).

%% Check a block
%% blockType(ctx,block, void)

blockType(ctx,block(Cs),T) :- cmdsType(ctx, Cs, T).

%% Check commands
%% cmdsType(Ctx, cmds, type)

cmdsType(Ctx, [Def|Cs], T) :- defType(Ctx, Def, CtxRes), cmdsType(CtxRes, Cs, T).
cmdsType(Ctx, [Stat|Cs], T) :- statType(Ctx, Stat, void), cmdsType(Ctx, Cs, T).
cmdsType(Ctx, [Stat|Cs], T) :- statType(Ctx, Stat, orVoid(T)), cmdsType(Ctx, Cs, T).
cmdsType(Ctx, [Stat], T) :- statType(Ctx, Stat, T).
cmdsType(Ctx, [return(E)], T) :- exprType(Ctx, E, T).
cmdsType(_,[],void).

%% Check a definition
%% defType(Ctx, def, CtxRes)

defType(Ctx, const(X,T,E), [(X,T)|Ctx]) :- exprType(Ctx,E,T).
defType(Ctx,fun(X,Tr,Xts,E), [(X, types(T,Tr))|Ctx]) :- checkArgsType(Xts, T), append(Xts, Ctx, CtxRes), exprType(CtxRes, E, Tr).
defType(Ctx,funrec(X,Tr,Xts,E), [(X, types(T,Tr))|Ctx]) :- checkArgsType(Xts, T), append(Xts, Ctx, CtxRes), exprType([(X,types(T,Tr))|CtxRes],E,Tr).
defType(Ctx,var(X,ref(T)),[(X,ref(T))|Ctx]).
defType(Ctx,proc(X,Xts,block(B)), [(X, types(T,void))|Ctx]) :- checkArgsType(Xts, T), append(Xts, Ctx, CtxRes), cmdsType(CtxRes, B, void).
defType(Ctx,procrec(X,Xts,block(B)), [(X, types(T,void))|Ctx]) :- checkArgsType(Xts, T), append(Xts, Ctx, CtxRes), cmdsType([(X,types(T,void))|CtxRes], B, void).
defType(Ctx,funproc(X,Tr,Xts,block(B)), [(X, types(T,Tr))|Ctx]) :- checkArgsType(Xts, T), append(Xts, Ctx, CtxRes), cmdsType(CtxRes, B, Tr).
defType(Ctx,funprocrec(X,Tr,Xts,block(B)), [(X, types(T,Tr))|Ctx]) :- checkArgsType(Xts, T), append(Xts, Ctx, CtxRes), cmdsType([(X,types(T,Tr))|CtxRes], B, Tr).

%% Check a statement
%% statType(ctx, stat, type)

statType(Ctx,echo(E),void) :- exprType(Ctx,E,int).
statType(Ctx,set(X,E),void) :- inCtx(Ctx,X,ref(T)), exprType(Ctx,E,T).
statType(Ctx,set(L,E),void) :- exprType(Ctx,L,T), exprType(Ctx,E,T).
statType(Ctx,ifstat(C,block(B1),block(B2)),T) :- exprType(Ctx,C,bool), cmdsType(Ctx, B1, T), cmdsType(Ctx, B2, T).
statType(Ctx,ifstat(C,block(B1),block(B2)),Ti) :- exprType(Ctx,C,bool), cmdsType(Ctx, B1, T), cmdsType(Ctx, B2, void), simplifier(orVoid(T),Ti).
statType(Ctx,ifstat(C,block(B1),block(B2)),Ti) :- exprType(Ctx,C,bool), cmdsType(Ctx, B1, void), cmdsType(Ctx, B2, T), simplifier(orVoid(T),Ti).
statType(Ctx,while(C, block(B)),orVoid(T)) :- exprType(Ctx,C,bool), cmdsType(Ctx, B, T).
statType(Ctx,call(X,E),void) :- checkListTypeP(Ctx, E, T), inCtx(Ctx, X, types(T,void)).


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
exprType(Ctx,app(E,Es),Tr) :- checkListType(Ctx, Es, Ts), exprType(Ctx,E, types(Ts,Tr)).
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