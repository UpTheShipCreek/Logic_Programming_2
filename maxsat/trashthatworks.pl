:- use_module(library(ic)).

find(NV,NC,D,F,S,M):-
    length(S,NV),
    S #:: 0..1,
    M #:: 1..NC,
    create_formula(NV,NC,D,F),
    formula_eval(F,S,M),
    labeling(S).

maxsat(NV,NC,D,F,S,Max):-
    maxsat(NV,NC,D,F,S,M,Max).
maxsat(NV,NC,D,F,S,M,Max):-
    findall(M,find(NV,NC,D,F,S,M),List),
    maxlist(List,Max).

formula_eval(F,S,NumOfTrueClauses):-
    formula_eval(F,F,S,0,NumOfTrueClauses),!.
formula_eval(_,[],_,NumOfTrueClauses,NumOfTrueClauses):-!.
formula_eval(F,[Clause|RestClauses],S,SoFarNum,NumOfTrueClauses):-
    clause_eval(Clause,S,Result),
    %(Result #> 0, ac_eq(NewSoFar,SoFarNum,1); ac_eq(NewSoFar,SoFarNum,0)),   
    ((Result #> 0, Add #= 1); Add #= 0),   
    NewSoFar #= SoFarNum + Add,
    formula_eval(F,RestClauses,S,NewSoFar,NumOfTrueClauses).

clause_eval(Clause,S,Result):- 
    clause_eval(Clause,Clause,S,0,Result),!.
clause_eval(_,[],_,Result,Result).
clause_eval(Clause,[Var|RestCl],S,SoFarRes,Result):- %Clause should be a list of form [X1,-X2,...]
    (abs(Var,Var),element(Var,S,Ele),NewRes #= (SoFarRes + Ele),clause_eval(Clause,RestCl,S,NewRes,Result);
    \+abs(Var,Var),abs(Var,Pos),element(Pos,S,Ele),NewRes #= (SoFarRes + (1 - Ele))),clause_eval(Clause,RestCl,S,NewRes,Result).
