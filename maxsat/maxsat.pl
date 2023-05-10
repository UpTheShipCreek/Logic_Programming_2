:- use_module(library(ic)).
:- use_module(library(ic_global)).

%index of an element in a list starting from 1
index(_, [], _):-fail.
index(Element,[Element|_],1).
index(Element,[_|List],Index):-
  index(Element,List,Index0),succ(Index0,Index).

head([H|_],H).

%maxsat(NV, NC, D, F, S, M)
%   NV -> Number of Variables
%   NC -> Number of Clauses
%   D -> Density
%   F -> Formula of form F = [[-4, -5], [-3, -4], [2, -3, 4], [-2, -3, -4, 5], [1]]
%   S -> True False List of the Variables
%   M -> Max number of Clauses that is true

maxsat(NV,NC,D,F,S,M):-
    length(S,NV),
    create_formula(NV,NC,D,F),
    S :: 0..1,
    formula_eval(F,S,M),
    find_max(NC,M),
    labeling(S).

find_max(NC,M):-
    M #= NC;
    NNC is NC-1, find_max(NNC,M).

formula_eval(F,S,NumOfTrueClauses):-
    formula_eval(F,F,S,0,NumOfTrueClauses),!.
formula_eval(_,[],_,NumOfTrueClauses,NumOfTrueClauses):-!.
formula_eval(F,[Clause|RestClauses],S,SoFarNum,NumOfTrueClauses):-
    clause_eval(Clause,S,Result),
    (Result #> 0, Add #= 1; Add #=0),
    NewSoFar #= SoFarNum + Add,
    formula_eval(F,RestClauses,S,NewSoFar,NumOfTrueClauses).

clause_eval(Clause,S,Result):- 
    clause_eval(Clause,Clause,S,0,Result).
clause_eval(_,[],_,Result,Result).
clause_eval(Clause,[Var|RestCl],S,SoFarRes,Result):- %Clause should be a list of form [X1,-X2,...]
    (abs(Var,Var),index(Ele,S,Var),NewRes #= (SoFarRes + Ele),clause_eval(Clause,RestCl,S,NewRes,Result);
    \+abs(Var,Var),abs(Var,Pos),index(Ele,S,Pos),NewRes #= (SoFarRes + (1 - Ele))),clause_eval(Clause,RestCl,S,NewRes,Result).
