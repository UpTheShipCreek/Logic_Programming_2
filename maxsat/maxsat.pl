:- use_module(library(ic)).

%index of an element in a list starting from 1
%index(_, [], _):-fail.
%index(Element,[Element|_],1).
%index(Element,[_|List],Index):-
%  index(Element,List,Index0),succ(Index0,Index).

%element(?Index, +Values, ?Element)
    %Element is the Index'th element of the collection Values.

head([H|_],H).
rhead([_|T],H):-reverse(T,R),head(R,H).

print_list([]).
print_list([H|T]) :-
   write(H), nl,
   print_list(T).

%maxsat(NV, NC, D, F, S, M)
%   NV -> Number of Variables
%   NC -> Number of Clauses
%   D -> Density
%   F -> Formula of form F = [[-4, -5], [-3, -4], [2, -3, 4], [-2, -3, -4, 5], [1]]
%   S -> True False List of the Variables
%   M -> Max number of Clauses that is true

maxsat(NV,NC,D,F,S,M):-
    length(S,NV),
    S #:: 0..1,
    N #:: 1..NC,
    M #:: 1..NC,
    %create_formula(NV,NC,D,F),
    %findall(N,formula_eval(F,S,N),List),
    %member(List,Member),
    %get_max(Member,N),
    formula_eval(F,S,N),
    %max_check(F,S,N),
    get_max(N,M),
    labeling(S).

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
