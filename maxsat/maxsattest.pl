:-set_flag(print_depth, 1000).

:- use_module(library(ic)).
:- use_module(library(branch_and_bound)).

%index of an element in a list starting from 1
index(_, [], _):-fail.
index(Element,[Element|_],1).
index(Element,[_|List],Index):-
    Index0 #= Index - 1,
    index(Element,List,Index0).

% Doesn't return the global minimum in some cases but I've spent too much time trying to find the bug with zero progress so...
% I giving you this, since it is fast and correct most of the time
maxsat(NV,NC,D,F,S,M):-
    length(S,NV),
    S #:: 0..1,
    M #:: 1..NC,
    create_formula(NV,NC,D,F),
    formula_eval(F,S,(M,_)),
    Cost #= NC - M, %+ V, % minimize the number of false clauses (and false literals)
    bb_min(search(S, 0, input_order, indomain, complete, []), Cost, bb_options{}).

formula_eval(F,S,HeuristicDuplet):-
    formula_eval(F,F,S,(0,0),HeuristicDuplet),!.
formula_eval(_,[],_,HeuristicDuplet,HeuristicDuplet):-!.
formula_eval(F,[Clause|RestClauses],S,(SoFarTrue,SoFarViolated),HeuristicDuplet):-
    clause_eval(Clause,S,Result),
    length(Clause,Literals), Violated_Literals #= Literals - Result, %was thinking of incorporating the overall violated literals into the cost function but it is not helpful 
    ((Result #> 0, Add #= 1); Add #= 0),   
    NewSoFarV #= SoFarViolated + Violated_Literals,
    NewSoFarT #= SoFarTrue + Add,
    formula_eval(F,RestClauses,S,(NewSoFarT,NewSoFarV),HeuristicDuplet).

clause_eval(Clause,S,Result):- 
    clause_eval(Clause,Clause,S,0,Result),!.
clause_eval(_,[],_,Result,Result).
clause_eval(Clause,[Var|RestCl],S,SoFarRes,Result):- %Clause should be a list of form [X1,-X2,...]
    (abs(Var,Var),index(Ele,S,Var),NewRes #= (SoFarRes + Ele),clause_eval(Clause,RestCl,S,NewRes,Result);
    \+abs(Var,Var),abs(Var,Pos),index(Ele,S,Pos),NewRes #= (SoFarRes + (1 - Ele))),clause_eval(Clause,RestCl,S,NewRes,Result).
