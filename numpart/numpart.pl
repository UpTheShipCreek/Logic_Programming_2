:- use_module(library(ic)).
:- use_module(library(ic_global)).

squares_sum(List,SquaredSum):-squares_sum(List,List,0,SquaredSum).
squares_sum(_,[],SquaredSum,SquaredSum).
squares_sum(List,[N|RemainingList],SoFarSum,SquaredSum):-
    NewSum #= SoFarSum + N*N,
    squares_sum(List,RemainingList,NewSum,SquaredSum).

check(List,N):-
    %calculate the n(n+1)/4 and check
    SumsoNums #= (N*(N+1))/4,
    ordered_sum(List,S1),
    SumsoNums #= S1,
    %calculate the n(n+1)(2*n+1)/12 and check
    SumsoNumSquares #= (N*(N+1)*((2*N)+1))/12,
    squares_sum(List,S2),
    SumsoNumSquares #= S2.

numpart(N, [1|L1], L2):-
    N1 #= (N/2)-1,N2 #= N/2,
    length(L1,N1),length(L2,N2),
    L1 :: 2..N,L2 :: 2..N,
    append([1|L1],L2,L),
    ic:alldifferent(L),
    check([1|L1],N),labeling(L1),
    sorted(L2,L2),%disjoint(L1,L2),
    labeling(L2).


