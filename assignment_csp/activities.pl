activity(a01, act(0,3)).
activity(a02, act(0,4)).
activity(a03, act(1,5)).
activity(a04, act(4,6)).
activity(a05, act(6,8)).
activity(a06, act(6,9)).
activity(a07, act(9,10)).
activity(a08, act(9,13)).
activity(a09, act(11,14)).
activity(a10, act(12,15)).
activity(a11, act(14,17)).
activity(a12, act(16,18)).
activity(a13, act(17,19)).
activity(a14, act(18,20)).
activity(a15, act(19,20)).

write_to_file(W,H) :-
    tell('C:\\Users\\north\\Desktop\\Uni\\Spring\\Logic_Programming\\Ergasia1\\assignment\\assignment.out'),
    findall(A, assignment(W, H, A, _), Solutions),
    length(Solutions, N),
    writeq(Solutions),
    nl, writeq(N),
    nl,
    told.
