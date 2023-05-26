:-lib(gfd).

head([H|_],H).
tail([_|T],T).

index(_, [], _):-fail.
index(1,[Element|_],Element).
index(Index,[_|List],Element):-
    Index0 #= Index - 1,
    index(Index0,List,Element).

count_to(N,N).
count_to(C,N):-
    N>1,
    N0 is N - 1,
    count_to(C,N0).

get_greater(X,Y,X):-
    X #>= Y.
get_greater(X,Y,Y):-
    X #< Y.
    
skyscr(PuzzleId, Grid):-
    %getting the parameters
    puzzle(PuzzleId,N,VerticalLeftCon,VerticalRightCon,HorizontalTopCon,HorizontalBottomCon,Grid),
    %write("Got the puzzle"),nl,
    %domain 
    Grid :: 1..N, 
    %write("Gave the domain to the grid"), nl,
    %constraints
    matrix_rows_constraints(Grid,VerticalLeftCon,VerticalRightCon), 
    %write("Passed matrix row constraints"),nl,
    matrix_column_constraints(Grid,HorizontalTopCon,HorizontalBottomCon), 
    %write("Passed matrix column constraints"),nl,
    %search
    flatten(Grid,FlattenedGrid),
    %search(L,0,input_order,indomain,complete,[]).
    search(FlattenedGrid,0,input_order,indomain,complete,[]),!.

create_an_empty_square_matrix(N,EmptyMatrix):-
    create_an_empty_square_matrix(N,N,EmptyMatrix).
create_an_empty_square_matrix(_,0,[]):-!.
create_an_empty_square_matrix(N,C,[Row|RestMatrix]):-
    length(Row,N),
    C0 #= C - 1,
    create_an_empty_square_matrix(N,C0,RestMatrix).

first_n(0,_,[]):-!. %creates a list using the first N elements of another
first_n(N,[Element|List],[Element|FirstNList]):-
    N0 is N-1,
    first_n(N0,List,FirstNList).

row_to_possible_max(Row,MaxList):- %creates the [Z1,Z2,...] list from the lecture, where the list is created by putting the max possible value at index n using only the first n-elements of the X Row
    length(Row,Length),row_to_possible_max(Length,1,Row,[],MaxList).
row_to_possible_max(Length,L,_,Reverse,MaxList):- 
    L #=  Length + 1, 
    reverse(Reverse,MaxList),!.
row_to_possible_max(Length,N,Row,SoFarList,MaxList):-
    first_n(N,Row,FirstNElements),
    gfd:max(FirstNElements,Element),
    N1 #= N + 1,
    row_to_possible_max(Length,N1,Row,[Element|SoFarList],MaxList).

row_constraints(Row,0):-
    alldifferent(Row).
row_constraints(Row,NumberofVisibleSkyscr):- %essentially this is the visible skyscrapers constraint
    %all different constraint 
    alldifferent(Row), 
    %visible skyscrapers constraints
    row_to_possible_max(Row,MaxList), 
    nvalues(MaxList,(#=),NumberofVisibleSkyscr).

matrix_rows_constraints([],[],[]):-!.
matrix_rows_constraints([Row|Matrix],[VL|VerticalLeftCon],[VR|VerticalRightCon]):- %visible skyscraper constraint from all rows in the matrix, both ways
    %write("Entring row constraints"),nl,
    row_constraints(Row,VL),
    %write("Passed the constraints some row"),nl,
    reverse(Row,ReverseRow),
    %write("Reversed the row"),nl,
    row_constraints(ReverseRow,VR),
    %write("Passed the constraints for the reverse"),nl,
    matrix_rows_constraints(Matrix,VerticalLeftCon,VerticalRightCon).

transpose(N,Matrix,Transpose):-
    flatten(Matrix,Flat),
    create_an_empty_square_matrix(N,Transpose),
    transpose(N,0,Matrix,Flat,Transpose),!.
transpose(_,_,_,[],_).
transpose(N,Counter,Matrix,[Element|RestFlat],Transpose):-
    div(Counter,N,IndexCounter),
    mod(Counter,N,RowCounter),
    TransposedIndex is IndexCounter + 1,
    TransposedRowIndex is RowCounter + 1,
    index(TransposedRowIndex,Transpose,TransposedRow),
    index(TransposedIndex,TransposedRow,Element),!,
    C1 is Counter + 1,
    transpose(N,C1,Matrix,RestFlat,Transpose).

matrix_column_constraints(Matrix,HorizontalTopCon,HorizontalBottomCon):- %constraints for the columns aka the rows of the transposed matrix
    length(Matrix,N),
    transpose(N,Matrix,Transpose),
    matrix_rows_constraints(Transpose,HorizontalTopCon,HorizontalBottomCon).
    
