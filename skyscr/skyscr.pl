:-lib(ic).
:-lib(ic_global).

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

%nvalue(??N, ??Vars)
%The collection Vars contains N different values.
%This constraint ensures that the collection Vars contains N different integer values. The value of N can therefore lie between 1 and the length of the list.

%index of an element in a list starting from 1

skyscr(PuzzleId, Grid):-
    %getting the parameters
    puzzle(PuzzleId,N,VerticalLeftCon,VerticalRightCon,HorizontalTopCon,HorizontalBottomCon,Grid),
    %domain 
    Grid::1..N, 
    %constraints
    matrix_rows_constraints(Grid,VerticalLeftCon,VerticalRightCon), 
    %write("Past row constraints"),nl,
    matrix_column_constraints(Grid,HorizontalTopCon,HorizontalBottomCon), 
    %write("Past column constraints"),nl,
    %search
    search(Grid,0,input_order,indomain,complete,[]).
    %labeling(Grid).

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
    ic:max(FirstNElements,Element),
    N1 #= N + 1,
    row_to_possible_max(Length,N1,Row,[Element|SoFarList],MaxList).

row_constraints(Row,0):-
    ic:alldifferent(Row).
row_constraints(Row,NumberofVisibleSkyscr):- %essentially this is the visible skyscrapers constraint
    %all different constraint 
    ic:alldifferent(Row), 
    %visible skyscrapers constraints
    row_to_possible_max(Row,MaxList), 
    nvalue(NumberofVisibleSkyscr,MaxList).

matrix_rows_constraints([],[],[]):-!.
matrix_rows_constraints([Row|Matrix],[VL|VerticalLeftCon],[VR|VerticalRightCon]):- %visible skyscraper constraint from all rows in the matrix, both ways
    %length([Row|Matrix],Length),
    %write("Length "),write(Length),nl,
    row_constraints(Row,VL),
    %write("Past the left to right constraints"),nl,
    reverse(Row,ReverseRow),
    row_constraints(ReverseRow,VR),
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
    element(TransposedIndex,TransposedRow,Element),!,
    C1 is Counter + 1,
    transpose(N,C1,Matrix,RestFlat,Transpose).

matrix_column_constraints(Matrix,HorizontalTopCon,HorizontalBottomCon):- %constraints for the columns aka the rows of the transposed matrix
    length(Matrix,N),
    %write("I bet this is the last message I can see"),nl,
    transpose(N,Matrix,Transpose),
    %write("I think it is failing here"),nl,
    matrix_rows_constraints(Transpose,HorizontalTopCon,HorizontalBottomCon).
    
