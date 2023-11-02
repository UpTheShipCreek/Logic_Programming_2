:-lib(gfd).
:-set_flag(print_depth, 1000).

create_grid(Grid,Rows,Columns):-
    length(Grid,Rows),
    create_columns(Grid,Columns).
create_columns([],_).
create_columns([Element|Rows],Columns):-
    length(Element,Columns),
    create_columns(Rows,Columns).

index(_, [], _):-fail.
index(1,[Element|_],Element).
index(Index,[_|List],Element):-
    Index0 #= Index - 1,
    index(Index0,List,Element).

boolean_seave([],[],[]).
boolean_seave([Bool|Booleans],[Element|List],[Element|Remaining]):-
    Bool #= 1,
    boolean_seave(Booleans,List,Remaining).
boolean_seave([Bool|Booleans],[_|List],Remaining):-
    Bool #\= 1,
    boolean_seave(Booleans,List,Remaining).

assignment_csp(NumberOfWorkers,MaxTime,Grid):-
    % Getting the size of the grid
    findall(activity(Name,act(Start,End)),activity(Name,act(Start,End)),AllActivities),
    length(AllActivities,NumberOfAllActivities),
    create_grid(Grid,NumberOfWorkers,NumberOfAllActivities),
    %write("Created Empty Grid"),nl,
    % The domain of the Grid
    Grid :: 0..1,  
    % Mirroring constraints
    %write("-------------Mirror constraints------------------"),nl,
    mirroring_constraints(Grid),
    %write("-------------Mirror constraints------------------"),nl,
    % Row constraints
    %write("-------------Row constraints------------------"),nl,
    row_constraints(Grid,AllActivities,MaxTime),
    %write("-------------Row constraints------------------"),nl,
    % Column constraints
    %write("-------------Column constraints------------------"),nl,
    transpose(Grid,Transpose),
    columns_constraints(Transpose),
    %write("-------------Column constraints------------------"),nl,
    % Search
    flatten(Grid,Flat),
    %write("Flattened and going to search"),nl,
    %labeling(Flat).
    search(Flat,0,most_constrained,indomain,complete,[]).
   

mirroring_constraints([]).
mirroring_constraints([B1|BooleansList]):- % we need to somehow express that row n can only get a true value only if row n-1 has taken a true value
    (sumlist(B1,N1), N1 #> 0,mirroring_constraints(BooleansList)); % either the n - 1 worker has taken a job
    (flatten(BooleansList,Flat),sumlist(Flat,N), N #= 0). % or no one further from n-1 has any jobs either

columns_constraints([]).
columns_constraints([Column|Rest]):-
    sumlist(Column,1),
    columns_constraints(Rest).

row_constraints([],_,_).
row_constraints([Booleans|BooleansList],Activities,MaxTime):-
    %member(Booleans,BooleansList),
    %%write("Scheduling constraints"),nl,
    row_scheduling_constraints(Booleans,Activities),
    %%write("Time constraints"),nl,
    row_time_constraints(Booleans,Activities,MaxTime),
    %%write("Recursive Call"),nl,
    row_constraints(BooleansList,Activities,MaxTime).

row_scheduling_constraints(Booleans,Activities):-
    %%write("Going into the seave"),nl,
    write(Booleans),%write(" "),%write(Activities),nl,
    boolean_seave(Booleans,Activities,RemainingActivities),
    %%write(RemainingActivities),nl,
    plausible_activities(RemainingActivities),!.

plausible_activities([]).
plausible_activities([_]).
plausible_activities([activity(_, act(_,End1)),activity(_, act(Start2,End2))|Rest]):-
    Start2 #> End1,
    plausible_activities([activity(_, act(Start2,End2))|Rest]).

row_time_constraints([],_,T):-
    T#>=0.
row_time_constraints([Bool|Row],[Activity|RestActivities],Time):-
    Time #>= 0,
    %%write(Time), nl,
    activity_duration(Activity,Duration),
    RemainingTime #= Time - (Bool*Duration),
    row_time_constraints(Row,RestActivities,RemainingTime).

activity_duration(activity(_, act(Start,End)),Duration):-
    Duration #= End - Start,!.

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).