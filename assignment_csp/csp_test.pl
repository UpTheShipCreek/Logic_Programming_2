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

boolean_sheeve(Booleans,List,Remaining):-
    length(Booleans,Number),
    length(List,Number),
    findall(Element,(element(Index,Booleans,1),element(Index,List,Element)),Remaining).

assignment_csp(NumberOfWorkers,MaxTime,Grid):-
    % Getting the size of the grid
    findall(activity(Name,act(Start,End)),activity(Name,act(Start,End)),AllActivities),
    length(AllActivities,NumberOfAllActivities),
    create_grid(Grid,NumberOfWorkers,NumberOfAllActivities),
    write("Created Empty Grid"),nl,
    % The domain of the Grid
    Grid :: 0..1,  
    % Row constraints
    write("-------------Row constraints------------------"),nl,
    row_constraints(Grid,AllActivities,MaxTime),
    write("-------------Row constraints------------------"),nl,
    % Column constraints
    write("-------------Column constraints------------------"),nl,
    transpose(NumberOfWorkers,NumberOfAllActivities,Grid,Transpose),
    columns_constraints(Transpose),
    write("-------------Column constraints------------------"),nl,
    % Search
    flatten(Grid,Flat),
    write("Flattened and going to search"),nl,
    search(Flat,0,input_order,indomain,complete,[]).

columns_constraints([]).
columns_constraints([Column|Rest]):-
    sumlist(Column,1),
    columns_constraints(Rest).

row_constraints(Booleans,Activities,MaxTime):-
    write("Scheduling constraints"),nl,
    row_scheduling_constraints(Booleans,Activities),
    write("Time constraints"),nl,
    row_time_constraints(Booleans,Activities,MaxTime).

row_scheduling_constraints(Booleans,Activities):-
    write("Going into the sheeve with "),write(Booleans),write(" "),write(Activities),
    nl,
    boolean_sheeve(Booleans,Activities,RemainingActivities),
    write(RemainingActivities),nl,
    plausible_activities(RemainingActivities).

plausible_activities([]).
plausible_activities([_]).
plausible_activities([activity(_, act(_,End1)),activity(_, act(Start2,End2))|Rest]):-
    Start2 #> End1,
    plausible_activities([activity(_, act(Start2,End2))|Rest]),!.

row_time_constraints([Bool|Row],[Activity|RestActivities],Time):-
    Time #>= 0,
    activity_duration(Activity,Duration),
    RemainingTime #= Time - (Bool*Duration),
    row_time_constraints(Row,RestActivities,RemainingTime).

activity_duration(activity(_, act(Start,End)),Duration):-
    Duration #= End - Start.

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



    